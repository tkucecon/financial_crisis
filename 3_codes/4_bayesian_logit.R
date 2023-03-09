
# ------------------------------------------------------------------------------
# About this code
# Estimate hierarchical Bayesian logit model
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("lubridate")
  library("haven")
  library("ggthemes")
  theme_set(theme_solarized())
  library("patchwork")
  
  library("tidymodels")
  library("rstan")
  library("bife")

# ------------------------------------------------------------------------------
# Load the JST data
# ------------------------------------------------------------------------------
  
  # load the JST data
  load("../4_data/df_JST.rda")
  
  # create a recipe to impute the variables
  recipe.bag <- 
    recipe(x = df.JST, formula = as.formula(crisis ~ .)) %>% 
    # remove year and country from the predictors %>% 
    update_role(year,    new_role = "id variable") %>% 
    update_role(country, new_role = "time variable") %>% 
    # normalize all the predictors to be distributed ~ N(0,1) 
    step_normalize(all_predictors(), skip = FALSE) %>% 
    # delete rows with no variance
    step_zv(all_predictors(), skip = FALSE) 
  
  # create a data frame with imputed variables
  df.JST <- 
    recipe.bag %>% 
    prep() %>% 
    bake(new_data = df.JST)
  
  # keep only the relevant variables and remove NA rows
  df.JST <- 
    df.JST %>% 
    select(year, country, crisis, 
           starts_with("credit"),
           starts_with("slope")) %>% 
    na.omit()

# ------------------------------------------------------------------------------
# Check the result of narrow variables with constant parameters (pooled)
# ------------------------------------------------------------------------------
  
  # create a model of narrow constant logistic model
  model.pooled.const <- 
    glm(crisis ~ credit.glo + slope.glo + credit.dom + slope.dom,
        family = binomial(),
        data = df.JST)

  # check the summary
  summary(model.pooled.const)
  
  # save the estimates (may be used as the prior later)
  mu.i  <- model.pooled.const$coefficients["(Intercept)"]
  mu.cd <- model.pooled.const$coefficients["credit.dom"]
  mu.cg <- model.pooled.const$coefficients["credit.glo"]
  mu.sd <- model.pooled.const$coefficients["slope.dom"]
  mu.sg <- model.pooled.const$coefficients["slope.glo"]

# ------------------------------------------------------------------------------
# Check the result of panel analysis
# ... now considers the difference among countries
# ------------------------------------------------------------------------------
  
  # create the country ID according to countries
  df.JST.panel <- 
    df.JST %>% 
    group_by(country) %>% 
    mutate(ctry.id = cur_group_id()) %>% 
    ungroup()
  
  # create a data frame indicating country and ID 
  df.ctry.id <- 
    df.JST.panel %>% 
    select(country, ctry.id) %>% 
    unique() %>% 
    arrange(ctry.id)
  
  # input data
  data.panel.const <- 
    list(I        = nrow(df.JST.panel), 
         C        = max(df.JST.panel$ctry.id),
         Cre_d    = df.JST.panel$credit.dom,
         Cre_g    = df.JST.panel$credit.glo,
         Slo_d    = df.JST.panel$slope.dom,
         Slo_g    = df.JST.panel$slope.glo,
         Crisis   = as.numeric(df.JST.panel$crisis) - 1,
         CID      = df.JST.panel$ctry.id,
         b_i_pri  = mu.i,
         b_cd_pri = mu.cd,
         b_cg_pri = mu.cg,
         b_sd_pri = mu.sd,
         b_sg_pri = mu.sg
    )
  
  # fit the Bayesian model
  fit.panel.const <- 
    stan(file   = "4_panel_constant.stan",
         data   = data.panel.const,
         seed   = 2292,
         iter   = 5000,
         warmup = 2000,
         pars   = c("b_cd",  "b_cg",  "b_sd",  "b_sg",
                    "mu_cd", "mu_cg", "mu_sd", "mu_sg",
                    "s_cd",  "s_cg",  "s_sd",  "s_sg"))
  
  # This process takes some time... save the result in the tmp folder
  save(fit.panel.const, file = "../5_tmp/fit_panel_const.rda")
  
  # extract the fitted model
  extracted.panel.const <- 
    extract(fit.panel.const)
  
  # check the trace plot
  stan_trace(fit.panel.const, pars = c("b_cd"))
  
  # check the density of each variable
  stan_dens( fit.panel.const, pars = c("b_cd"))
  
  # save the median of each variable
  b.cd.panel <- 
    extracted.panel.const$b_cd %>% 
    as_tibble() %>% 
    summarise_all(median) %>% 
    t()
  
  b.cg.panel <- 
    extracted.panel.const$b_cg %>% 
    as_tibble() %>% 
    summarise_all(median) %>% 
    t()
  
  b.sd.panel <- 
    extracted.panel.const$b_sd %>% 
    as_tibble() %>% 
    summarise_all(median) %>% 
    t()
  
  b.sg.panel <- 
    extracted.panel.const$b_sg %>% 
    as_tibble() %>% 
    summarise_all(median) %>% 
    t()

  df.median.panel <- 
    df.ctry.id %>% 
    cbind(cd = b.cd.panel,
          cg = b.cg.panel,
          sd = b.sd.panel,
          sg = b.sg.panel)
  
  df.se.panel <- 
    data.frame(cd = extracted.panel.const$s_cd,
               cg = extracted.panel.const$s_cg,
               sd = extracted.panel.const$s_sd,
               sg = extracted.panel.const$s_sg)

  # plot and check graphically
  g.panel.cd <- 
    df.median.panel %>% 
    ggplot(aes(x = cd, y = country)) + 
    geom_bar(stat = "identity") + 
    xlim(0, 0.7) + 
    labs(x = "domestic credit")
  
  g.panel.cg <- 
    df.median.panel %>% 
    ggplot(aes(x = cg, y = country)) + 
    geom_bar(stat = "identity") +
    xlim(0, 0.7) + 
    labs(x = "global credit")
  
  g.panel.sd <- 
    df.median.panel %>% 
    ggplot(aes(x = sd, y = country)) + 
    geom_bar(stat = "identity") +
    xlim(-1, 0) + 
    labs(x = "domestic slope")
  
  g.panel.sg <- 
    df.median.panel %>% 
    ggplot(aes(x = sg, y = country)) + 
    geom_bar(stat = "identity") +
    xlim(-1, 0) + 
    labs(x = "global slope")
  
  # distribution of the standard error
  g.panel.se <- 
    df.se.panel %>% 
    gather(key = "vars", value = "value") %>% 
    ggplot() + 
    geom_density(aes(x = value, color = vars)) + 
    xlim(0, 1) + 
    labs(x = "standard error of the coefs")

  # save the graph
  ggsave(plot = g.panel.cd + g.panel.cg, 
         width = 10, height = 4, 
         filename = "../6_outputs/4_credit_panel.pdf")
  
  ggsave(plot = g.panel.sd + g.panel.sg, 
         width = 10, height = 4, 
         filename = "../6_outputs/4_slope_panel.pdf")
  
# ------------------------------------------------------------------------------
# Check the result of dynamic analysis
# ... now considers the coefficients to evolve along time
# ------------------------------------------------------------------------------
  
  # create the country ID according to countries
  df.JST.dynamic <- 
    df.JST %>% 
    group_by(year) %>% 
    mutate(time.id = cur_group_id()) %>% 
    ungroup()
  
  # create a data frame indicating year and ID 
  df.time.id <- 
    df.JST.dynamic %>% 
    select(year, time.id) %>% 
    unique() %>% 
    arrange(time.id)
  
  # input data
  data.dynamic <- 
    list(I        = nrow(df.JST.dynamic), 
         Time     = max(df.JST.dynamic$time.id),
         Cre_d    = df.JST.dynamic$credit.dom,
         Cre_g    = df.JST.dynamic$credit.glo,
         Slo_d    = df.JST.dynamic$slope.dom,
         Slo_g    = df.JST.dynamic$slope.glo,
         Crisis   = as.numeric(df.JST.dynamic$crisis) - 1,
         TID      = df.JST.dynamic$time.id,
         b_i_pri  = mu.i,
         b_cd_pri = mu.cd,
         b_cg_pri = mu.cg,
         b_sd_pri = mu.sd,
         b_sg_pri = mu.sg
    )
  
  # fit the Bayesian model
  fit.dynamic <- 
    stan(file   = "4_pooled_dynamic.stan",
         data   = data.dynamic,
         seed   = 2292,
         pars   = c("b_i", "b_cd",  "b_cg",  "b_sd",  "b_sg",
                    "s_i", "s_cd",  "s_cg",  "s_sd",  "s_sg"))
  
  # This process takes some time... save the result in the tmp folder
  # heavier than the previous simulation
  save(fit.dynamic, file = "../5_tmp/fit_dynamic.rda")
  
  # extract the fitted model
  extracted.dynamic <- 
    extract(fit.dynamic)
  
  # check the dynamics of the series
  df.b_i.dynamic <- 
    extracted.dynamic$b_i %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(time.id = row_number()) %>% 
    gather(key = iter, value = value, -time.id) %>% 
    group_by(time.id) %>% 
    summarise(p10 = quantile(value, 0.10),
              p25 = quantile(value, 0.25),
              p50 = quantile(value, 0.5),
              p75 = quantile(value, 0.75),
              p90 = quantile(value, 0.90)) %>% 
    left_join(df.time.id, by = "time.id")

  df.b_cd.dynamic <- 
    extracted.dynamic$b_cd %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(time.id = row_number()) %>% 
    gather(key = iter, value = value, -time.id) %>% 
    group_by(time.id) %>% 
    summarise(p10 = quantile(value, 0.10),
              p25 = quantile(value, 0.25),
              p50 = quantile(value, 0.5),
              p75 = quantile(value, 0.75),
              p90 = quantile(value, 0.90)) %>% 
    left_join(df.time.id, by = "time.id")
  
  df.b_cg.dynamic <- 
    extracted.dynamic$b_cg %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(time.id = row_number()) %>% 
    gather(key = iter, value = value, -time.id) %>% 
    group_by(time.id) %>% 
    summarise(p10 = quantile(value, 0.10),
              p25 = quantile(value, 0.25),
              p50 = quantile(value, 0.5),
              p75 = quantile(value, 0.75),
              p90 = quantile(value, 0.90)) %>% 
    left_join(df.time.id, by = "time.id")
  
  df.b_sd.dynamic <- 
    extracted.dynamic$b_sd %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(time.id = row_number()) %>% 
    gather(key = iter, value = value, -time.id) %>% 
    group_by(time.id) %>% 
    summarise(p10 = quantile(value, 0.10),
              p25 = quantile(value, 0.25),
              p50 = quantile(value, 0.5),
              p75 = quantile(value, 0.75),
              p90 = quantile(value, 0.90)) %>% 
    left_join(df.time.id, by = "time.id")
  
  df.b_sg.dynamic <- 
    extracted.dynamic$b_sg %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(time.id = row_number()) %>% 
    gather(key = iter, value = value, -time.id) %>% 
    group_by(time.id) %>% 
    summarise(p10 = quantile(value, 0.10),
              p25 = quantile(value, 0.25),
              p50 = quantile(value, 0.5),
              p75 = quantile(value, 0.75),
              p90 = quantile(value, 0.90)) %>% 
    left_join(df.time.id, by = "time.id")
  
  # plot and check graphically
  g.dynamic.b_i <- 
    df.b_i.dynamic %>% 
    ggplot() + 
    geom_ribbon(aes(x = year,
                    ymin = p10,
                    ymax = p90),
                fill = "gray",
                alpha = 0.5) + 
    geom_ribbon(aes(x = year,
                    ymin = p25,
                    ymax = p75),
                fill = "gray") + 
    geom_line(aes(x = year, y = p50)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    labs(x = "Year", 
         y = "intercept") 
  
  g.dynamic.b_cd <- 
    df.b_cd.dynamic %>% 
    ggplot() + 
    geom_ribbon(aes(x = year,
                    ymin = p10,
                    ymax = p90),
                fill = "gray",
                alpha = 0.5) + 
    geom_ribbon(aes(x = year,
                    ymin = p25,
                    ymax = p75),
                fill = "gray") + 
    geom_line(aes(x = year, y = p50)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    ylim(-2, 3) + 
    labs(x = "Year", 
         y = "domestic credit") 
  
  g.dynamic.b_cg <- 
    df.b_cg.dynamic %>% 
    ggplot() + 
    geom_ribbon(aes(x = year,
                    ymin = p10,
                    ymax = p90),
                fill = "gray",
                alpha = 0.5) + 
    geom_ribbon(aes(x = year,
                    ymin = p25,
                    ymax = p75),
                fill = "gray") + 
    geom_line(aes(x = year, y = p50)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    ylim(-2, 3) + 
    labs(x = "Year", 
         y = "global credit") 
  
  g.dynamic.b_sd <- 
    df.b_sd.dynamic %>% 
    ggplot() + 
    geom_ribbon(aes(x = year,
                    ymin = p10,
                    ymax = p90),
                fill = "gray",
                alpha = 0.5) + 
    geom_ribbon(aes(x = year,
                    ymin = p25,
                    ymax = p75),
                fill = "gray") + 
    geom_line(aes(x = year, y = p50)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    ylim(-3, 2) + 
    labs(x = "Year", 
         y = "domestic slope") 
  
  g.dynamic.b_sg <- 
    df.b_sg.dynamic %>% 
    ggplot() + 
    geom_ribbon(aes(x = year,
                    ymin = p10,
                    ymax = p90),
                fill = "gray",
                alpha = 0.5) + 
    geom_ribbon(aes(x = year,
                    ymin = p25,
                    ymax = p75),
                fill = "gray") + 
    geom_line(aes(x = year, y = p50)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    ylim(-3, 2) + 
    labs(x = "Year", 
         y = "global slope") 
  
  # save the graph
  ggsave(plot = g.dynamic.b_cd + g.dynamic.b_cg, 
         width = 10, height = 4, 
         filename = "../6_outputs/4_credit_dynamic.pdf")
  
  ggsave(plot = g.dynamic.b_sd + g.dynamic.b_sg, 
         width = 10, height = 4, 
         filename = "../6_outputs/4_slope_dynamic.pdf")
  
# ------------------------------------------------------------------------------
# Check the result of narrow variables with constant parameters (pooled)
# ------------------------------------------------------------------------------
  
  # check with segmented data
  df.JST.former <- 
    df.JST %>% 
    filter(year < 1980)
  
  df.JST.latter <- 
    df.JST %>% 
    filter(year >= 1980)
  
  # create a model of narrow constant logistic model
  glm(crisis ~ credit.glo + slope.glo + credit.dom + slope.dom,
      family = binomial(),
      data = df.JST.former) %>% 
    summary()

  # create a model of narrow constant logistic model
  glm(crisis ~ credit.glo + slope.glo + credit.dom + slope.dom,
      family = binomial(),
      data = df.JST.latter) %>% 
    summary()
  