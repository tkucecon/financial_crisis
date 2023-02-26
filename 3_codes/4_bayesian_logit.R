
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
    step_zv(all_predictors(), skip = FALSE) %>% 
    # impute with bagging
    step_impute_bag(all_predictors(), 
                    impute_with = imp_vars(all_predictors(), country),
                    skip = FALSE)
  
  # create a data frame with imputed variables
  df.JST <- 
    recipe.bag %>% 
    prep() %>% 
    bake(new_data = df.JST)

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
  mu.cd <- model.pooled.const$coefficients["credit.dom"]
  mu.cg <- model.pooled.const$coefficients["credit.glo"]
  mu.sd <- model.pooled.const$coefficients["slope.dom"]
  mu.sg <- model.pooled.const$coefficients["slope.glo"]
  
# ------------------------------------------------------------------------------
# Check the result of narrow variables with the Bayesian estimation (pooled)
# ------------------------------------------------------------------------------
  
  # input data
  data.pooled.const <- 
    list(I = nrow(df.JST.imputed), 
         Cre_d  = df.JST.imputed$credit.dom,
         Cre_g  = df.JST.imputed$credit.glo,
         Slo_d  = df.JST.imputed$slope.dom,
         Slo_g  = df.JST.imputed$slope.glo,
         Crisis = as.numeric(df.JST.imputed$crisis) - 1
         )
  
  # fit the Bayesian model
  fit.pooled.const <- 
    stan(file = "4_pooled_constant.stan",
         data = data.pooled.const,
         seed = 2292)
  
  extracted.pooled.const <- 
    extract(fit.pooled.const)
  
  summary(extracted.pooled.const$b)
  stan_trace(fit.const, pars = c("b[2]", "b[3]", "b[4]", "b[5]"))
  stan_hist(fit.const, pars  = c("b[2]", "b[3]", "b[4]", "b[5]"))
  
  # seems to be very consistent

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

  # input data
  data.panel.const <- 
    list(I      = nrow(df.JST.panel), 
         C      = max(df.JST.panel$ctry.id),
         Cre_d  = df.JST.panel$credit.dom,
         Cre_g  = df.JST.panel$credit.glo,
         Slo_d  = df.JST.panel$slope.dom,
         Slo_g  = df.JST.panel$slope.glo,
         Crisis = as.numeric(df.JST.panel$crisis) - 1,
         CID    = df.JST.panel$ctry.id
    )
  
  # fit the Bayesian model
  fit.panel.const <- 
    stan(file   = "4_panel_constant.stan",
         data   = data.panel.const,
         seed   = 2292,
         iter   = 3000,
         warmup = 1000,
         pars   = c("b_cd",  "b_cg",  "b_sd",  "b_sg",
                    "mu_cd", "mu_cg", "mu_sd", "mu_sg",
                    "s_cd",  "s_cg",  "s_sd",  "s_sg"))
  
  # This process takes some time... save the result in the tmp folder
  save(fit.panel.const, file = "../5_tmp/fit_panel_const.rda")
  
  # extract the fitted model
  extracted.panel.const <- 
    extract(fit.panel.const)
  
  summary(extracted.panel.const)
  stan_trace(fit.panel.const, pars = c("b_cd", "b_cg"))
  stan_hist( fit.panel.const, pars = c("b_cd", "b_cg"))
  
  # seems to be very consistent