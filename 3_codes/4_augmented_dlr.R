
# ------------------------------------------------------------------------------
# About this code
# Run Kalman filtering: logistic regression augmented with latent variable
# see Bakerman et al.(2022)
# estimated with the function I created under util folder
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("ggthemes")
  theme_set(theme_solarized())
  library("patchwork")
  
  library("tidymodels")
  library("BayesLogit")
  source("util/FFBS_logit.r")

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
  
  # preprocess the data frame
  df.JST <-
    recipe.bag %>%
    prep() %>%
    bake(new_data = df.JST)
  
# ------------------------------------------------------------------------------
# Input data for the Kalman filtering
# ------------------------------------------------------------------------------
  
  # leave only the relevant variables
  df.JST <- 
    df.JST %>% 
    select(year, country, crisis,
           starts_with("credit"),
           starts_with("slope")
    ) %>% 
    na.omit()
  
  # covariates
  X <- 
    df.JST %>% 
    select(-crisis)
  
  # crisis dummy
  Y <- 
    df.JST %>% 
    select(year, country, crisis)
  
# ------------------------------------------------------------------------------
# Estimate the optimal W (variance of states) with ML
# ------------------------------------------------------------------------------
  
# ------------------------------------------------------------------------------
# Run FFBS algorithm 
# ------------------------------------------------------------------------------
  
  # run the FFSB algorithm
  fit.FFSB <- 
    FFSB_logit(X = X, Y = Y)

  # check the evolution of coefficients
  fit.coefs <- 
    fit.FFSB[[1]] %>% 
    unlist() %>% 
    matrix(ncol = ncol(X) - 2, byrow = TRUE)
  colnames(fit.coefs) <- colnames(X)[-c(1:2)]
  
  # year contained
  year <- 
    Y %>% 
    select(year) %>% 
    unique()  
  
  # plot
  df.coefs <- 
    fit.coefs %>% 
    cbind(year) 
    
  # check the evolution of coefficients
  df.coefs %>% 
    select(year, starts_with("credit")) %>% 
    gather(key = "vars", value = "value", -year) %>% 
    ggplot() + 
    geom_line(aes(x = year, y = value, color = vars))
    
  df.coefs %>% 
    select(year, starts_with("slope")) %>% 
    gather(key = "vars", value = "value", -year) %>% 
    ggplot() + 
    geom_line(aes(x = year, y = value, color = vars))
  