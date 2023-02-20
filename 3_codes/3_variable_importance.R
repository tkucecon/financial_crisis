
# ------------------------------------------------------------------------------
# About this code
# comparison of the variable importance based on random forest model
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("lubridate")
  library("haven")
  library("ggthemes")
  theme_set(theme_solarized())
  
  library("tidymodels")
  library("themis")
  library("vip")

# ------------------------------------------------------------------------------
# Load the JST data
# ------------------------------------------------------------------------------
  
  # load the JST data
  load("../4_data/df_JST.rda")
  
  df.JST <- 
    df.JST %>% 
    select(-starts_with("tbus"), -starts_with("thh"))
  
# ------------------------------------------------------------------------------
# split the data into training and testing samples
# ------------------------------------------------------------------------------
  
  # set the seed
  set.seed(2292)
  
  # split the data: stratify with the crisis (it's a rare event!)
  df.JST.split <- 
    df.JST %>% 
    initial_split(strata = crisis) 
  
  # split into training and testing samples
  # now the proportion is 75:25 (adjustable if needed)
  df.JST.train <- 
    training(df.JST.split)
  
  df.JST.test <- 
    testing(df.JST.split)
  
# ------------------------------------------------------------------------------
# Define the resampling method
# ------------------------------------------------------------------------------
  
  # resampling method: 10-fold cross validation
  folds.JST <- 
    df.JST.train %>% 
    vfold_cv(v = 10, strata = crisis)
  
# ------------------------------------------------------------------------------
# Define the formulas
# ------------------------------------------------------------------------------

  # define a base formula
  formula.base <- 
    as.formula(crisis ~ .)
  
# ------------------------------------------------------------------------------
# Define recipes to process the features
# ... to improve the accuracy and interpretability of the model
# ------------------------------------------------------------------------------
  
  # create a base recipe 
  recipe.base <- 
    recipe(x = df.JST.train, formula = formula.base) %>% 
    # remove year and country from the predictors %>% 
    update_role(year,    new_role = "id variable") %>% 
    update_role(country, new_role = "time variable") %>% 
    # normalize all the predictors to be distributed ~ N(0,1) 
    step_normalize(all_predictors(), skip = FALSE) %>% 
    # delete rows with no variance
    step_zv(all_predictors(), skip = FALSE)
  
  # impute NA values with bagging
  recipe.bag <- 
    recipe.base %>% 
    step_impute_bag(all_predictors(), 
                    impute_with = imp_vars(all_predictors(), country),
                    skip = FALSE)

# ------------------------------------------------------------------------------
# Define models for the prediction
# ------------------------------------------------------------------------------

  # random forest spec
  forest.spec <- 
    rand_forest(mtry = 10,
                trees = 1000) %>% 
    set_engine("ranger",
               importance = "impurity") %>% 
    set_mode(mode = "classification")
  
# ------------------------------------------------------------------------------
# Determine how to deal with missing variables
# ------------------------------------------------------------------------------

  # define the workflows
  wflow.JST <-
    workflow() %>% 
    add_recipe(recipe = recipe.bag) %>% 
    add_model(spec = forest.spec)
  
  # fit the model
  # here I intentionally use the entire sample (the purpose is not prediction)
  fitted.JST <- 
    wflow.JST %>% 
    fit(data = df.JST)
  
  # check the result
  print(fitted.JST)

  # check the importance of each variable
  g.importance <- 
    fitted.JST %>% 
    extract_fit_parsnip() %>% 
    vip(num_features = 10)
  
  # save the graph
  ggsave(plot = g.importance, width = 5, height = 4, 
         filename = "../6_outputs/3_importance.pdf")
  