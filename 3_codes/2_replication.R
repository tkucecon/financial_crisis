
# ------------------------------------------------------------------------------
# About this code
# replication of Bluwstein et al. (2020)
# https://ideas.repec.org/p/boe/boeewp/0848.html
# seems there are several versions... (ECB working paper and so on)
# ------------------------------------------------------------------------------

# set up------------------------------------------------------------------------

# library
  library("tidyverse")
  library("lubridate")
  library("haven")
  library("ggthemes")
  theme_set(theme_solarized())
  
  library("skimr")
  library("tidymodels")
  library("themis")
  library("discrim")
  
# ------------------------------------------------------------------------------
# Load the JST data
# ------------------------------------------------------------------------------
  
  # load the JST data
  load("../4_data/df_JST.rda")
  
  # here only a fraction of variables is used...
  # and NA rows should be dropped
  df.JST <- 
    df.JST %>% 
    select(crisis,
           slope.dom, slope.glo, credit.dom, credit.glo,
           dsr.dom, iy.dom, pdebt.dom, money.dom, ca.dom, 
           pi.dom, rcon.dom, equity.dom) %>% 
    na.omit()
  
  # 1260 samples as in the original paper
  
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
# Define the formulas
# ------------------------------------------------------------------------------

  # define a base formula
  formula.base <- 
    as.formula(crisis ~ .)
  
# ------------------------------------------------------------------------------
# Define recipes to process the features
# ... to improve the accuracy and interpretability of the model
  
# This classification is highly biased toward normal regime,
# which leads to a terrible prediction power of the crisis
# Thus here I employed some algorithms to deal with this imbalance
# see footnote 17 of the paper
# ------------------------------------------------------------------------------
  
  # create a base recipe 
  recipe.base <- 
    recipe(x = df.JST.train, formula = formula.base) %>% 
    # normalize all the predictors to be distributed ~ N(0,1) 
    step_normalize(all_predictors()) 
  
  # create a recipe using down sampling
  recipe.downsampling <- 
    recipe.base %>% 
    # conduct down sampling to deal with the unbalanced data
    # here under ratio is 1 (default)... can be modified
    step_downsample(all_outcomes(), seed = 2292, skip = TRUE)
  
  # create a recipe using down sampling
  recipe.upsampling <- 
    recipe.base %>% 
    # conduct up sampling: over ratio is adjustable
    step_upsample(all_outcomes(), seed = 2292, skip = TRUE, 
                  over_ratio = 0.5)
  
  # create a recipe using SMOTE algorithm
  # this is a kind of up sampling, but more advanced (imputes the data from knn)
  recipe.smote <- 
    recipe.base %>% 
    # conduct smote: over ratio is adjustable
    step_smote(all_outcomes(), seed = 2292, skip = TRUE, 
               over_ratio = 0.5, neighbors = 5)

# ------------------------------------------------------------------------------
# Define models for the prediction
# ------------------------------------------------------------------------------
  
  # logistic regression spec
  lr.spec <- 
    logistic_reg() %>% 
    set_engine("glm") %>% 
    set_mode(mode = "classification")
  
  # decision tree spec
  tree.spec <- 
    decision_tree() %>% 
    set_engine("rpart") %>% 
    set_mode(mode = "classification")

  # random forest spec
  forest.spec <- 
    rand_forest() %>% 
    set_engine("ranger") %>% 
    set_mode(mode = "classification")
  
  # SVM (radial basis function)
  svm.spec <- 
    svm_rbf() %>% 
    set_engine("kernlab") %>% 
    set_mode(mode = "classification")
  
  # XGBoost tree
  xgboost.spec <- 
    boost_tree() %>% 
    set_engine("xgboost") %>% 
    set_mode(mode = "classification")
  
  # Single Layer Neural Network
  nnet.spec <- 
    mlp() %>% 
    set_engine("nnet") %>% 
    set_mode(mode = "classification")
  
  # Naive Bayes
  bayes.spec <- 
    naive_Bayes() %>% 
    set_engine("klaR") %>% 
    set_mode(mode = "classification")
  
# ------------------------------------------------------------------------------
# Set the workflow
# ------------------------------------------------------------------------------
  
  # define the workflows
  wflow.JST <-
    workflow_set(
      # add the recipes here
      preproc = list(
        # downsampling = recipe.downsampling,
        # upsampling   = recipe.upsampling,
        # smote        = recipe.smote,
        base         = recipe.base
      ),
      # indicate models here
      models = list(
        tree     = tree.spec,
        forest   = forest.spec,
        svm      = svm.spec,
        xgboost  = xgboost.spec,
        nnet     = nnet.spec,
        bayes    = bayes.spec,
        logistic = lr.spec
        ),
      cross = TRUE
    )

# ------------------------------------------------------------------------------
# Define the resampling method
# ------------------------------------------------------------------------------
  
  # resampling method: k-fold cross validation
  folds.JST <- 
    df.JST.train %>% 
    vfold_cv(v = 10, strata = crisis)
  
# ------------------------------------------------------------------------------
# Apply workflows to the resampled data
# ------------------------------------------------------------------------------
  
  # fit the model
  fitted.JST <- 
    wflow.JST %>% 
    workflow_map(
      fn = "fit_resamples",
      resamples = folds.JST,
      seed = 2292
    )
    
# ------------------------------------------------------------------------------
# Evaluate the result
# ------------------------------------------------------------------------------

  # check and compare the result in data
  fitted.JST %>% 
    rank_results(rank_metric = "roc_auc", select_best = TRUE)
  
  # plot the graph to compare
  g.fit.comparison <- 
    fitted.JST %>% 
    autoplot(metric = "roc_auc") + 
    labs(x = "Rank",
         y = "AUC of ROC") 
  
  # save the graph
  ggsave(plot = g.fit.comparison, width = 5, height = 4, 
         filename = "../6_outputs/2_compare_ML.pdf")

  