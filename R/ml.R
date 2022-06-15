
remove(list = ls())  # clear all workspace variables

library(tidyverse)
library(tidymodels)
library(stringr)
library(glmnet)
library(doParallel)
library(workflowsets)
library(finetune)

nfl <- readRDS(here::here("data", "nfl_wins.rds")) 

feat_select <- readRDS(here::here("data", "nfl_feature_selection.rds")) %>% 
  dplyr::filter(abs_corr >= 0.1) 

nfl_df  <- 
  nfl %>% 
  dplyr::select(season, feat_select$Var1) %>% 
  dplyr::mutate(
    win2 = factor(win, levels = c("win", "loss"))
  )

set.seed(234)

# split data for train/test, stratify months
nfl_split <- initial_split(nfl_df, strata = season)

# training data split
nfl_train <- training(nfl_split)

# testinng data split
nfl_test  <- testing(nfl_split)

usemodels::use_earth(win~., data = nfl_train)

# ---- Recipes ----

logger::log_info("Data preprocessing...")

kknn_recipe <- 
  recipe(formula = win ~ ., data = nfl_train) %>% 
  recipes::update_role(
    season, 
    new_role = "id") %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

glmnet_recipe <- 
  recipe(formula = win ~ ., data = nfl_train) %>% 
  recipes::update_role(
    season, 
    new_role = "id") %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

xgboost_recipe <- 
  recipe(
    formula = win ~ ., 
    data = nfl_train
    ) %>% 
  recipes::update_role(
    season, 
    new_role = "id") %>% 
  step_zv(all_predictors()) 

earth_recipe <- 
  recipe(formula = win ~ ., data = nfl_train) %>% 
  recipes::update_role(
    season, 
    new_role = "id") %>%
  step_zv(all_predictors()) 

# ---- Spec ----
kknn_spec <- 
  nearest_neighbor(
    neighbors   = tune(),
    weight_func = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("kknn") 

glmnet_spec <- 
  linear_reg(
    penalty = tune(), 
    mixture = tune()
    ) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet") 

xgboost_spec <- 
  boost_tree(
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(), 
    learn_rate = tune(), 
    loss_reduction = tune(), 
    sample_size = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost") 

earth_spec <- 
  mars(
    num_terms    = tune(),
    prod_degree  = tune(),
    prune_method = "none") %>% 
  set_mode("regression") %>% 
  set_engine("earth") 

# ---- Workflows ----

kknn_workflow <- 
  workflow() %>% 
  add_recipe(kknn_recipe) %>% 
  add_model(kknn_spec) 

glmnet_workflow <- 
  workflow() %>% 
  add_recipe(glmnet_recipe) %>% 
  add_model(glmnet_spec) 

xgboost_workflow <- 
  workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_spec) 

earth_workflow <- 
  workflow() %>% 
  add_recipe(earth_recipe) %>% 
  add_model(earth_spec) 

earth_grid <- tidyr::crossing(num_terms = 2 * (1:6), prod_degree = 1:2) 

# ---- Cross Val ----

# Set seed for resampling 
set.seed(589)

# CV folds
nfl_folds <- rsample::vfold_cv(nfl_train, v = 10, strata = season)

# ---- Workflow set of models ----
nfl_wfs <- 
  workflow_set(
    preproc = list(
      kknn_rec     = kknn_recipe,
      glmnet_rec   = glmnet_recipe,
      xgboost_rec  = xgboost_recipe,
      earth_rec    = earth_recipe
    ),
    models  = list(
      kknn      = kknn_spec,
      glmnet    = glmnet_spec,
      xgboost   = xgboost_spec,
      earth     = earth_spec
    ),
    cross = F
  )

# Choose metrics
my_metrics <- yardstick::metric_set(rsq, rmse, mae)


# Set up parallelization, using computer's other cores
parallel::detectCores(logical = FALSE)
modeltime::parallel_start(6, .method = "parallel")

# Set Random seed
set.seed(589)

# Tune models in workflowset
# nfl_wfs <-
#   nfl_wfs %>%
#   workflow_map(
#     "tune_grid",
#     resamples = nfl_folds ,
#     grid      = 12,
#     metrics   = my_metrics,
#     control   = control_grid(
#       verbose   = TRUE,
#       save_pred = TRUE),
#     verbose   = TRUE
#   )

# Efficient Tuning of models in workflowset
nfl_wfs <-
  nfl_wfs %>%
  workflowsets::workflow_map(
    "tune_race_anova",
    resamples = nfl_folds,
    # resamples = flow_roll_splits,
    grid      = 12,
    metrics   = my_metrics,
    control = finetune::control_race(
      verbose       = TRUE,
      save_pred     = TRUE,
      verbose_elim  = TRUE,
      save_workflow = TRUE
    ),
    verbose   = TRUE
  )

# Stop parrallelization
modeltime::parallel_stop()

# **************************
# ---- View WFS results ----
# **************************
ml_data_path <- "D:/ml/nfl/data/"

# Save workflowset
saveRDS(nfl_wfs, paste0(ml_data_path, "wfs/wins_regression_wfs.rds"))
# nfl_wfs <- readRDS("dflow/models/spec/workflows/dflow_bimonth_reg_model_workflowset.rds")
# # flow_split <- readRDS("dflow/models/validation/dflow_model_data.rds")
# rm(flow_split)
# flow_train <- readRDS("dflow/models/validation/dflow_train_data.rds")
# flow_test  <- readRDS("dflow/models/validation/dflow_test_data.rds")
# Table of model ranks
rank_results(nfl_wfs)

# Comparing Accuracy and ROC AUC of 7 models
reg_mod_comp_plot <-
    nfl_wfs %>%
    autoplot() + 
    labs(
      col = "",
      title    = "Regression Model comparisons",
      subtitle = "Predicting wins"
    ) 

reg_mod_comp_plot

# Save plot
ggsave(
  paste0(ml_data_path, "plots/regression_model_rank.png"),
  plot   = reg_mod_comp_plot
)






