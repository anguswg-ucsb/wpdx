
remove(list = ls())  # clear all workspace variables

library(tidyverse)
library(tidymodels)
library(stringr)
library(glmnet)
library(doParallel)
library(workflowsets)
library(finetune)

# Full NFL weekly data set
nfl <- readRDS(here::here("data", "nfl_wins.rds")) 

# Correlation datarame
feat_select <- readRDS(here::here("data", "nfl_feature_selection.rds"))

# remove 2pt conversion plays and low correlated values
corr_select <- 
  feat_select %>%  
  dplyr::filter(!grepl("2pt", Var1)) %>%
  dplyr::filter(abs_corr >= 0.05)

# Select columns for modeling
nfl_df  <- 
  nfl %>% 
  dplyr::select(season, home_away, passing_yards, corr_select$Var1) %>% 
  dplyr::select(-passing_air_yards) %>% 
  dplyr::mutate(
    # season    = as.character(season),
    home_away = factor(home_away, 
                       levels = c("home_team", "away_team")),
    win       = factor(win, levels = c(1, 0))
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
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 


glmnet_recipe <- 
  recipe(formula = win ~ ., data = nfl_train) %>% 
  recipes::update_role(
    season, 
    new_role = "id") %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
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
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_zv(all_predictors()) 


earth_recipe <- 
  recipe(formula = win ~ ., data = nfl_train) %>% 
  recipes::update_role(
    season, 
    new_role = "id") %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 

# ---- Spec ----
kknn_spec <- 
  nearest_neighbor(
    neighbors   = tune(),
    weight_func = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kknn") 

glmnet_spec <- 
  logistic_reg(
    penalty = tune(), 
    mixture = tune()
    ) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet") 

xgboost_spec <- 
  boost_tree(
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(), 
    learn_rate = tune(), 
    loss_reduction = tune(), 
    sample_size = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost") 

earth_spec <- 
  mars(
    num_terms    = tune(),
    prod_degree  = tune(),
    prune_method = "none") %>% 
  set_mode("classification") %>% 
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
# my_metrics <- yardstick::metric_set(acc, rmse, mae)
my_metrics <- yardstick::metric_set(roc_auc, pr_auc, accuracy)

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
# ---- Rank WFS results ----
# **************************
ml_data_path <- "D:/ml/nfl/data/"

# Save workflowset
saveRDS(nfl_wfs, paste0(ml_data_path, "wfs/wins_classification_wfs.rds"))
# nfl_wfs <- readRDS("dflow/models/spec/workflows/dflow_bimonth_reg_model_workflowset.rds")

# Table of model ranks
workflowsets::rank_results(nfl_wfs)

# Comparing Accuracy and ROC AUC of 7 models
class_mod_comp_plot <-
  nfl_wfs %>%
  autoplot() + 
  labs(
    col = "",
    title    = "Classification Model comparisons",
    subtitle = "Predicting wins"
  ) 

class_mod_comp_plot

# Save plot
ggsave(
  paste0(ml_data_path, "plots/classification_model_rank.png"),
  plot   = class_mod_comp_plot
)

# ****************************
# ---- Select best models ----
# ****************************

class_metrics_lst <- list()

# rm(acc)
# i <- 3
# rm(i, model, model_name, mod_workflow, mod_final_fit, mod_last_fit, mod_workflow_fit, resample_roc_plot, vip_plot,
#    mod_test,mod_train, train_acc, test_acc, overall_aucroc,
# roc_auc_curve_plot, mod_results)

# Extract wf set results
for (i in 1:length(nfl_wfs$wflow_id)) {
  
  model       <- nfl_wfs$wflow_id[i]
  model_name  <- nfl_wfs$info[[i]]$model
  
  model
  
  mod_results <- 
    nfl_wfs %>% 
    workflowsets::extract_workflow_set_result(model)
  
  # Extract workflows
  mod_workflow <- 
    nfl_wfs %>%  
    extract_workflow(model)
  
  # Model Engine text
  model_engine <- mod_workflow$fit$actions$model$spec$engine
  
  # Model Engine text
  model_mode <- mod_workflow$fit$actions$model$spec$mode
  
  logger::log_info("\n\nExtracting workflow & finalizing model fit:\n  --->  {model_name} - {model_mode}")
  # mod_results$.metrics[[1]]
  
  print(select_best(mod_results, metric = "accuracy"))
  print(select_best(mod_results, metric = "roc_auc"))
  # select_best(mod_results, metric = "roc_auc")
  
  # Finalize workflow fit
  mod_workflow_fit <- 
    mod_workflow %>% 
    finalize_workflow(select_best(mod_results, metric = "roc_auc")) %>% 
    fit(data = nfl_train)
  
  # Fit model to split train/test data
  mod_last_fit <- tune::last_fit(mod_workflow_fit, nfl_split)
  
  # print(tune::collect_metrics(mod_results)$mean)
  print(tune::collect_metrics(mod_last_fit))
  
  # Extract & save final fit to use for predictions
  mod_final_fit <- mod_last_fit$.workflow[[1]]
  
  # Resampled CV Fold AUC ROC Curve
  resample_roc_plot <- 
    mod_results %>%
    collect_predictions() %>%
    group_by(id) %>% 
    roc_curve(win, .pred_1) %>%
    ggplot(aes(1 - specificity, sensitivity, color = id)) +
    geom_abline(lty = 2, color = "gray80", size = 1.5) +
    geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
    coord_equal() +
    labs(
      title    = paste0("AUC-ROC Curve - ", model_name),
      subtitle = "Resample results from 10 Fold Cross Validation",
      x        = "1 - Specificity",
      y        = "Sensitivity"
    ) 
  
  # save ROC AUC plot to "aw-poudre-2020/dflow/boatable_day_plots/
  resample_plot_path  <-   paste0(ml_data_path, "plots/win_class_resample_aucroc_", model_name, ".png")
  logger::log_info("\n\nSaving Resamples AUC-ROC curve: \n{resample_plot_path}")
  
  # Export plot
  ggsave(
    resample_plot_path,
    plot   = resample_roc_plot
  )
  
  # Plot variable importance if avaliable for model
  tryCatch( 
    {
      vip_plot <-
        mod_last_fit %>% 
        pluck(".workflow", 1) %>%   
        extract_fit_parsnip() %>% 
        # vip::vip() + 
        vip::vip(num_features = 50) +
        labs(
          title    = paste0("Variable Importance Scores - ", model_name),
          subtitle = "Classification",
          y        = "Importance",
          x        = "Variables"
        )
      
      # save ROC AUC plot to "aw-poudre-2020/dflow/boatable_day_plots/
      vip_path  <-   paste0(ml_data_path, "plots/win_class_vip_", model_name, ".png")
      logger::log_info("\n\nSaving Variable Importance plot:\n{vip_path}")
      
      # Export plot
      ggsave(
        vip_path,
        plot   = vip_plot
      )
    },
    error = function(e) {
      logger::log_error('Variable Importance is not avalaible for {model_name} model')
      logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
      logger::log_error(message(e))
      # stop()
    }
  )
  # training set predictions
  mod_train <- 
    predict(mod_final_fit, nfl_train) %>% 
    bind_cols(predict(mod_final_fit, nfl_train, type = "prob")) %>%
    bind_cols(dplyr::select(nfl_train, win)) # Add the true outcome data back in
  
  # testing set predictions
  mod_test <-
    predict(mod_final_fit, nfl_test) %>% 
    bind_cols(predict(mod_final_fit, nfl_test, type = "prob")) %>%
    bind_cols(dplyr::select(nfl_test, win)) 

  # Train accuracy
  train_acc <- 
    mod_train %>% 
    accuracy(truth = win, .pred_class) %>% 
    mutate(
      data   = "train",
      model  = model_name
    )
  print(train_acc)
  # Test accuracy
  test_acc <- 
    mod_test %>% 
    accuracy(truth = win, .pred_class) %>% 
    mutate(
      data   = "test",
      model  = model_name
    )
  print(test_acc)
  # class(mod_results)
  
  # overall AUC ROC
  overall_aucroc <-
    mod_test %>% 
    yardstick::roc_auc(
      truth = win,
      c(names(mod_test)[2:3]),
      estimator = "hand_till"
    ) %>%
    mutate(
      data   = "test",
      model  = model_name
    )
    # data(hpc_cv)
  
  # Accuracy
  acc <- bind_rows(train_acc, test_acc, overall_aucroc)
  
  class_metrics_lst[[i]] <- acc
  
  # AUC-ROC One vs All Curve
  roc_auc_curve_plot <-
    mod_test %>% 
    yardstick::roc_curve(
      truth = win,
      c(names(mod_test)[2])
    )  %>% 
    ggplot2::autoplot() +
    labs(
      title    = paste0("AUC-ROC - ", model_name),
      subtitle = "Final fitted model using tuned hyperparameters",
      x        = "1 - Specificity",
      y        = "Sensitivity"
    ) 
    # th
  

  # save ROC AUC plot to "aw-poudre-2020/dflow/boatable_day_plots/
  rocauc_plot_path  <-   paste0(ml_data_path, "plots/win_class_aucroc_", model_name, ".png")
  logger::log_info("\n\nSaving final fitted AUC ROC curve:\n{rocauc_plot_path}")
  
  # Export plot
  ggsave(
    rocauc_plot_path,
    plot   = roc_auc_curve_plot
  )

  # Save Workflows/Resample results/Final fitted model
  saveRDS(
    mod_workflow_fit, 
    paste0(ml_data_path, "wf/win_class_workflow_", model_name, ".rds")
  )
  
  saveRDS(
    mod_last_fit,    
    paste0(ml_data_path, "resamples/win_class_resamples_", model_name, ".rds")
  )
  
  saveRDS(
    mod_final_fit, 
    paste0(ml_data_path, "fit/win_class_", model_name, ".rds")
  )
  
  
}













