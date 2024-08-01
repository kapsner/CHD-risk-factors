# Copyright (C) 2022-2024 Uniklinikum Erlangen

xgboost_trainer <- function(train_x,
                            train_y,
                            test_x,
                            seed,
                            hparam_nfolds,
                            n_cores,
                            optim_args,
                            train_split_criterion,
                            validation_fold_list,
                            cat_vars,
                            var_names_mapping,
                            only_full_shap
) {
  outlist <- list()

  outlist$param_grid_xgboost <- expand.grid(
    subsample = seq(0.5, 0.8, 0.3),
    colsample_bytree = seq(0.5, 0.8, 0.3),
    min_child_weight = seq(1, 9, 4),
    learning_rate = seq(0.01, 0.11, 0.05),
    max_depth = seq(1, 9, 4)
  )
  outlist$param_list_xgboost <- list(
    objective = "survival:cox",
    eval_metric = "cox-nloglik"
  )

  outlist$xgb_bounds <- list(
    subsample = c(0.3, 1),
    colsample_bytree = c(0.3, 1),
    min_child_weight = c(0., 10.),
    learning_rate = c(0.001, 0.2),
    max_depth = c(1L, 40L)
  )

  outlist$xgboost_tuner <- mlexperiments::MLTuneParameters$new(
    learner = mlsurvlrnrs::LearnerSurvXgboostCox$new(
      metric_optimization_higher_better = FALSE
    ),
    seed = seed,
    strategy = "bayesian",
    ncores = n_cores
  )
  #outlist$xgboost_tuner$learner$seed <- seed
  outlist$xgboost_tuner$parameter_grid <- outlist$param_grid_xgboost
  outlist$xgboost_tuner$parameter_bounds <- outlist$xgb_bounds
  outlist$xgboost_tuner$learner_args <- outlist$param_list_xgboost
  outlist$xgboost_tuner$optim_args <- optim_args
  outlist$xgboost_tuner$split_vector <- train_split_criterion
  outlist$xgboost_tuner$set_data(
    x = train_x,
    y = train_y
  )
  outlist$xgboost_tuning_params <- outlist$xgboost_tuner$execute(k = hparam_nfolds)
  outlist$best_params_xgboost <- outlist$xgboost_tuner$results$best.setting
  gc();gc()
  message(paste0(
    "\n\nBest 'xgboost' parameters: ", as.list(outlist$best_params_xgboost), "\n\n"
  ))

  outlist$xgboost_validator <- mlexperiments::MLCrossValidation$new(
    learner = mlsurvlrnrs::LearnerSurvXgboostCox$new(
      metric_optimization_higher_better = FALSE
    ),
    fold_list = validation_fold_list,
    seed = seed,
    ncores = n_cores,
    return_models = TRUE
  )
  #outlist$xgboost_validator$learner$seed <- seed
  outlist$xgboost_validator$learner_args <- outlist$best_params_xgboost[
    !kdry::misc_duplicated_by_names(outlist$best_params_xgboost)
  ][-1]
  outlist$xgboost_validator$performance_metric <- mlsurvlrnrs::c_index
  outlist$xgboost_validator$set_data(
    x = train_x,
    y = train_y
  )
  outlist$res_xgboost <- outlist$xgboost_validator$execute()
  gc();gc()

  #############################################################################
  ### Get best models
  #############################################################################
  outlist$selected_xgb_models <- list(
    "max" = outlist$res_xgboost[
      which.max(outlist$res_xgboost$performance),
      get("fold")
    ],
    "min" = outlist$res_xgboost[
      which.min(outlist$res_xgboost$performance),
      get("fold")
    ],
    "mean" = outlist$res_xgboost[
      which.min(abs(outlist$res_xgboost$performance -
                      mean(outlist$res_xgboost$performance, na.rm = TRUE))),
      get("fold")
    ],
    "median" = outlist$res_xgboost[
      which.min(abs(outlist$res_xgboost$performance -
                      median(outlist$res_xgboost$performance, na.rm = TRUE))),
      get("fold")
    ]
  )

  #############################################################################
  ### Compute SHAP for all models
  #############################################################################
  outlist$shap_list <- generate_shap_comparison(
    validator = outlist$xgboost_validator,
    type = "xgboost",
    selected_models = outlist$selected_xgb_models,
    train_x = train_x,
    test_x = test_x,
    var_names_mapping = var_names_mapping,
    only_full_shap = only_full_shap
  )

  #############################################################################
  ### Combine results
  #############################################################################

  outlist$imp_rank_xgb <- extract_importance_comparison(outlist$shap_list)
  return(outlist)
}
