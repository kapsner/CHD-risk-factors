# Copyright (C) 2022-2024 Uniklinikum Erlangen

ranger_trainer <- function(train_x,
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


  outlist$param_grid_ranger <- expand.grid(
    num.trees = seq(500, 1000, 500),
    mtry = seq(2, 6, 2),
    min.node.size = seq(1, 9, 4),
    max.depth = seq(1, 9, 4),
    sample.fraction = seq(0.5, 0.8, 0.3)
  )

  outlist$ranger_bounds <- list(
    num.trees = c(100L, 1000L),
    mtry = c(2L, 9L),
    min.node.size = c(1L, 20L),
    max.depth = c(1L, 40L),
    sample.fraction = c(0.3, 1.)
  )

  outlist$ranger_tuner <- mlexperiments::MLTuneParameters$new(
    learner = mlsurvlrnrs::LearnerSurvRangerCox$new(),
    seed = seed,
    strategy = "bayesian",
    ncores = n_cores
  )
  #outlist$ranger_tuner$learner$seed <- seed
  outlist$ranger_tuner$learner_args <- list(
    "respect.unordered.factors" = TRUE
  )
  outlist$ranger_tuner$parameter_grid <- outlist$param_grid_ranger
  outlist$ranger_tuner$parameter_bounds <- outlist$ranger_bounds
  outlist$ranger_tuner$optim_args <- optim_args
  outlist$ranger_tuner$split_vector <- train_split_criterion
  outlist$ranger_tuner$set_data(
    x = train_x,
    y = train_y,
    cat_vars = cat_vars
  )
  outlist$ranger_tuning_params <- outlist$ranger_tuner$execute(k = hparam_nfolds)
  outlist$best_params_ranger <- outlist$ranger_tuner$results$best.setting
  gc();gc()

  message(paste0(
    "\n\nBest 'ranger' parameters: ", as.list(outlist$best_params_ranger), "\n\n"
  ))


  outlist$ranger_validator <- mlexperiments::MLCrossValidation$new(
    learner = mlsurvlrnrs::LearnerSurvRangerCox$new(),
    fold_list = validation_fold_list,
    seed = seed,
    ncores = n_cores,
    return_models = TRUE
  )
  #outlist$ranger_validator$learner$seed <- seed
  outlist$ranger_validator$learner_args <- outlist$best_params_ranger[
    !kdry::misc_duplicated_by_names(outlist$best_params_ranger)
  ][-1]
  outlist$ranger_validator$performance_metric <- mlsurvlrnrs::c_index
  outlist$ranger_validator$set_data(
    x = train_x,
    y = train_y,
    cat_vars = cat_vars
  )
  outlist$res_ranger <- outlist$ranger_validator$execute()
  gc();gc()

  #############################################################################
  ### Get best models
  #############################################################################
  outlist$selected_ranger_models <- list(
    "max" = outlist$res_ranger[
      which.max(outlist$res_ranger$performance),
      get("fold")
    ],
    "min" = outlist$res_ranger[
      which.min(outlist$res_ranger$performance),
      get("fold")
    ],
    "mean" = outlist$res_ranger[
      which.min(abs(outlist$res_ranger$performance -
                      mean(outlist$res_ranger$performance, na.rm = TRUE))),
      get("fold")
    ],
    "median" = outlist$res_ranger[
      which.min(abs(outlist$res_ranger$performance -
                      median(outlist$res_ranger$performance, na.rm = TRUE))),
      get("fold")
    ]
  )

  #############################################################################
  ### Compute SHAP for all models
  #############################################################################

  outlist$shap_list <- generate_shap_comparison(
    validator = outlist$ranger_validator,
    type = "ranger",
    selected_models = outlist$selected_ranger_models,
    train_x = train_x,
    test_x = test_x,
    var_names_mapping = var_names_mapping,
    only_full_shap = only_full_shap
  )

  #############################################################################
  ### Combine results
  #############################################################################

  outlist$imp_rank_ranger <- extract_importance_comparison(outlist$shap_list)
  return(outlist)
}
