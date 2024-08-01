# Copyright (C) 2022-2024 Uniklinikum Erlangen

cox_feature_cols <- top_features_union |>
  dplyr::recode(!!!setNames(names(var_names_mapping), var_names_mapping))

if (!diagnosis_group %in% cox_feature_cols) {
  cox_feature_cols <- c(cox_feature_cols, diagnosis_group)
  added_diagnosis_group <- TRUE
} else {
  added_diagnosis_group <- FALSE
}

coxph_validator <- mlexperiments::MLCrossValidation$new(
  learner = mlsurvlrnrs::LearnerSurvCoxPHCox$new(),
  fold_list = validation_fold_list,
  seed = seed,
  ncores = n_cores,
  return_models = TRUE
)
coxph_validator$performance_metric <- mlsurvlrnrs::c_index
coxph_validator$learner_args <- list(cat_vars = ml_cat_vars[which(ml_cat_vars %in% cox_feature_cols)])
coxph_validator$set_data(
  x = train_x[, which(colnames(train_x) %in% cox_feature_cols)],
  y = train_y,
  cat_vars = ml_cat_vars[which(ml_cat_vars %in% cox_feature_cols)]
)
res_coxph <- coxph_validator$execute()

coxph_all_rep_cv <- data.table::rbindlist(
  l = lapply(
    X = names(coxph_validator$results$folds),
    FUN = function(x) {
      cbind(
        "model" = x,
        "n_train" = length(coxph_validator$results$folds[[x]]$fold_ids),
        cox_model_output_parser(
          model = coxph_validator$results$folds[[x]]$model
        )
      )
    }
  )
)

# get weighted median, 2.5% and 97.5% quantiles
cox_all_rep_cv_median <- sapply(
  X = colnames(coxph_all_rep_cv)[3:ncol(coxph_all_rep_cv)],
  FUN = function(x) {
    paste_weighted_median_ci(
      x = coxph_all_rep_cv[, get(x)],
      wt = coxph_all_rep_cv[, get("n_train")],
      digits = 3
    )
  }
) %>%
  data.table::as.data.table(keep.rownames = "Predictors")
colnames(cox_all_rep_cv_median)[2] <- "value"

cox_all_rep_cv_median[, ("var") := gsub(
  pattern = ".*(\\_(p|Estimate))$",
  replacement = "\\2",
  x = get("Predictors")
)]
cox_all_rep_cv_median[, ("Predictors") := gsub(
  pattern = "\\_(p|Estimate)$",
  replacement = "",
  x = get("Predictors")
)]

coxph_model_results <- cox_all_rep_cv_median %>%
  data.table::dcast.data.table(
    formula = Predictors ~ var,
    value.var = "value"
  )
colnames(coxph_model_results)[2:3] <- c("est_repcv", "p_repcv")

# format results table for repeated cv
coxph_model_results[, ("Predictors") := format_cox_model_output(
  predictors = get("Predictors"),
  var_names_mapping = var_names_mapping
)]

# generate classical cox-results (with imputed values) (no repeated cv)
coxph_data <- kdry::dtr_matrix2df(
  matrix = ml_dat[ml_dat_part$train, .SD, .SDcols = cox_feature_cols] |>
    data.matrix(),
  cat_vars = ml_cat_vars[which(ml_cat_vars %in% cox_feature_cols)]
)
y <- survival::Surv(
  time = ml_dat[ml_dat_part$train, get(time)],
  event = ml_dat[ml_dat_part$train, get(event)] |>
    as.character() |>
    as.numeric()
)
classic_cox_model <- survival::coxph(
  formula = y ~ .,
  data = coxph_data
)

classic_coxph_model_results <- sjPlot::tab_model(classic_cox_model, digits = 3) |>
  sjtable2df::mtab2df(n_models = 1)
classic_coxph_model_results[, ("Estimate") := paste0(
  get("Estimates"), " ",
  gsub(
    pattern = "^(\\d+\\.\\d+).*((\\d+\\.\\d+)|Inf)$",
    replacement = "[\\1; \\2]",
    x = get("CI")
  )
)]
classic_coxph_model_results <- classic_coxph_model_results[
  ,
  .SD,
  .SDcols = c("Predictors", "Estimate", "p")
]
colnames(classic_coxph_model_results)[2:3] <- c("est_classic", "p_classic")
classic_coxph_model_results[, ("Predictors") := gsub("beforesurg", "before surg", Predictors)]
classic_coxph_model_results[, ("Predictors") := gsub("\\s\\[(\\d)\\]", "\\1", Predictors)]
classic_coxph_model_results[, ("Predictors") := gsub("\\s", "_", Predictors)]

classic_coxph_model_results[, ("Predictors") := format_cox_model_output(
  predictors = get("Predictors"),
  var_names_mapping = var_names_mapping
)]

# combine results of both models to be displayed in on final table
cox_results_combined <- data.table::merge.data.table(
  x = coxph_model_results,
  y = classic_coxph_model_results,
  by = "Predictors",
  all = TRUE,
  sort = FALSE
)
cox_results_combined[is.na(cox_results_combined)] = ""

colnames(cox_results_combined)[2:5] <- c("Median HR (95%-CI)", "Median p-value (95%-CI)",
                                         "HR (95%-CI)", "p-value (95%-CI)")
