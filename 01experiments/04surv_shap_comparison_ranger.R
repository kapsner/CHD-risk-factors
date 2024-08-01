# Copyright (C) 2022-2024 Uniklinikum Erlangen

# iterate over all models in parallel
progressr::handlers("progress")
model_names <- names(ranger_results$ranger_validator$results$folds)

future::plan("multisession", workers = 8)
progressr::with_progress({

  p <- progressr::progressor(along = model_names)

  set.seed(seed)
  full_survshap_results <- future.apply::future_sapply(
    X = seq_len(length(model_names)),
    FUN = function(x) {
      m_name <- model_names[x]
      explainer <- survex::explain(
        model = ranger_results$ranger_validator$results$folds[[m_name]]$model,
        data = train_x,
        y = train_y,
        times = seq(1, unname(ceiling(dataset_995_time)), 1)
      )
      set.seed(seed)
      ret <- survex::model_survshap(
        explainer,
        new_observation = test_x,
        y_true = test_y,
        aggregation_method = "mean_absolute",
        calculation_method = "treeshap"
      )
      p(sprintf("x=%g", x))
      ret
    },
    future.seed = TRUE,
    USE.NAMES = TRUE,
    simplify = FALSE
  )
})
future::plan("sequential")
gc();gc()

# list of aggregated global survshap for each model
temp_survshap_res <- sapply(
  as.character(seq_len(length(full_survshap_results))),
  function(x) {
    i <- as.integer(x)
    res <- full_survshap_results[[i]]
    # aggregate global survshap for each model
    survex:::aggregate_shap_multiple_observations(
      shap_res_list = res$result,
      feature_names = colnames(res$result[[1]]),
      mean
    ) |>
      data.table::as.data.table(keep.rownames = TRUE)
  },
  USE.NAMES = TRUE,
  simplify = FALSE
)

# put in big dataframe
dtList <- data.table::rbindlist(
  l = temp_survshap_res,
  use.names = TRUE,
  idcol = TRUE
)

# aggregate over all models
full_survshap <- dtList[
  , lapply(.SD, mean), by = rn, .SDcols = colnames(test_x)
]

survshap_values <- full_survshap[, .SD, .SDcols = setdiff(colnames(full_survshap), "rn")]
# transform to data.frame to make everything compatible with
# previous code
survshap_values <- as.data.frame(survshap_values)
rownames(survshap_values) <- survshap_values$rn

tmp_survshap_container <- full_survshap_results[[1]]
tmp_survshap_container$result <- survshap_values
tmp_survshap_container$aggregate <- colMeans(survshap_values)
