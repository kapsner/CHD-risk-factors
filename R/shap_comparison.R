# Copyright (C) 2022-2024 Uniklinikum Erlangen

generate_shap_comparison <- function(
    validator,
    type = c("ranger", "xgboost"),
    selected_models,
    train_x,
    test_x,
    var_names_mapping,
    best_models = c("min", "mean", "median", "max"),
    only_full_shap = FALSE
    ) {

  type <- match.arg(type)
  #############################################################################
  ### Compute SHAP for all models
  #############################################################################
  shap_list <- list()

  shap_objects_full <- sapply(
    X = names(validator$results$folds),
    FUN = function(x) {
      if (type == "ranger") {
        unified <- treeshap::ranger_surv.unify(
          rf_model = validator$results$folds[[x]]$model,
          data = train_x
        )
        set.seed(123)
        tmp_shap <- treeshap::treeshap(
          unified_model = unified,
          x = test_x
        )
        shap_obj <- shapviz::shapviz(
          tmp_shap,
          X = test_x
        )
      } else if (type == "xgboost") {
        shap_obj <- shapviz::shapviz(
          validator$results$folds[[x]]$model,
          X_pred = test_x
        )
      }
      return(shap_obj)
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )

  shap_list[["raw_results"]] <- shap_objects_full

  # aggregated shap
  dtList <- data.table::rbindlist(
    l = sapply(
      shap_objects_full,
      function(x) {
        x$S |>
          data.table::as.data.table()
      },
      USE.NAMES = TRUE, simplify = FALSE
    ),
    use.names = TRUE,
    idcol = TRUE
  )
  full_shap <- dtList[
    , ("rn") := (1:.N), by = .id
  ][
    , lapply(.SD, mean), by = rn, .SDcols = colnames(test_x)
  ][
    , rn := NULL
  ][]

  # aggregated baseline
  baseline <- sapply(
    shap_objects_full,
    function(x) {
      x$baseline
    },
    USE.NAMES = FALSE, simplify = TRUE
  )

  shapviz_full <- shapviz::shapviz(
    object = as.matrix(full_shap),
    X = test_x,
    baseline = mean(baseline)
  )
  colnames(shapviz_full$S) <- colnames(shapviz_full$S) |>
    dplyr::recode(!!!var_names_mapping)
  colnames(shapviz_full$X) <- colnames(shapviz_full$S)

  shap_list[["full"]][["importance"]] <- shapviz::sv_importance(
    object = shapviz_full,
    kind = "no",
    max_display = Inf,
  ) |> round(digits = 3)
  shap_list[["full"]][["importance_names"]] <- names(
    shap_list[["full"]][["importance"]]
  )
  shap_list[["full"]][["imp_rank"]] <- rank(
    x = shap_list[["full"]][["importance"]],
    ties.method = "min"
  )
  shap_list[["full"]][["shapvis_obj"]] <- shapviz_full

  if (isFALSE(only_full_shap)) {
    #############################################################################
    ### Get shap values
    #############################################################################
    for (t in best_models) {
      model_type <- selected_models[[t]]

      shap_viz_obj <- shap_objects_full[[model_type]]

      shap_list[[t]][["shap"]] <- shap_viz_obj$S

      colnames(shap_viz_obj$S) <- colnames(shap_viz_obj$S) |>
        dplyr::recode(!!!var_names_mapping)
      colnames(shap_viz_obj$X) <- colnames(shap_viz_obj$S)

      shap_list[[t]][["importance"]] <- shapviz::sv_importance(
        object = shap_viz_obj,
        kind = "no",
        max_display = Inf,
      ) |> round(digits = 3)
      shap_list[[t]][["importance_names"]] <- names(
        shap_list[[t]][["importance"]]
      )
      shap_list[[t]][["imp_rank"]] <- rank(
        x = shap_list[[t]][["importance"]],
        ties.method = "min"
      )
      shap_list[[t]][["shapvis_obj"]] <- shap_viz_obj
    }
  }
  return(shap_list)
}


extract_importance_comparison <- function(shap_list) {
  imp_tmp <- sapply(
    X = c("importance", "imp_rank"),
    FUN = function(y) {
      sapply(
        X = names(shap_list),
        FUN = function(x) {
          shap_list[[x]][[y]] |>
            data.table::as.data.table(keep.rownames = TRUE)
        },
        simplify = FALSE,
        USE.NAMES = TRUE
      ) |>
        data.table::rbindlist(
          idcol = TRUE
        )
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )

  imp_comb_tmp <- data.table::merge.data.table(
    x = imp_tmp$importance,
    y = imp_tmp$imp_rank,
    by = c(".id", "V1")
  )
  colnames(imp_comb_tmp)[3:4] <- c("shap", "rank")
  imp_comb_tmp[, ("rank") := (abs(get("rank") - nrow(imp_comb_tmp)) + 1)]
  imp_comb_tmp[, ("comb") := paste0(shap, " (", rank, ")")]

  imp_rank <- data.table::dcast.data.table(
    data = imp_comb_tmp,
    formula = V1 ~ .id,
    value.var = "comb"
  )
  # merge full for sorting
  imp_rank <- data.table::merge.data.table(
    x = imp_rank,
    y = imp_comb_tmp[get(".id") == "full", .(V1, rank)],
    by = "V1"
  )
  return(imp_rank[order(rank, decreasing = FALSE)][, ("rank") := NULL])
}


imp_most_important <- function(shap_list, n, best_models = c("min", "mean", "median", "max")) {
  n_most_important <- data.table::data.table(
    "Variable" = shap_list$full$importance_names[1:n]
  )
  join_tmp <- sapply(
    X = best_models,
    FUN = function(bm) {
      sapply(
        X = n_most_important$Variable,
        FUN = function(x) {
          x %in% shap_list[[bm]]$importance_names[1:n]
        },
        simplify = FALSE,
        USE.NAMES = TRUE
      )
    }
  ) |>
    data.table::as.data.table(keep.rownames = TRUE)

  n_most_important <- data.table::merge.data.table(
    x = n_most_important,
    y = join_tmp,
    by.x = "Variable",
    by.y = "rn",
    sort = FALSE
  )

  n_most_important[, (best_models) := lapply(.SD, as.numeric), .SDcols = best_models]

  n_most_important <- data.table::rbindlist(
    l = list(
      n_most_important,
      cbind(Variable = "SUM", n_most_important[, lapply(.SD, sum), .SDcols = best_models])
    )
  )
  return(n_most_important)
}
