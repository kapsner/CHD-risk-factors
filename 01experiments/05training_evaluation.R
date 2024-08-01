# Copyright (C) 2022-2024 Uniklinikum Erlangen

library(magrittr)
ml_tuner <- list(
  ranger_results$ranger_tuner,
  xgboost_results$xgboost_tuner
)
mlexperiments::validate_fold_equality(ml_tuner)

ml_experiments <- list(
  ranger_results$ranger_validator,
  xgboost_results$xgboost_validator
)
mlexperiments::validate_fold_equality(ml_experiments)


validation_results <- rbind(
  cbind(name = "xgboost", performance = xgboost_results$res_xgboost$performance),
  cbind(name = "ranger", performance = ranger_results$res_ranger$performance)
) |>
  data.table::as.data.table()
validation_results[, ("performance") := as.numeric(get("performance"))]

ggpubr::ggboxplot(
  data = validation_results,
  x = "name",
  y = "performance"
)

p_validation_ml <- ggpubr::ggboxplot(
  data = validation_results,
  x = "name",
  xlab = "Algorithm",
  y = "performance",
  add = "dotplot",
  add.params = list(size = 0.5),
  ylab = "C-index",
  ylim = c(0, 1),
  ggtheme = ggpubr::theme_pubr(base_size = 15)
) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

ggplot2::ggsave(
  filename = "final_validation_results.png",
  plot = p_validation_ml,
  device = "png",
  path = here::here("figures"),
  width = 10,
  height = 7,
  dpi = 300
)


top_xgboost <- xgboost_results$shap_list$full$importance[1:top_n]

top_ranger <- ranger_results$shap_list$full$importance[1:top_n]

# most important features:
top_features_union <- union(
  x = names(top_xgboost),
  y = names(top_ranger)
)

top_features_union <- sort(top_features_union)
length(top_features_union)

top_features_overview <- data.table::data.table(
  "Top features" = top_features_union,
  "Xgboost" = sapply(
    X = top_features_union,
    FUN = function(x) {
      ifelse(
        test = x %in% names(top_xgboost),
        yes = format(top_xgboost[x], digits = 2, nsmall = 2),
        no = "n/a"
      )
    },
    simplify = FALSE
  ),
  "Ranger" =  sapply(
    X = top_features_union,
    FUN = function(x) {
      ifelse(
        test = x %in% names(top_ranger),
        yes = format(top_ranger[x], digits = 2, nsmall = 2),
        no = "n/a"
      )
    },
    simplify = FALSE
  )
)
