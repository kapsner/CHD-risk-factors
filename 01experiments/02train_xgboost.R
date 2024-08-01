# Copyright (C) 2022-2024 Uniklinikum Erlangen

xgboost_args <- list(
  train_x = train_x,
  train_y = train_y,
  test_x = test_x,
  seed = seed,
  hparam_nfolds = hparam_nfolds,
  n_cores = n_cores,
  optim_args = optim_args,
  train_split_criterion = train_split_criterion,
  validation_fold_list = validation_fold_list,
  var_names_mapping = var_names_mapping,
  cat_vars = NULL,
  only_full_shap = TRUE
)

xgboost_results <- do.call(xgboost_trainer, xgboost_args)


# hyperparameter search
cols_to_plot <- which(colnames(xgboost_results$xgboost_tuning_params) %in% c(
  names(xgboost_results$param_grid_xgboost)
))

p_xgboost_pcp <- kdry::plt_parallel_coordinates(
  data = xgboost_results$xgboost_tuning_params,
  cols = colnames(xgboost_results$xgboost_tuning_params)[cols_to_plot],
  color_variable = "metric_optim_mean",
  color_args = list(option = "inferno",
                    begin = 0.1,
                    end = 0.9,
                    alpha = 0.6,
                    direction = -1)
) +
  ggpubr::theme_pubr(base_size = 15, legend = "right") +
  ggplot2::labs(
    title = "Parallel coordinates: plot of xgboost's hyperparameter tuning",
    color = xgboost_results$xgboost_tuning_params$eval_metric[1]
  ) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = "pcp_xgboost.png",
  plot = p_xgboost_pcp,
  device = "png",
  path = here::here("figures"),
  width = 10,
  height = 7,
  dpi = 300
)


p_xgboost_shap <- shapviz::sv_importance(
  object = xgboost_results$shap_list$full$shapvis_obj,
  kind = "beeswarm",
  max_display = Inf,
) +
  ggpubr::theme_classic2(base_size = 18) +
  ggplot2::ggtitle("XGB")
ggplot2::ggsave(
  filename = "shap_xgboost.png",
  plot = p_xgboost_shap,
  device = "png",
  path = here::here("figures"),
  width = 10,
  height = 6,
  dpi = 300
)
