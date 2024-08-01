# Copyright (C) 2022-2024 Uniklinikum Erlangen

ranger_args <- list(
  train_x = train_x,
  train_y = train_y,
  test_x = test_x,
  seed = seed,
  hparam_nfolds = hparam_nfolds,
  n_cores = n_cores,
  optim_args = optim_args,
  train_split_criterion = train_split_criterion,
  validation_fold_list = validation_fold_list,
  cat_vars = setdiff(ml_cat_vars, c(diagnosis_group, event)),
  var_names_mapping = var_names_mapping,
  only_full_shap = TRUE
)

ranger_results <- do.call(ranger_trainer, ranger_args)


# hyperparameter search
cols_to_plot <- which(colnames(ranger_results$ranger_tuning_params) %in% c(
  names(ranger_results$param_grid_ranger)
))

p_ranger_pcp <- kdry::plt_parallel_coordinates(
  data = ranger_results$ranger_tuning_params,
  cols = colnames(ranger_results$ranger_tuning_params)[cols_to_plot],
  color_variable = "metric_optim_mean"
) +
  ggpubr::theme_pubr(base_size = 15, legend = "right") +
  ggplot2::labs(
    title = "Parallel coordinates: plot of ranger's hyperparameter tuning",
    color = ranger_results$ranger_tuning_params$eval_metric[1]
  ) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = "pcp_ranger.png",
  plot = p_ranger_pcp,
  device = "png",
  path = here::here("figures"),
  width = 10,
  height = 7,
  dpi = 300
)


p_ranger_shap <- shapviz::sv_importance(
  object = ranger_results$shap_list$full$shapvis_obj,
  kind = "beeswarm",
  max_display = Inf,
) +
  ggpubr::theme_classic2(base_size = 18) +
  ggplot2::ggtitle("RSF")
ggplot2::ggsave(
  filename = "shap_ranger.png",
  plot = p_ranger_shap,
  device = "png",
  path = here::here("figures"),
  width = 10,
  height = 6,
  dpi = 300
)
