# Copyright (C) 2022-2024 Uniklinikum Erlangen

# create a copy of the dataset before removing variables
ml_dat_prep <- data.table::copy(feature_data)


num_vars <- colnames(ml_dat_prep)[ml_dat_prep[, sapply(.SD, is.numeric)]] |>
  sort()
# remove days discharge, as this is an outcome variable, not a feature
num_vars <- setdiff(num_vars, time)

corrmat <- ml_dat_prep[, num_vars, with = FALSE] |>
  cor(method = "pearson")

# rename for better readability of plot
row.names(corrmat) <- row.names(corrmat) |>
  dplyr::recode(!!!var_names_mapping)

colnames(corrmat) <- colnames(corrmat) |>
  dplyr::recode(!!!var_names_mapping)

p_mat <- ggcorrplot::cor_pmat(corrmat)

p_corr_ml <- corrmat |>
  ggcorrplot::ggcorrplot(
    colors = viridis::plasma(n = 3),
    legend.title = "",
    hc.order = TRUE,
    #show.diag = TRUE,
    #p.mat = p_mat,
    sig.level = 0.05,
    insig = "pch",
    ggtheme = ggpubr::theme_pubr(base_size = 12, legend = "right")
  )

ggplot2::ggsave(
  filename = "ml_corrplot.png",
  plot = p_corr_ml,
  device = "png",
  path = here::here("figures"),
  width = 8,
  height = 6,
  dpi = 300
)

ml_dat_prep_desc <- data.table::copy(ml_dat_prep)
ml_dat_prep_desc[, ("heart_defect") := dplyr::recode(
  .x = get("heart_defect"),
  "uni_1" = "UVHD I",
  "bi_cplx" = "BVHD cmplx.",
  "uni_2" = "UVHD II",
  "bi_spl" = "BVHD smpl."
)]
ml_dat_prep_desc[, ("heart_defect_base") := NULL]

# descrtab to data.table for more control when using kableExtra
descr_tab <- DescrTab2::descr(
  dat = ml_dat_prep_desc[, -1],
  group = "deceased",
  group_labels = list("censored" = "Censored", "deceased" = "Deceased"),
  summary_stats_cont = list(
    mean = DescrTab2:::.mean, sd = DescrTab2:::.sd,
    min = DescrTab2:::.min, max = DescrTab2:::.max
  ),
  format_options = list(
    combine_mean_sd = TRUE
  ),
  test_options = list(nonparametric = TRUE)
) |>
  descrtab2df()

# add variable category
descr_tab <- cbind(Category = "", descr_tab)
descr_tab[, ("Category") := dplyr::recode(
  .x = get("Variables"),
  !!!var_category_mapping
)]

# rename Variables to human readable names
descr_tab[, ("Variables") := dplyr::recode(
  .x = get("Variables"),
  !!!var_names_mapping
)]

# reorder for structured displaying
descr_tab <- descr_tab[order(Category, Variables, ` `)]

# prepare footnote
descr_footnote <- unique(descr_tab$Test)
descr_footnote <- descr_footnote[descr_footnote != ""]
# Mann-Whitney U = Wilcoxon Rank Sum test
descr_footnote <- c("Wilcoxon Rank Sum test", descr_footnote[2])

# formatting of p-values for knitr to pdf / latex
descr_tab[, ("p") := ifelse(
  test = grepl("Mann-Whitney", get("Test")),
  yes = paste0(get("p"), "HACKITwrs"),
  no = get("p")
)]
descr_tab[, ("p") := ifelse(
  test = grepl("Pearson", get("Test")),
  yes = paste0(get("p"), "HACKITchi2"),
  no = get("p")
)]

# define features for learning, remove patient-id
rownames(ml_dat_prep) <- ml_dat_prep$patient_id
ml_dat_prep[, ("patient_id") := NULL]
