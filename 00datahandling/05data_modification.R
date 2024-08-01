# Copyright (C) 2022-2024 Uniklinikum Erlangen

############################################# CODE 2 ############################
#############################################

# dataset-modifications
# first save copy
dataset_modify <- data.table::copy(original_dataset)

# percentiles of survival time
dataset_995_time <- quantile(dataset[, get(time)], probs = 0.995)
dataset_995_time_n <- nrow(dataset_modify[get(time) >= ceiling(dataset_995_time), ])
dataset_995_time_n_deceased <- nrow(dataset_modify[get(event) == 1 & get(time) >= ceiling(dataset_995_time), ])
#
# censor at 99.5%
dataset_modify[get(time) >= ceiling(dataset_995_time), `:=` (
  "days_discharge_after_surg" = ceiling(dataset_995_time),
  "deceased" = "0"
)]

set.seed(seed)
dataset_split_criterion <- splitTools::multi_strata(
  df = dataset_modify[, .SD, .SDcols = c(time, event, "heart_defect_base")],
  strategy = "interaction",
  k = 4
)
ml_dat_part <- splitTools::partition(
  y = dataset_split_criterion,
  p = c(train = 0.6, test = 0.4),
  type = "stratified",
  seed = seed
)

########################## train proportions <!-- ##########################
# train proportions
ml_xtab_train <- sjPlot::tab_xtab(
  var.row = dataset_modify[ml_dat_part$train, get(diagnosis_group)],
  var.col = dataset_modify[ml_dat_part$train, get(event)],
  var.labels = c("Diagnosis group", "Deceased"),
  show.row.prc = TRUE
) %>%
  sjtable2df::xtab2df(
    output = "data.frame"
  )
ml_xtab_test <- sjPlot::tab_xtab(
  var.row = dataset_modify[ml_dat_part$test, get(diagnosis_group)],
  var.col = dataset_modify[ml_dat_part$test, get(event)],
  var.labels = c("Diagnosis group", "Deceased"),
  show.row.prc = TRUE
) %>%
  sjtable2df::xtab2df(
    output = "data.frame"
  )
results_dataset_proportions <- data.table::merge.data.table(
  x = ml_xtab_train,
  y = ml_xtab_test,
  by = "Diagnosis group",
  suffixes = c("train", "test"),
  sort = FALSE
) %>%
  data.table::as.data.table()

########################## Kaplan-Meier <!-- ##########################

kaplan_meier_train <- survival::Surv(
  time = dataset_modify[ml_dat_part$train, get(time)],
  event = (dataset_modify[ml_dat_part$train, get(event)] |>
             as.character() |>
             as.integer()),
  type = "right"
)
fit_kaplan_meier_train <- survival::survfit(
  kaplan_meier_train ~ heart_defect,
  data = dataset_modify[ml_dat_part$train, ]
)

surv_30d_train <- tryCatch(
  expr = {
    summary(fit_kaplan_meier_train, times = 30)
  },
  error = function(e) {
    message(e)
    summary(fit_kaplan_meier_train, times = 1)
  }
)


p_km_train <- survminer::ggsurvplot(
  fit = fit_kaplan_meier_train,
  risk.table = TRUE,
  conf.int = FALSE,
  ggtheme = ggpubr::theme_pubr(base_size = 10),
  legend.labs = names(grp_levels),
  legend.title = "",
  palette = wesanderson::wes_palette(name = "IsleofDogs1", n = length(grp_levels))
)

# ggplot2::ggsave does not work directly for ggsurvplot
# (see https://github.com/kassambara/survminer/issues/152)
ggplot2::ggsave(
  filename = "km_train.png",
  plot = survminer:::.build_ggsurvplot(p_km_train),
  device = "png",
  path = here::here("figures"),
  width = 6.5,
  height = 7.5,
  dpi = 300
)

kaplan_meier_test <- survival::Surv(
  time = dataset_modify[ml_dat_part$test, get(time)],
  event = (dataset_modify[ml_dat_part$test, get(event)] |>
             as.character() |>
             as.integer()),
  type = "right"
)
fit_kaplan_meier_test <- survival::survfit(
  kaplan_meier_test ~ heart_defect,
  data = dataset_modify[ml_dat_part$test, ]
)

surv_30d_test <- tryCatch(
  expr = {
    summary(fit_kaplan_meier_test, times = 30)
  },
  error = function(e) {
    message(e)
    summary(fit_kaplan_meier_test, times = 1)
  }
)

p_km_test <- survminer::ggsurvplot(
  fit = fit_kaplan_meier_test,
  risk.table = TRUE,
  conf.int = FALSE,
  ggtheme = ggpubr::theme_pubr(base_size = 10),
  legend.labs = names(grp_levels),
  legend.title = "",
  palette = wesanderson::wes_palette(name = "IsleofDogs1", n = length(grp_levels))
)

# ggplot2::ggsave does not work directly for ggsurvplot
# (see https://github.com/kassambara/survminer/issues/152)
ggplot2::ggsave(
  filename = "km_test.png",
  plot = survminer:::.build_ggsurvplot(p_km_test),
  device = "png",
  path = here::here("figures"),
  width = 6.5,
  height = 7.5,
  dpi = 300
)


########################## Missing Table <!-- ########################## -->

# first, replace laboratory values for cases that died during the surgery
lab_vars <- names(var_category_mapping)[var_category_mapping == "Laboratory analytes"]
relevant_ids <- dataset_modify[
  days_discharge_after_surg == 0 &
    is.na(creatinine_min_after_surg),
  .SD,
  .SDcols = c("patient_id", lab_vars)
][, patient_id]

for (lvar in lab_vars[grepl("after", lab_vars)]) {
  dataset_modify[
    patient_id %in% relevant_ids,
    (lvar) := get(gsub("after", "before", lvar))
  ]
}
# test replacements
dataset_modify[
  patient_id %in% relevant_ids,
  .SD,
  .SDcols = c("patient_id", lab_vars)
]
rm(relevant_ids, lab_vars, lvar)


# total missings
missing_vals_total <- dataset_modify[, lapply(
  X = .SD,
  FUN = function(x) {
    paste_sum_pct(sum(is.na(x)), nrow(dataset_modify))
  }),
  by = diagnosis_group
][order(get(diagnosis_group))] |>
  data.table::transpose(keep.names = "Variable")

colnames(missing_vals_total)[2:3] <- c("uni_total", "bi_total")
colnames(missing_vals_total)[2:5] <- c("uni1_total", "uni2_total", "bicplx_total", "bispl_total")
missing_vals_total <- missing_vals_total[-1, ]

# training split missings
missing_vals_train <- dataset_modify[ml_dat_part$train, lapply(
  X = .SD,
  FUN = function(x) {
    paste_sum_pct(sum(is.na(x)), nrow(dataset_modify))
  }),
  by = diagnosis_group
][order(get(diagnosis_group))] |>
  data.table::transpose(keep.names = "Variable")

colnames(missing_vals_train)[2:3] <- c("uni_train", "bi_train")
colnames(missing_vals_train)[2:5] <- c("uni1_train", "uni2_train", "bicplx_train", "bispl_train")
missing_vals_train <- missing_vals_train[-1, ]

# test split missings
missing_vals_test <- dataset_modify[ml_dat_part$test, lapply(
  X = .SD,
  FUN = function(x) {
    paste_sum_pct(sum(is.na(x)), nrow(dataset_modify))
  }),
  by = diagnosis_group
][order(get(diagnosis_group))] |>
  data.table::transpose(keep.names = "Variable")

colnames(missing_vals_test)[2:3] <- c("uni_test", "bi_test")
colnames(missing_vals_test)[2:5] <- c("uni1_test", "uni2_test", "bicplx_test", "bispl_test")
missing_vals_test <- missing_vals_test[-1, ]

# merge to one big table
missing_vals <- data.table::merge.data.table(
  x = missing_vals_total,
  y = missing_vals_train,
  by = "Variable"
)
missing_vals <- data.table::merge.data.table(
  x = missing_vals,
  y = missing_vals_test,
  by = "Variable"
)

# get absolute missings per variable
absolute_missings <- dataset_modify[, lapply(.SD, function(x) {sum(is.na(x))}), .SDcols = colnames(dataset_modify)] |>
  t() |>
  data.table::as.data.table(keep.rownames = TRUE)
# only those with missings
absolute_missings <- absolute_missings[which(absolute_missings$V1 > 0), ]

missing_vals <- missing_vals[Variable %in% absolute_missings$rn, ]
missing_vals <- cbind(name = "", missing_vals)

missing_vals[grepl("before", Variable), ("Timepoint") := "before"]
missing_vals[grepl("after", Variable), ("Timepoint") := "after"]

# make pretty
missing_vals_variables <- missing_vals[, get("Variable")]

# rename analyte types
for (var_name in names(laboratory_analytes_list$value_list2)) {
  missing_vals[
    grepl(pattern = var_name, x = get("Variable")),
    ("name") := laboratory_analytes_list$value_list2[[var_name]]
  ]
}

# rename variables
for (var_name in names(laboratory_analytes_list$var_list)) {
  missing_vals[
    grepl(pattern = var_name, x = get("Variable")),
    ("Variable") := gsub(
      pattern = "(.*) \\[.*",
      replacement = "\\1",
      x = laboratory_analytes_list$var_list[[var_name]]
    )
  ]
}

# set demographics
missing_vals[get("Variable") == "height", ("name") := "Demographics"]
missing_vals[get("Variable") == "height", ("Variable") := "Height"]
missing_vals[get("Variable") == "weight", ("name") := "Demographics"]
missing_vals[get("Variable") == "weight", ("Variable") := "Weight"]

missing_vals <- data.table::rbindlist(
  l = list(
    missing_vals[get("name") == "Demographics", ],
    missing_vals[get("name") != "Demographics", ][order(Variable, -name, -Timepoint)]
  )
)
missing_vals[is.na(missing_vals)] <- ""

# remove before
missing_vals <- missing_vals[Timepoint != "before", ][, .SD, .SDcols = !"Timepoint"]

missing_vals <- rbind(
  as.vector(c("Type", "Variable", rep(c("UVHF I", "UVHF II", "BVHF cmplx.", "BVHF smpl."), 3))),
  data.frame(missing_vals)
)
colnames(missing_vals) <- c(" ", " ", rep("Total", 4), rep("Training Dataset", 4), rep("Holdout Test Dataset", 4))

# remove missings with threshold here
# exclude missings
absolute_missings$rel <- absolute_missings$V1 / nrow(dataset_modify)
exclude_missing_names <- absolute_missings[rel > 0.05, get("rn")]
final_dataset_cols <- setdiff(colnames(dataset_modify), exclude_missing_names)

# finally
dataset_modify <- dataset_modify[, .SD, .SDcols = final_dataset_cols]

# ########################## Impute Missings <!-- ########################## -->

impute_ignore_vector <- (1:nrow(dataset_modify) %in% ml_dat_part$test)
imp_model <- mice::mice(
  data = dataset_modify,
  m = 10,
  ignore = impute_ignore_vector
)

#imp_complete <- mice::complete(imp_model)
imp_complete <- sjmisc::merge_imputations(
  dat = dataset_modify,
  imp = imp_model
)

columns_imputed <- data.table::as.data.table(imp_complete)

dataset_imputed <- cbind(
  dataset_modify[, .SD, .SDcols = setdiff(colnames(dataset_modify), colnames(columns_imputed))],
  columns_imputed
)
