# Copyright (C) 2022-2024 Uniklinikum Erlangen

ml_dat <- data.table::copy(ml_dat_prep)
# recode heart defect:
ml_dat[, `:=` (
  "heart_defect" = dplyr::recode_factor(
    .x = get("heart_defect"),
    "uni_1" = 0,
    "uni_2" = 1,
    "bi_cplx" = 2,
    "bi_spl" = 3
  ),
  "sex" = dplyr::recode_factor(
    .x = get("sex"),
    "m" = 0,
    "w" = 1
  )
)]

# save vector with categorical data elements
ml_cat_vars <- colnames(ml_dat)[ml_dat[, sapply(X = .SD, FUN = is.factor)]]

set.seed(seed)
train_split_criterion <- splitTools::multi_strata(
  df = ml_dat[ml_dat_part$train, .SD, .SDcols = c(time, event, diagnosis_group)],
  strategy = "interaction",
  k = 4
)
hparam_fold_list <- splitTools::create_folds(
  y = train_split_criterion,
  k = hparam_nfolds,
  type = "stratified",
  seed = seed
)

validation_fold_list <- splitTools::create_folds(
  y = train_split_criterion,
  k = outer_nfolds,
  m_rep = cv_repetitions,
  type = "stratified",
  seed = seed
)


remove_cols <- c("deceased", "days_discharge_after_surg", "heart_defect_base")
feature_cols <- setdiff(colnames(ml_dat), remove_cols)
ml_cat_vars <- setdiff(ml_cat_vars, "heart_defect_base")

bin_vars <- c("sex", "malformations", "syndrome", "chromosomal",
              "pulm_art_hypertension", "circulatory_arrest",
              "open_thorax", "low_weight")
cat_vars <- c("n_previous_admissions", "heart_defect", "bypass_group",
              "hypothermia_group")
num_vars <- setdiff(feature_cols, c(bin_vars, cat_vars))
stopifnot(
  length(intersect(c(bin_vars, cat_vars, num_vars), feature_cols)) ==
    length(feature_cols)
)

train_x <- ml_dat[ml_dat_part$train, .SD, .SDcols = feature_cols] |>
  data.matrix()
train_y <- survival::Surv(
  time = ml_dat[ml_dat_part$train, get(time)],
  event = (ml_dat[ml_dat_part$train, get(event)] |>
             as.character() |>
             as.integer()),
  type = "right"
)

test_x <- ml_dat[ml_dat_part$test, .SD, .SDcols = feature_cols] |>
  data.matrix()
test_y <- survival::Surv(
  time = ml_dat[ml_dat_part$test, get(time)],
  event = (ml_dat[ml_dat_part$test, get(event)] |>
             as.character() |>
             as.integer()),
  type = "right"
)

optim_args <- list(
  iters.n = niter_bayes,
  acq = "ucb",
  kappa = 3.5
)
