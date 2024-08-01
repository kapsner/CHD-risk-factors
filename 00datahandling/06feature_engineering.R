# Copyright (C) 2022-2024 Uniklinikum Erlangen

# create a copy of the dataset before adding new variables
feature_data <- data.table::copy(dataset_imputed)

variables_to_remove <- c("height", "year_of_admission", "days_duration_of_stay",
                         "age_days_at_admission",
                         "creatinine_min_after_surg", "urea_min_after_surg",
                         "crp_min_after_surg", "leukocytes_max_after_surg")

# weight_below_2500g
feature_data[, ("low_weight") := dplyr::case_when(
  get("weight") >= 2.5 ~ "0",
  get("weight") < 2.5 ~ "1"
)]
feature_data[, ("low_weight") := factor(get("low_weight"))]
variables_to_remove <- c(variables_to_remove, "weight")

feature_data[, (variables_to_remove) := lapply(.SD, function(x) NULL), .SDcols = variables_to_remove]

# add new variables to mapping vector
var_names_mapping <- c(
  var_names_mapping,
  "low_weight" = "Weight < 2500 g"
)
var_category_mapping <- c(
  var_category_mapping,
  "low_weight" = "Demographics"
)

# low_weight
plt_dat <- feature_data[
  ,
  c("deceased", "heart_defect", "low_weight"),
  with = FALSE
]
plt_dat[, ("heart_defect") := dplyr::recode_factor(
  .x = get("heart_defect"),
  "univentricular" = "Univentricular",
  "biventricular" = "Biventricular"
)]
