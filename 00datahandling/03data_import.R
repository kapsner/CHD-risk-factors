# Copyright (C) 2022-2024 Uniklinikum Erlangen

dataset_file <- here::here("export", "dataset.csv")

original_dataset <- data.table::fread(
  file = dataset_file
)

# cast datatypes - factors
cat_vars <- c(
  "sex",
  "year_of_admission",
  "heart_defect",
  "history_max_defect",
  "malformations",
  # "syndrome",
  "chromosomal",
  "pulm_art_hypertension",
  "deceased",
  "bypass_group",
  "circulatory_arrest",
  "hypothermia_group",
  "open_thorax"
)
original_dataset[, (cat_vars) := lapply(.SD, factor), .SDcols = cat_vars]

# numeric variables
num_vars <- colnames(original_dataset)[original_dataset[, sapply(.SD, is.numeric)]]
original_dataset[, (num_vars) := lapply(.SD, as.numeric), .SDcols = num_vars]



# define some global variables to be used later on
time <- "days_discharge_after_surg"
event <- "deceased"
diagnosis_group <- "heart_defect"



# variable names mapping-vector for more readable names in plots etc.
var_names_mapping <- c(
  "patient_id" = "Patient ID",
  "sex" = "Sex",
  "height" = "Height",
  "weight" = "Weight",
  "year_of_admission" = "Year of admission",
  "n_previous_admissions" = "No. of previous admissions",
  "days_duration_of_stay" = "Duration of hospital stay",
  "days_admission_before_surg" = "Days between admission and surgery",
  "days_discharge_after_surg" = "Days until discharge after surgery",
  "age_days_at_admission" = "Age at admission",
  "heart_defect" = "Disease group",
  "history_max_defect" = "Heart disease history",
  "malformations" = "Malformations",
  # "syndrome" = "Syndrome",
  "chromosomal" = "Chrom. alterations",
  "pulm_art_hypertension" = "Pulm. hypertension",
  "deceased" = "Deceased",
  "age_days_at_surg" = "Age at surgery",
  "bypass_group" = "Heart lung machine during surgery",
  "circulatory_arrest" = "Circulatory arrest during surgery",
  "hypothermia_group" = "Hypothermia during surgery",
  "aortic_cross_clamp_time" = "Aortic cross clamp time",
  "open_thorax" = "Open thorax",
  "creatinine_min_after_surg" = "Serum creatinine (minimum)",
  "creatinine_max_after_surg" = "Serum creatinine (maximum)",
  "creatinine_min_before_surg" = "Serum creatinine - pre (minimum)",
  "creatinine_max_before_surg" = "Serum creatinine - pre (maximum)",
  "urea_min_after_surg" = "Urea (minimum)",
  "urea_max_after_surg" = "Urea (maximum)",
  "urea_min_before_surg" = "Urea - pre (minimum)",
  "urea_max_before_surg" = "Urea - pre (maximum)",
  "crp_min_after_surg" = "C-reactive protein (minimum)",
  "crp_max_after_surg" = "C-reactive protein (maximum)",
  "crp_min_before_surg" = "C-reactive protein - pre (minimum)",
  "crp_max_before_surg" = "C-reactive protein - pre (maximum)",
  "leukocytes_min_after_surg" = "Leukocytes (minimum)",
  "leukocytes_max_after_surg" = "Leukocytes (maximum)",
  "leukocytes_min_before_surg" = "Leukocytes - pre (minimum)",
  "leukocytes_max_before_surg" = "Leukocytes - pre (maximum)"
)

names_tmp <- grep(pattern = "leuko|crea|urea|crp", x = names(var_names_mapping), value = TRUE)
var_category_mapping <- rep("Laboratory analytes", length(names_tmp))
names(var_category_mapping) <- names_tmp

names_tmp <- grep(pattern = "age_days.*admission|sex|height|weight", x = names(var_names_mapping), value = TRUE)
var_category_append <- rep("Demographics", length(names_tmp))
names(var_category_append) <- names_tmp
var_category_mapping <- c(var_category_mapping, var_category_append)

names_tmp <- grep(pattern = "days_duration|days_admission|days_discharge|year|n_previous", x = names(var_names_mapping), value = TRUE)
var_category_append <- rep("Encounter-related", length(names_tmp))
names(var_category_append) <- names_tmp
var_category_mapping <- c(var_category_mapping, var_category_append)

names_tmp <- grep(pattern = "history|heart|deceased|malfor|syndr|chromo|pulm_art", x = names(var_names_mapping), value = TRUE)
var_category_append <- rep("Disease-related", length(names_tmp))
names(var_category_append) <- names_tmp
var_category_mapping <- c(var_category_mapping, var_category_append)

names_tmp <- grep(pattern = "age_days.*surg|open|hypothermia|bypass|aortic|circulatory", x = names(var_names_mapping), value = TRUE)
var_category_append <- rep("Surgery-related", length(names_tmp))
names(var_category_append) <- names_tmp
var_category_mapping <- c(var_category_mapping, var_category_append)

rm(var_category_append)
