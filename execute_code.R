# Copyright (C) 2022-2024 Uniklinikum Erlangen

here::set_here(getwd())
library(magrittr)
library(data.table)

debug_run <- FALSE

# datahandling
source(here::here("00datahandling", "02setup.R"))
source(here::here("00datahandling", "03data_import.R"))
source(here::here("00datahandling", "05data_modification.R"))
source(here::here("00datahandling", "06feature_engineering.R"))
source(here::here("00datahandling", "07feature_selection.R"))

# experiments
source(here::here("R", "shap_comparison.R"))
source(here::here("R", "xgboost_trainer.R"))
source(here::here("R", "ranger_trainer.R"))
source(here::here("01experiments", "01train_preparations.R"))
source(here::here("01experiments", "02train_xgboost.R"))
source(here::here("01experiments", "03train_ranger.R"))
source(here::here("01experiments", "04surv_shap_comparison_ranger.R"))
source(here::here("01experiments", "05training_evaluation.R"))
source(here::here("01experiments", "06train_coxph.R"))

gc();gc()
save.image(file = here::here(".all_data"))
rm(list = ls())
gc();gc()
.rs.restartR()
