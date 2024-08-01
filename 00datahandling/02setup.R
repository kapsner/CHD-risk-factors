# Copyright (C) 2022-2024 Uniklinikum Erlangen

# requires R 4.3.2
# sudo apt install r-base-core_4.3.2-1.2204.0

##################### !!!!! ##########################
# run with: rmarkdown::render_site(encoding = 'UTF-8')
##################### !!!!! ##########################
library(ParBayesianOptimization)
library(xgboost)
library(ranger)
library(fastshap)
library(PearsonDS)
requireNamespace("mlr3measures")
library(DIZtools)

# set global seed-variable here to be used later
seed <- 123

# top-n most important features to be used for proportional-hazards
top_n <- 5

# get n_cores
#n_cores <- pmin(parallel::detectCores() - 1L, 32L)
n_cores <- ifelse(
  test = parallel::detectCores() >= 8,
  yes = 8L,
  no = ifelse(
    test = parallel::detectCores() >= 4,
    yes = 4L,
    no = 2L
  )
)

if (isTRUE(debug_run)) {
  knitr_cache <- FALSE

  hparam_nfolds <- 3L
  outer_nfolds <- 10L
  cv_repetitions <- 5L

  stratified_cox <- NULL

  shap_nsim <- 2L

  niter_bayes <- n_cores * 2L
  time_limit <- 2L # time limit for bayesian hyperparameter optimization
  n_random_search <- 10L

} else {
  knitr_cache <- FALSE

  hparam_nfolds <- 3L
  outer_nfolds <- 10L
  cv_repetitions <- 10L

  stratified_cox <- NULL

  shap_nsim <- 100L

  niter_bayes <- 128L

  time_limit <- 30L # time limit for bayesian hyperparameter optimization
  n_random_search <- NULL
}

# setup renv
renv_version <- "1.0.3"
if (packageVersion("renv") != renv_version) {
  remotes::install_version(package = "renv", version = renv_version)
}

# # first, initialize
# renv::init()
# renv::snapshot()

# restore renv; use .Library.site as this prevents issues when using parallel
# backend.
# on installation issues: options(install.opts = "--no-lock")
renv::restore(library = .Library.site, prompt = FALSE)

# set folder for figures
figurepath <- here::here("figures")

# create directory, if it does not exists
if (!dir.exists(figurepath)) {
  dir.create(figurepath)
}
