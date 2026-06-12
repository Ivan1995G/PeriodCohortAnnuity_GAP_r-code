## ==============================================================
## Main entry point for manuscript AAS-2026-0050
## Single-population mortality ensemble and annuity valuation
## ==============================================================

## Select the population:
## "female", "male", or "total population" (use "total" below).
population <- "total"

## 1) Required packages -------------------------------------------------------
required_packages <- c(
  "demography", "StMoMo", "dplyr", "tidyr", "tibble", "purrr",
  "ggplot2", "forecast", "mgcv", "svcm", "MortalitySmooth", "readxl"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Restore the renv environment. Missing packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(demography)
  library(StMoMo)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(ggplot2)
  library(forecast)
  library(mgcv)
  library(svcm)
  library(MortalitySmooth)
  library(readxl)
})
options(stringsAsFactors = FALSE)

analysis <- new.env(parent = globalenv())
analysis$root <- root
analysis$population <- population

run_module <- function(filename) {
  sys.source(file.path(root, "R", filename), envir = analysis)
}

## 2) General pipeline configuration -----------------------------------------
run_module("config.R")

## 3) Auxiliary functions ----------------------------------------------------
run_module("functions_auxiliary.R")
run_module("functions_ensemble.R")
run_module("functions_financial.R")
if (!file.exists(analysis$camarda_functions_file)) {
  stop("Missing Camarda functions file: ", analysis$camarda_functions_file)
}
sys.source(analysis$camarda_functions_file, envir = analysis)

## 4) Required mortality data ------------------------------------------------
run_module("data.R")

## 5) Statistical mortality models ------------------------------------------
run_module("mortality_models.R")

## 6) Complete pipeline for the selected population --------------------------
run_module("pipeline_single_population.R")

## 7) Financial component required by the paper ------------------------------
run_module("financial_pipeline.R")

## 8) Separate figures and final numerical outputs ---------------------------
run_module("graphs.R")
run_module("outputs.R")

results_mortality_ensemble_test <- analysis$results_mortality_ensemble_test

cat("\nDONE: mortality ensemble validation/test/liability completed.\n")
cat("Population:", analysis$sex_series, "\n")
cat("Models considered:", paste(analysis$model_names, collapse = ", "), "\n")
cat("Best models selected:", paste(analysis$selected_players, collapse = ", "), "\n")
cat("Graphs:", analysis$graph_dir, "\n")
cat("Numerical outputs:", analysis$output_dir, "\n")

