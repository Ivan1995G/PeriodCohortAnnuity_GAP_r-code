## General configuration block.
## main.R defines `root` and `population` before executing this file.

if (!exists("root", inherits = FALSE) || !exists("population", inherits = FALSE)) {
  stop("R/config.R must be executed by main.R.", call. = FALSE)
}

population <- tolower(trimws(population))
if (population == "total population") population <- "total"
allowed_populations <- c("female", "male", "total")
if (!population %in% allowed_populations) {
  stop("Population must be one of: female, male, total.", call. = FALSE)
}

country_code <- "ITA"
sex_series <- population

years_start <- 1960L
years_valid <- 2000:2010
years_test <- 2011:2019

ages_fit <- 60:100
x_ret <- 67L
n_best_models <- 5L

years_liability <- 2020:2050
i_rate <- 0.03
flat_rate <- i_rate
benefit_annual <- 1
v_discount <- 1 / (1 + i_rate)

eiopa_reference_date <- as.Date("2019-12-31")
eiopa_workbook <- file.path(
  root, "data", "eiopa", "EIOPA_RFR_20191231_Term_Structures.xlsx"
)
camarda_functions_file <- file.path(
  root, "R", "vendor", "camarda_cpspline_functions.R"
)

output_dir <- file.path(root, "output", population)
graph_dir <- file.path(root, "graph", population)
reference_dir <- file.path(root, "data", "reference")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(graph_dir, recursive = TRUE, showWarnings = FALSE)
