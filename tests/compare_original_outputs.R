original_rds <- Sys.getenv(
  "ORIGINAL_RDS",
  file.path("..", "_equivalence", "original_male_output",
            "results_mortality_ensemble_eiopa.rds")
)
modular_rds <- Sys.getenv(
  "MODULAR_RDS",
  file.path("output", "male", "results_mortality_ensemble_eiopa.rds")
)
report_file <- Sys.getenv(
  "COMPARISON_REPORT",
  file.path("output", "male", "original_equivalence_check.csv")
)

if (!file.exists(original_rds)) {
  stop("Original RDS not found: ", original_rds, call. = FALSE)
}
if (!file.exists(modular_rds)) {
  stop("Modular RDS not found: ", modular_rds, call. = FALSE)
}

original <- readRDS(original_rds)
modular <- readRDS(modular_rds)

sort_frame <- function(x, keys) {
  keys <- intersect(keys, names(x))
  if (length(keys) == 0L || nrow(x) == 0L) {
    return(x)
  }
  x[do.call(order, unname(x[keys])), , drop = FALSE]
}

compare_frame <- function(block, original_frame, modular_frame, keys = character()) {
  common <- intersect(names(original_frame), names(modular_frame))
  original_frame <- sort_frame(original_frame[, common, drop = FALSE], keys)
  modular_frame <- sort_frame(modular_frame[, common, drop = FALSE], keys)
  numeric_columns <- common[
    vapply(original_frame[, common, drop = FALSE], is.numeric, logical(1)) &
      vapply(modular_frame[, common, drop = FALSE], is.numeric, logical(1))
  ]
  non_numeric <- setdiff(common, numeric_columns)

  same_shape <- nrow(original_frame) == nrow(modular_frame)
  same_labels <- same_shape && all(vapply(non_numeric, function(column) {
    identical(
      as.character(original_frame[[column]]),
      as.character(modular_frame[[column]])
    )
  }, logical(1)))

  max_difference <- if (same_shape && length(numeric_columns) > 0L) {
    max(unlist(lapply(numeric_columns, function(column) {
      abs(original_frame[[column]] - modular_frame[[column]])
    })), na.rm = TRUE)
  } else {
    NA_real_
  }

  data.frame(
    block = block,
    original_rows = nrow(original_frame),
    modular_rows = nrow(modular_frame),
    labels_identical = same_labels,
    max_abs_difference = max_difference,
    status = if (
      same_shape && same_labels && is.finite(max_difference) &&
        max_difference <= 1e-12
    ) {
      "identical"
    } else {
      "different"
    }
  )
}

common_scenarios <- c(
  "eiopa_spot_no_VA",
  "eiopa_spot_no_VA_down",
  "eiopa_spot_no_VA_up"
)

original_liability <- original$liability$table |>
  subset(scenario %in% common_scenarios)
modular_liability <- modular$liability$table |>
  subset(scenario %in% common_scenarios)
modular_liability$stress_type[
  modular_liability$stress_type == "spot"
] <- "base"

original_summary <- original$liability$summary |>
  subset(scenario %in% common_scenarios)
modular_summary <- modular$liability$summary |>
  subset(scenario %in% common_scenarios)

original_term_structures <- original$liability$term_structure_assumptions |>
  subset(scenario %in% common_scenarios)
modular_term_structures <- modular$liability$term_structure_assumptions |>
  subset(scenario %in% common_scenarios)
modular_term_structures$stress_type[
  modular_term_structures$stress_type == "spot"
] <- "base"

checks <- list(
  compare_frame(
    "validation model accuracy",
    original$validation$model_accuracy,
    modular$validation$model_accuracy,
    "model"
  ),
  compare_frame(
    "ensemble weights",
    original$weights$table,
    modular$weights$table,
    "model"
  ),
  compare_frame(
    "test ensemble accuracy",
    original$test$ensemble_accuracy,
    modular$test$ensemble_accuracy,
    "method"
  ),
  compare_frame(
    "test accuracy by age",
    original$test$ensemble_accuracy_by_age,
    modular$test$ensemble_accuracy_by_age,
    c("method", "age")
  ),
  compare_frame(
    "test accuracy by year",
    original$test$ensemble_accuracy_by_year,
    modular$test$ensemble_accuracy_by_year,
    c("method", "year")
  ),
  compare_frame(
    "EIOPA term structures",
    original_term_structures,
    modular_term_structures,
    c("scenario", "maturity")
  ),
  compare_frame(
    "liability values",
    original_liability,
    modular_liability,
    c("model", "scenario", "year")
  ),
  compare_frame(
    "liability summary",
    original_summary,
    modular_summary,
    c("model", "scenario")
  )
)

report <- do.call(rbind, checks)
dir.create(dirname(report_file), recursive = TRUE, showWarnings = FALSE)
write.csv(report, report_file, row.names = FALSE)
print(report)

if (!identical(original$selected_players, modular$selected_players)) {
  warning("Selected model sets differ.", call. = FALSE)
}
