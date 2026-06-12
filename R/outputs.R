## Numerical outputs and proof-value checks.
## This final block is executed after mortality and financial results exist.

population_label <- dplyr::recode(
  sex_series,
  male = "Male",
  female = "Female",
  total = "Total"
)

write.csv(
  table_validation_model_accuracy,
  file.path(output_dir, "table_1_validation_accuracy.csv"),
  row.names = FALSE
)
write.csv(
  weights_mortality_table,
  file.path(output_dir, "table_2_ensemble_weights.csv"),
  row.names = FALSE
)
write.csv(
  table_test_ensemble_accuracy,
  file.path(output_dir, "table_3_test_accuracy.csv"),
  row.names = FALSE
)
write.csv(
  table_test_ensemble_accuracy_by_age,
  file.path(output_dir, "test_accuracy_by_age.csv"),
  row.names = FALSE
)
write.csv(
  table_test_ensemble_accuracy_by_year,
  file.path(output_dir, "test_accuracy_by_year.csv"),
  row.names = FALSE
)
write.csv(
  term_structure_assumptions,
  file.path(output_dir, "table_4_eiopa_curves.csv"),
  row.names = FALSE
)
write.csv(
  table_shapley_level1_for_paper,
  file.path(output_dir, "table_5_shapley_liabilities.csv"),
  row.names = FALSE
)
write.csv(
  table_model_comparison_level1_for_paper,
  file.path(output_dir, "table_6_model_gap.csv"),
  row.names = FALSE
)
write.csv(
  table_shapley_stress_for_paper,
  file.path(output_dir, "table_7_shapley_stress.csv"),
  row.names = FALSE
)
write.csv(
  table_flat_vs_eiopa_for_paper,
  file.path(output_dir, "table_8_flat_vs_eiopa.csv"),
  row.names = FALSE
)
write.csv(
  liability_table,
  file.path(output_dir, "liability_period_vs_cohort_all_models.csv"),
  row.names = FALSE
)
write.csv(
  liability_summary_table,
  file.path(output_dir, "liability_summary.csv"),
  row.names = FALSE
)
write.csv(
  shapley_gap_values,
  file.path(output_dir, "shapley_liability_gap_values.csv"),
  row.names = FALSE
)

results_mortality_ensemble_test <- list(
  parameters = list(
    country_code = country_code,
    sex_series = sex_series,
    years_start = years_start,
    years_valid = years_valid,
    years_test = years_test,
    ages_fit = ages_fit,
    x_ret = x_ret,
    n_best_models = n_best_models,
    years_liability = years_liability,
    benefit_annual = benefit_annual,
    flat_rate = flat_rate,
    forecast_end_liability = forecast_end_liability,
    eiopa_reference_date = eiopa_reference_date,
    eiopa_effective_reference_date = eiopa_effective_reference_date,
    eiopa_country_name = eiopa_country_name,
    eiopa_workbook = eiopa_workbook
  ),
  models = models,
  valid_players = valid_players,
  selected_players = selected_players,
  validation = list(
    mortality_panel = validation_mortality_panel,
    model_accuracy = table_validation_model_accuracy,
    wide_complete = validation_mortality_wide_complete
  ),
  weights = list(
    shapley = w_sh_mort,
    performance = w_perf_mort,
    equal = w_ew_mort,
    table = weights_mortality_table,
    shapley_raw = shapley_mort_out,
    performance_raw = perf_mort_out
  ),
  test = list(
    mortality_panel = test_mortality_panel,
    ensemble_panel = test_ensemble_panel,
    ensemble_accuracy = table_test_ensemble_accuracy,
    ensemble_accuracy_by_age = table_test_ensemble_accuracy_by_age,
    ensemble_accuracy_by_year = table_test_ensemble_accuracy_by_year
  ),
  liability = list(
    outputs = liability_outputs,
    surfaces = liability_mx_list,
    shapley_weights = w_sh_liability,
    discount_curve_panel = discount_curve_panel,
    term_structure_assumptions = term_structure_assumptions,
    table = liability_table,
    summary = liability_summary_table,
    shapley_gap_values = shapley_gap_values,
    tables_for_paper = list(
      table_4_discount_curve = term_structure_assumptions,
      table_5_shapley_level1 = table_shapley_level1_for_paper,
      table_6_model_comparison_level1 = table_model_comparison_level1_for_paper,
      table_7_shapley_stress = table_shapley_stress_for_paper,
      table_8_flat_vs_eiopa = table_flat_vs_eiopa_for_paper
    )
  ),
  plots = list(
    validation_age = p_validation_mortality_age,
    weights = p_weights_mortality,
    test_age = p_test_ensemble_age,
    test_rmse_by_age = p_test_rmse_by_age,
    test_rmse_by_year = p_test_rmse_by_year,
    eiopa_curves = p_eiopa_curves,
    shapley_level1_liabilities = p_shapley_level1_liabilities,
    shapley_relative_gap_stress = p_shapley_relative_gap_stress,
    model_relative_gap_level1 = p_model_relative_gap_level1
  )
)

saveRDS(
  results_mortality_ensemble_test,
  file.path(output_dir, "results_mortality_ensemble_eiopa.rds")
)

audit_tables <- list(
  table_1_validation_accuracy = table_validation_model_accuracy |>
    dplyr::select(model, MSE, RMSE, MAE, Bias) |>
    dplyr::mutate(population = population_label, .before = 1),
  table_2_ensemble_weights = weights_mortality_table |>
    dplyr::transmute(
      population = population_label,
      model,
      Shapley = w_shapley_mortality,
      Performance = w_perf_mortality,
      Equal = w_equal
    ),
  table_3_test_accuracy = table_test_ensemble_accuracy |>
    dplyr::select(method, MSE, RMSE, MAE, Bias) |>
    dplyr::mutate(population = population_label, .before = 1),
  table_4_eiopa_curves = term_structure_assumptions |>
    dplyr::transmute(
      scenario = dplyr::recode(
        scenario,
        eiopa_spot_no_VA = "Baseline without VA",
        eiopa_spot_no_VA_up = "Interest-rate up",
        eiopa_spot_no_VA_down = "Interest-rate down"
      ),
      maturity,
      zero_rate
    ),
  table_5_shapley_liabilities = table_shapley_level1_for_paper |>
    dplyr::transmute(
      population = population_label,
      year,
      V_period,
      V_cohort,
      gap = gap_abs,
      rel_gap_percent,
      correction_factor
    ),
  table_6_model_gap = table_model_comparison_level1_for_paper |>
    dplyr::transmute(
      population = population_label,
      model,
      mean_gap_abs,
      mean_rel_gap_percent,
      min_rel_gap_percent,
      max_rel_gap_percent
    ),
  table_7_shapley_stress = table_shapley_stress_for_paper |>
    dplyr::transmute(
      population = population_label,
      scenario = as.character(scenario),
      year,
      gap_abs,
      rel_gap_percent,
      correction_factor
    ),
  table_8_flat_vs_eiopa = table_flat_vs_eiopa_for_paper |>
    dplyr::mutate(population = population_label, .before = 1)
)

check_spec <- data.frame(
  table = names(audit_tables),
  tolerance = c(5e-5, 5e-4, 5e-6, 5e-5, 5e-2, 5e-2, 5e-2, 5.1e-3)
)

compare_with_proof <- function(name, tolerance) {
  generated <- audit_tables[[name]]
  reference <- read.csv(
    file.path(reference_dir, paste0(name, ".csv")),
    check.names = FALSE
  )
  if ("population" %in% names(reference)) {
    reference <- reference[reference$population == population_label, , drop = FALSE]
  }

  common_numeric <- intersect(
    names(reference)[vapply(reference, is.numeric, logical(1))],
    names(generated)[vapply(generated, is.numeric, logical(1))]
  )
  key_cols <- setdiff(intersect(names(reference), names(generated)), common_numeric)
  if (length(key_cols) > 0L) {
    generated <- generated[do.call(order, unname(generated[key_cols])), , drop = FALSE]
    reference <- reference[do.call(order, unname(reference[key_cols])), , drop = FALSE]
  }

  if (nrow(generated) != nrow(reference) || length(common_numeric) == 0L) {
    return(data.frame(
      table = name,
      status = "shape mismatch",
      max_abs_difference = NA_real_,
      tolerance = tolerance
    ))
  }

  differences <- unlist(lapply(common_numeric, function(column) {
    abs(generated[[column]] - reference[[column]])
  }))
  max_difference <- max(differences, na.rm = TRUE)
  data.frame(
    table = name,
    status = if (is.finite(max_difference) && max_difference <= tolerance) {
      "pass"
    } else {
      "review"
    },
    max_abs_difference = max_difference,
    tolerance = tolerance
  )
}

reproduction_check <- purrr::map2_dfr(
  check_spec$table,
  check_spec$tolerance,
  compare_with_proof
)
write.csv(
  reproduction_check,
  file.path(output_dir, "reproduction_check.csv"),
  row.names = FALSE
)
print(reproduction_check)

if (any(reproduction_check$status != "pass")) {
  warning(
    "One or more proof-value checks require review; all outputs were still written.",
    call. = FALSE
  )
}
