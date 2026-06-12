## Validation, model selection, ensemble weighting, and independent test
## for the single population selected in main.R.

mx_obs_validation <- get_observed_mx_surface(
  DataStMoMo = DataStMoMo,
  ages_fit   = ages_fit,
  years_vec  = years_valid
)

validation_observed_panel <- surface_to_panel(mx_obs_validation) |>
  dplyr::rename(m_obs = m)

validation_roll <- compute_rolling_one_step(
  years_eval  = years_valid,
  models      = models,
  model_names = model_names,
  DataStMoMo  = DataStMoMo,
  MorData     = MorData,
  sex_series  = sex_series,
  ages_fit    = ages_fit,
  years_start = years_start,
  label       = "VALIDATION"
)

validation_prediction_panel <- validation_roll$prediction_panel

validation_mortality_panel <- validation_prediction_panel |>
  dplyr::left_join(validation_observed_panel, by = c("year", "age")) |>
  dplyr::filter(
    is.finite(m_hat), is.finite(m_obs),
    m_hat > 0, m_obs > 0
  ) |>
  dplyr::mutate(
    error_m = m_hat - m_obs
  )

validation_mortality_wide <- validation_mortality_panel |>
  dplyr::select(year, age, model, m_hat, m_obs) |>
  tidyr::pivot_wider(names_from = model, values_from = m_hat) |>
  dplyr::arrange(year, age)

valid_players <- model_names[
  model_names %in% names(validation_mortality_wide) &
    sapply(model_names, function(mn) {
      all(is.finite(validation_mortality_wide[[mn]]))
    })
]

validation_mortality_wide_complete <- validation_mortality_wide |>
  dplyr::select(year, age, m_obs, dplyr::all_of(valid_players)) |>
  dplyr::filter(is.finite(m_obs), m_obs > 0) |>
  dplyr::filter(
    dplyr::if_all(dplyr::all_of(valid_players), ~ is.finite(.x) & .x > 0)
  )

if (length(valid_players) < n_best_models) {
  stop("Fewer valid models than n_best_models are available.")
}

table_validation_model_accuracy <- validation_mortality_panel |>
  dplyr::filter(model %in% valid_players) |>
  make_accuracy_table(model_col = "model")

selected_players <- table_validation_model_accuracy |>
  dplyr::slice_head(n = n_best_models) |>
  dplyr::pull(model)

validation_mortality_wide_selected <- validation_mortality_wide_complete |>
  dplyr::select(year, age, m_obs, dplyr::all_of(selected_players))

cat("\n==============================================================\n")
cat("VALIDATION ACCURACY: INDIVIDUAL MODELS\n")
cat("==============================================================\n")
print(table_validation_model_accuracy, n = Inf)

cat("\n==============================================================\n")
cat("SELECTED BEST MODELS FOR ENSEMBLES\n")
cat("==============================================================\n")
print(selected_players)

shapley_mort_out <- compute_shapley_weights_mortality(
  validation_mortality_wide_selected,
  selected_players
)

perf_mort_out <- compute_performance_weights_mortality(
  validation_mortality_wide_selected,
  selected_players
)

w_sh_mort <- shapley_mort_out$weights
w_perf_mort <- perf_mort_out$weights
w_ew_mort <- setNames(rep(1 / length(selected_players), length(selected_players)), selected_players)

weights_mortality_table <- tibble::tibble(
  model = selected_players,
  MSE_mortality_valid = as.numeric(perf_mort_out$mse[selected_players]),
  phi_shapley_mortality = as.numeric(shapley_mort_out$phi[selected_players]),
  w_shapley_mortality = as.numeric(w_sh_mort[selected_players]),
  w_perf_mortality = as.numeric(w_perf_mort[selected_players]),
  w_equal = as.numeric(w_ew_mort[selected_players])
) |>
  dplyr::arrange(dplyr::desc(w_shapley_mortality))

cat("\n==============================================================\n")
cat("MORTALITY ENSEMBLE WEIGHTS FROM VALIDATION: SELECTED MODELS ONLY\n")
cat("==============================================================\n")
print(weights_mortality_table, n = Inf)

mx_obs_test <- get_observed_mx_surface(
  DataStMoMo = DataStMoMo,
  ages_fit   = ages_fit,
  years_vec  = years_test
)

test_observed_panel <- surface_to_panel(mx_obs_test) |>
  dplyr::rename(m_obs = m)

test_roll <- compute_rolling_one_step(
  years_eval  = years_test,
  models      = models,
  model_names = selected_players,
  DataStMoMo  = DataStMoMo,
  MorData     = MorData,
  sex_series  = sex_series,
  ages_fit    = ages_fit,
  years_start = years_start,
  label       = "TEST"
)

test_prediction_panel <- test_roll$prediction_panel

test_mortality_panel <- test_prediction_panel |>
  dplyr::left_join(test_observed_panel, by = c("year", "age")) |>
  dplyr::filter(
    model %in% selected_players,
    is.finite(m_hat), is.finite(m_obs),
    m_hat > 0, m_obs > 0
  ) |>
  dplyr::mutate(
    error_m = m_hat - m_obs
  )

test_mortality_wide <- test_mortality_panel |>
  dplyr::select(year, age, model, m_hat, m_obs) |>
  tidyr::pivot_wider(names_from = model, values_from = m_hat) |>
  dplyr::arrange(year, age) |>
  dplyr::select(year, age, m_obs, dplyr::all_of(selected_players)) |>
  dplyr::filter(is.finite(m_obs), m_obs > 0) |>
  dplyr::filter(
    dplyr::if_all(dplyr::all_of(selected_players), ~ is.finite(.x) & .x > 0)
  )

test_ensemble_panel <- dplyr::bind_rows(
  make_ensemble_panel(test_mortality_wide, w_sh_mort, "Shapley"),
  make_ensemble_panel(test_mortality_wide, w_perf_mort, "Performance"),
  make_ensemble_panel(test_mortality_wide, w_ew_mort, "Equal")
)

table_test_ensemble_accuracy <- test_ensemble_panel |>
  make_accuracy_table(model_col = "method")

cat("\n==============================================================\n")
cat("TEST ACCURACY: ENSEMBLES BUILT ON SELECTED MODELS\n")
cat("==============================================================\n")
print(table_test_ensemble_accuracy, n = Inf)

table_test_ensemble_accuracy_by_age <- test_ensemble_panel |>
  dplyr::group_by(method, age) |>
  dplyr::summarise(
    MSE = mean(error_m^2, na.rm = TRUE),
    RMSE = sqrt(mean(error_m^2, na.rm = TRUE)),
    MAE = mean(abs(error_m), na.rm = TRUE),
    Bias = mean(error_m, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(age, RMSE)

table_test_ensemble_accuracy_by_year <- test_ensemble_panel |>
  dplyr::group_by(method, year) |>
  dplyr::summarise(
    MSE = mean(error_m^2, na.rm = TRUE),
    RMSE = sqrt(mean(error_m^2, na.rm = TRUE)),
    MAE = mean(abs(error_m), na.rm = TRUE),
    Bias = mean(error_m, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(year, RMSE)

cat("\n==============================================================\n")
cat("TEST ACCURACY BY AGE: ENSEMBLES\n")
cat("==============================================================\n")
print(table_test_ensemble_accuracy_by_age, n = Inf)

cat("\n==============================================================\n")
cat("TEST ACCURACY BY YEAR: ENSEMBLES\n")
cat("==============================================================\n")
print(table_test_ensemble_accuracy_by_year, n = Inf)
