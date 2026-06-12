## Financial block corresponding to section 12 of the original script.
## It refits selected mortality models, values period/cohort annuities,
## and constructs the financial tables reported in the paper.

max_age_liability <- max(ages_fit)
forecast_end_liability <- max(years_liability) + (max_age_liability - x_ret)
final_fit_years <- years_start:max(years_test)
max_k_liability <- max_age_liability - x_ret

## --------------------------------------------------------------
## 12.1) EIOPA zero-coupon curves
## --------------------------------------------------------------

eiopa_effective_reference_date <- eiopa_reference_date
eiopa_country_name <- "Euro"

maturities_report <- sort(unique(c(1, 5, 10, 20, 30)))
maturities_report <- maturities_report[
  maturities_report >= 1 & maturities_report <= max_k_liability
]

discount_curve_panel <- load_paper_discount_curves(
  workbook = eiopa_workbook,
  k_max = max_k_liability,
  flat_rate = flat_rate
) |>
  dplyr::mutate(
    discount_level = dplyr::case_when(
      .data$scenario == "eiopa_spot_no_VA" ~ "Base",
      .data$scenario == "flat_3pct" ~ "Benchmark",
      TRUE ~ "Stress"
    )
  ) |>
  dplyr::select(
    discount_level, scenario, scenario_label, stress_type,
    maturity, zero_rate, discount_factor
  ) |>
  dplyr::arrange(.data$discount_level, .data$scenario, .data$maturity)

discount_scenarios <- discount_curve_panel |>
  dplyr::distinct(
    .data$discount_level,
    .data$scenario,
    .data$scenario_label,
    .data$stress_type
  ) |>
  dplyr::arrange(.data$discount_level, .data$scenario)

discount_scenario_labels <- stats::setNames(
  discount_scenarios$scenario_label,
  discount_scenarios$scenario
)

term_structure_assumptions <- discount_curve_panel |>
  dplyr::filter(
    .data$scenario != "flat_3pct",
    .data$maturity %in% maturities_report
  ) |>
  dplyr::mutate(
    zero_rate_percent = 100 * .data$zero_rate
  ) |>
  dplyr::arrange(.data$discount_level, .data$scenario, .data$maturity)

cat("\n==============================================================\n")
cat("TABLE 4: EIOPA ZERO-COUPON DISCOUNT CURVES\n")
cat("==============================================================\n")
cat("EIOPA reference date:", as.character(eiopa_effective_reference_date), "\n")
cat("Country/currency area:", eiopa_country_name, "\n")
print(term_structure_assumptions, n = Inf)

liability_outputs <- list()

for (mn in selected_players) {
  cat("[LIABILITY FORECAST] model =", mn, "\n")

  liability_outputs[[mn]] <- tryCatch(
    fit_and_forecast_model(
      spec              = models[[mn]],
      DataStMoMo        = DataStMoMo,
      MorData           = MorData,
      sex_series        = sex_series,
      ages_fit          = ages_fit,
      fit_years         = final_fit_years,
      forecast_end_year = forecast_end_liability,
      model_name        = mn
    ),
    error = function(e) {
      warning(paste("Liability forecast failed:", mn, "|", e$message))
      NULL
    }
  )
}

liability_mx_list <- lapply(selected_players, function(mn) {
  obj <- liability_outputs[[mn]]

  if (is.null(obj) || is.null(obj$mx_fc)) return(NULL)

  mx_tmp <- obj$mx_fc[
    as.character(ages_fit),
    as.character(min(years_liability):forecast_end_liability),
    drop = FALSE
  ]

  ## Reject models whose forecast mortality surface contains non-positive
  ## or non-finite entries. Use na.rm = TRUE to avoid propagating NAs
  ## through the any() call: if na.rm = FALSE and the vector contains
  ## only NA values, any() would return NA rather than FALSE.
  if (any(!is.finite(mx_tmp) | mx_tmp <= 0, na.rm = TRUE)) {
    warning(paste("Model removed from liability analysis due to invalid mx:", mn))
    return(NULL)
  }

  mx_tmp
})

names(liability_mx_list) <- selected_players
liability_mx_list <- liability_mx_list[!sapply(liability_mx_list, is.null)]

if (length(intersect(selected_players, names(liability_mx_list))) < 2) {
  stop("Insufficient selected models available for the Shapley liability ensemble.")
}

w_sh_liability <- w_sh_mort[names(liability_mx_list)]
w_sh_liability <- w_sh_liability / sum(w_sh_liability)

liability_mx_list[["Shapley"]] <- combine_mx_surfaces(
  mx_list = liability_mx_list,
  weights = w_sh_liability
)

## --------------------------------------------------------------
## 12.4) Liability tables
## --------------------------------------------------------------

liability_table <- purrr::imap_dfr(liability_mx_list, function(mx_surface, mn) {
  purrr::map_dfr(seq_len(nrow(discount_scenarios)), function(ii) {
    sc <- discount_scenarios[ii, ]

    discount_curve <- discount_curve_panel |>
      dplyr::filter(.data$scenario == sc$scenario) |>
      dplyr::arrange(.data$maturity)

    purrr::map_dfr(years_liability, function(tt) {
      metrics_period <- annuity_metrics_zero_curve(
        mx_surface = mx_surface,
        valuation_year = tt,
        x_ret = x_ret,
        max_age = max_age_liability,
        discount_curve = discount_curve,
        benefit = benefit_annual,
        mortality_type = "period"
      )

      metrics_cohort <- annuity_metrics_zero_curve(
        mx_surface = mx_surface,
        valuation_year = tt,
        x_ret = x_ret,
        max_age = max_age_liability,
        discount_curve = discount_curve,
        benefit = benefit_annual,
        mortality_type = "cohort"
      )

      tibble::tibble(
        model = mn,
        discount_level = sc$discount_level,
        scenario = sc$scenario,
        scenario_label = sc$scenario_label,
        stress_type = sc$stress_type,
        year = tt,
        liability_period = metrics_period$value,
        liability_cohort = metrics_cohort$value,
        gap_abs = metrics_cohort$value - metrics_period$value,
        rel_gap = (metrics_cohort$value - metrics_period$value) / metrics_period$value,
        correction_factor = metrics_period$value / metrics_cohort$value
      )
    })
  })
})

liability_summary_table <- liability_table |>
  dplyr::group_by(.data$model, .data$discount_level, .data$scenario, .data$scenario_label) |>
  dplyr::summarise(
    mean_gap_abs = mean(.data$gap_abs, na.rm = TRUE),
    mean_rel_gap = mean(.data$rel_gap, na.rm = TRUE),
    min_rel_gap = min(.data$rel_gap, na.rm = TRUE),
    max_rel_gap = max(.data$rel_gap, na.rm = TRUE),
    mean_correction_factor = mean(.data$correction_factor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$discount_level, .data$scenario, dplyr::desc(.data$mean_rel_gap))

shapley_gap_values <- liability_table |>
  dplyr::filter(.data$model == "Shapley") |>
  dplyr::arrange(.data$discount_level, .data$scenario, .data$year)

selected_liability_years_for_paper <- sort(unique(c(
  min(years_liability),
  2025,
  2030,
  2040,
  max(years_liability)
)))
selected_liability_years_for_paper <- selected_liability_years_for_paper[
  selected_liability_years_for_paper %in% years_liability
]

## Paper Table 5: Shapley results under the baseline EIOPA curve.
table_shapley_level1_for_paper <- shapley_gap_values |>
  dplyr::filter(
    .data$scenario == "eiopa_spot_no_VA",
    .data$year %in% selected_liability_years_for_paper
  ) |>
  dplyr::transmute(
    year,
    V_period = liability_period,
    V_cohort = liability_cohort,
    gap_abs,
    rel_gap_percent = 100 * rel_gap,
    correction_factor
  )

## Paper Table 6: model comparison under the baseline EIOPA curve.
table_model_comparison_level1_for_paper <- liability_summary_table |>
  dplyr::filter(.data$scenario == "eiopa_spot_no_VA") |>
  dplyr::transmute(
    model,
    mean_gap_abs,
    mean_rel_gap_percent = 100 * mean_rel_gap,
    min_rel_gap_percent = 100 * min_rel_gap,
    max_rel_gap_percent = 100 * max_rel_gap,
    mean_correction_factor
  ) |>
  dplyr::arrange(dplyr::desc(.data$mean_rel_gap_percent))

## Paper Table 7: Shapley sensitivity to EIOPA interest-rate stresses.
table_shapley_stress_for_paper <- shapley_gap_values |>
  dplyr::filter(
    .data$scenario != "flat_3pct",
    .data$year %in% selected_liability_years_for_paper
  ) |>
  dplyr::transmute(
    scenario = dplyr::recode(stress_type, base = "spot"),
    year,
    gap_abs,
    rel_gap_percent = 100 * rel_gap,
    correction_factor
  ) |>
  dplyr::mutate(
    scenario = factor(.data$scenario, levels = c("spot", "down", "up"))
  ) |>
  dplyr::arrange(.data$scenario, .data$year)

## Paper Table 8: flat 3% versus baseline EIOPA mean relative gap.
table_flat_vs_eiopa_for_paper <- shapley_gap_values |>
  dplyr::filter(.data$scenario %in% c("flat_3pct", "eiopa_spot_no_VA")) |>
  dplyr::group_by(.data$scenario) |>
  dplyr::summarise(
    mean_rel_gap_percent = 100 * mean(.data$rel_gap, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = scenario,
    values_from = mean_rel_gap_percent
  ) |>
  dplyr::rename(
    flat_3_percent = flat_3pct,
    baseline_eiopa = eiopa_spot_no_VA
  )

cat("\n==============================================================\n")
cat("TABLE 5: SHAPLEY LIABILITY UNDER BASELINE EIOPA CURVE\n")
cat("==============================================================\n")
print(table_shapley_level1_for_paper, n = Inf)

cat("\n==============================================================\n")
cat("TABLE 6: MODEL COMPARISON UNDER BASELINE EIOPA CURVE\n")
cat("==============================================================\n")
print(table_model_comparison_level1_for_paper, n = Inf)

cat("\n==============================================================\n")
cat("TABLE 7: SHAPLEY SENSITIVITY TO EIOPA RATE STRESSES\n")
cat("==============================================================\n")
print(table_shapley_stress_for_paper, n = Inf)

cat("\n==============================================================\n")
cat("TABLE 8: FLAT 3% VERSUS BASELINE EIOPA\n")
cat("==============================================================\n")
print(table_flat_vs_eiopa_for_paper, n = Inf)
