## Graph construction for the nine figures reported in the paper.
## Each figure is written to a separate PDF under graph/<population>/.

validation_age_panel <- validation_mortality_panel |>
  dplyr::filter(age == x_ret, model %in% valid_players)

observed_age_validation <- validation_observed_panel |>
  dplyr::filter(age == x_ret)

p_validation_mortality_age <- ggplot() +
  geom_line(
    data = validation_age_panel,
    aes(x = year, y = m_hat, colour = model),
    linewidth = 0.8
  ) +
  geom_point(
    data = validation_age_panel,
    aes(x = year, y = m_hat, colour = model),
    size = 1.5
  ) +
  geom_line(
    data = observed_age_validation,
    aes(x = year, y = m_obs),
    colour = "black",
    linewidth = 1.2
  ) +
  geom_point(
    data = observed_age_validation,
    aes(x = year, y = m_obs),
    colour = "black",
    size = 2
  ) +
  scale_y_log10() +
  scale_x_continuous(
    breaks = seq(min(years_valid), max(years_valid), by = 2)
  ) +
  labs(
    x = "Validation year",
    y = expression(m[x,t]),
    colour = "Model"
  ) +
  theme_minimal()+theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 26),
    axis.text.x = element_text(
      size = 22,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 24),
    legend.title = element_blank(),
    legend.text = element_text(size = 26),
    legend.position = "top"
    #axis.title = element_text(size = 16),
    #axis.text = element_text(size = 14),
    #legend.title = element_text(size = 19),
    #legend.text = element_text(size = 17)
  )

p_weights_mortality <- weights_mortality_table |>
  dplyr::select(model, w_shapley_mortality, w_perf_mortality, w_equal) |>
  tidyr::pivot_longer(
    cols = c(w_shapley_mortality, w_perf_mortality, w_equal),
    names_to = "method",
    values_to = "weight"
  ) |>
  dplyr::mutate(
    method = dplyr::recode(
      method,
      w_shapley_mortality = "Shapley",
      w_perf_mortality = "Performance",
      w_equal = "Equal"
    )
  ) |>
  ggplot(aes(x = model, y = weight, fill = method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "Weight",
    fill = NULL
  ) +
  theme_minimal(base_size = 20) +
  theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 26),
      axis.text.x = element_text(
        size = 22,
        angle = 45,
        hjust = 1
      ),
      axis.text.y = element_text(size = 24),
      legend.title = element_blank(),
      legend.text = element_text(size = 26),
      legend.position = "top"
  )


test_age_panel <- test_ensemble_panel |>
  dplyr::filter(age == x_ret)

observed_age_test <- test_observed_panel |>
  dplyr::filter(age == x_ret)

p_test_ensemble_age <- ggplot() +
  geom_line(
    data = test_age_panel,
    aes(x = year, y = m_hat, colour = method),
    linewidth = 1.4
  ) +
  geom_point(
    data = test_age_panel,
    aes(x = year, y = m_hat, colour = method),
    size = 3
  ) +
  geom_line(
    data = observed_age_test,
    aes(x = year, y = m_obs),
    colour = "black",
    linewidth = 1.8
  ) +
  geom_point(
    data = observed_age_test,
    aes(x = year, y = m_obs),
    colour = "black",
    size = 3.4
  ) +
  scale_y_log10() +
  scale_x_continuous(
    breaks = seq(min(test_age_panel$year), max(test_age_panel$year), by = 2)
  ) +
  labs(
    x = NULL,
    y = expression(m[x,t]),
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 26),
    axis.text.x = element_text(
      size = 22,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 24),
    legend.title = element_blank(),
    legend.text = element_text(size = 26),
    legend.position = "top"
  )

p_test_rmse_by_age <- table_test_ensemble_accuracy_by_age |>
  ggplot(aes(x = age, y = RMSE, colour = method)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 3) +
  labs(
    x = NULL,
    y = expression(RMSE(m[x,t])),
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 26),
    axis.text.x = element_text(
      size = 22,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 24),
    legend.title = element_blank(),
    legend.text = element_text(size = 26),
    legend.position = "top"
  )

p_test_rmse_by_year <- table_test_ensemble_accuracy_by_year |>
  ggplot(aes(x = year, y = RMSE, colour = method)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = seq(
      min(table_test_ensemble_accuracy_by_year$year),
      max(table_test_ensemble_accuracy_by_year$year),
      by = 2
    )
  ) +
  labs(
    x = NULL,
    y = expression(RMSE(m[x,t])),
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 26),
    axis.text.x = element_text(
      size = 22,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 24),
    legend.title = element_blank(),
    legend.text = element_text(size = 26),
    legend.position = "top"
  )

paper_theme <- theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 26),
    axis.text.x = element_text(
      size = 22,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 23),
    legend.title = element_blank(),
    legend.text = element_text(size = 17),
    legend.position = "top"
  )

percent_lab <- function(x) paste0(round(100 * x, 1), "%")

p_eiopa_curves <- discount_curve_panel |>
  dplyr::filter(
    .data$scenario != "flat_3pct",
    .data$maturity <= max(maturities_report)
  ) |>
  ggplot(aes(x = maturity, y = zero_rate, colour = scenario_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(
    data = discount_curve_panel |>
      dplyr::filter(
        .data$scenario != "flat_3pct",
        .data$maturity %in% maturities_report
      ),
    size = 2.4
  ) +
  scale_y_continuous(labels = percent_lab) +
  scale_x_continuous(breaks = maturities_report) +
  labs(x = "Maturity", y = "Zero-coupon spot rate") +
  paper_theme

p_shapley_level1_liabilities <- shapley_gap_values |>
  dplyr::filter(.data$scenario == "eiopa_spot_no_VA") |>
  dplyr::select(year, liability_period, liability_cohort) |>
  tidyr::pivot_longer(
    cols = c(liability_period, liability_cohort),
    names_to = "valuation",
    values_to = "liability"
  ) |>
  dplyr::mutate(
    valuation = dplyr::recode(
      .data$valuation,
      liability_period = "Period",
      liability_cohort = "Cohort"
    )
  ) |>
  ggplot(aes(x = year, y = liability, colour = valuation)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = seq(min(years_liability), max(years_liability), by = 5)) +
  labs(x = NULL, y = "Liability") +
  paper_theme

p_shapley_relative_gap_stress <- shapley_gap_values |>
  dplyr::filter(.data$scenario != "flat_3pct") |>
  ggplot(aes(x = year, y = rel_gap, colour = scenario_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.0) +
  scale_y_continuous(labels = percent_lab) +
  scale_x_continuous(breaks = seq(min(years_liability), max(years_liability), by = 5)) +
  labs(x = NULL, y = "Relative gap") +
  paper_theme

p_model_relative_gap_level1 <- liability_table |>
  dplyr::filter(.data$scenario == "eiopa_spot_no_VA") |>
  ggplot(aes(x = year, y = rel_gap, colour = model)) +
  geom_line(linewidth = 1.0) +
  geom_line(
    data = liability_table |>
      dplyr::filter(
        .data$scenario == "eiopa_spot_no_VA",
        .data$model == "Shapley"
      ),
    linewidth = 1.7
  ) +
  scale_y_continuous(labels = percent_lab) +
  scale_x_continuous(breaks = seq(min(years_liability), max(years_liability), by = 5)) +
  labs(x = NULL, y = "Relative gap") +
  paper_theme

dir.create(graph_dir, recursive = TRUE, showWarnings = FALSE)

ggplot2::ggsave(file.path(graph_dir, "figure_1_validation_age67.pdf"),
  p_validation_mortality_age, width = 5.8, height = 5.5, units = "in")
ggplot2::ggsave(file.path(graph_dir, "figure_2_ensemble_weights.pdf"),
  p_weights_mortality, width = 8.5, height = 8.8, units = "in")
ggplot2::ggsave(file.path(graph_dir, "figure_3_test_age67.pdf"),
  p_test_ensemble_age, width = 8.5, height = 8.8, units = "in")
ggplot2::ggsave(file.path(graph_dir, "figure_4_test_rmse_by_age.pdf"),
  p_test_rmse_by_age, width = 8.5, height = 8.8, units = "in")
ggplot2::ggsave(file.path(graph_dir, "figure_5_test_rmse_by_year.pdf"),
  p_test_rmse_by_year, width = 8.5, height = 8.8, units = "in")
ggplot2::ggsave(file.path(graph_dir, "figure_6_eiopa_curves.pdf"),
  p_eiopa_curves, width = 8.5, height = 6.5, units = "in")
ggplot2::ggsave(file.path(graph_dir, "figure_7_shapley_liabilities.pdf"),
  p_shapley_level1_liabilities, width = 8.5, height = 6.5, units = "in")
ggplot2::ggsave(file.path(graph_dir, "figure_8_model_gap.pdf"),
  p_model_relative_gap_level1, width = 8.5, height = 6.5, units = "in")
ggplot2::ggsave(file.path(graph_dir, "figure_9_shapley_stress_gap.pdf"),
  p_shapley_relative_gap_stress, width = 8.5, height = 6.5, units = "in")
