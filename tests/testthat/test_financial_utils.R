root <- Sys.getenv("REPOSITORY_ROOT")
source(file.path(root, "R", "functions_financial.R"))

testthat::test_that("period annuity valuation follows the original formula", {
  ages <- as.character(67:100)
  years <- as.character(2020:2052)
  surface <- matrix(0.02, nrow = length(ages), ncol = length(years))
  rownames(surface) <- ages
  colnames(surface) <- years
  curve <- data.frame(
    maturity = 1:33,
    zero_rate = rep(0.03, 33),
    discount_factor = (1.03)^(-(1:33))
  )

  observed <- annuity_metrics_zero_curve(
    surface, 2020, 67, 100, curve, mortality_type = "period"
  )$value
  expected <- sum((1.03)^(-(1:33)) * exp(-0.02 * (1:33)))
  testthat::expect_equal(observed, expected)
})

