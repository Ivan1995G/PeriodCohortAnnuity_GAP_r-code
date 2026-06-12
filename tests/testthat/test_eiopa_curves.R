root <- Sys.getenv("REPOSITORY_ROOT")
source(file.path(root, "R", "functions_financial.R"))

testthat::test_that("EIOPA workbook reproduces published selected rates", {
  workbook <- file.path(
    root,
    "data", "eiopa", "EIOPA_RFR_20191231_Term_Structures.xlsx"
  )
  curves <- load_paper_discount_curves(workbook, k_max = 33L)

  selected <- curves |>
    dplyr::filter(
      scenario != "flat_3pct",
      maturity %in% c(1L, 5L, 10L, 20L, 30L)
    ) |>
    dplyr::select(scenario, maturity, zero_rate)

  expected <- data.frame(
    scenario = rep(
      c("eiopa_spot_no_VA", "eiopa_spot_no_VA_up", "eiopa_spot_no_VA_down"),
      each = 5
    ),
    maturity = rep(c(1L, 5L, 10L, 20L, 30L), 3),
    zero_rate = c(
      -0.00421, -0.00229, 0.00113, 0.00500, 0.01201,
       0.00579,  0.00771, 0.01113, 0.01500, 0.02201,
      -0.00421, -0.00229, 0.00078, 0.00355, 0.00868
    )
  )

  testthat::expect_equal(as.data.frame(selected), expected, tolerance = 1e-12)
})
