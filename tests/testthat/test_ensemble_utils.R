root <- Sys.getenv("REPOSITORY_ROOT")
source(file.path(root, "R", "functions_auxiliary.R"))

testthat::test_that("weighted mortality surfaces are convex combinations", {
  a <- matrix(c(0.01, 0.02, 0.03, 0.04), 2)
  b <- matrix(c(0.03, 0.04, 0.05, 0.06), 2)
  out <- combine_mx_surfaces(list(a = a, b = b), c(a = 0.25, b = 0.75))
  testthat::expect_equal(out, 0.25 * a + 0.75 * b)
})

testthat::test_that("annual zero rates produce correct discount factors", {
  rate <- 0.03
  maturity <- 1:5
  observed <- (1 + rate)^(-maturity)
  testthat::expect_equal(observed[[1]], 1 / 1.03)
  testthat::expect_true(all(diff(observed) < 0))
})

