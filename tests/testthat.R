root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(root, "main.R"))) {
  stop("Run tests from the repository root.", call. = FALSE)
}
Sys.setenv(REPOSITORY_ROOT = root)
testthat::test_dir(file.path(root, "tests", "testthat"))

