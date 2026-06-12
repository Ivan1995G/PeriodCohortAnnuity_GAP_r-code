root <- Sys.getenv("REPOSITORY_ROOT")

testthat::test_that("main.R exposes the required ordered stages", {
  main <- readLines(file.path(root, "main.R"), warn = FALSE)
  expected <- c(
    'run_module("config.R")',
    'run_module("functions_auxiliary.R")',
    'run_module("data.R")',
    'run_module("mortality_models.R")',
    'run_module("pipeline_single_population.R")',
    'run_module("financial_pipeline.R")',
    'run_module("graphs.R")',
    'run_module("outputs.R")'
  )
  positions <- vapply(expected, function(x) {
    hit <- grep(x, main, fixed = TRUE)
    if (length(hit) == 0L) NA_integer_ else hit[[1]]
  }, integer(1))
  testthat::expect_false(anyNA(positions))
  testthat::expect_true(all(diff(positions) > 0))
})

testthat::test_that("graph block writes nine separate PDF files", {
  graph_code <- readLines(file.path(root, "R", "graphs.R"), warn = FALSE)
  filenames <- regmatches(
    graph_code,
    regexpr("figure_[0-9][^\"]*[.]pdf", graph_code)
  )
  filenames <- filenames[nzchar(filenames)]
  testthat::expect_length(unique(filenames), 9L)
})

testthat::test_that("reference values are inputs and obsolete files are absent", {
  testthat::expect_true(dir.exists(file.path(root, "data", "reference")))
  testthat::expect_false(dir.exists(file.path(root, "output", "reference")))
  testthat::expect_false(file.exists(file.path(root, "REPOSITORY_TEXTS.md")))
  testthat::expect_false(dir.exists(file.path(root, "scripts")))
  output_code <- readLines(file.path(root, "R", "outputs.R"), warn = FALSE)
  testthat::expect_false(any(grepl("audit_dir", output_code, fixed = TRUE)))
})

testthat::test_that("graph directories contain PDF figures only", {
  graph_files <- list.files(
    file.path(root, "graph"),
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  testthat::expect_true(all(tolower(tools::file_ext(graph_files)) == "pdf"))
})
