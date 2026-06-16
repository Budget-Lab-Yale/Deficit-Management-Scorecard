test_that("manifest inputs are present after bootstrap", {
  repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  manifest <- readr::read_csv(file.path(repo_root, "config", "input_manifest.csv"), show_col_types = FALSE)
  input_dir <- Sys.getenv("DEFICIT_SCORECARD_INPUT_DIR", "data-raw")
  paths <- file.path(repo_root, input_dir, manifest$dest_path)
  missing_paths <- paths[!file.exists(paths)]
  expect_equal(length(missing_paths), 0)
})
