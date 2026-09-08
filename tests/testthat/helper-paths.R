scorecard_repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

scorecard_path <- function(env_var, default) {
  value <- Sys.getenv(env_var, default)
  is_absolute <- startsWith(value, "/") || grepl("^[A-Za-z]:[/\\\\]", value)
  normalizePath(
    if (is_absolute) value else file.path(scorecard_repo_root, value),
    mustWork = FALSE
  )
}

scorecard_input_dir <- scorecard_path("DEFICIT_SCORECARD_INPUT_DIR", "inputs")
scorecard_data_dir <- scorecard_path("DEFICIT_SCORECARD_DATA_DIR", file.path("data", "processed"))
scorecard_output_dir <- scorecard_path("DEFICIT_SCORECARD_OUTPUT_DIR", "output")
