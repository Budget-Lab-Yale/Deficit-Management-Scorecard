test_that("pipeline outputs satisfy scorecard invariants", {
  repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  dataset_path <- file.path(repo_root, "data", "processed", "dataset_for_regression_26.rds")
  score_path <- file.path(repo_root, "data", "processed", "scorecard_unified.rds")
  csv_path <- file.path(repo_root, "output", "scorecard_unified.csv")

  expect_true(file.exists(dataset_path), info = "Run Rscript scripts/run_pipeline.R first")
  expect_true(file.exists(score_path), info = "Run Rscript scripts/run_pipeline.R first")
  expect_true(file.exists(csv_path), info = "Run Rscript scripts/run_pipeline.R first")

  dataset <- readRDS(dataset_path)
  score <- readRDS(score_path)
  scorecard <- readr::read_csv(csv_path, show_col_types = FALSE)

  expect_true(202601L %in% dataset$periodid)
  expect_true(all(c("2025a", "2025b", "2025b*", "2026a") %in% scorecard$period_label))

  coefficient <- unname(coef(score$model)[["deltabexp_t0_t4"]])
  expect_equal(coefficient, 0.093, tolerance = 0.02)

  highlights <- scorecard |>
    dplyr::filter(.data$period_label %in% c("2025a", "2025b", "2025b*", "2026a"))
  expect_equal(
    highlights$period_label[which.max(highlights$failure_value)],
    "2025b"
  )

  expect_true(file.exists(file.path(repo_root, "output", "residuals_basefit_1984b2026a_nozlb_new_updated_debt.pdf")))
  expect_true(file.exists(file.path(repo_root, "output", "fig3_distribution_residuals_new_updated_kunits_10_debt.pdf")))
})
