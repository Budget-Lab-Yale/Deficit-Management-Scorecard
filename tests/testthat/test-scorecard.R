test_that("pipeline outputs satisfy scorecard invariants", {
  dataset_path <- file.path(scorecard_data_dir, "dataset_for_regression_26.rds")
  alternative_path <- file.path(
    scorecard_data_dir,
    "dataset_for_regression_26__techcustomdutiesinleg.rds"
  )
  score_path <- file.path(scorecard_data_dir, "scorecard_unified.rds")
  csv_path <- file.path(scorecard_output_dir, "scorecard_unified.csv")

  expect_true(file.exists(dataset_path), info = "Run Rscript scripts/run_pipeline.R first")
  expect_true(file.exists(alternative_path), info = "Run Rscript scripts/run_pipeline.R first")
  expect_true(file.exists(score_path), info = "Run Rscript scripts/run_pipeline.R first")
  expect_true(file.exists(csv_path), info = "Run Rscript scripts/run_pipeline.R first")

  dataset <- readRDS(dataset_path)
  alternative <- readRDS(alternative_path)
  score <- readRDS(score_path)
  scorecard <- readr::read_csv(csv_path, show_col_types = FALSE)

  expect_true(202601L %in% dataset$periodid)
  expect_true("lag_deltabexp_t0_t4" %in% names(dataset))
  expect_true(all(c("2025a", "2025b", "2025b*", "2026a") %in% scorecard$period_label))

  coefficient <- unname(coef(score$model)[["lag_deltabexp_t0_t4"]])
  expect_equal(coefficient, 0.1762817, tolerance = 1e-5)
  expect_equal(stats::nobs(score$model), 39)

  first_era <- dataset |>
    dplyr::filter(.data$sample_1)
  expect_equal(nrow(first_era), 39)
  expect_true(first_era$sample_1[first_era$periodid == 200302L])
  expect_false(dataset$sample_1[dataset$periodid == 200401L])

  ordered <- dataset |>
    dplyr::arrange(.data$report_year, .data$report_half)
  expect_equal(
    ordered$lag_deltabexp_t0_t4[-1],
    ordered$deltabexp_t0_t4[-nrow(ordered)],
    tolerance = 1e-12
  )

  row_2023b <- dataset |>
    dplyr::filter(.data$periodid == 202302L)
  expect_equal(nrow(row_2023b), 1)
  expect_equal(row_2023b$lag_outgap_pgdp, -0.008761564929, tolerance = 1e-12)

  pgdp_1996 <- unique(dataset$pgdp_lead_0[dataset$report_year == 1996L])
  expect_equal(length(pgdp_1996), 1)
  expect_equal(pgdp_1996, 7945.18, tolerance = 0.01)

  expect_identical(score$data$group[score$data$periodid == 200302L], "first_era")
  expect_identical(score$data$group[score$data$periodid == 200401L], "later_era")

  alternative <- alternative |>
    dplyr::arrange(.data$report_year, .data$report_half)
  alt_2025b_index <- which(alternative$periodid == 202502L)
  expect_equal(length(alt_2025b_index), 1)
  expect_equal(
    alternative$lag_deltabexp_t0_t4[[alt_2025b_index]],
    alternative$deltabexp_t0_t4[[alt_2025b_index - 1L]],
    tolerance = 1e-12
  )
  expect_equal(
    score$data$lag_deltabexp_t0_t4[score$data$periodidchar == "2025b*"],
    100 * alternative$lag_deltabexp_t0_t4[[alt_2025b_index]],
    tolerance = 1e-12
  )

  highlights <- scorecard |>
    dplyr::filter(.data$period_label %in% c("2025a", "2025b", "2025b*", "2026a"))
  expect_equal(highlights$period_label[which.max(highlights$failure_value)], "2025b")

  expect_true(file.exists(file.path(
    scorecard_output_dir,
    "residuals_basefit_1984b2026a_nozlb_new_updated_debt.pdf"
  )))
  expect_true(file.exists(file.path(
    scorecard_output_dir,
    "fig3_distribution_residuals_new_updated_kunits_10_debt.pdf"
  )))
  expect_true(file.exists(file.path(scorecard_output_dir, "empirical_regression_summary.csv")))
})
