# Golden regression test: pins the published numeric outputs so a parsing/join
# change between CBO updates can't shift the figures undetected. Values below are
# the February 2026 vintage outputs; update them deliberately (with a noted
# reason) whenever a new CBO release legitimately changes the numbers.

golden_repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

read_output_csv <- function(name) {
  path <- file.path(golden_repo_root, "output", name)
  testthat::skip_if_not(file.exists(path), "Run Rscript scripts/run_pipeline.R first")
  readr::read_csv(path, show_col_types = FALSE)
}

test_that("scorecard highlight values match the February 2026 golden reference", {
  scorecard <- read_output_csv("scorecard_unified.csv")
  pick <- function(label, col) scorecard[[col]][scorecard$period_label == label]

  expect_equal(pick("2026a", "failure_value"), 0.307925, tolerance = 1e-4)
  expect_equal(pick("2026a", "actual_deficit_reduction"), -0.030125, tolerance = 1e-4)
  expect_equal(pick("2026a", "predicted_deficit_reduction"), 0.277800, tolerance = 1e-4)
  expect_equal(pick("2026a", "percentile_all"), 0.777778, tolerance = 1e-4)
  expect_equal(pick("2026a", "percentile_post_2004"), 0.631579, tolerance = 1e-4)

  expect_equal(pick("2025b", "failure_value"), 1.778613, tolerance = 1e-4)
  expect_equal(pick("2025b", "percentile_all"), 1.0, tolerance = 1e-9)
  expect_equal(pick("2025b*", "failure_value"), 0.672320, tolerance = 1e-4)
  expect_equal(pick("2025a", "failure_value"), 0.359422, tolerance = 1e-4)
})

test_that("forward table values match the February 2026 golden reference (c = 0.19)", {
  rows <- read_output_csv("forward_deficit_reduction_table.csv")
  cell <- function(panel_prefix, line_item, col) {
    r <- rows[startsWith(rows$panel, panel_prefix) & rows$line_item == line_item, ]
    r[[col]]
  }

  # Panel A: CBO baseline primary deficit and debt/GDP
  expect_equal(cell("Panel A", "Primary deficit", "2027-2036"), 2.084, tolerance = 1e-3)
  expect_equal(cell("Panel A", "Debt/GDP", "2036"), 120.223, tolerance = 1e-2)

  # Panel B: required primary-deficit reduction (the headline c* = 0.19 path)
  expect_equal(cell("Panel B", "Primary deficit", "2027"), 0.27816, tolerance = 1e-3)
  expect_equal(cell("Panel B", "Primary deficit", "2036"), 1.69956, tolerance = 1e-3)
  expect_equal(cell("Panel B", "Primary deficit", "2027-2036"), 1.16622, tolerance = 1e-3)
})
