# Golden tests pin the published numeric outputs so that a code change cannot
# shift the figures undetected. The values are the February 2026 vintage. A new
# CBO release changes them legitimately; docs/update_procedure.md describes how
# to refresh them.

read_output_csv <- function(name) {
  path <- file.path(scorecard_output_dir, name)
  readr::read_csv(path, show_col_types = FALSE)
}

test_that("scorecard highlight values match the February 2026 golden reference", {
  scorecard <- read_output_csv("scorecard.csv")
  pick <- function(label, col) scorecard[[col]][scorecard$period_label == label]

  # The scorecard uses the prior report's projected debt change and a
  # 1984b-2003b benchmark. Percentiles use historical reference observations
  # only: 39 in the first era and 21 in the later era.
  expect_equal(pick("2026a", "failure_value"), 0.501300, tolerance = 1e-4)
  expect_equal(pick("2026a", "actual_deficit_reduction"), -0.030125, tolerance = 1e-4)
  expect_equal(pick("2026a", "predicted_deficit_reduction"), 0.471175, tolerance = 1e-4)
  expect_equal(pick("2026a", "percentile_historical"), 0.900000, tolerance = 1e-9)
  expect_equal(pick("2026a", "percentile_later_era"), 0.761905, tolerance = 1e-4)

  expect_equal(pick("2025b", "failure_value"), 1.959554, tolerance = 1e-4)
  expect_equal(pick("2025b", "percentile_historical"), 1.0, tolerance = 1e-9)
  expect_equal(pick("2025b*", "failure_value"), 0.853261, tolerance = 1e-4)
  expect_equal(pick("2025b*", "percentile_historical"), 0.966667, tolerance = 1e-4)
  expect_equal(pick("2025a", "failure_value"), 0.622166, tolerance = 1e-4)
  expect_equal(pick("2025a", "percentile_historical"), 0.900000, tolerance = 1e-9)
})

test_that("empirical regression summary matches the February 2026 golden reference", {
  rows <- read_output_csv("regression_summary.csv")
  projected_surplus <- rows[rows$specification == "projected_surplus", ]
  debt_ratio <- rows[rows$specification == "debt_ratio", ]

  expect_equal(projected_surplus$observations, 39)
  expect_equal(projected_surplus$feedback_coefficient, -0.1436693, tolerance = 1e-6)
  expect_equal(projected_surplus$feedback_robust_se, 0.0324377, tolerance = 1e-6)
  expect_equal(debt_ratio$observations, 39)
  expect_equal(debt_ratio$feedback_coefficient, 0.1762817, tolerance = 1e-6)
  expect_equal(debt_ratio$feedback_robust_se, 0.0401087, tolerance = 1e-6)
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
