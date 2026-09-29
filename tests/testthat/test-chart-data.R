read_chart_csv <- function(name) {
  path <- file.path(scorecard_output_dir, "chart_data", name)
  expect_true(file.exists(path), info = "Run Rscript scripts/run_pipeline.R first")
  readr::read_csv(path, show_col_types = FALSE)
}

test_that("chart data files agree with the scorecard", {
  scorecard <- readr::read_csv(file.path(scorecard_output_dir, "scorecard.csv"), show_col_types = FALSE)
  scatter <- read_chart_csv("scorecard_scatter.csv")
  distribution <- read_chart_csv("deviation_distribution.csv")

  expect_setequal(scatter$period, scorecard$period_label)
  expect_setequal(distribution$period, scorecard$period_label)
  expect_setequal(unique(scatter$era), c("pre_2004", "post_2004", "recent"))
  expect_equal(
    scatter$deviation_pct_gdp[match(scorecard$period_label, scatter$period)],
    scorecard$failure_value
  )

  eras <- c("pre_2004", "post_2004", "recent")
  weights <- vapply(eras, function(era) sum(distribution$weight_pct[distribution$era == era]), numeric(1))
  expect_equal(unname(weights), c(100, 100, 0))
})

test_that("the required deficit reduction chart data covers every table cell", {
  table <- read_chart_csv("required_deficit_reduction.csv")
  forward_rows <- readr::read_csv(
    file.path(scorecard_output_dir, "forward_deficit_reduction_table.csv"),
    show_col_types = FALSE
  )
  value_columns <- setdiff(names(forward_rows), c("panel", "line_item", "variable"))

  expect_equal(nrow(table), nrow(forward_rows) * length(value_columns))
  expect_setequal(table$group, c("baseline", "reduction", "resulting"))
  expect_setequal(table$tier, c("annual", "average"))
  expect_equal(
    table$value[table$group == "reduction" & table$row == "primary_deficit" & table$col == value_columns[[1]]],
    forward_rows[[value_columns[[1]]]][
      startsWith(forward_rows$panel, "Panel B") & forward_rows$line_item == "Primary deficit"
    ],
    tolerance = 1e-9
  )
})
