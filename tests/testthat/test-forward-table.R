test_that("forward table is generated from fixed c value", {
  forward_path <- file.path(scorecard_data_dir, "forward_table.rds")
  csv_path <- file.path(scorecard_output_dir, "forward_deficit_reduction_table.csv")

  expect_true(file.exists(forward_path), info = "Run Rscript scripts/run_pipeline.R first")
  expect_true(file.exists(csv_path), info = "Run Rscript scripts/run_pipeline.R first")

  forward <- readRDS(forward_path)
  rows <- readr::read_csv(csv_path, show_col_types = FALSE)

  expect_equal(forward$c_value, 0.19, tolerance = 1e-12)
  expect_equal(nrow(rows), 9)
  expect_true(all(as.character(2027:2036) %in% names(rows)))
  expect_true(all(c("2027-2031", "2032-2036", "2027-2036") %in% names(rows)))

  primary_reduction <- rows |>
    dplyr::filter(
      .data$panel == "Panel B. Minimum reductions from CBO baseline required to achieve fiscal sustainability",
      .data$line_item == "Primary deficit"
    )
  expect_equal(primary_reduction[["2027"]], 0.28, tolerance = 0.03)
  expect_gt(primary_reduction[["2036"]], 1)
})

test_that("forward table uses the February 2026 CBO path", {
  forward <- readRDS(file.path(scorecard_data_dir, "forward_table.rds"))

  expect_identical(range(forward$paths$year), c(2026L, 2056L))
  expect_equal(nrow(forward$paths), 31)
  expect_equal(forward$paths$b_cbo[forward$paths$year == 2026L], 1.00605)
  expect_equal(forward$paths$b_cbo[forward$paths$year == 2036L], 1.20223, tolerance = 1e-5)
  expect_false(anyNA(forward$paths))
})
