test_that("forward table is generated from fixed c value", {
  repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  forward_path <- file.path(repo_root, "data", "processed", "forward_table.rds")
  csv_path <- file.path(repo_root, "output", "forward_deficit_reduction_table.csv")
  tex_path <- file.path(repo_root, "output", "panel_b_deficit_reduction.tex")

  expect_true(file.exists(forward_path), info = "Run Rscript scripts/run_pipeline.R first")
  expect_true(file.exists(csv_path), info = "Run Rscript scripts/run_pipeline.R first")
  expect_true(file.exists(tex_path), info = "Run Rscript scripts/run_pipeline.R first")

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
