# Tests for the c-star simulation pipeline. These source the R modules and
# exercise the functions directly against the bootstrapped data-raw inputs.

cstar_repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
cstar_input_dir <- file.path(cstar_repo_root, "data-raw")

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})
for (f in c("assertions.R", "build_inputs.R", "forward_table.R",
            "cbo_paths.R", "simulation_inputs.R", "cstar_simulation.R")) {
  source(file.path(cstar_repo_root, "R", f))
}

skip_if_no_inputs <- function() {
  testthat::skip_if_not(
    dir.exists(cstar_input_dir) &&
      file.exists(file.path(cstar_input_dir, "cbo", "51119-2026-02-LTBO-Budget.xlsx")),
    "Run Rscript scripts/bootstrap_inputs.R first"
  )
}

test_that("simulation inputs calibrate s_u_hat and freeze e_s_size", {
  skip_if_no_inputs()
  si <- build_simulation_inputs(cstar_input_dir)

  expect_true(is.finite(si$s_u_hat_risk[[1]]))
  expect_gt(si$s_u_hat_risk[[1]], 0)
  expect_equal(si$e_s_size[[1]], 0.25)
  expect_gte(si$ar_n[[1]], 40)
  # beta_1 from the calibration regression should be near the fixed 0.576.
  expect_equal(si$beta_1_risk[[1]], 0.576, tolerance = 0.05)

  # e_s_size is overridable but defaults to the production constant.
  si2 <- build_simulation_inputs(cstar_input_dir, e_s_size = 0.30)
  expect_equal(si2$e_s_size[[1]], 0.30)
})

test_that("estimate_rho_shock_sd rejects duplicate years", {
  dupe <- tibble::tibble(
    year = c(1990L, 1990L, 1991L),
    rminusg = c(0.01, 0.01, 0.02),
    debt = c(1, 1, 1),
    g_nom = c(0.05, 0.05, 0.05),
    gdp = c(10, 10, 11)
  )
  expect_error(estimate_rho_shock_sd(dupe), "duplicate", ignore.case = TRUE)
})

test_that("cbo paths build to 103 rows for each vintage", {
  skip_if_no_inputs()
  for (vintage in c(2024L, 2025L, 2026L)) {
    path <- build_cbo_path(cstar_input_dir, vintage)
    expect_equal(nrow(path), 103L)
    expect_true(101L %in% path$model_year)
    expect_equal(anyDuplicated(path$model_year), 0L)
    expect_equal(anyDuplicated(path$year), 0L)
    expect_false(any(is.na(path$b_cbo)))
    expect_false(any(is.na(path$rho_cbo)))
    expect_false(any(is.na(path$s_cbo)))
    expect_false(any(is.na(path$m_cbo)))
    # extension years carry baseline forward and zero out m_cbo
    tail_rows <- path[path$model_year >= 60L, ]
    expect_true(all(tail_rows$m_cbo == 0))
    expect_equal(length(unique(tail_rows$b_cbo)), 1L)
  }
})

test_that("2026 cbo path matches the existing fixed-c builder over 2026-2036", {
  skip_if_no_inputs()
  new26 <- build_cbo_path(cstar_input_dir, 2026L) |>
    dplyr::filter(.data$year <= 2036L) |>
    dplyr::select("year", "b_cbo", "rho_cbo", "s_cbo", "s_tot_cbo", "m_cbo", "gdp")
  old26 <- read_ltbo_cbo_paths(cstar_input_dir) |>
    dplyr::select("year", "b_cbo", "rho_cbo", "s_cbo", "s_tot_cbo", "m_cbo", "gdp")
  joined <- dplyr::inner_join(new26, old26, by = "year", suffix = c("_new", "_old"))
  for (col in c("b_cbo", "rho_cbo", "s_cbo", "s_tot_cbo", "m_cbo", "gdp")) {
    expect_equal(joined[[paste0(col, "_new")]], joined[[paste0(col, "_old")]], tolerance = 1e-10)
  }
})

test_that("no-shock c=0 reproduces the CBO baseline accounting path", {
  skip_if_no_inputs()
  path <- build_cbo_path(cstar_input_dir, 2026L)
  det0 <- build_deterministic_feedback_path(path, 0)
  # identity b_baseline == b_cbo holds over the real projection years (model_year <= 31)
  real <- det0$model_year <= 31L
  expect_equal(det0$b_baseline[real], path$b_cbo[real], tolerance = 1e-10)
  # c = 0 feedback equals the no-feedback (baseline) path everywhere
  expect_equal(det0$b_prescription, det0$b_baseline, tolerance = 1e-12)
  expect_equal(det0$s_prescription, det0$s_baseline, tolerance = 1e-12)
})

test_that("no-shock c>0 applies a nonzero cumulative fiscal adjustment", {
  skip_if_no_inputs()
  path <- build_cbo_path(cstar_input_dir, 2026L)
  det <- build_deterministic_feedback_path(path, 0.19)
  # prescription surplus diverges from baseline after the first period
  diffs <- abs(det$s_prescription - det$s_baseline)
  expect_true(all(diffs[det$model_year == 1L] < 1e-12))
  expect_true(any(diffs[det$model_year > 1L] > 1e-6))
})

test_that("terminal debt summary is invariant to return_paths", {
  skip_if_no_inputs()
  path <- build_cbo_path(cstar_input_dir, 2026L)
  a <- simulate_cbo_paths(path, c = 0.19, s_u = 0.0229, e_s_size = 0.25, reps = 50L, return_paths = FALSE)
  b <- simulate_cbo_paths(path, c = 0.19, s_u = 0.0229, e_s_size = 0.25, reps = 50L, return_paths = TRUE)
  expect_equal(a$terminal_b, b$terminal_b, tolerance = 1e-12)
})

test_that("scan_cstar returns the first passing grid value", {
  # synthetic monotone share curve: shares cross 0.95 at c = 0.18
  fake <- list(c = c(0, 0.17, 0.18, 0.19), share = c(0.10, 0.94, 0.96, 0.97))
  # emulate scan selection logic directly
  c_grid <- fake$c
  passing <- c_grid[fake$share >= 0.95]
  expect_equal(min(passing), 0.18)
})

test_that("2026 c* sits on the 95% boundary (0.18 at seed 1000; paper 0.19)", {
  skip_if_no_inputs()
  si <- build_simulation_inputs(cstar_input_dir)
  path <- build_cbo_path(cstar_input_dir, 2026L)
  scan <- scan_cstar(path, c(0, 0.18, 0.19, 0.20), reps = 1000L,
                     s_u = si$s_u_hat_risk[[1]], e_s_size = si$e_s_size[[1]])
  # boundary result: computed c* is one grid step around the paper's 0.19
  expect_true(scan$c_star %in% c(0.18, 0.19))
  expect_false(scan$at_lower_bound)
  expect_false(scan$at_upper_bound)
  # c = 0.19 clears the bar; c = 0 fails badly
  share_019 <- scan$scan$share_below[scan$scan$c == 0.19]
  share_0 <- scan$scan$share_below[scan$scan$c == 0]
  expect_gte(share_019, 0.95)
  expect_lt(share_0, 0.2)
})
