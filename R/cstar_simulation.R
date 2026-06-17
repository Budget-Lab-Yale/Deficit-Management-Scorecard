# Vectorized c-star Monte Carlo simulator.
#
# Ports sim_full_cbo (m0_programs_scorecard*.do) and the c-star scan in
# m3_exhibits_prod_scorecard26.do. The Stata "deterministic" scenario equals the
# "stochastic" scenario with shocks switched off, so we carry only two scenarios:
#   - stoch: no fiscal feedback (the CBO baseline path under shocks)
#   - feedback: fiscal feedback at strength c (equals stoch when c == 0)
# Reps are vectorized; the year loop runs over 103 model years.

SIM_BETA_1 <- 0.576
SIM_BETA_2 <- 0.00848
SIM_LAMBDA <- 0.02
SIM_LAST_PERIOD <- 101L
SIM_TERMINAL_MODEL_YEAR <- 101L
SIM_DEBT_THRESHOLD <- 2.5
SIM_TARGET_PROBABILITY <- 0.95
SIM_SEED <- 1000L

# Per-vintage risk reps, c-grid, and the calendar window for the pdchange table.
cstar_grid_config <- function(vintage) {
  configs <- list(
    `2024` = list(risk_reps = 1000L, c_grid = c(0, 0.17, 0.18, 0.19), year_start = 2025L, terminal_year = 2034L),
    `2025` = list(risk_reps = 5000L, c_grid = c(0, 0.17, 0.18, 0.19), year_start = 2026L, terminal_year = 2035L),
    `2026` = list(risk_reps = 1000L, c_grid = c(0, 0.18, 0.19, 0.20), year_start = 2027L, terminal_year = 2036L)
  )
  key <- as.character(vintage)
  if (!key %in% names(configs)) {
    abort(sprintf("No c-star grid configured for vintage %s", vintage))
  }
  configs[[key]]
}

# Simulate `reps` economies for one feedback strength `c`. Returns the terminal
# debt-to-GDP ratio (at model_year 101) across reps, and optionally the full
# deterministic baseline/prescription paths (use only with small reps).
simulate_cbo_paths <- function(cbo_path, c, s_u, e_s_size, reps,
                               shocks = TRUE, seed = SIM_SEED, return_paths = FALSE,
                               beta_1 = SIM_BETA_1, beta_2 = SIM_BETA_2, lambda = SIM_LAMBDA,
                               last_period = SIM_LAST_PERIOD) {
  set.seed(seed)

  cbo_path <- dplyr::arrange(cbo_path, .data$model_year)
  obs <- nrow(cbo_path)
  rho_cbo <- cbo_path$rho_cbo
  s_cbo <- cbo_path$s_cbo
  b_cbo <- cbo_path$b_cbo
  m_cbo <- cbo_path$m_cbo

  # Current-period state, one element per rep.
  rho_s <- rep(rho_cbo[[1]], reps)
  b_s <- rep(b_cbo[[1]], reps)
  rho_f <- rep(rho_cbo[[1]], reps)
  b_f <- rep(b_cbo[[1]], reps)
  cum <- rep(0, reps)

  term_b_s <- NULL
  term_b_f <- NULL

  if (return_paths) {
    B_s <- matrix(NA_real_, obs, reps)
    B_f <- matrix(NA_real_, obs, reps)
    S_f <- matrix(NA_real_, obs, reps)
    B_s[1, ] <- b_s
    B_f[1, ] <- b_f
    S_f[1, ] <- s_cbo[[1]]
  }

  for (t in 2:obs) {
    if (shocks && t <= last_period) {
      e_u <- stats::rnorm(reps, 0, s_u)
      e_s <- e_s_size * stats::rpois(reps, lambda)
    } else {
      e_u <- 0
      e_s <- 0
    }

    rho_s_new <- rho_cbo[[t]] + beta_1 * (rho_s - rho_cbo[[t - 1]]) +
      beta_2 * (b_s - b_cbo[[t - 1]]) + e_u
    b_s_new <- b_s * (1 + rho_s_new) - s_cbo[[t]] + m_cbo[[t]] + e_s

    if (c > 0) {
      rho_f_new <- rho_cbo[[t]] + beta_1 * (rho_f - rho_cbo[[t - 1]]) +
        beta_2 * (b_f - b_cbo[[t - 1]]) + e_u
      delta_s <- c * (rho_f_new * b_f + m_cbo[[t]] - (s_cbo[[t]] + cum))
      cum_new <- cum + delta_s
      s_f_new <- s_cbo[[t]] + cum_new
      b_f_new <- b_f * (1 + rho_f_new) - s_f_new + m_cbo[[t]] + e_s
    } else {
      rho_f_new <- rho_s_new
      cum_new <- cum
      s_f_new <- s_cbo[[t]]
      b_f_new <- b_s_new
    }

    rho_s <- rho_s_new
    b_s <- b_s_new
    rho_f <- rho_f_new
    b_f <- b_f_new
    cum <- cum_new

    if (t == SIM_TERMINAL_MODEL_YEAR) {
      term_b_s <- b_s
      term_b_f <- b_f
    }
    if (return_paths) {
      B_s[t, ] <- b_s
      B_f[t, ] <- b_f
      S_f[t, ] <- s_f_new
    }
  }

  terminal_b <- if (c > 0) term_b_f else term_b_s

  result <- list(terminal_b = terminal_b, c = c, reps = reps)
  if (return_paths) {
    result$paths <- tibble::tibble(
      model_year = cbo_path$model_year,
      year = cbo_path$year,
      b_baseline = B_s[, 1],
      s_baseline = s_cbo,
      b_prescription = B_f[, 1],
      s_prescription = S_f[, 1]
    )
  }
  result
}

# Scan a c-grid and find c* = first grid value with at least `probability` of
# terminal debt paths below `threshold`. Returns the c* and per-c diagnostics.
scan_cstar <- function(cbo_path, c_grid, reps, s_u, e_s_size,
                       threshold = SIM_DEBT_THRESHOLD, probability = SIM_TARGET_PROBABILITY,
                       seed = SIM_SEED) {
  c_grid <- sort(unique(c_grid))
  scan <- dplyr::bind_rows(lapply(c_grid, function(c) {
    sim <- simulate_cbo_paths(cbo_path, c = c, s_u = s_u, e_s_size = e_s_size,
                              reps = reps, shocks = TRUE, seed = seed)
    tibble::tibble(c = c, share_below = mean(sim$terminal_b < threshold))
  }))

  passing <- scan$c[scan$share_below >= probability]
  c_star <- if (length(passing) > 0) min(passing) else NA_real_

  list(
    scan = scan,
    c_star = c_star,
    grid_min = min(c_grid),
    grid_max = max(c_grid),
    at_lower_bound = !is.na(c_star) && abs(c_star - min(c_grid)) < 1e-8,
    at_upper_bound = !is.na(c_star) && abs(c_star - max(c_grid)) < 1e-8
  )
}

# Deterministic (no-shock) baseline and c*-prescription paths for one vintage.
build_deterministic_feedback_path <- function(cbo_path, c_star) {
  sim <- simulate_cbo_paths(cbo_path, c = c_star, s_u = 0, e_s_size = 0,
                            reps = 1L, shocks = FALSE, return_paths = TRUE)
  sim$paths
}

# Run the full c-star scan across vintages; returns summary + grid diagnostics +
# deterministic paths keyed by vintage.
build_cstar_summary <- function(input_dir, simulation_inputs,
                                vintages = c(2024L, 2025L, 2026L),
                                reps_scale = 1) {
  s_u <- simulation_inputs$s_u_hat_risk[[1]]
  e_s_size <- simulation_inputs$e_s_size[[1]]

  summary_rows <- list()
  grid_rows <- list()
  pd_rows <- list()
  det_paths <- list()
  cbo_paths <- list()

  for (vintage in vintages) {
    grid_cfg <- cstar_grid_config(vintage)
    reps <- max(1L, as.integer(round(grid_cfg$risk_reps * reps_scale)))
    cbo_path <- build_cbo_path(input_dir, vintage)
    cbo_paths[[as.character(vintage)]] <- cbo_path

    scan <- scan_cstar(cbo_path, grid_cfg$c_grid, reps = reps, s_u = s_u, e_s_size = e_s_size)

    summary_rows[[length(summary_rows) + 1L]] <- tibble::tibble(
      ltbo = as.integer(vintage),
      risk_reps = reps,
      grid_min = scan$grid_min,
      grid_max = scan$grid_max,
      c_star_250_95 = scan$c_star,
      at_lower_bound = as.integer(scan$at_lower_bound),
      at_upper_bound = as.integer(scan$at_upper_bound)
    )
    grid_rows[[length(grid_rows) + 1L]] <- scan$scan |>
      dplyr::mutate(ltbo = as.integer(vintage), risk_reps = reps, .before = 1)

    pd_raw <- NA_real_
    if (!is.na(scan$c_star)) {
      det <- build_deterministic_feedback_path(cbo_path, scan$c_star)
      det_paths[[as.character(vintage)]] <- det
      window <- det |>
        dplyr::filter(dplyr::between(.data$year, grid_cfg$year_start, grid_cfg$terminal_year)) |>
        dplyr::mutate(
          pd_base = -.data$s_baseline * 100,
          pd_presc = -.data$s_prescription * 100,
          pd_change = .data$pd_base - .data$pd_presc
        )
      pd_raw <- window$pd_change[window$year == grid_cfg$terminal_year]
      if (length(pd_raw) != 1) pd_raw <- NA_real_
    }
    pd_rows[[length(pd_rows) + 1L]] <- tibble::tibble(
      ltbo = as.integer(vintage),
      terminal_year = grid_cfg$terminal_year,
      c_star_used = scan$c_star,
      pd_change_terminal_raw = pd_raw,
      pd_change_terminal_abs = abs(pd_raw)
    )
  }

  list(
    summary = dplyr::bind_rows(summary_rows),
    grid_scan = dplyr::bind_rows(grid_rows),
    pdchange = dplyr::bind_rows(pd_rows),
    deterministic_paths = det_paths,
    cbo_paths = cbo_paths
  )
}

# Orchestrate the full c-star run: calibrate, scan all vintages, persist paths,
# and write the diagnostic CSVs plus the computed-c* forward table. Shared by the
# standalone entrypoint and run_pipeline.R. Returns the scan summary invisibly.
# Requires R/forward_table.R, R/build_inputs.R, and R/simulation_inputs.R sourced.
write_cstar_outputs <- function(input_dir, data_dir, output_dir, reps_scale = 1) {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  simulation_inputs <- build_simulation_inputs(input_dir)
  saveRDS(simulation_inputs, file.path(data_dir, "simulation_inputs.rds"))

  result <- build_cstar_summary(input_dir, simulation_inputs, reps_scale = reps_scale)

  for (vintage in names(result$cbo_paths)) {
    saveRDS(result$cbo_paths[[vintage]], file.path(data_dir, sprintf("cbo_paths_%s.rds", vintage)))
  }

  readr::write_csv(result$summary, file.path(output_dir, "ltbo_cstar_values.csv"))
  readr::write_csv(result$grid_scan, file.path(output_dir, "ltbo_cstar_grid_scan.csv"))
  readr::write_csv(result$pdchange, file.path(output_dir, "ltbo_pdchange_terminal_values.csv"))

  c_star_2026 <- result$summary$c_star_250_95[result$summary$ltbo == 2026L]
  if (length(c_star_2026) != 1 || is.na(c_star_2026)) {
    abort("Could not determine 2026 c* from the scan; cannot build the c-star forward table.")
  }

  forward_cstar <- build_forward_table_data(input_dir, c_value = c_star_2026)
  forward_rows <- build_forward_table_rows(forward_cstar$table_data)
  readr::write_csv(forward_rows, file.path(output_dir, "forward_table_cstar.csv"))
  readr::write_csv(forward_cstar$simulated, file.path(output_dir, "forward_deterministic_feedback_path_cstar.csv"))

  result$simulation_inputs <- simulation_inputs
  result$c_star_2026 <- c_star_2026
  invisible(result)
}
