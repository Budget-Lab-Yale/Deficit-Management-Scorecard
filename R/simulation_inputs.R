# Simulation calibration inputs for the c-star pipeline.
#
# Only s_u_hat is data-dependent. It is the RMSE of the unrestricted AR
# regression `reg rho L.rho L.b if year >= 1972` from m1_sims_inputs.do, fit on
# the annual realized debt-evolution series. e_s_size is a frozen constant
# (0.25) in all three m2_sims_master_scorecard*.do masters; the m1 potential-GDP
# formula that nominally produces it is never read by a simulation.

# Fit rho_t = cons + beta_1 * rho_{t-1} + beta_2 * b_{t-1} + u_t on year >= min_year
# and return the RMSE as s_u_hat_risk. b = debt / gdp; rho = rminusg / (1 + g_nom).
estimate_rho_shock_sd <- function(historical_debt_evolution, min_year = 1972L) {
  assert_required_columns(
    historical_debt_evolution,
    c("year", "rminusg", "debt", "g_nom", "gdp"),
    "historical debt evolution"
  )

  data <- historical_debt_evolution |>
    dplyr::arrange(.data$year) |>
    dplyr::mutate(b = .data$debt / .data$gdp)

  if (!("rho" %in% names(data))) {
    data <- dplyr::mutate(data, rho = .data$rminusg / (1 + .data$g_nom))
  }

  assert_unique_key(data, "year", "historical debt evolution")

  # Lag by calendar year (gap-safe) to mirror Stata's `tsset year; L.`.
  lagged <- data |>
    dplyr::transmute(year = .data$year + 1L, L_rho = .data$rho, L_b = .data$b)

  sample <- data |>
    dplyr::left_join(lagged, by = "year") |>
    dplyr::filter(
      .data$year >= min_year,
      !is.na(.data$rho),
      !is.na(.data$L_rho),
      !is.na(.data$L_b)
    )

  if (nrow(sample) < 10L) {
    abort(sprintf(
      "AR calibration sample has only %s usable rows (need year >= %s with lags); check the historical series.",
      nrow(sample),
      min_year
    ))
  }

  model <- stats::lm(rho ~ L_rho + L_b, data = sample)

  list(
    beta_1_risk = unname(stats::coef(model)[["L_rho"]]),
    beta_2_risk = unname(stats::coef(model)[["L_b"]]),
    cons_risk = unname(stats::coef(model)[["(Intercept)"]]),
    s_u_hat_risk = stats::sigma(model),
    n = nrow(sample)
  )
}

# One-row tibble of calibration inputs consumed by the simulator. e_s_size is the
# production constant 0.25 by default and is intentionally not derived from data.
build_simulation_inputs <- function(input_dir, e_s_size = 0.25) {
  historical <- read_historical_debt_evolution(input_dir)
  ar <- estimate_rho_shock_sd(historical)

  if (!is.finite(ar$s_u_hat_risk) || ar$s_u_hat_risk <= 0) {
    abort(sprintf("s_u_hat_risk must be finite and positive, got %s", ar$s_u_hat_risk))
  }
  if (!is.finite(e_s_size) || e_s_size <= 0) {
    abort(sprintf("e_s_size must be finite and positive, got %s", e_s_size))
  }

  tibble::tibble(
    beta_1_risk = ar$beta_1_risk,
    beta_2_risk = ar$beta_2_risk,
    cons_risk = ar$cons_risk,
    s_u_hat_risk = ar$s_u_hat_risk,
    e_s_size = e_s_size,
    ar_n = ar$n
  )
}
