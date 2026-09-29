simulate_deterministic_feedback <- function(c_value, cbo_paths, beta_1 = 0.576, beta_2 = 0.00848) {
  first_year <- scorecard_vintage()$latest_report_year
  assert_no_missing(
    dplyr::filter(cbo_paths, dplyr::between(.data$year, first_year, max(forward_table_years()))),
    c("b_cbo", "rho_cbo", "s_cbo", "s_tot_cbo", "m_cbo", "g"),
    "CBO forward table paths"
  )

  paths <- cbo_paths |>
    dplyr::arrange(.data$year) |>
    dplyr::mutate(
      rho_det = NA_real_,
      s_det = NA_real_,
      b_det = NA_real_,
      rho_feedback = NA_real_,
      s_feedback = NA_real_,
      b_feedback = NA_real_,
      cumulative_adjustment = NA_real_
    )

  paths$rho_det[[1]] <- paths$rho_cbo[[1]]
  paths$s_det[[1]] <- paths$s_cbo[[1]]
  paths$b_det[[1]] <- paths$b_cbo[[1]]
  paths$rho_feedback[[1]] <- paths$rho_cbo[[1]]
  paths$s_feedback[[1]] <- paths$s_cbo[[1]]
  paths$b_feedback[[1]] <- paths$b_cbo[[1]]
  paths$cumulative_adjustment[[1]] <- 0

  for (i in 2:nrow(paths)) {
    paths$rho_det[[i]] <- paths$rho_cbo[[i]] +
      beta_1 * (paths$rho_det[[i - 1]] - paths$rho_cbo[[i - 1]]) +
      beta_2 * (paths$b_det[[i - 1]] - paths$b_cbo[[i - 1]])
    paths$s_det[[i]] <- paths$s_cbo[[i]]
    paths$b_det[[i]] <- paths$b_det[[i - 1]] * (1 + paths$rho_det[[i]]) -
      paths$s_det[[i]] + paths$m_cbo[[i]]

    paths$rho_feedback[[i]] <- paths$rho_cbo[[i]] +
      beta_1 * (paths$rho_feedback[[i - 1]] - paths$rho_cbo[[i - 1]]) +
      beta_2 * (paths$b_feedback[[i - 1]] - paths$b_cbo[[i - 1]])
    delta_s <- c_value * (
      paths$rho_feedback[[i]] * paths$b_feedback[[i - 1]] +
        paths$m_cbo[[i]] -
        (paths$s_cbo[[i]] + paths$cumulative_adjustment[[i - 1]])
    )
    paths$cumulative_adjustment[[i]] <- paths$cumulative_adjustment[[i - 1]] + delta_s
    paths$s_feedback[[i]] <- paths$s_cbo[[i]] + paths$cumulative_adjustment[[i]]
    paths$b_feedback[[i]] <- paths$b_feedback[[i - 1]] * (1 + paths$rho_feedback[[i]]) -
      paths$s_feedback[[i]] + paths$m_cbo[[i]]
  }

  paths |>
    dplyr::mutate(c_value = c_value, beta_1 = beta_1, beta_2 = beta_2)
}

build_forward_table_data <- function(input_dir, c_value = 0.19) {
  vintage <- scorecard_vintage()
  table_years <- forward_table_years(vintage)
  paths <- build_cbo_path(input_dir)
  simulated <- simulate_deterministic_feedback(c_value, paths)

  table_data <- simulated |>
    dplyr::filter(dplyr::between(.data$year, vintage$latest_report_year, max(table_years))) |>
    dplyr::mutate(
      b_baseline = .data$b_det,
      s_baseline = .data$s_det,
      rho_baseline = .data$rho_det,
      b_prescription = .data$b_feedback,
      s_prescription = .data$s_feedback,
      rho_prescription = .data$rho_feedback,
      td_baseline = -.data$s_tot_cbo,
      i_prescription = .data$rho_prescription * (1 + .data$g) + .data$g,
      interest_prescription = .data$i_prescription * dplyr::lag(.data$b_prescription) / (1 + .data$g),
      td_prescription = (-.data$s_prescription) + .data$interest_prescription,
      pd_baseline = -.data$s_baseline * 100,
      pd_prescription = -.data$s_prescription * 100,
      pd_change = .data$pd_baseline - .data$pd_prescription,
      td_baseline_pct = .data$td_baseline * 100,
      td_prescription_pct = .data$td_prescription * 100,
      td_change = .data$td_baseline_pct - .data$td_prescription_pct,
      b_baseline = .data$b_baseline * 100,
      b_prescription = .data$b_prescription * 100,
      b_change = .data$b_baseline - .data$b_prescription
    ) |>
    dplyr::filter(.data$year >= min(table_years))

  list(
    paths = paths,
    simulated = simulated,
    table_data = table_data,
    c_value = c_value
  )
}

append_average_columns <- function(values, years) {
  table_years <- forward_table_years()
  averages <- vapply(
    forward_table_windows(),
    function(window) mean(values[years %in% window], na.rm = TRUE),
    numeric(1)
  )
  c(stats::setNames(values[match(table_years, years)], as.character(table_years)), averages)
}

build_forward_table_rows <- function(table_data) {
  rows <- list(
    list("Panel A. CBO baseline fiscal path", "Primary deficit", "pd_baseline"),
    list("Panel A. CBO baseline fiscal path", "Deficit", "td_baseline_pct"),
    list("Panel A. CBO baseline fiscal path", "Debt/GDP", "b_baseline"),
    list("Panel B. Minimum reductions from CBO baseline required to achieve fiscal sustainability", "Primary deficit", "pd_change"),
    list("Panel B. Minimum reductions from CBO baseline required to achieve fiscal sustainability", "Deficit", "td_change"),
    list("Panel B. Minimum reductions from CBO baseline required to achieve fiscal sustainability", "Debt/GDP", "b_change"),
    list("Panel C. Fiscal path after minimum reductions required to achieve fiscal sustainability", "Primary deficit", "pd_prescription"),
    list("Panel C. Fiscal path after minimum reductions required to achieve fiscal sustainability", "Deficit", "td_prescription_pct"),
    list("Panel C. Fiscal path after minimum reductions required to achieve fiscal sustainability", "Debt/GDP", "b_prescription")
  )

  dplyr::bind_rows(lapply(rows, function(row) {
    values <- append_average_columns(table_data[[row[[3]]]], table_data$year)
    tibble::tibble(
      panel = row[[1]],
      line_item = row[[2]],
      variable = row[[3]],
      !!!as.list(values)
    )
  }))
}

write_forward_table_outputs <- function(forward, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  rows <- build_forward_table_rows(forward$table_data)

  readr::write_csv(forward$paths, file.path(output_dir, "forward_cbo_baseline_path.csv"))
  readr::write_csv(forward$simulated, file.path(output_dir, "forward_feedback_path.csv"))
  readr::write_csv(forward$table_data, file.path(output_dir, "forward_table_detail.csv"))
  readr::write_csv(rows, file.path(output_dir, "forward_deficit_reduction_table.csv"))

  rows
}
