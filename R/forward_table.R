read_ltbo_cbo_paths <- function(input_dir) {
  ltbo_path <- file.path(input_dir, "cbo", "51119-2026-02-LTBO-Budget.xlsx")
  hist_path <- file.path(input_dir, "cbo", "51134-2026-02-Historical-Budget-Data.xlsx")
  assert_files_exist(c(ltbo_path, hist_path))

  ltbo <- readxl::read_excel(
    ltbo_path,
    sheet = "Supplemental Table 1",
    range = "A9:P40"
  )

  assert_required_columns(
    ltbo,
    c(
      "Fiscal year",
      "Revenues minus total noninterest spendingb",
      "Federal debt held by the public",
      "Revenues minus total spendingb",
      "GDP (billions of dollars)",
      "Net interest"
    ),
    "LTBO Supplemental Table 1"
  )

  ltbo <- ltbo |>
    dplyr::mutate(gdp = as.numeric(.data[["GDP (billions of dollars)"]])) |>
    dplyr::transmute(
      year = as.integer(.data[["Fiscal year"]]),
      b_cbo = as.numeric(.data[["Federal debt held by the public"]]) / 100,
      s_cbo = as.numeric(.data[["Revenues minus total noninterest spendingb"]]) / 100,
      s_tot_cbo = as.numeric(.data[["Revenues minus total spendingb"]]) / 100,
      gdp = .data$gdp,
      interest = as.numeric(.data[["Net interest"]]) / 100 * .data$gdp
    )

  debt <- readxl::read_excel(
    hist_path,
    sheet = "1. Rev, Outlays, Surplus, Debt",
    range = "A9:H73"
  )
  debt_year_col <- names(debt)[1]
  debt_col <- names(debt)[grepl("^Debt held by the public", names(debt))]
  debt <- debt |>
    dplyr::transmute(
      year = as.integer(.data[[debt_year_col]]),
      debt = as.numeric(.data[[debt_col]])
    ) |>
    dplyr::filter(.data$year == 2025L)

  debt_gdp <- readxl::read_excel(
    hist_path,
    sheet = "1a. Rev, Outlays, Surplus (GDP)",
    range = "A9:H73"
  )
  debt_gdp_year_col <- names(debt_gdp)[1]
  debt_gdp_col <- names(debt_gdp)[grepl("^Debt held by the public", names(debt_gdp))]
  debt_gdp <- debt_gdp |>
    dplyr::transmute(
      year = as.integer(.data[[debt_gdp_year_col]]),
      b_cbo = as.numeric(.data[[debt_gdp_col]]) / 100
    ) |>
    dplyr::filter(.data$year == 2025L)

  initial <- checked_left_join(debt, debt_gdp, "year", "2025 debt", "2025 debt/GDP") |>
    dplyr::mutate(
      gdp = .data$debt / .data$b_cbo,
      s_cbo = NA_real_,
      s_tot_cbo = NA_real_,
      interest = NA_real_
    ) |>
    dplyr::select("year", "b_cbo", "s_cbo", "s_tot_cbo", "gdp", "interest")

  dplyr::bind_rows(initial, ltbo) |>
    dplyr::arrange(.data$year) |>
    dplyr::mutate(
      debt = .data$b_cbo * .data$gdp,
      primarydeficit = -(.data$s_cbo * .data$gdp),
      i = .data$interest / dplyr::lag(.data$debt),
      g = .data$gdp / dplyr::lag(.data$gdp) - 1,
      rho_cbo = (.data$i - .data$g) / (1 + .data$g),
      debtchange = .data$debt - dplyr::lag(.data$debt),
      m_cbo = (.data$debtchange - .data$primarydeficit - .data$interest) / .data$gdp
    ) |>
    dplyr::filter(.data$year >= 2026L) |>
    dplyr::mutate(model_year = dplyr::row_number()) |>
    dplyr::select("model_year", "year", "b_cbo", "rho_cbo", "s_cbo", "s_tot_cbo", "m_cbo", "gdp", "g")
}

simulate_deterministic_feedback <- function(c_value, cbo_paths, beta_1 = 0.576, beta_2 = 0.00848) {
  assert_no_missing(
    dplyr::filter(cbo_paths, dplyr::between(.data$year, 2026L, 2036L)),
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
  paths <- read_ltbo_cbo_paths(input_dir)
  simulated <- simulate_deterministic_feedback(c_value, paths)

  table_data <- simulated |>
    dplyr::filter(dplyr::between(.data$year, 2026L, 2036L)) |>
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
    dplyr::filter(.data$year >= 2027L)

  list(
    paths = paths,
    simulated = simulated,
    table_data = table_data,
    c_value = c_value
  )
}

append_average_columns <- function(values, years) {
  c(
    stats::setNames(values[match(2027:2036, years)], as.character(2027:2036)),
    "2027-2031" = mean(values[years %in% 2027:2031], na.rm = TRUE),
    "2032-2036" = mean(values[years %in% 2032:2036], na.rm = TRUE),
    "2027-2036" = mean(values[years %in% 2027:2036], na.rm = TRUE)
  )
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

format_table_number <- function(value, digits = 2) {
  if (is.na(value)) {
    return("")
  }
  rounded_value <- round(value, digits)
  if (rounded_value < 0) {
    paste0("$-$", formatC(abs(rounded_value), format = "f", digits = digits))
  } else {
    formatC(rounded_value, format = "f", digits = digits)
  }
}

write_forward_table_latex <- function(rows, c_value, path) {
  years <- c(as.character(2027:2036), "2027-2031", "2032-2036", "2027-2036")
  c_str <- formatC(c_value, format = "f", digits = 2)

  format_row <- function(row) {
    digits <- if (row$line_item == "Debt/GDP" && grepl("Panel A|Panel C", row$panel)) 1 else 2
    values <- vapply(unlist(row[years]), format_table_number, character(1), digits = digits)
    paste0("\\quad ", row$line_item, " & ", paste(values, collapse = " & "), " \\\\")
  }

  lines <- c(
    "%\\vspace{0.5cm}",
    "\\begin{table}[ht!]",
    "\\caption{Minimum Required Deficit Reduction Over the Next Ten Years}",
    "\\centering",
    "\\scriptsize",
    "\\label{tab:needed_deficit_reduction}",
    "",
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{l*{13}{c}}",
    "\\toprule",
    paste0(
      "& 2027 & 2028 & 2029 & 2030 & 2031 & 2032 & 2033 & 2034 & 2035 & 2036",
      " & \\makecell{2027--\\\\2031} & \\makecell{2032--\\\\2036} & \\makecell{2027--\\\\2036} \\\\"
    ),
    "\\midrule"
  )

  panel_names <- unique(rows$panel)
  for (panel_name in panel_names) {
    panel_label <- if (startsWith(panel_name, "Panel C.")) {
      paste0("\\makecell[l]{", panel_name, "}")
    } else {
      panel_name
    }
    lines <- c(lines, paste0("\\multicolumn{14}{@{}l}{\\textit{", panel_label, "}} \\\\"))
    panel_rows <- rows |>
      dplyr::filter(.data$panel == panel_name)
    lines <- c(lines, vapply(seq_len(nrow(panel_rows)), function(i) format_row(panel_rows[i, ]), character(1)))
    if (panel_name != panel_names[[length(panel_names)]]) {
      lines <- c(lines, "\\addlinespace")
    }
  }

  note <- paste0(
    " Notes: This table applies a fixed fiscal feedback parameter from the paper, ",
    "$c^*=", c_str, "$. Panel B applies that fiscal feedback rule to the February 2026 CBO baseline fiscal path listed in Panel A. ",
    "Panel C lists Panel A minus Panel B. All values are expressed as a percent of GDP. ",
    "For example, the first cell of Panel B is the required primary deficit reduction in 2027, equal to ",
    c_str, " times the projected debt-GDP ratio change from 2026 to 2027, with later years taking into account the reduced debt-GDP ratio change due to prior permanent deficit reductions. ",
    "The required total deficit reduction exceeds the required primary deficit reduction due to lower interest payments on the reduced debt path."
  )

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    "\\footnotesize",
    "\\begin{justify}",
    note,
    "\\end{justify}",
    "\\end{table}%"
  )

  writeLines(lines, path)
  invisible(path)
}

write_forward_table_outputs <- function(forward, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  rows <- build_forward_table_rows(forward$table_data)

  readr::write_csv(forward$paths, file.path(output_dir, "forward_cbo_paths_2026.csv"))
  readr::write_csv(forward$simulated, file.path(output_dir, "forward_deterministic_feedback_path.csv"))
  readr::write_csv(forward$table_data, file.path(output_dir, "forward_table_detail.csv"))
  readr::write_csv(rows, file.path(output_dir, "forward_deficit_reduction_table.csv"))
  write_forward_table_latex(
    rows,
    forward$c_value,
    file.path(output_dir, "panel_b_deficit_reduction.tex")
  )

  rows
}
