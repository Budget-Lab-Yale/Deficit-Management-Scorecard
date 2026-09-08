# Build the February 2026 CBO baseline used by the forward table.
#
# Surpluses are positive and deficits are negative. The LTBO workbook reports
# debt and budget values as shares of GDP; this module converts them to ratios.

cbo_path_config <- function() {
  list(
    ltbo_file = "51119-2026-02-LTBO-Budget.xlsx",
    ltbo_sheet = "Supplemental Table 1",
    ltbo_range = "A9:P40",
    historical_file = "51134-2026-02-Historical-Budget-Data.xlsx",
    seed_year = 2025L
  )
}

find_col <- function(column_names, pattern, label) {
  hits <- column_names[grepl(pattern, column_names)]
  if (length(hits) != 1) {
    abort(sprintf(
      "Expected exactly one column matching %s for %s, found %s: %s",
      pattern, label, length(hits), paste(hits, collapse = ", ")
    ))
  }
  hits
}

read_ltbo_projection <- function(input_dir) {
  cfg <- cbo_path_config()
  path <- file.path(input_dir, "cbo", cfg$ltbo_file)
  assert_files_exist(path)

  ltbo <- readxl::read_excel(
    path,
    sheet = cfg$ltbo_sheet,
    range = cfg$ltbo_range,
    .name_repair = "unique_quiet"
  )
  nm <- names(ltbo)

  year_col <- nm[[1]]
  debt_col <- find_col(nm, "^Federal debt held by the public", "debt-to-GDP")
  primary_col <- find_col(nm, "Revenues minus total noninterest", "primary surplus")
  total_col <- find_col(nm, "Revenues minus total spending", "total surplus")
  gdp_col <- find_col(nm, "^GDP \\(billions", "GDP")
  interest_col <- find_col(nm, "^Net interest", "net interest")

  ltbo |>
    dplyr::transmute(
      year = as.integer(.data[[year_col]]),
      b_cbo = as.numeric(.data[[debt_col]]) / 100,
      s_cbo = as.numeric(.data[[primary_col]]) / 100,
      s_tot_cbo = as.numeric(.data[[total_col]]) / 100,
      gdp = as.numeric(.data[[gdp_col]]),
      interest = as.numeric(.data[[interest_col]]) / 100 * .data$gdp
    ) |>
    dplyr::filter(!is.na(.data$year), .data$year >= 2000L)
}

read_previous_year_budget <- function(input_dir) {
  cfg <- cbo_path_config()
  path <- file.path(input_dir, "cbo", cfg$historical_file)
  assert_files_exist(path)

  debt <- readxl::read_excel(
    path,
    sheet = "1. Rev, Outlays, Surplus, Debt",
    range = "A9:H73",
    .name_repair = "unique_quiet"
  )
  debt_col <- find_col(names(debt), "^Debt held by the public", "historical debt level")
  debt <- debt |>
    dplyr::transmute(
      year = suppressWarnings(as.integer(.data[[names(debt)[[1]]]])),
      debt = suppressWarnings(as.numeric(.data[[debt_col]]))
    ) |>
    dplyr::filter(.data$year == cfg$seed_year)

  debt_gdp <- readxl::read_excel(
    path,
    sheet = "1a. Rev, Outlays, Surplus (GDP)",
    range = "A9:H73",
    .name_repair = "unique_quiet"
  )
  debt_gdp <- debt_gdp |>
    dplyr::transmute(
      year = suppressWarnings(as.integer(.data[[names(debt_gdp)[[1]]]])),
      b_cbo = suppressWarnings(as.numeric(.data[[names(debt_gdp)[[ncol(debt_gdp)]]]])) / 100
    ) |>
    dplyr::filter(.data$year == cfg$seed_year)

  checked_left_join(debt, debt_gdp, "year", "seed debt", "seed debt/GDP") |>
    dplyr::mutate(
      gdp = .data$debt / .data$b_cbo,
      s_cbo = NA_real_,
      s_tot_cbo = NA_real_,
      interest = NA_real_
    ) |>
    dplyr::select("year", "b_cbo", "s_cbo", "s_tot_cbo", "gdp", "interest")
}

build_cbo_path <- function(input_dir) {
  projection <- read_ltbo_projection(input_dir)
  seed <- read_previous_year_budget(input_dir)

  path <- dplyr::bind_rows(seed, projection) |>
    dplyr::arrange(.data$year) |>
    dplyr::mutate(
      debt = .data$b_cbo * .data$gdp,
      primary_deficit = -(.data$s_cbo * .data$gdp),
      interest_rate = .data$interest / dplyr::lag(.data$debt),
      g = .data$gdp / dplyr::lag(.data$gdp) - 1,
      rho_cbo = (.data$interest_rate - .data$g) / (1 + .data$g),
      debt_change = .data$debt - dplyr::lag(.data$debt),
      m_cbo = (.data$debt_change - .data$primary_deficit - .data$interest) / .data$gdp
    ) |>
    dplyr::filter(.data$year > cbo_path_config()$seed_year) |>
    dplyr::mutate(model_year = dplyr::row_number()) |>
    dplyr::select(
      "model_year", "year", "b_cbo", "rho_cbo", "s_cbo", "s_tot_cbo", "m_cbo", "gdp", "g"
    )

  assert_unique_key(path, "model_year", "February 2026 CBO path")
  assert_unique_key(path, "year", "February 2026 CBO path")
  assert_no_missing(
    path,
    c("b_cbo", "rho_cbo", "s_cbo", "s_tot_cbo", "m_cbo", "gdp", "g"),
    "February 2026 CBO path"
  )
  path
}
