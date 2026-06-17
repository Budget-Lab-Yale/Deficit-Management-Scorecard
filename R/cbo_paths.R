# CBO long-term baseline paths for the c-star simulation.
#
# Ports m0_prepare_cbo_data_scorecard{24,25,26}.do. Each vintage reads its LTBO
# workbook for the projection path, seeds the prior year's debt/GDP from that
# vintage's Historical-Budget-Data workbook (used only to compute the first
# year's implied excess interest rate and other-means-of-financing), then
# extends the path to 103 model years by carrying baseline levels forward.
#
# Sign convention: s_cbo and s_tot_cbo are primary / total *surplus* as a share
# of GDP (negative in deficit). The 2024/2025 "Primary deficit (-)" columns and
# the 2026 "Revenues minus total noninterest spending" column are all stored
# this way (negative in deficit), so dividing by 100 with no flip is correct.

CBO_PATH_OBS <- 103L

# Vintage-specific workbook/sheet/range/column configuration.
cbo_path_config <- function(vintage) {
  configs <- list(
    `2024` = list(
      ltbo_file = "51119-2024-03-LTBO-budget.xlsx",
      ltbo_sheet = "1. Summary Ext Baseline",
      ltbo_range = "A10:AC41",
      gdp_pattern = "^GDP \\(trillions",
      gdp_multiplier = 1000,
      s_cbo_pattern = "^Primary deficit",
      s_tot_pattern = NA_character_,
      hist_file = "51134-2024-02-Historical-Budget-Data.xlsx",
      seed_year = 2023L
    ),
    `2025` = list(
      ltbo_file = "51119-2025-03-LTBO-budget.xlsx",
      ltbo_sheet = "1. Summary Ext Baseline",
      ltbo_range = "A10:AC41",
      gdp_pattern = "^Gross domestic product",
      gdp_multiplier = 1000,
      s_cbo_pattern = "^Primary deficit",
      s_tot_pattern = "^Total deficit",
      hist_file = "51134-2025-01-Historical-Budget-Data.xlsx",
      seed_year = 2024L
    ),
    `2026` = list(
      ltbo_file = "51119-2026-02-LTBO-Budget.xlsx",
      ltbo_sheet = "Supplemental Table 1",
      ltbo_range = "A9:P40",
      gdp_pattern = "^GDP \\(billions",
      gdp_multiplier = 1,
      s_cbo_pattern = "Revenues minus total noninterest",
      s_tot_pattern = "Revenues minus total spending",
      hist_file = "51134-2026-02-Historical-Budget-Data.xlsx",
      seed_year = 2025L
    )
  )
  key <- as.character(vintage)
  if (!key %in% names(configs)) {
    abort(sprintf("Unknown CBO LTBO vintage: %s (expected 2024, 2025, or 2026)", vintage))
  }
  configs[[key]]
}

# Find the single column whose name matches `pattern`, or abort.
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

read_ltbo_projection <- function(input_dir, vintage) {
  cfg <- cbo_path_config(vintage)
  path <- file.path(input_dir, "cbo", cfg$ltbo_file)
  assert_files_exist(path)

  ltbo <- readxl::read_excel(path, sheet = cfg$ltbo_sheet, range = cfg$ltbo_range)
  nm <- names(ltbo)

  year_col <- nm[[1]]
  b_col <- find_col(nm, "^Federal debt held by the public", "debt-to-GDP")
  s_col <- find_col(nm, cfg$s_cbo_pattern, "primary surplus")
  gdp_col <- find_col(nm, cfg$gdp_pattern, "GDP")
  int_col <- find_col(nm, "^Net interest", "net interest")

  out <- ltbo |>
    dplyr::transmute(
      year = as.integer(.data[[year_col]]),
      b_cbo = as.numeric(.data[[b_col]]) / 100,
      s_cbo = as.numeric(.data[[s_col]]) / 100,
      gdp = as.numeric(.data[[gdp_col]]) * cfg$gdp_multiplier,
      interest = as.numeric(.data[[int_col]]) / 100 * .data$gdp
    )

  if (is.na(cfg$s_tot_pattern)) {
    out$s_tot_cbo <- NA_real_
  } else {
    s_tot_col <- find_col(nm, cfg$s_tot_pattern, "total surplus")
    out$s_tot_cbo <- as.numeric(ltbo[[s_tot_col]]) / 100
  }

  out |>
    dplyr::filter(!is.na(.data$year), .data$year >= 2000L) |>
    dplyr::select("year", "b_cbo", "s_cbo", "s_tot_cbo", "gdp", "interest")
}

read_previous_year_budget <- function(input_dir, vintage) {
  cfg <- cbo_path_config(vintage)
  path <- file.path(input_dir, "cbo", cfg$hist_file)
  assert_files_exist(path)

  debt <- readxl::read_excel(path, sheet = "1. Rev, Outlays, Surplus, Debt", range = "A9:H73")
  debt_col <- find_col(names(debt), "^Debt held by the public", "historical debt level")
  debt <- debt |>
    dplyr::transmute(
      year = suppressWarnings(as.integer(.data[[names(debt)[[1]]]])),
      debt = suppressWarnings(as.numeric(.data[[debt_col]]))
    ) |>
    dplyr::filter(.data$year == cfg$seed_year)

  # Debt-to-GDP is the last (8th) column of A9:H73. Its header is blank in some
  # vintages (the 2024 file), so select it positionally as Stata does (rename H).
  debt_gdp <- readxl::read_excel(path, sheet = "1a. Rev, Outlays, Surplus (GDP)", range = "A9:H73")
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

# Carry baseline levels forward to `obs` model years by replicating the last
# projection row (so b_cbo/rho_cbo/s_cbo/s_tot_cbo/g hold constant), advancing
# year and model_year, and zeroing m_cbo (matching Stata's extension years).
extend_cbo_path <- function(path, obs = CBO_PATH_OBS) {
  n <- nrow(path)
  if (n >= obs) {
    return(path[seq_len(obs), , drop = FALSE])
  }
  k <- obs - n
  extension <- path[rep(n, k), , drop = FALSE]
  extension$model_year <- seq.int(n + 1L, obs)
  extension$year <- path$year[[n]] + seq_len(k)
  extension$m_cbo <- 0
  dplyr::bind_rows(path, extension)
}

# CBO baseline path for a vintage, ready for the simulator. With extend = TRUE
# (the default) it is carried out to `obs` model years; with extend = FALSE it
# returns just the LTBO projection rows (used by the forward table).
build_cbo_path <- function(input_dir, vintage, obs = CBO_PATH_OBS, extend = TRUE) {
  projection <- read_ltbo_projection(input_dir, vintage)
  seed <- read_previous_year_budget(input_dir, vintage)

  combined <- dplyr::bind_rows(seed, projection) |>
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
    dplyr::filter(.data$year > cbo_path_config(vintage)$seed_year) |>
    dplyr::mutate(model_year = dplyr::row_number()) |>
    dplyr::select(
      "model_year", "year", "b_cbo", "rho_cbo", "s_cbo", "s_tot_cbo", "m_cbo", "gdp", "g"
    )

  path <- if (extend) extend_cbo_path(combined, obs = obs) else combined

  assert_unique_key(path, "model_year", sprintf("CBO path %s", vintage))
  assert_unique_key(path, "year", sprintf("CBO path %s", vintage))
  assert_no_missing(path, c("b_cbo", "rho_cbo", "s_cbo", "m_cbo"), sprintf("CBO path %s", vintage))
  if (extend && (nrow(path) != obs || !any(path$model_year == obs - 2L))) {
    abort(sprintf("CBO path %s must have %s rows including the terminal model year", vintage, obs))
  }
  path
}
