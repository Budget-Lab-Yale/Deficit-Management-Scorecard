# Single source of truth for the current CBO vintage. Update these fields
# together each release (typically February and August). Filenames are paths
# relative to the input directory (data-raw); latest_report_year/_half identify
# the newest CBO report so the pipeline can assert that data and config agree.
scorecard_vintage <- function() {
  list(
    label = "February 2026",
    latest_report_year = 2026L,
    latest_report_half = 1L, # 1 = winter report (a), 2 = summer report (b)
    pgdp_csv = file.path("historical", "Annual_FY_February2026.csv"),
    outgap_csv = file.path("historical", "Quarterly_February2026.csv"),
    historical_budget_xlsx = file.path("cbo", "51134-2026-02-Historical-Budget-Data.xlsx"),
    omb_gdp_price_xlsx = file.path("historical", "hist10z1_fy2026.xlsx"),
    revision_main_xlsx = "stata_import_file_2026_new.xlsx",
    revision_alt_xlsx = "stata_import_file_2026_techcustomdutiesinleg.xlsx"
  )
}

# periodid of the newest report, e.g. 202601 for 2026a. periodid encodes the
# semiannual report as report_year * 100 + report_half.
scorecard_latest_periodid <- function(vintage = scorecard_vintage()) {
  as.integer(vintage$latest_report_year * 100L + vintage$latest_report_half)
}

# Canonical empirical samples. The first era includes 2003b, as agreed in the
# author correspondence; the later historical comparison begins at 2004a.
scorecard_periods <- function() {
  list(
    first_era_start = 198402L,
    first_era_end = 200302L,
    later_era_start = 200401L,
    later_era_end = 202402L
  )
}

read_pgdp <- function(input_dir) {
  path <- file.path(input_dir, scorecard_vintage()$pgdp_csv)
  assert_files_exist(path)
  data <- readr::read_csv(path, show_col_types = FALSE)
  assert_required_columns(data, c("date", "potential_gdp"), "potential GDP CSV")

  data |>
    dplyr::transmute(
      report_year = as.integer(.data$date),
      pgdp = as.numeric(.data$potential_gdp)
    ) |>
    dplyr::filter(.data$report_year >= 1980)
}

read_outgap <- function(input_dir) {
  path <- file.path(input_dir, scorecard_vintage()$outgap_csv)
  assert_files_exist(path)
  data <- readr::read_csv(path, show_col_types = FALSE)
  assert_required_columns(data, c("date", "output_gap"), "output gap CSV")

  data |>
    dplyr::mutate(
      report_year = as.integer(substr(.data$date, 1, 4)),
      quarter = as.integer(substr(.data$date, 6, 6)),
      report_half = dplyr::case_when(
        .data$quarter == 2L ~ 1L,
        .data$quarter == 4L ~ 2L,
        TRUE ~ NA_integer_
      )
    ) |>
    dplyr::filter(.data$report_year >= 1980, !is.na(.data$report_half)) |>
    dplyr::group_by(.data$report_year, .data$report_half) |>
    dplyr::summarise(outgap = mean(.data$output_gap, na.rm = TRUE), .groups = "drop")
}

read_budget_vars <- function(input_dir, pgdp) {
  path <- file.path(input_dir, scorecard_vintage()$historical_budget_xlsx)
  assert_files_exist(path)
  data <- readxl::read_excel(
    path,
    sheet = "1. Rev, Outlays, Surplus, Debt",
    range = "A9:H72"
  )

  debt_col <- names(data)[grepl("^Debt held by the public", names(data))]
  if (length(debt_col) != 1) {
    abort("Could not identify debt-held-by-public column in CBO historical budget data")
  }
  year_col <- names(data)[1]

  budget <- data |>
    dplyr::transmute(
      report_year = as.integer(.data[[year_col]]),
      surplus_act = as.numeric(.data[["On-budget"]]),
      debt_act = as.numeric(.data[[debt_col]])
    )

  checked_left_join(budget, pgdp, "report_year", "budget vars", "pgdp")
}

read_historical_debt_evolution <- function(input_dir) {
  vintage <- scorecard_vintage()
  cbo_path <- file.path(input_dir, vintage$historical_budget_xlsx)
  hist10_path <- file.path(input_dir, vintage$omb_gdp_price_xlsx)
  assert_files_exist(c(cbo_path, hist10_path))

  debt <- readxl::read_excel(
    cbo_path,
    sheet = "1. Rev, Outlays, Surplus, Debt",
    range = "A9:H73"
  )
  debt_col <- names(debt)[grepl("^Debt held by the public", names(debt))]
  debt_year_col <- names(debt)[1]
  debt <- debt |>
    dplyr::transmute(
      year = as.integer(.data[[debt_year_col]]),
      debt = as.numeric(.data[[debt_col]])
    )

  debt_gdp <- readxl::read_excel(
    cbo_path,
    sheet = "1a. Rev, Outlays, Surplus (GDP)",
    range = "A9:H73"
  )
  debt_gdp_col <- names(debt_gdp)[grepl("^Debt held by the public", names(debt_gdp))]
  debt_gdp_year_col <- names(debt_gdp)[1]
  debt_gdp <- debt_gdp |>
    dplyr::transmute(
      year = as.integer(.data[[debt_gdp_year_col]]),
      debt_gdp = as.numeric(.data[[debt_gdp_col]])
    )

  interest <- readxl::read_excel(
    cbo_path,
    sheet = "3. Outlays",
    range = "A9:F73"
  )
  interest_col <- names(interest)[grepl("^Net interest", names(interest), ignore.case = TRUE)]
  interest_year_col <- names(interest)[1]
  interest <- interest |>
    dplyr::transmute(
      year = as.integer(.data[[interest_year_col]]),
      interest = as.numeric(.data[[interest_col]])
    )

  price_raw <- readxl::read_excel(hist10_path, range = "A6:C91")
  price_cols <- names(price_raw)[1:3]
  price <- price_raw |>
    dplyr::transmute(
      year = suppressWarnings(as.integer(.data[[price_cols[[1]]]])),
      gdp1962 = suppressWarnings(as.numeric(.data[[price_cols[[2]]]])),
      pricelevel2017 = suppressWarnings(as.numeric(.data[[price_cols[[3]]]]))
    ) |>
    dplyr::filter(!is.na(.data$year), dplyr::between(.data$year, 1962L, vintage$latest_report_year))

  base_2012 <- price$pricelevel2017[price$year == 2012]
  if (length(base_2012) != 1 || is.na(base_2012)) {
    abort("Could not identify 2012 price-level base in hist10z1_fy2026.xlsx")
  }

  price <- price |>
    dplyr::mutate(pricelevel = .data$pricelevel2017 / base_2012) |>
    dplyr::select(year, pricelevel)

  debt |>
    checked_left_join(debt_gdp, "year", "debt", "debt_gdp") |>
    checked_left_join(interest, "year", "debt", "interest") |>
    checked_left_join(price, "year", "debt", "price") |>
    dplyr::mutate(
      gdp = .data$debt / (.data$debt_gdp / 100),
      inflation = .data$pricelevel / dplyr::lag(.data$pricelevel) - 1,
      g_nom = .data$gdp / dplyr::lag(.data$gdp) - 1,
      g = .data$g_nom - .data$inflation,
      r_nom = .data$interest / dplyr::lag(.data$debt),
      r = .data$r_nom - .data$inflation,
      rminusg = .data$r - .data$g,
      rho = .data$rminusg / (1 + .data$g_nom)
    ) |>
    dplyr::select(
      year, r, g, rminusg, debt, g_nom, gdp, inflation, interest, rho
    )
}

clean_description_vars <- function(data) {
  data |>
    dplyr::mutate(
      des_temp = trimws(.data$description),
      descript_var = "",
      descript_var = dplyr::case_when(
        .data$des_temp %in% c("Economic", "Economic Changes") ~ "econ_total",
        .data$des_temp %in% c("Technical", "Technical Changes") ~ "tech_total",
        .data$des_temp %in% c(
          "Policy",
          "Legislative",
          "Legislative Changes",
          "Total Legislative"
        ) ~ "leg_total",
        grepl("Revenues", .data$description) |
          (grepl("Revenue", .data$description) & startsWith(.data$description, "Change")) |
          .data$des_temp %in% c("ChangestoRevenueProjections", "ChangesinRevenues") ~ "rev_total",
        (grepl("Outlays", .data$description) & startsWith(.data$description, "Change")) |
          .data$des_temp %in% c("Outlays", "ChangestoOutlayProjections") ~ "out_total",
        .data$des_temp %in% c(
          "Interest",
          "Net interest",
          "Net interest outlays",
          "Netinterestoutlays",
          "Net Interest",
          "Net interes"
        ) ~ "int_total",
        TRUE ~ .data$descript_var
      )
    ) |>
    dplyr::select(-des_temp)
}

read_cbo_revision_workbook <- function(input_dir, filename) {
  path <- file.path(input_dir, "cbo", filename)
  assert_files_exist(path)
  raw <- readxl::read_excel(path, sheet = "CBO Data", range = "A5:AI3744")
  names(raw) <- tolower(names(raw))
  names(raw) <- gsub("\\+", "", names(raw))

  t_cols <- paste0("t", 0:12)
  keep_cols <- c("report_month", "report_year", "change_type", "budget_line", "description", t_cols)
  assert_required_columns(raw, keep_cols, filename)

  raw |>
    dplyr::select(dplyr::all_of(keep_cols)) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(t_cols), as.numeric),
      report_month = as.integer(.data$report_month),
      report_year = as.integer(.data$report_year),
      change_type = trimws(as.character(.data$change_type)),
      budget_line = trimws(as.character(.data$budget_line)),
      description = as.character(.data$description),
      budget_line = dplyr::na_if(.data$budget_line, "NA"),
      change_type = dplyr::na_if(.data$change_type, "NA")
    ) |>
    dplyr::filter(!is.na(.data$description), .data$description != "", !is.na(.data$report_year)) |>
    clean_description_vars()
}

add_report_weights <- function(data) {
  data |>
    dplyr::mutate(
      disc = 1,
      a0 = 0.516129032,
      factor_w = 0.5,
      augshift = 0.5,
      w_t0 = dplyr::if_else(.data$report_half == 1L, .data$a0, .data$a0 * (1 - .data$augshift)),
      w_t1 = dplyr::if_else(.data$report_half == 1L, 0.258064516, 0.387096774),
      w_t2 = dplyr::if_else(.data$report_half == 1L, 0.129032258, 0.193548387),
      w_t3 = dplyr::if_else(.data$report_half == 1L, 0.064516129, 0.096774194),
      w_t4 = 1 - (.data$w_t0 + .data$w_t1 + .data$w_t2 + .data$w_t3),
      w_t5 = 0
    )
}

apply_report_timing_adjustments <- function(data) {
  t_cols <- paste0("t", 0:12)
  data <- data |>
    dplyr::mutate(
      report_half_final = dplyr::case_when(
        .data$report_month %in% c(12L, 1L, 2L) ~ 1L,
        .data$report_month >= 3L & .data$report_month < 12L ~ 2L,
        TRUE ~ NA_integer_
      ),
      report_half_final = dplyr::if_else(
        .data$report_month == 4L & .data$report_year == 2018L,
        1L,
        .data$report_half_final
      ),
      report_half_final = dplyr::if_else(
        .data$report_month == 5L & .data$report_year == 2022L,
        1L,
        .data$report_half_final
      )
    )

  multiplier <- rep(1, nrow(data))
  multiplier[data$report_month == 12L & data$report_year == 1996L & data$budget_line == "rev"] <- -0.5
  multiplier[data$report_month == 1L & data$report_year == 1998L & data$budget_line == "rev"] <- 0.5
  multiplier[data$report_month == 1L & data$report_year == 1998L & data$budget_line == "out"] <- -0.5
  multiplier[data$report_month == 8L & data$report_year == 1998L & data$budget_line == "out"] <- -1
  multiplier[data$report_month == 8L & data$report_year == 1995L &
    data$change_type == "leg" & data$budget_line == "rev"] <- -0.5
  multiplier[data$report_month == 8L & data$report_year == 1995L &
    data$change_type == "leg" & data$budget_line == "out"] <- 0.5

  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(t_cols), ~ .x * multiplier))

  jan_feb <- data |>
    dplyr::group_by(.data$report_year) |>
    dplyr::summarise(
      has_jan = any(.data$report_month == 1L),
      has_feb = any(.data$report_month == 2L),
      .groups = "drop"
    )

  data |>
    checked_left_join(jan_feb, "report_year", "adjusted reports", "jan/feb flags") |>
    dplyr::mutate(
      report_half_final = dplyr::if_else(
        .data$report_month == 2L & .data$has_jan & .data$has_feb,
        2L,
        .data$report_half_final
      )
    ) |>
    dplyr::select(-has_jan, -has_feb)
}

max_or_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    max(x, na.rm = TRUE)
  }
}

build_leg_regression_data <- function(complete_data, pgdp, budget_vars, outgap) {
  t_cols <- paste0("t", 0:12)
  weight_cols <- c("disc", "a0", "factor_w", "augshift", paste0("w_t", 0:5))

  aggregate_data <- complete_data |>
    dplyr::group_by(.data$report_year, .data$report_month, .data$change_type, .data$budget_line) |>
    dplyr::mutate(max_occur = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(!(grepl("total", .data$descript_var) & .data$max_occur > 1L)) |>
    dplyr::group_by(.data$report_month, .data$report_year, .data$change_type, .data$budget_line) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(t_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(.data$report_month), !is.na(.data$budget_line), .data$budget_line != "") |>
    # Reassign the December 1995 outlook before joining annual potential GDP.
    # Joining first would leave that row carrying 1995 PGDP after it becomes a
    # 1996 report, which was the source-code error corrected in the July 28 kit.
    dplyr::mutate(
      report_year = dplyr::if_else(
        .data$report_month == 12L & .data$report_year == 1995L,
        .data$report_year + 1L,
        .data$report_year
      )
    ) |>
    checked_left_join(pgdp, "report_year", "aggregate CBO data", "pgdp") |>
    dplyr::mutate(report_half = dplyr::if_else(.data$report_month <= 6L, 1L, 2L)) |>
    add_report_weights() |>
    apply_report_timing_adjustments()

  mean_cols <- c("pgdp", weight_cols)
  exclude_def <- aggregate_data |>
    dplyr::filter(.data$budget_line != "def") |>
    dplyr::group_by(.data$report_year, .data$report_half_final, .data$change_type, .data$budget_line) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(t_cols), ~ sum(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(mean_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(t_cols),
        ~ dplyr::if_else(.data$report_year == 1997L & .data$budget_line == "rev", -.x, .x)
      )
    )

  deficits <- aggregate_data |>
    dplyr::arrange(.data$report_year, .data$report_month) |>
    dplyr::filter(.data$budget_line == "def") |>
    dplyr::group_by(.data$report_year, .data$report_half_final, .data$change_type, .data$budget_line) |>
    dplyr::summarise(
      # Stata's `collapse (last) t* pgdp-w_t5` takes the latest report's weights
      # (not the mean across reports), so a March+August half carries the pure
      # August weight vector into the deficit-projection variables.
      dplyr::across(dplyr::all_of(t_cols), dplyr::last),
      dplyr::across(dplyr::all_of(mean_cols), dplyr::last),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(t_cols),
        ~ dplyr::if_else(.data$report_year >= 2012L, -.x, .x)
      )
    )

  regression_data <- dplyr::bind_rows(deficits, exclude_def) |>
    dplyr::rename(report_half = report_half_final) |>
    dplyr::mutate(change_type = trimws(.data$change_type)) |>
    dplyr::arrange(.data$report_year, .data$report_half)

  analysis_data <- regression_data |>
    dplyr::filter(.data$change_type %in% c("leg", "baseline")) |>
    dplyr::filter(.data$budget_line %in% c("rev", "out", "def")) |>
    checked_left_join(
      dplyr::select(budget_vars, report_year, surplus_act, debt_act),
      "report_year",
      "regression data",
      "budget vars"
    ) |>
    checked_left_join(outgap, c("report_year", "report_half"), "regression data", "outgap") |>
    dplyr::filter(.data$report_year >= 1983L)

  baseline_def <- analysis_data |>
    dplyr::filter(.data$change_type == "baseline", .data$budget_line == "def")

  pgdp_lookup <- pgdp |>
    dplyr::select(report_year, pgdp)

  weighted_value <- function(report_year, values, weights, sign = 1) {
    out <- numeric(length(report_year))
    for (i in 0:4) {
      future_pgdp <- pgdp_lookup$pgdp[match(report_year + i, pgdp_lookup$report_year)]
      out <- out + sign * (weights[[paste0("w_t", i)]] * values[[paste0("t", i)]]) / future_pgdp
    }
    out
  }

  equal_weighted_value <- function(report_year, values, sign = 1) {
    out <- numeric(length(report_year))
    for (i in 0:4) {
      future_pgdp <- pgdp_lookup$pgdp[match(report_year + i, pgdp_lookup$report_year)]
      out <- out + sign * (0.2 * values[[paste0("t", i)]]) / future_pgdp
    }
    out
  }

  scored_rows <- analysis_data |>
    dplyr::mutate(
      rev = dplyr::if_else(
        .data$budget_line == "rev",
        weighted_value(.data$report_year, dplyr::pick(dplyr::all_of(t_cols)), dplyr::pick(dplyr::all_of(paste0("w_t", 0:4))), 1),
        NA_real_
      ),
      out = dplyr::if_else(
        .data$budget_line == "out",
        weighted_value(.data$report_year, dplyr::pick(dplyr::all_of(t_cols)), dplyr::pick(dplyr::all_of(paste0("w_t", 0:4))), 1),
        NA_real_
      ),
      def = dplyr::if_else(
        .data$budget_line == "def",
        weighted_value(.data$report_year, dplyr::pick(dplyr::all_of(t_cols)), dplyr::pick(dplyr::all_of(paste0("w_t", 0:4))), -1),
        NA_real_
      ),
      rev_ewtd = dplyr::if_else(
        .data$budget_line == "rev",
        equal_weighted_value(.data$report_year, dplyr::pick(dplyr::all_of(t_cols)), 1),
        NA_real_
      ),
      out_ewtd = dplyr::if_else(
        .data$budget_line == "out",
        equal_weighted_value(.data$report_year, dplyr::pick(dplyr::all_of(t_cols)), 1),
        NA_real_
      ),
      def_ewtd = dplyr::if_else(
        .data$budget_line == "def",
        equal_weighted_value(.data$report_year, dplyr::pick(dplyr::all_of(t_cols)), -1),
        NA_real_
      )
    )

  by_period <- scored_rows |>
    dplyr::group_by(.data$report_year, .data$report_half) |>
    dplyr::summarise(
      revenue = max_or_na(.data$rev),
      outlays = max_or_na(.data$out),
      surplus_exp_cur = max_or_na(.data$def),
      revenue_ewtd = max_or_na(.data$rev_ewtd),
      outlays_ewtd = max_or_na(.data$out_ewtd),
      surplus_exp_cur_ewtd = max_or_na(.data$def_ewtd),
      surplus_act = mean(.data$surplus_act, na.rm = TRUE),
      debt_act = mean(.data$debt_act, na.rm = TRUE),
      pgdp = mean(.data$pgdp, na.rm = TRUE),
      outgap = mean(.data$outgap, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$report_year, .data$report_half) |>
    dplyr::mutate(
      lag_key_year = dplyr::if_else(.data$report_half == 1L, .data$report_year - 1L, .data$report_year),
      lag_key_half = dplyr::if_else(.data$report_half == 1L, 2L, 1L)
    )

  surplus_lookup <- by_period |>
    dplyr::select(
      lag_key_year = report_year,
      lag_key_half = report_half,
      surplus_exp = surplus_exp_cur,
      surplus_exp_ewtd = surplus_exp_cur_ewtd
    )

  by_period <- by_period |>
    checked_left_join(surplus_lookup, c("lag_key_year", "lag_key_half"), "period data", "lagged surplus") |>
    dplyr::select(-lag_key_year, -lag_key_half)

  manual_2023 <- by_period |>
    dplyr::filter(.data$report_year == 2022L, .data$report_half == 1L) |>
    dplyr::slice(1)
  if (nrow(manual_2023) == 1) {
    by_period <- by_period |>
      dplyr::mutate(
        surplus_exp = dplyr::if_else(
          .data$report_year == 2023L & .data$report_half == 1L,
          manual_2023$surplus_exp_cur,
          .data$surplus_exp
        ),
        surplus_exp_ewtd = dplyr::if_else(
          .data$report_year == 2023L & .data$report_half == 1L,
          manual_2023$surplus_exp_cur_ewtd,
          .data$surplus_exp_ewtd
        )
      )
  }

  lag_same_half <- by_period |>
    dplyr::transmute(
      report_year = .data$report_year + 1L,
      report_half = .data$report_half,
      lag_surp_pgdp = .data$surplus_act / .data$pgdp,
      lag_debt_pgdp = .data$debt_act / .data$pgdp
    )

  # The output-gap lag must come from the quarterly series, not from report
  # rows: half-years with no fiscal report (2022b) still have to serve as the
  # lag source for the following year's report (2023b).
  lag_outgap_lookup <- outgap |>
    dplyr::transmute(
      report_year = .data$report_year + 1L,
      report_half = .data$report_half,
      lag_outgap_pgdp = -0.01 * .data$outgap
    )

  leg_full <- by_period |>
    dplyr::mutate(
      surp_pgdp = .data$surplus_act / .data$pgdp,
      debt_pgdp = .data$debt_act / .data$pgdp,
      outgap_pgdp = .data$outgap * -0.01,
      surplus = .data$revenue - .data$outlays,
      surplus_ewtd = .data$revenue_ewtd - .data$outlays_ewtd
    ) |>
    checked_left_join(lag_same_half, c("report_year", "report_half"), "period data", "lagged actuals") |>
    checked_left_join(lag_outgap_lookup, c("report_year", "report_half"), "period data", "lagged outgap") |>
    dplyr::select(
      report_year, report_half, outgap, surplus, surplus_exp, surplus_ewtd,
      revenue, outlays, lag_outgap_pgdp, surplus_exp_ewtd, surplus_exp_cur
    ) |>
    dplyr::arrange(.data$report_year, .data$report_half) |>
    dplyr::group_by(.data$report_year, .data$report_half) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  list(full = leg_full, baseline_def = baseline_def)
}

build_merged_budget_data <- function(leg_full, baseline_def, historical, pgdp) {
  hist_to_merge <- historical |>
    dplyr::mutate(
      b = .data$debt / .data$gdp,
      rho = .data$rminusg / (1 + .data$g_nom)
    ) |>
    dplyr::select(
      year, r, g, rminusg, debt, g_nom, gdp, inflation, interest, b, rho
    )

  merged <- leg_full |>
    dplyr::mutate(year = .data$report_year) |>
    checked_left_join(hist_to_merge, "year", "leg regression data", "historical debt") |>
    dplyr::filter(.data$year >= 1980L) |>
    dplyr::mutate(report_year = dplyr::coalesce(.data$report_year, .data$year))

  baseline_keep <- baseline_def |>
    dplyr::select(
      report_year, report_half, change_type, budget_line,
      dplyr::all_of(paste0("t", 0:12)),
      dplyr::all_of(paste0("w_t", 0:5)),
      disc, a0, factor_w, augshift
    )

  merged <- merged |>
    checked_left_join(baseline_keep, c("report_year", "report_half"), "merged budget data", "baseline def") |>
    dplyr::filter(.data$report_year >= 1983L, .data$budget_line == "def") |>
    dplyr::arrange(.data$report_year, .data$report_half) |>
    checked_left_join(pgdp, "report_year", "merged budget data", "pgdp")

  yearly <- pgdp |>
    dplyr::rename(year = report_year) |>
    dplyr::left_join(
      hist_to_merge |>
        dplyr::select(year, b),
      by = "year"
    ) |>
    dplyr::arrange(.data$year) |>
    dplyr::mutate(
      pgdp_lead_0 = dplyr::lead(.data$pgdp, 0),
      pgdp_lead_1 = dplyr::lead(.data$pgdp, 1),
      pgdp_lead_2 = dplyr::lead(.data$pgdp, 2),
      pgdp_lead_3 = dplyr::lead(.data$pgdp, 3),
      pgdp_lead_4 = dplyr::lead(.data$pgdp, 4),
      pgdp_lead_5 = dplyr::lead(.data$pgdp, 5),
      pgdp_lag_1 = dplyr::lag(.data$pgdp, 1),
      pgdp_lag_2 = dplyr::lag(.data$pgdp, 2),
      b_lag_1 = dplyr::lag(.data$b, 1),
      b_lag_2 = dplyr::lag(.data$b, 2)
    ) |>
    dplyr::select(-pgdp, -b) |>
    dplyr::rename(report_year = year)

  merged <- checked_left_join(merged, yearly, "report_year", "merged budget data", "leads/lags")

  for (i in 0:5) {
    merged[[paste0("surplus_t", i)]] <- -merged[[paste0("t", i)]]
    merged[[paste0("surplus_t", i, "_wtd")]] <- merged[[paste0("w_t", i)]] * merged[[paste0("surplus_t", i)]]
    merged[[paste0("surplus_t", i, "_wtd_norm")]] <-
      merged[[paste0("surplus_t", i, "_wtd")]] / merged[[paste0("pgdp_lead_", i)]]
    merged[[paste0("surplus_t", i, "_norm")]] <-
      merged[[paste0("surplus_t", i)]] / merged[[paste0("pgdp_lead_", i)]]
  }

  merged
}

build_dataset_for_regression <- function(merged_budget) {
  dataset <- merged_budget |>
    dplyr::filter(.data$year >= 1983L) |>
    dplyr::mutate(
      gnomexp_t0 = .data$pgdp_lead_0 / .data$pgdp_lag_1 - 1,
      gnomexp_t1 = .data$pgdp_lead_1 / .data$pgdp_lead_0 - 1,
      gnomexp_t2 = .data$pgdp_lead_2 / .data$pgdp_lead_1 - 1,
      gnomexp_t3 = .data$pgdp_lead_3 / .data$pgdp_lead_2 - 1,
      gnomexp_t4 = .data$pgdp_lead_4 / .data$pgdp_lead_3 - 1
    )

  dataset$deltabexp_t0 <- -dataset$surplus_t0 / dataset$pgdp_lead_0 -
    dataset$gnomexp_t0 / (1 + dataset$gnomexp_t0) * dataset$b_lag_1
  dataset$bexp_t0 <- dataset$b_lag_1 + dataset$deltabexp_t0

  for (i in 1:4) {
    previous <- i - 1
    dataset[[paste0("deltabexp_t", i)]] <-
      -dataset[[paste0("surplus_t", i)]] / dataset[[paste0("pgdp_lead_", i)]] -
      dataset[[paste0("gnomexp_t", i)]] / (1 + dataset[[paste0("gnomexp_t", i)]]) *
        dataset[[paste0("bexp_t", previous)]]
    dataset[[paste0("bexp_t", i)]] <- dataset[[paste0("bexp_t", previous)]] +
      dataset[[paste0("deltabexp_t", i)]]
  }

  periods <- scorecard_periods()

  dataset |>
    dplyr::mutate(
      deltabexp_t0_t4 = (.data$bexp_t4 - .data$b_lag_1) / 5,
      lag_outgap_pgdp2 = .data$lag_outgap_pgdp^2,
      lag_outgap_pgdp3 = .data$lag_outgap_pgdp^3,
      lag_outgap_pgdp4 = .data$lag_outgap_pgdp^4,
      periodid = as.integer(.data$report_year * 100L + .data$report_half)
    ) |>
    dplyr::arrange(.data$report_year, .data$report_half) |>
    dplyr::mutate(
      # Congress's action in a report interval is graded against the outlook
      # available at the prior CBO report, not the report that already embeds
      # that action.
      lag_deltabexp_t0_t4 = dplyr::lag(.data$deltabexp_t0_t4),
      sample_1 = dplyr::between(
        .data$periodid,
        periods$first_era_start,
        periods$first_era_end
      ),
      sample_2 = .data$periodid >= periods$later_era_start &
        .data$periodid <= scorecard_latest_periodid() &
        .data$periodid != 202002L
    )
}

build_all_datasets <- function(input_dir) {
  vintage <- scorecard_vintage()
  pgdp <- read_pgdp(input_dir)
  outgap <- read_outgap(input_dir)
  budget_vars <- read_budget_vars(input_dir, pgdp)
  historical <- read_historical_debt_evolution(input_dir)

  # The 5-year forward weighting needs potential GDP out to report_year + 4 for
  # the latest report; a short PGDP series would otherwise inject silent NAs.
  horizon_year <- vintage$latest_report_year + 4L
  if (max(pgdp$report_year, na.rm = TRUE) < horizon_year) {
    abort(sprintf(
      "Potential-GDP series ends at %d but the %d weighting horizon needs %d. Refresh %s.",
      max(pgdp$report_year, na.rm = TRUE), vintage$latest_report_year, horizon_year, vintage$pgdp_csv
    ))
  }

  build_one <- function(filename) {
    complete <- read_cbo_revision_workbook(input_dir, filename)
    leg <- build_leg_regression_data(complete, pgdp, budget_vars, outgap)
    merged <- build_merged_budget_data(leg$full, leg$baseline_def, historical, pgdp)
    build_dataset_for_regression(merged)
  }

  datasets <- list(
    main = build_one(vintage$revision_main_xlsx),
    alternative = build_one(vintage$revision_alt_xlsx)
  )

  # Tripwire: the newest period in the data must match the configured vintage.
  # Catches both a stale/truncated read (data behind config) and an appended CBO
  # release that nobody recorded in scorecard_vintage() (data ahead of config).
  data_latest <- max(datasets$main$periodid, na.rm = TRUE)
  expected_latest <- scorecard_latest_periodid(vintage)
  if (data_latest != expected_latest) {
    abort(sprintf(
      paste0(
        "Latest period in data is %d but scorecard_vintage() expects %d. ",
        "If you appended a new CBO release, bump scorecard_vintage(); ",
        "if the read truncated, check the Excel ranges/filenames."
      ),
      data_latest, expected_latest
    ))
  }

  datasets
}
