# Data behind the website charts. Each CSV has a fixed column order and is
# written unquoted with blanks for missing values. Requires R/scorecard.R,
# R/forward_table.R, and the vintage helpers in R/build_inputs.R.

# Comparison groups shown in the charts. The highlighted observations form the
# "recent" group; observations in other groups are not charted.
era_from_group <- function(group) {
  era <- rep(NA_character_, length(group))
  era[group == "first_era"] <- "pre_2004"
  era[group == "later_era"] <- "post_2004"
  era[grepl("^highlight_", group)] <- "recent"
  era
}

charted_observations <- function(score_data) {
  score_data$era <- era_from_group(score_data$group)
  score_data <- score_data[!is.na(score_data$era), , drop = FALSE]
  score_data[order(score_data$periodid), , drop = FALSE]
}

build_scatter_chart_data <- function(score_data) {
  scatter <- make_scatter_plot_data(score_data)
  points <- scatter$points
  points$pre_2004_rule_adjusted_pct_gdp <- as.numeric(stats::predict(scatter$fit, newdata = points))
  points <- charted_observations(points)

  data.frame(
    period = points$periodidchar,
    projected_debt_gdp_change_adjusted_pp = points$lag_deltabexp_t0_t4_resid,
    deficit_reduction_adjusted_pct_gdp = points$surplus_resid,
    pre_2004_rule_adjusted_pct_gdp = points$pre_2004_rule_adjusted_pct_gdp,
    projected_debt_gdp_change_pp = points$lag_deltabexp_t0_t4,
    output_gap_pct_gdp = points$lag_outgap_pgdp,
    actual_deficit_reduction_pct_gdp = points$surplus,
    pre_2004_rule_prediction_pct_gdp = points$predicted,
    deviation_pct_gdp = points$failure_value,
    era = points$era,
    check.names = FALSE
  )
}

# Each observation in the two historical eras carries an equal share of 100
# percent within its era. Recent observations carry no weight.
build_distribution_chart_data <- function(score_data) {
  rows <- charted_observations(score_data)
  era_counts <- table(rows$era)
  rows$weight_pct <- ifelse(rows$era == "recent", 0, 100 / as.numeric(era_counts[rows$era]))

  data.frame(
    period = rows$periodidchar,
    deviation_pct_gdp = rows$failure_value,
    era = rows$era,
    weight_pct = rows$weight_pct,
    actual_deficit_reduction_pct_gdp = rows$surplus,
    pre_2004_rule_prediction_pct_gdp = rows$predicted,
    check.names = FALSE
  )
}

# The forward table in long format, one row per panel, line item, and column.
build_required_reduction_chart_data <- function(forward_rows) {
  panel_group <- c(A = "baseline", B = "reduction", C = "resulting")
  line_item_row <- c(
    "Primary deficit" = "primary_deficit",
    "Deficit" = "deficit",
    "Debt/GDP" = "debt_gdp"
  )
  annual_columns <- as.character(forward_table_years())
  windows <- forward_table_windows()
  average_keys <- vapply(
    windows,
    function(window) sprintf("avg_%d_%02d", min(window), max(window) %% 100L),
    character(1)
  )

  columns <- data.frame(
    tier = rep(c("annual", "average"), c(length(annual_columns), length(windows))),
    col = c(annual_columns, unname(average_keys)),
    source = c(annual_columns, names(windows)),
    stringsAsFactors = FALSE
  )

  panel_letter <- sub("^Panel ([ABC])\\..*$", "\\1", forward_rows$panel)
  long <- lapply(seq_len(nrow(forward_rows)), function(i) {
    data.frame(
      group = unname(panel_group[panel_letter[[i]]]),
      row = unname(line_item_row[forward_rows$line_item[[i]]]),
      tier = columns$tier,
      col = columns$col,
      value = as.numeric(unlist(forward_rows[i, columns$source])),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, long)
}

write_chart_csv <- function(x, path) {
  old_options <- options(digits = 17)
  on.exit(options(old_options), add = TRUE)
  utils::write.table(x, path, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE, na = "")
}

write_chart_data <- function(score_data, forward_rows, output_dir) {
  chart_dir <- file.path(output_dir, "chart_data")
  dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)

  write_chart_csv(build_scatter_chart_data(score_data), file.path(chart_dir, "scorecard_scatter.csv"))
  write_chart_csv(build_distribution_chart_data(score_data), file.path(chart_dir, "deviation_distribution.csv"))
  write_chart_csv(build_required_reduction_chart_data(forward_rows), file.path(chart_dir, "required_deficit_reduction.csv"))

  invisible(chart_dir)
}
