add_period_fields <- function(data) {
  data |>
    dplyr::mutate(
      periodid = as.integer(.data$periodid),
      report_half_char = dplyr::if_else(.data$report_half == 2L, "b", "a"),
      periodidchar = paste0(.data$report_year, .data$report_half_char),
      zlb = (
        dplyr::between(.data$report_year, 2009L, 2016L) |
          dplyr::between(.data$report_year, 2020L, 2022L) |
          (.data$report_year == 2008L & .data$report_half == 2L)
      ),
      zlb = dplyr::if_else(
        (.data$report_year == 2016L & .data$report_half == 2L) |
          (.data$report_year == 2020L & .data$report_half == 1L) |
          (.data$report_year == 2022L & .data$report_half == 2L),
        FALSE,
        .data$zlb
      )
    )
}

prepare_scorecard_data <- function(main_data, alternative_data) {
  periods <- scorecard_periods()

  base_data <- main_data |>
    add_period_fields() |>
    dplyr::filter(
      dplyr::between(.data$periodid, periods$first_era_start, scorecard_latest_periodid()),
      !.data$zlb
    ) |>
    dplyr::mutate(
      surplus = 100 * .data$surplus,
      lag_deltabexp_t0_t4 = 100 * .data$lag_deltabexp_t0_t4,
      lag_outgap_pgdp = 100 * .data$lag_outgap_pgdp,
      base = dplyr::between(
        .data$periodid,
        periods$first_era_start,
        periods$first_era_end
      )
    ) |>
    dplyr::filter(
      !is.na(.data$surplus),
      !is.na(.data$lag_deltabexp_t0_t4),
      !is.na(.data$lag_outgap_pgdp)
    )

  model <- lm(
    surplus ~ lag_deltabexp_t0_t4 + lag_outgap_pgdp,
    data = dplyr::filter(base_data, .data$base)
  )

  alt_2025b <- alternative_data |>
    add_period_fields() |>
    dplyr::filter(.data$periodid == 202502L) |>
    dplyr::slice(1) |>
    dplyr::mutate(
      surplus = 100 * .data$surplus,
      lag_deltabexp_t0_t4 = 100 * .data$lag_deltabexp_t0_t4,
      lag_outgap_pgdp = 100 * .data$lag_outgap_pgdp,
      periodid = 202503L,
      periodidchar = "2025b*",
      base = FALSE,
      zlb = FALSE
    )

  score_data <- dplyr::bind_rows(base_data, alt_2025b)
  score_data$predicted <- as.numeric(predict(model, newdata = score_data))

  score_data <- score_data |>
    dplyr::mutate(
      failure_value = .data$predicted - .data$surplus,
      group = dplyr::case_when(
        dplyr::between(
          .data$periodid,
          periods$first_era_start,
          periods$first_era_end
        ) ~ "first_era",
        dplyr::between(
          .data$periodid,
          periods$later_era_start,
          periods$later_era_end
        ) ~ "later_era",
        .data$periodid == 202501L ~ "highlight_2025a",
        .data$periodid == 202502L ~ "highlight_2025b",
        .data$periodid == 202503L ~ "highlight_2025b_star",
        .data$periodid == 202601L ~ "highlight_2026a",
        TRUE ~ "other"
      )
    )

  first_values <- score_data$failure_value[score_data$group == "first_era"]
  later_values <- score_data$failure_value[score_data$group == "later_era"]
  historical_values <- c(first_values, later_values)
  percentile_against <- function(values, reference) {
    vapply(values, function(x) mean(reference <= x, na.rm = TRUE), numeric(1))
  }

  score_data <- score_data |>
    dplyr::mutate(
      percentile_first_era = percentile_against(.data$failure_value, first_values),
      percentile_later_era = percentile_against(.data$failure_value, later_values),
      percentile_historical = percentile_against(.data$failure_value, historical_values)
    )

  list(data = score_data, model = model)
}

build_empirical_regression_summary <- function(main_data, unified_model) {
  standard_data <- main_data |>
    dplyr::filter(.data$sample_1) |>
    dplyr::transmute(
      surplus = 100 * .data$surplus,
      surplus_exp = 100 * .data$surplus_exp,
      lag_outgap_pgdp = 100 * .data$lag_outgap_pgdp
    )
  standard_model <- lm(
    surplus ~ surplus_exp + lag_outgap_pgdp,
    data = standard_data
  )

  summarize_model <- function(model, specification, feedback_term) {
    robust_se <- sqrt(diag(sandwich::vcovHC(model, type = "HC1")))
    estimates <- stats::coef(model)
    feedback_coefficient <- unname(estimates[[feedback_term]])
    feedback_robust_se <- unname(robust_se[[feedback_term]])
    tibble::tibble(
      specification = specification,
      feedback_term = feedback_term,
      observations = stats::nobs(model),
      feedback_coefficient = feedback_coefficient,
      feedback_robust_se = feedback_robust_se,
      feedback_t_statistic = feedback_coefficient / feedback_robust_se,
      output_gap_coefficient = unname(estimates[["lag_outgap_pgdp"]]),
      output_gap_robust_se = unname(robust_se[["lag_outgap_pgdp"]]),
      r_squared = summary(model)$r.squared
    )
  }

  dplyr::bind_rows(
    summarize_model(standard_model, "standard_projected_surplus", "surplus_exp"),
    summarize_model(unified_model, "unified_prior_report_debt_change", "lag_deltabexp_t0_t4")
  )
}

write_empirical_regression_summary <- function(main_data, unified_model, output_dir) {
  summary_rows <- build_empirical_regression_summary(main_data, unified_model)
  readr::write_csv(summary_rows, file.path(output_dir, "empirical_regression_summary.csv"))
  summary_rows
}

residualize_on_outgap <- function(data, value_col) {
  base_rows <- data$base
  fit <- lm(stats::as.formula(paste(value_col, "~ lag_outgap_pgdp")), data = data[base_rows, ])
  mean_base <- mean(data[[value_col]][base_rows], na.rm = TRUE)
  data[[paste0(value_col, "_resid")]] <- data[[value_col]] -
    as.numeric(predict(fit, newdata = data)) + mean_base
  data
}

make_scatter_plot_data <- function(score_data) {
  plot_data <- score_data |>
    residualize_on_outgap("surplus") |>
    residualize_on_outgap("lag_deltabexp_t0_t4")

  fit <- lm(
    surplus_resid ~ lag_deltabexp_t0_t4_resid,
    data = dplyr::filter(plot_data, .data$base)
  )
  line_data <- tibble::tibble(
    lag_deltabexp_t0_t4_resid = seq(
      min(plot_data$lag_deltabexp_t0_t4_resid, na.rm = TRUE),
      max(plot_data$lag_deltabexp_t0_t4_resid, na.rm = TRUE),
      length.out = 100
    )
  )
  line_data$surplus_resid <- as.numeric(predict(fit, newdata = line_data))

  list(points = plot_data, line = line_data)
}

write_scorecard_outputs <- function(score_data, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  scorecard <- score_data |>
    dplyr::transmute(
      period_label = .data$periodidchar,
      report_year = .data$report_year,
      report_half = .data$report_half,
      actual_deficit_reduction = .data$surplus,
      predicted_deficit_reduction = .data$predicted,
      failure_value = .data$failure_value,
      group = .data$group,
      percentile_first_era = .data$percentile_first_era,
      percentile_later_era = .data$percentile_later_era,
      percentile_historical = .data$percentile_historical
    ) |>
    dplyr::arrange(.data$period_label)

  readr::write_csv(scorecard, file.path(output_dir, "scorecard_unified.csv"))
  scorecard
}

plot_scatter <- function(score_data, output_dir) {
  periods <- scorecard_periods()
  plot_parts <- make_scatter_plot_data(score_data)
  points <- plot_parts$points |>
    dplyr::mutate(
      plot_group = dplyr::case_when(
        dplyr::between(.data$periodid, periods$first_era_start, periods$first_era_end) ~ "1984b-2003b",
        dplyr::between(.data$periodid, periods$later_era_start, periods$later_era_end) ~ "2004a-2024b",
        dplyr::between(.data$periodid, 202501L, 202503L) | .data$periodid == 202601L ~ "Highlighted",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data$plot_group))
  line <- plot_parts$line

  colors <- c("1984b-2003b" = "darkgreen", "2004a-2024b" = "navy", "Highlighted" = "darkorchid3")
  plot <- ggplot2::ggplot(points, ggplot2::aes(x = .data$lag_deltabexp_t0_t4_resid, y = .data$surplus_resid)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25) +
    ggplot2::geom_line(
      data = line,
      ggplot2::aes(x = .data$lag_deltabexp_t0_t4_resid, y = .data$surplus_resid),
      inherit.aes = FALSE,
      color = "darkgreen",
      linewidth = 0.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$periodidchar, color = .data$plot_group),
      size = 2.3,
      hjust = 0,
      vjust = -0.25,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::coord_cartesian(xlim = c(-3, 4), ylim = c(-1.75, 1), clip = "off") +
    ggplot2::labs(
      x = "CBO's projected debt-GDP ratio change as of last period (percentage points of GDP)",
      y = "Congress's deficit reduction this period (% of GDP)"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 20, 10, 18)
    )

  ggplot2::ggsave(
    file.path(output_dir, "residuals_basefit_1984b2026a_nozlb_new_updated_debt.pdf"),
    plot,
    width = 7.5,
    height = 4.5
  )
  ggplot2::ggsave(
    file.path(output_dir, "residuals_basefit_1984b2026a_nozlb_new_updated_debt.png"),
    plot,
    width = 7.5,
    height = 4.5,
    dpi = 200
  )
}

plot_distribution_histogram <- function(score_data, output_dir) {
  binwidth <- 0.25
  hist_data <- score_data |>
    dplyr::filter(.data$group %in% c("first_era", "later_era")) |>
    dplyr::mutate(
      group_label = dplyr::if_else(.data$group == "first_era", "1984b-2003b", "2004a-2024b"),
      bin = floor(.data$failure_value / binwidth) * binwidth
    ) |>
    dplyr::count(.data$group_label, .data$bin) |>
    dplyr::group_by(.data$group_label) |>
    dplyr::mutate(pct = 100 * .data$n / sum(.data$n)) |>
    dplyr::ungroup()

  highlights <- score_data |>
    dplyr::filter(.data$periodidchar %in% c("2025a", "2025b", "2025b*", "2026a")) |>
    dplyr::select("periodidchar", "failure_value")

  label_y <- max(hist_data$pct) * c(0.95, 0.8, 0.65, 0.5)
  highlights <- highlights |>
    dplyr::arrange(.data$failure_value) |>
    dplyr::mutate(label_y = label_y[seq_len(dplyr::n())])

  plot <- ggplot2::ggplot(
    hist_data,
    ggplot2::aes(
      x = .data$bin + binwidth / 2, y = .data$pct,
      fill = .data$group_label, color = .data$group_label
    )
  ) +
    ggplot2::geom_col(position = "identity", alpha = 0.35, width = binwidth, linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = 0, color = "gray40", linewidth = 0.25) +
    ggplot2::geom_vline(
      data = highlights,
      ggplot2::aes(xintercept = .data$failure_value),
      inherit.aes = FALSE,
      color = "darkorchid3",
      linetype = "dashed",
      linewidth = 0.45
    ) +
    ggplot2::geom_text(
      data = highlights,
      ggplot2::aes(x = .data$failure_value, y = .data$label_y, label = .data$periodidchar),
      inherit.aes = FALSE,
      color = "darkorchid3",
      size = 2.7,
      hjust = -0.05
    ) +
    ggplot2::scale_fill_manual(
      values = c("1984b-2003b" = "darkgreen", "2004a-2024b" = "navy"),
      breaks = c("1984b-2003b", "2004a-2024b")
    ) +
    ggplot2::scale_color_manual(
      values = c("1984b-2003b" = "darkgreen", "2004a-2024b" = "navy"),
      breaks = c("1984b-2003b", "2004a-2024b")
    ) +
    ggplot2::coord_cartesian(xlim = c(-1, 2.3), clip = "off") +
    ggplot2::labs(
      x = "Deficit increase relative to pre-2004-based prediction (% of GDP)",
      y = sprintf("%% of observations per %.2f percentage-point bin", binwidth),
      fill = NULL,
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 20, 10, 18)
    )

  ggplot2::ggsave(
    file.path(output_dir, "fig3_distribution_histogram_new_updated_debt.pdf"),
    plot,
    width = 6,
    height = 4
  )
  ggplot2::ggsave(
    file.path(output_dir, "fig3_distribution_histogram_new_updated_debt.png"),
    plot,
    width = 6,
    height = 4,
    dpi = 200
  )
}

plot_distribution <- function(score_data, output_dir) {
  density_data <- function(values, label) {
    dens <- stats::density(values, na.rm = TRUE, kernel = "epanechnikov")
    tibble::tibble(x = dens$x, y = dens$y * 10, group = label)
  }

  plot_data <- dplyr::bind_rows(
    density_data(score_data$failure_value[score_data$group == "first_era"], "1984b-2003b"),
    density_data(score_data$failure_value[score_data$group == "later_era"], "2004a-2024b")
  )
  highlights <- score_data |>
    dplyr::filter(.data$periodidchar %in% c("2025a", "2025b", "2025b*", "2026a")) |>
    dplyr::select("periodidchar", "failure_value")

  label_y <- max(plot_data$y, na.rm = TRUE) * c(0.8, 0.65, 0.5, 0.35)
  highlights <- highlights |>
    dplyr::arrange(.data$failure_value) |>
    dplyr::mutate(label_y = label_y[seq_len(dplyr::n())])

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y, color = .data$group, linetype = .data$group)) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::geom_vline(xintercept = 0, color = "gray40", linewidth = 0.25) +
    ggplot2::geom_vline(
      data = highlights,
      ggplot2::aes(xintercept = .data$failure_value),
      inherit.aes = FALSE,
      color = "darkorchid3",
      linetype = "dashed",
      linewidth = 0.45
    ) +
    ggplot2::geom_text(
      data = highlights,
      ggplot2::aes(x = .data$failure_value, y = .data$label_y, label = .data$periodidchar),
      inherit.aes = FALSE,
      color = "darkorchid3",
      size = 2.7,
      hjust = -0.05
    ) +
    ggplot2::scale_color_manual(
      values = c("1984b-2003b" = "darkgreen", "2004a-2024b" = "navy"),
      breaks = c("1984b-2003b", "2004a-2024b")
    ) +
    ggplot2::scale_linetype_manual(
      values = c("1984b-2003b" = "solid", "2004a-2024b" = "longdash"),
      breaks = c("1984b-2003b", "2004a-2024b")
    ) +
    ggplot2::coord_cartesian(xlim = c(-1, 2.3), clip = "off") +
    ggplot2::labs(
      x = "Deficit increase relative to pre-2004-based prediction (% of GDP)",
      y = "Approximate % of observations per 0.1 percentage-point bin",
      color = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 20, 10, 18)
    )

  ggplot2::ggsave(
    file.path(output_dir, "fig3_distribution_residuals_new_updated_kunits_10_debt.pdf"),
    plot,
    width = 6,
    height = 4
  )
  ggplot2::ggsave(
    file.path(output_dir, "fig3_distribution_residuals_new_updated_kunits_10_debt.png"),
    plot,
    width = 6,
    height = 4,
    dpi = 200
  )
}
