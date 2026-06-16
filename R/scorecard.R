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
  base_data <- main_data |>
    add_period_fields() |>
    dplyr::filter(dplyr::between(.data$periodid, 198402L, 202601L), .data$periodid != 202002L, !.data$zlb) |>
    dplyr::mutate(
      surplus = 100 * .data$surplus,
      deltabexp_t0_t4 = 100 * .data$deltabexp_t0_t4,
      lag_outgap_pgdp = 100 * .data$lag_outgap_pgdp,
      base = dplyr::between(.data$periodid, 198402L, 200301L)
    ) |>
    dplyr::filter(!is.na(.data$surplus), !is.na(.data$deltabexp_t0_t4), !is.na(.data$lag_outgap_pgdp))

  model <- lm(surplus ~ deltabexp_t0_t4 + lag_outgap_pgdp, data = dplyr::filter(base_data, .data$base))

  alt_2025b <- alternative_data |>
    add_period_fields() |>
    dplyr::filter(.data$periodid == 202502L) |>
    dplyr::slice(1) |>
    dplyr::mutate(
      surplus = 100 * .data$surplus,
      deltabexp_t0_t4 = 100 * .data$deltabexp_t0_t4,
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
      residual = .data$surplus - .data$predicted,
      failure_value = .data$predicted - .data$surplus,
      group = dplyr::case_when(
        dplyr::between(.data$periodid, 198402L, 200301L) ~ "pre_2004",
        dplyr::between(.data$periodid, 200402L, 202402L) ~ "post_2004",
        .data$periodid == 202501L ~ "highlight_2025a",
        .data$periodid == 202502L ~ "highlight_2025b",
        .data$periodid == 202503L ~ "highlight_2025b_star",
        .data$periodid == 202601L ~ "highlight_2026a",
        TRUE ~ "other"
      ),
      percentile_all = stats::ecdf(.data$failure_value)(.data$failure_value),
      percentile_post_2004 = {
        post_values <- .data$failure_value[.data$group == "post_2004"]
        vapply(.data$failure_value, function(x) mean(post_values <= x, na.rm = TRUE), numeric(1))
      }
    )

  list(data = score_data, model = model)
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
    residualize_on_outgap("deltabexp_t0_t4")

  fit <- lm(surplus_resid ~ deltabexp_t0_t4_resid, data = dplyr::filter(plot_data, .data$base))
  line_data <- tibble::tibble(
    deltabexp_t0_t4_resid = seq(
      min(plot_data$deltabexp_t0_t4_resid, na.rm = TRUE),
      max(plot_data$deltabexp_t0_t4_resid, na.rm = TRUE),
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
      percentile_all = .data$percentile_all,
      percentile_post_2004 = .data$percentile_post_2004
    ) |>
    dplyr::arrange(.data$period_label)

  readr::write_csv(scorecard, file.path(output_dir, "scorecard_unified.csv"))
  scorecard
}

plot_scatter <- function(score_data, output_dir) {
  plot_parts <- make_scatter_plot_data(score_data)
  points <- plot_parts$points |>
    dplyr::mutate(
      plot_group = dplyr::case_when(
        .data$group == "pre_2004" ~ "Pre-2004",
        .data$group == "post_2004" ~ "Post-2004",
        TRUE ~ "Highlighted"
      )
    )
  line <- plot_parts$line

  colors <- c("Pre-2004" = "darkgreen", "Post-2004" = "navy", "Highlighted" = "red3")
  plot <- ggplot2::ggplot(points, ggplot2::aes(x = .data$deltabexp_t0_t4_resid, y = .data$surplus_resid)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25) +
    ggplot2::geom_line(
      data = line,
      ggplot2::aes(x = .data$deltabexp_t0_t4_resid, y = .data$surplus_resid),
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
    ggplot2::coord_cartesian(xlim = c(-2, 4), ylim = c(-1.75, 1), clip = "off") +
    ggplot2::labs(
      x = "CBO's projected debt-GDP ratio change as of last period (pp of GDP)",
      y = "Congress's deficit reduction this period (% of GDP)"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 20, 10, 10)
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

plot_distribution <- function(score_data, output_dir) {
  density_data <- function(values, label) {
    dens <- stats::density(values, na.rm = TRUE)
    tibble::tibble(x = dens$x, y = dens$y * 10, group = label)
  }

  plot_data <- dplyr::bind_rows(
    density_data(score_data$failure_value[score_data$group == "pre_2004"], "Pre-2004"),
    density_data(score_data$failure_value[score_data$group == "post_2004"], "Post-2004")
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
      color = "red3",
      linetype = "dashed",
      linewidth = 0.45
    ) +
    ggplot2::geom_text(
      data = highlights,
      ggplot2::aes(x = .data$failure_value, y = .data$label_y, label = .data$periodidchar),
      inherit.aes = FALSE,
      color = "red3",
      size = 2.7,
      hjust = -0.05
    ) +
    ggplot2::scale_color_manual(values = c("Pre-2004" = "darkgreen", "Post-2004" = "navy")) +
    ggplot2::scale_linetype_manual(values = c("Pre-2004" = "solid", "Post-2004" = "longdash")) +
    ggplot2::coord_cartesian(xlim = c(-1, 2.3), clip = "off") +
    ggplot2::labs(
      x = "Deficit increase relative to pre-2004-based prediction (% of GDP)",
      y = "% of observations in smoothed x-axis bins of width 0.1",
      color = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 20, 10, 10)
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
