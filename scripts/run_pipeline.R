suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(readxl)
  library(tidyr)
})

source("R/assertions.R")
source("R/build_inputs.R")
source("R/scorecard.R")
source("R/forward_table.R")
source("R/cbo_paths.R")
source("R/simulation_inputs.R")
source("R/cstar_simulation.R")

input_dir <- Sys.getenv("DEFICIT_SCORECARD_INPUT_DIR", "data-raw")
output_dir <- Sys.getenv("DEFICIT_SCORECARD_OUTPUT_DIR", "output")
processed_dir <- file.path("data", "processed")

manifest <- readr::read_csv(file.path("config", "input_manifest.csv"), show_col_types = FALSE)
required_inputs <- file.path(input_dir, manifest$dest_path)
assert_files_exist(required_inputs)

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

datasets <- build_all_datasets(input_dir)

assert_no_missing(
  datasets$main |>
    dplyr::filter(
      .data$periodid >= 198402L,
      .data$periodid <= scorecard_latest_periodid(),
      .data$periodid != 202002L
    ),
  c("surplus", "lag_deltabexp_t0_t4", "lag_outgap_pgdp"),
  "main regression dataset"
)

saveRDS(datasets$main, file.path(processed_dir, "dataset_for_regression_26.rds"))
saveRDS(datasets$alternative, file.path(processed_dir, "dataset_for_regression_26__techcustomdutiesinleg.rds"))

score <- prepare_scorecard_data(datasets$main, datasets$alternative)
saveRDS(score, file.path(processed_dir, "scorecard_unified.rds"))

scorecard <- write_scorecard_outputs(score$data, output_dir)
regression_summary <- write_empirical_regression_summary(datasets$main, score$model, output_dir)
plot_scatter(score$data, output_dir)
plot_distribution(score$data, output_dir)
plot_distribution_histogram(score$data, output_dir)

forward_c_value <- as.numeric(Sys.getenv("DEFICIT_SCORECARD_FORWARD_C", "0.19"))
if (is.na(forward_c_value) || forward_c_value <= 0) {
  abort("DEFICIT_SCORECARD_FORWARD_C must be a positive number")
}
forward <- build_forward_table_data(input_dir, c_value = forward_c_value)
saveRDS(forward, file.path(processed_dir, "forward_table.rds"))
forward_rows <- write_forward_table_outputs(forward, output_dir)

cat(sprintf("Wrote %s scorecard rows to %s\n", nrow(scorecard), file.path(output_dir, "scorecard_unified.csv")))
cat(sprintf("Wrote %s empirical regression rows to %s\n", nrow(regression_summary), file.path(output_dir, "empirical_regression_summary.csv")))
cat(sprintf("Wrote %s forward table rows to %s with c = %.2f (published, paper-aligned)\n", nrow(forward_rows), file.path(output_dir, "forward_deficit_reduction_table.csv"), forward_c_value))

# Forward-looking c-star simulation (diagnostic). The published forward table
# above stays at the paper value (c = 0.19) for continuity; this step computes
# c* from the stochastic simulation and writes it alongside as a diagnostic.
# It needs the extra 2024/2025 LTBO vintages, so a failure here (e.g. those
# inputs not bootstrapped) must not sink the published scorecard/forward outputs
# already written above — warn and continue instead.
cstar_reps_scale <- as.numeric(Sys.getenv("DEFICIT_SCORECARD_CSTAR_REPS_SCALE", "1"))
if (is.na(cstar_reps_scale) || cstar_reps_scale <= 0) {
  abort("DEFICIT_SCORECARD_CSTAR_REPS_SCALE must be a positive number")
}
tryCatch(
  {
    cstar <- write_cstar_outputs(input_dir, processed_dir, output_dir, reps_scale = cstar_reps_scale)
    cat(sprintf(
      "Wrote c-star diagnostics (%d computed c* = %.2f; paper/published value = %.2f)\n",
      cstar$published_vintage, cstar$c_star_published, forward_c_value
    ))
  },
  error = function(e) {
    cat(sprintf("WARNING: c-star diagnostic skipped: %s\n", conditionMessage(e)))
    cat("  (Published scorecard and forward table above are unaffected.)\n")
  }
)
