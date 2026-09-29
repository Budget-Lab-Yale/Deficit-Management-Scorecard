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
source("R/cbo_paths.R")
source("R/forward_table.R")
source("R/chart_data.R")

input_dir <- Sys.getenv("DEFICIT_SCORECARD_INPUT_DIR", "inputs")
output_dir <- Sys.getenv("DEFICIT_SCORECARD_OUTPUT_DIR", "output")
processed_dir <- Sys.getenv("DEFICIT_SCORECARD_DATA_DIR", file.path("data", "processed"))

manifest <- readr::read_csv(file.path("config", "input_manifest.csv"), show_col_types = FALSE)
required_inputs <- file.path(input_dir, manifest$path)
assert_files_exist(required_inputs)

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

datasets <- build_all_datasets(input_dir)

assert_no_missing(
  datasets$main |>
    dplyr::filter(
      .data$periodid >= 198402L,
      .data$periodid <= scorecard_latest_periodid()
    ),
  c("surplus", "lag_deltabexp_t0_t4", "lag_outgap_pgdp"),
  "main regression dataset"
)

saveRDS(datasets$main, file.path(processed_dir, "regression_data.rds"))
saveRDS(datasets$alternative, file.path(processed_dir, "regression_data_tariffs_as_legislation.rds"))

score <- prepare_scorecard_data(datasets$main, datasets$alternative)
saveRDS(score, file.path(processed_dir, "scorecard.rds"))

scorecard <- write_scorecard_outputs(score$data, output_dir)
regression_summary <- write_regression_summary(datasets$main, score$model, output_dir)
plot_scatter(score$data, output_dir)
plot_distribution_histogram(score$data, output_dir)

forward_c_value <- as.numeric(Sys.getenv("DEFICIT_SCORECARD_FORWARD_C", "0.19"))
if (is.na(forward_c_value) || forward_c_value <= 0) {
  abort("DEFICIT_SCORECARD_FORWARD_C must be a positive number")
}
forward <- build_forward_table_data(input_dir, c_value = forward_c_value)
saveRDS(forward, file.path(processed_dir, "forward_table.rds"))
forward_rows <- write_forward_table_outputs(forward, output_dir)
write_chart_data(score$data, forward_rows, output_dir)

cat(sprintf("Wrote %s scorecard rows to %s\n", nrow(scorecard), file.path(output_dir, "scorecard.csv")))
cat(sprintf("Wrote %s regression summary rows to %s\n", nrow(regression_summary), file.path(output_dir, "regression_summary.csv")))
cat(sprintf(
  "Wrote %s forward table rows to %s with feedback coefficient c = %.2f\n",
  nrow(forward_rows), file.path(output_dir, "forward_deficit_reduction_table.csv"), forward_c_value
))
cat(sprintf("Wrote chart data to %s\n", file.path(output_dir, "chart_data")))
