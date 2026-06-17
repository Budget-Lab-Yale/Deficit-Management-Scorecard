# Forward-looking c-star simulation entrypoint.
#
#   Rscript scripts/run_cstar_simulation.R
#
# Computes the calibration inputs, builds the 2024/2025/2026 CBO baseline paths,
# scans the paper's c-grid for c* (first grid value keeping >=95% of terminal
# debt paths below 250% of GDP), and writes the c-star forward table.
#
# Note: 2026 c* sits on the 95% boundary (true share ~0.951 at c=0.18). The
# published paper reports c*=0.19; the one-grid-step difference is Monte Carlo
# noise at 1000 reps, not a calibration difference (see docs plan). The fixed-c
# forward table from run_pipeline.R (c=0.19) remains the paper-aligned output;
# this script's forward_table_cstar.csv uses the simulation's computed c*.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(tibble)
})

source("R/assertions.R")
source("R/build_inputs.R")
source("R/forward_table.R")
source("R/cbo_paths.R")
source("R/simulation_inputs.R")
source("R/cstar_simulation.R")

input_dir <- Sys.getenv("DEFICIT_SCORECARD_INPUT_DIR", "data-raw")
data_dir <- Sys.getenv("DEFICIT_SCORECARD_DATA_DIR", file.path("data", "processed"))
output_dir <- Sys.getenv("DEFICIT_SCORECARD_OUTPUT_DIR", "output")
reps_scale <- as.numeric(Sys.getenv("DEFICIT_SCORECARD_CSTAR_REPS_SCALE", "1"))
if (is.na(reps_scale) || reps_scale <= 0) {
  abort("DEFICIT_SCORECARD_CSTAR_REPS_SCALE must be a positive number")
}

result <- write_cstar_outputs(input_dir, data_dir, output_dir, reps_scale = reps_scale)

si <- result$simulation_inputs
cat(sprintf(
  "Calibration: s_u_hat = %.5f (AR n=%d), e_s_size = %.2f\n",
  si$s_u_hat_risk[[1]], si$ar_n[[1]], si$e_s_size[[1]]
))
cat("c-star scan:\n")
print(as.data.frame(result$summary))
cat(sprintf(
  "\nWrote c-star outputs to %s (2026 computed c* = %.2f; paper value = 0.19).\n",
  output_dir, result$c_star_2026
))
