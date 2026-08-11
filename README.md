# Deficit Scorecard

This repository contains the R implementation of the Budget Lab deficit
scorecard. The scorecard is based on Danny Yagan and Alan Auerbach's forthcoming
National Tax Journal paper and is designed to be updated as new CBO budget
reports arrive.

The pipeline rebuilds the empirical "unified" scorecard used for website
publication. The benchmark uses the prior report's projected debt-GDP change
over 1984b-2003b, with 2004a-2024b as the later historical comparison era. See
[`docs/empirical_scorecard_specification.md`](docs/empirical_scorecard_specification.md)
for the full specification and the documented divergence from the unchanged
author Stata repkit. The pipeline produces updated versions of the Appendix
Figure A-1 debt-GDP-change scatter and scorecard distribution. It
also builds the forward-looking deficit-reduction table by applying a fixed
fiscal-feedback parameter from the paper (`c = 0.19`) to the February 2026 CBO
baseline. The forward-looking Monte Carlo simulation that estimates `c*` is now
ported as well (see "The c\* simulation" below); it runs as a diagnostic and
does not replace the published `c = 0.19` table.

## Repository Layout

- `R/`: reusable R functions for validation, input cleaning, dataset construction,
  scorecard calculations, plots, CBO-path construction, and the c\* simulation.
- `R/build_inputs.R`: the data pipeline. `scorecard_vintage()` at the top is the
  single source of truth for the current CBO release (filenames + the latest
  report period) — **update it each release** (see "Updating for a new release").
- `scripts/bootstrap_inputs.R`: copies required inputs from the replication kit
  into ignored local folders.
- `scripts/run_pipeline.R`: rebuilds processed artifacts, scorecard outputs, the
  fixed-`c` forward table, and the c\* diagnostics.
- `scripts/run_cstar_simulation.R`: standalone entrypoint for just the c\*
  simulation (same outputs as the c\* step in `run_pipeline.R`).
- `config/input_manifest.csv`: tracked list of required source files.
- `tests/testthat/`: validation, parity, and golden-output regression checks.

Raw inputs, processed data, and generated outputs are intentionally ignored by
Git. Do not commit files under `data-raw/`, `data/`, or `output/`.

## Requirements

R (developed on 4.3) with CRAN packages: `dplyr`, `tidyr`, `tibble`, `readr`,
`readxl`, `ggplot2`, `sandwich`, `testthat`. The optional Stata-data parity
test uses `haven`. There is no dependency lockfile; if you need
to reproduce a specific set of published numbers later, record the package
versions (`sessionInfo()`) alongside the outputs.

## Workflow

From this repository:

```sh
Rscript scripts/bootstrap_inputs.R --repkit ../final_repkit
Rscript scripts/run_pipeline.R
Rscript -e "testthat::test_dir('tests/testthat')"
```

Optional environment variables:

- `DEFICIT_SCORECARD_INPUT_DIR`: input directory, default `data-raw`.
- `DEFICIT_SCORECARD_DATA_DIR`: processed-data directory, default `data/processed`.
- `DEFICIT_SCORECARD_OUTPUT_DIR`: output directory, default `output`.
- `DEFICIT_SCORECARD_FORWARD_C`: published forward-table fiscal feedback
  parameter, default `0.19`.
- `DEFICIT_SCORECARD_CSTAR_REPS_SCALE`: multiplier on the c\* simulation rep
  counts, default `1` (e.g. `0.1` for a fast smoke run).

Generated outputs include:

- `data/processed/dataset_for_regression_26.rds`
- `data/processed/dataset_for_regression_26__techcustomdutiesinleg.rds`
- `output/scorecard_unified.csv`
- `output/empirical_regression_summary.csv`
- `output/forward_deficit_reduction_table.csv` (published, `c = 0.19`)
- `output/forward_table_detail.csv`
- `output/panel_b_deficit_reduction.tex`
- `output/residuals_basefit_1984b2026a_nozlb_new_updated_debt.pdf`
- `output/fig3_distribution_residuals_new_updated_kunits_10_debt.pdf`

c\* simulation outputs (also produced by `run_cstar_simulation.R`):

- `output/ltbo_cstar_values.csv` (computed c\* by LTBO vintage)
- `output/ltbo_cstar_grid_scan.csv` (share-below-250% by c, the diagnostic)
- `output/ltbo_pdchange_terminal_values.csv`
- `output/forward_table_cstar.csv` (forward table using the computed c\*)
- `data/processed/simulation_inputs.rds`, `data/processed/cbo_paths_{2024,2025,2026}.rds`

## The c\* simulation

`c*` is the minimum fiscal-feedback coefficient that keeps the debt-to-GDP ratio
below 250% over 100 years in at least 95% of simulated economies. The R port
(`R/cstar_simulation.R`, `R/cbo_paths.R`, `R/simulation_inputs.R`) reproduces the
replication kit's Stata simulation and runs in seconds.

**Boundary caveat (important).** For the 2026 LTBO the computed `c*` lands at
**0.18**, one grid step below the paper's published **0.19**. This is a genuine
95%-boundary result: at high rep counts the share-below-250% at `c = 0.18` is
~0.951, just above the threshold, and at the published 1,000 reps it straddles
0.95 by seed. It is **not** a calibration difference (the paper's own raw-CSV
`s_u` matches the reconstructed value) — it is Monte Carlo noise at the boundary.
The published forward table therefore keeps `c = 0.19` for continuity with the
paper; the computed `c*` is written as a diagnostic only.

## Updating for a new release

When a new CBO budget outlook arrives:

1. Append the new rows to the maintained `stata_import_file_*.xlsx` and refresh
   the CBO/historical inputs in the replication kit.
2. Re-run `scripts/bootstrap_inputs.R` (update `config/input_manifest.csv` if
   filenames changed).
3. Update `scorecard_vintage()` at the top of `R/build_inputs.R` (filenames and
   `latest_report_year`/`latest_report_half`) and add the new period to the
   highlight/group logic in `R/scorecard.R`.
4. Re-run `scripts/run_pipeline.R`. The pipeline asserts that the latest period
   in the data matches `scorecard_vintage()`, so a mismatch (stale read, or data
   appended without updating the config) fails loudly rather than silently
   dropping the newest period.
5. Run the tests; update the golden reference values in
   `tests/testthat/test-golden.R` deliberately if the new release legitimately
   changes the published numbers.

## Validation

The tests check input availability, observation coverage through `2026a`, the
39-observation 1984b-2003b benchmark, the prior-report unified coefficient,
the 2003b/2004a era boundary, the 1996 potential-GDP value, the restored 2023b
control, the presence of highlighted 2025-2026 periods, and parity with the
author-supplied Stata dataset where the specifications overlap. They also check
the ordering of the current largest deviation and the c\* simulation (calibration,
path construction, recursion, and the 2026 boundary result), and golden
reference values for the published scorecard and forward-table numbers. Several
tests read pipeline outputs, so run `scripts/run_pipeline.R` first (they skip
cleanly if outputs are absent). For visual parity, compare the generated PDFs
against the replication-kit Stata outputs:

- `../final_repkit/out/prod/residuals_basefit_1984b2026a_nozlb_new_updated_debt.pdf`
- `../final_repkit/out/prod/fig3_distribution_residuals_new_updated_kunits_10_debt.pdf`

## Non-Goals

This pass does not implement the web UI. Exact draw-by-draw RNG parity with
Stata is also out of scope (the c\* simulation matches Stata in formulas,
inputs, sample sizes, and grid, but not the random stream — see the boundary
caveat above).
