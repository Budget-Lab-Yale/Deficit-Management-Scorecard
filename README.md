# Deficit Scorecard

This repository contains the R implementation of the empirical Budget Lab deficit
scorecard. The scorecard is based on Danny Yagan and Alan Auerbach's forthcoming
National Tax Journal paper and is designed to be updated as new CBO budget
reports arrive.

The first deliverable is the empirical "unified" scorecard used for website
publication. It rebuilds updated versions of the Appendix Figure A-1 debt-GDP
change scatter and scorecard distribution from the replication kit. The pipeline
does not port the forward-looking simulation stack or the deficit-reduction table.

## Repository Layout

- `R/`: reusable R functions for validation, input cleaning, dataset construction,
  scorecard calculations, and plots.
- `scripts/bootstrap_inputs.R`: copies required inputs from the replication kit
  into ignored local folders.
- `scripts/run_pipeline.R`: rebuilds processed artifacts and scorecard outputs.
- `config/input_manifest.csv`: tracked list of required source files.
- `tests/testthat/`: validation and parity checks.

Raw inputs, processed data, and generated outputs are intentionally ignored by
Git. Do not commit files under `data-raw/`, `data/`, or `output/`.

## Workflow

From this repository:

```sh
Rscript scripts/bootstrap_inputs.R --repkit ../final_repkit/final_repkit
Rscript scripts/run_pipeline.R
Rscript -e "testthat::test_dir('tests/testthat')"
```

Optional environment variables:

- `DEFICIT_SCORECARD_INPUT_DIR`: input directory, default `data-raw`.
- `DEFICIT_SCORECARD_OUTPUT_DIR`: output directory, default `output`.

Generated outputs include:

- `data/processed/dataset_for_regression_26.rds`
- `data/processed/dataset_for_regression_26__techcustomdutiesinleg.rds`
- `output/scorecard_unified.csv`
- `output/residuals_basefit_1984b2026a_nozlb_new_updated_debt.pdf`
- `output/fig3_distribution_residuals_new_updated_kunits_10_debt.pdf`

## Validation

The tests check input availability, observation coverage through `2026a`, the
pre-2004 unified coefficient, the presence of highlighted 2025-2026 periods, and
the ordering of the current largest deviation. For visual parity, compare the
generated PDFs against the replication-kit Stata outputs:

- `../final_repkit/final_repkit/out/prod/residuals_basefit_1984b2026a_nozlb_new_updated_debt.pdf`
- `../final_repkit/final_repkit/out/prod/fig3_distribution_residuals_new_updated_kunits_10_debt.pdf`

## Non-Goals

This pass does not implement the web UI, Monte Carlo simulation stack, or
forward-looking policy tables. It focuses only on the empirical unified
scorecard needed for the initial website release.

