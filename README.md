# Deficit Management Scorecard

This repository produces The Budget Lab's February 2026 Deficit Management
Scorecard. The scorecard grades recent congressional deficit management against
the fiscal response observed from 1984b through 2003b. It also applies the
paper's fixed fiscal-feedback rule to the Congressional Budget Office (CBO)
baseline through 2036.

The method comes from Alan Auerbach and Danny Yagan, *Grading Government: A
Deficit Management Scorecard* (forthcoming in the *National Tax Journal*).
The source inputs needed to reproduce the scorecard findings are committed to
this repository.

## Reproduce the results

The project uses R 4.3.3 and package versions recorded in `renv.lock`. From a
fresh clone, run:

```sh
Rscript -e 'renv::restore()'
Rscript scripts/run_pipeline.R
Rscript scripts/run_tests.R
```

The pipeline must run before the tests because the golden-result checks read
generated files from `output/`. GitHub Actions runs the same workflow on every
push and pull request.

## Outputs

The pipeline writes tables and figures to `output/` and intermediate data to
`data/processed/`. Its principal outputs are:

| Output | Contents |
|---|---|
| `scorecard_unified.csv` | Actual and predicted deficit reduction, shortfalls, and historical percentiles |
| `empirical_regression_summary.csv` | Benchmark coefficients, robust standard errors, sample sizes, and fit statistics |
| `forward_deficit_reduction_table.csv` | Fixed-feedback forward table |
| `residuals_basefit_1984b2026a_nozlb_new_updated_debt.{pdf,png}` | Scorecard scatter plot |
| `fig3_distribution_residuals_new_updated_kunits_10_debt.{pdf,png}` | Distribution plot |
| `fig3_distribution_histogram_new_updated_debt.{pdf,png}` | Distribution histogram |

## Method

The empirical scorecard regresses the deficit reduction enacted after each CBO
report on the projected debt-to-GDP change in the prior report, controlling for
the output gap. The benchmark contains 39 observations from 1984b through
2003b. Each later observation is scored by its shortfall from the benchmark
response and its percentile in fixed historical comparison pools.

The forward table is a deterministic calculation based on CBO's February 2026
long-term budget outlook. It uses the paper's annual feedback coefficient of
0.19 as an input; this repository does not estimate that coefficient.

The [empirical specification](docs/empirical_scorecard_specification.md)
documents the sample definitions, zero-lower-bound exclusions, and timing
conventions.

## Data and repository structure

The source files under `inputs/` include CBO budget data, CBO potential GDP and
output-gap series, an Office of Management and Budget price-level series, and
the maintained scorecard series of CBO baseline revisions and legislative
changes. [Input documentation](inputs/README.md) gives their provenance and
update procedure. `config/input_manifest.csv` records the SHA-256 digest of
each source file.

```
R/               Data construction, estimation, figures, and forward table
config/          Input manifest and checksums
docs/            Empirical specification
inputs/          Source data
scripts/         Pipeline and test entrypoints
tests/testthat/  Integrity, specification, and golden-result tests
```

The default paths reproduce the publication from the repository root. The
`DEFICIT_SCORECARD_INPUT_DIR`, `DEFICIT_SCORECARD_DATA_DIR`, and
`DEFICIT_SCORECARD_OUTPUT_DIR` environment variables can set alternate input,
intermediate-data, and output locations.

## Citation

Software citation metadata are in [`CITATION.cff`](CITATION.cff). For the
method and annual feedback coefficient, cite Alan Auerbach and Danny Yagan,
*Grading Government: A Deficit Management Scorecard*, forthcoming in the
*National Tax Journal*.
