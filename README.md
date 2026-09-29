# Deficit Management Scorecard

This repository produces The Budget Lab's Deficit Management Scorecard. The
scorecard grades recent congressional deficit management against the fiscal
response observed from 1984b through 2003b, and it applies a fixed
fiscal-feedback rule to the Congressional Budget Office (CBO) baseline through
2036. The current vintage uses CBO's February 2026 outlook and accompanies the
Budget Lab post *Congress Now Does Far Less About Rising Debt Than It Once
Did* (September 2026).

The method comes from Alan Auerbach and Danny Yagan, *Grading Government: A
Deficit Management Scorecard* (forthcoming in the *National Tax Journal*).
All source inputs are committed to this repository.

## Reproduce the results

The project uses R 4.3.3 and the package versions recorded in `renv.lock`.
From a fresh clone:

```sh
Rscript -e 'renv::restore()'
Rscript scripts/run_pipeline.R
Rscript scripts/run_tests.R
```

The pipeline must run before the tests because the golden tests read the
generated files in `output/`. GitHub Actions runs both steps on every push and
pull request.

## Outputs

The pipeline writes results to `output/` and intermediate data to
`data/processed/`. Its principal outputs are:

| Output | Contents |
|---|---|
| `scorecard.csv` | Actual and predicted deficit reduction, deviations from the benchmark rule (`failure_value`), era membership, and percentiles for every observation |
| `regression_summary.csv` | Benchmark coefficients, HC1 standard errors, sample sizes, and fit statistics for the `debt_ratio` and `projected_surplus` specifications |
| `forward_deficit_reduction_table.csv` | Required deficit reduction under the fixed feedback rule, 2027 to 2036 |
| `forward_table_detail.csv`, `forward_cbo_baseline_path.csv`, `forward_feedback_path.csv` | The annual paths behind the forward table |
| `scorecard_scatter.{pdf,png}` | Scorecard scatter plot |
| `deviation_histogram.{pdf,png}` | Distribution of deviations, histogram |
| `chart_data/` | The three CSV files behind the website charts: `scorecard_scatter.csv`, `deviation_distribution.csv`, and `required_deficit_reduction.csv` |

## Method

The scorecard regresses the deficit reduction Congress enacted in each
half-year on the change in the debt-to-GDP ratio that CBO projected in the
prior report, controlling for the output gap. The benchmark regression uses
the 39 observations from 1984b through 2003b. Each later observation is
scored by its shortfall from the benchmark response and its percentile in
fixed historical comparison pools.

The forward table applies the paper's minimum sustainable annual feedback
coefficient of 0.19 to CBO's long-term baseline. The repository takes that
coefficient as an input and does not estimate it.

The [specification](docs/empirical_scorecard_specification.md) documents the
observation rules, variable definitions, samples, and estimation. The
[update procedure](docs/update_procedure.md) lists what changes with each new
CBO vintage, and [`CHANGELOG.md`](CHANGELOG.md) records the vintages.

## Repository structure

```
R/               Data construction, estimation, figures, and forward table
config/          Input manifest with SHA-256 digests
docs/            Specification and update procedure
inputs/          Source data, with provenance in inputs/README.md
scripts/         Pipeline and test entry points
tests/testthat/  Input-integrity, invariant, and golden tests
```

The `DEFICIT_SCORECARD_INPUT_DIR`, `DEFICIT_SCORECARD_DATA_DIR`, and
`DEFICIT_SCORECARD_OUTPUT_DIR` environment variables set alternate input,
intermediate-data, and output locations. `DEFICIT_SCORECARD_FORWARD_C`
overrides the forward-table coefficient.

## Tests

`tests/testthat/` holds three kinds of test. The input tests check that every
file under `inputs/` matches its recorded digest. The invariant tests check
properties that must hold for any vintage, such as sample sizes and the lag
structure. The golden tests pin the published numbers for the current vintage
so that a code change cannot move them unnoticed; they are updated
deliberately with each CBO release.

## Citation and license

Software citation metadata are in [`CITATION.cff`](CITATION.cff). For the
method and the feedback coefficient, cite Alan Auerbach and Danny Yagan,
*Grading Government: A Deficit Management Scorecard*, forthcoming in the
*National Tax Journal*. The code and documentation are released under the
[MIT License](LICENSE); CBO source files are in the public domain.
