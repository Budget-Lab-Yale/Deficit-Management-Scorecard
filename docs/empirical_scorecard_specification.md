# Empirical scorecard specification

The Budget Lab R pipeline is the maintained implementation of the empirical
deficit scorecard. It uses the February 2026 CBO vintage and the unified
debt-change specification.

## Samples and timing

- The benchmark era is 1984b through 2003b, inclusive.
- The later historical comparison era is 2004a through 2024b, inclusive.
- Highlighted observations begin in 2025 and do not enter either historical
  percentile reference pool.
- Congress's action in each report interval is regressed on the projected
  debt-GDP change from the prior available CBO report.
- The output-gap control uses the corresponding half-year from the prior year.
  This rule supplies 2023b with the 2022b quarterly output gap even though
  there was no 2022b fiscal-report observation.
- Zero-lower-bound observations follow the paper's exclusions. The figures
  also omit 2020b.

The first-era unified regression has 39 observations. With heteroskedasticity-
consistent HC1 standard errors, its projected-debt-change coefficient is
0.176282 (standard error 0.040109) and its R-squared is 0.478105. The standard
projected-surplus regression has a coefficient of -0.143669 (standard error
0.032438) and an R-squared of 0.457948.

## Data construction

The December 1995 report is assigned to report year 1996 before annual
potential GDP is merged. Its projection calculations therefore use 1996
potential GDP rather than carrying forward the 1995 value.

When more than one report falls in the same constructed half-year, the deficit
projection uses the latest report's horizon values and weights. Actual
legislative revenue and outlay changes retain the aggregation rules in the
replication kit.

The scorecard reports a positive `failure_value` when actual deficit reduction
falls short of the amount predicted by the benchmark rule. Percentiles are the
share of a fixed historical pool with a failure value less than or equal to the
highlighted observation's value.

## Relationship to the Stata replication kit

The top-level `final_repkit/` directory is an unchanged extraction of the
authors' July 28, 2026 delivery. Its ZIP archive has SHA-256 digest
`940ea98598d3c31f51e1803ca034b904b485cbf4711333ff4769567b1d6773b2`.
The Stata source and generated files are retained as author-supplied reference
artifacts and are not maintained by this project.

The R implementation intentionally differs from that delivery in one
substantive sample decision: the author repkit ends its benchmark era at 2003a,
while the R scorecard includes 2003b in the benchmark era, consistent with the
subsequent author correspondence. The later era begins at 2004a in R.

The July 28 Stata appendix and scorecard routines and the R pipeline use the
prior-report unified regressor and correct the December 1995 potential-GDP
merge order. The repkit's separate extended-table routine retains a
current-report version of the unified regressor; the maintained R scorecard
does not reproduce that specification. For shared observations and variables,
automated tests compare the R dataset with the supplied Stata `.dta` file. No
native Stata execution is part of the validation workflow because the project
does not have access to a Stata license.

The R figures also use deterministic fitted-line rendering, purple highlights,
and an additional histogram. These presentation choices do not alter the
regressions or scorecard values.

The paper transcriptions under `docs/deficit_management_scorecard_paper*` are
snapshots of the author manuscript and are not statements of the maintained R
specification. They remain unchanged for the separate article-revision step.

## Generated results

`scripts/run_pipeline.R` writes:

- `output/empirical_regression_summary.csv`, containing the standard and
  unified benchmark coefficients, HC1 standard errors, t-statistics, sample
  sizes, output-gap coefficients, and R-squared values;
- `output/scorecard_unified.csv`, containing actual and predicted deficit
  reduction, failure values, era membership, and fixed-pool percentiles; and
- the unified scatter, density, and histogram figures in PDF and PNG formats.

The forward-looking table and c-star simulation are separate modules. Changes
to the empirical benchmark do not change their inputs or results.
