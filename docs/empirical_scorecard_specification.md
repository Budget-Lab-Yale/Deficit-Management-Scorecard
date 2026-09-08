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
legislative revenue and outlay changes are aggregated within each constructed
half-year.

The scorecard reports a positive `failure_value` when actual deficit reduction
falls short of the amount predicted by the benchmark rule. Percentiles are the
share of a fixed historical pool with a failure value less than or equal to the
highlighted observation's value.

## Maintained specification

The scorecard includes 2003b in the benchmark era and begins the later era at
2004a. The projected-debt-change regressor uses the prior CBO report, and the
December 1995 report is assigned to 1996 before potential GDP is merged. These
choices define the publication series and are covered by the golden tests.

The figures use deterministic fitted-line rendering, purple highlights, and a
histogram alongside the scatter and density charts. These presentation choices
do not alter the regressions or scorecard values.

## Generated results

`scripts/run_pipeline.R` writes:

- `output/empirical_regression_summary.csv`, containing the standard and
  unified benchmark coefficients, HC1 standard errors, t-statistics, sample
  sizes, output-gap coefficients, and R-squared values;
- `output/scorecard_unified.csv`, containing actual and predicted deficit
  reduction, failure values, era membership, and fixed-pool percentiles; and
- the unified scatter, density, and histogram figures in PDF and PNG formats.

The forward-looking table is a separate module. It applies the paper's fixed
annual fiscal-feedback coefficient of 0.19 to the February 2026 CBO baseline.
This repository does not re-estimate that coefficient.
