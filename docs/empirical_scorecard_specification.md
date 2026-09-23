# Empirical scorecard specification

This document describes how the R pipeline builds the Deficit Management
Scorecard from CBO data. It is written for two readers: a maintainer preparing
the next CBO vintage, and a reader checking the numbers in a Budget Lab post
against the code. The method follows Auerbach and Yagan, *Grading Government:
A Deficit Management Scorecard* (forthcoming, *National Tax Journal*), using
the debt-ratio ("unified") specification of their Appendix Table A-1.

## Observations

Each observation is a half-year of congressional action, identified by a
report year and a half: `1996a` is the winter observation and `1996b` the
summer observation. Observations are built from CBO's semiannual budget
outlooks, which attribute the change in projected deficits since the prior
outlook to legislation, economic revisions, and technical revisions.

### Assigning reports to observations

| Rule | Detail |
|---|---|
| Month rule | Reports released December through February form the winter observation (`a`); reports released March through November form the summer observation (`b`). |
| December reports | A December report belongs to the following year's winter observation and takes that year's potential GDP. December 1995 is the only December report in the series and forms `1996a`. |
| January and February in one year | When a year has both a January and a February report, the February report joins the summer observation. This applies to 1984, 1989, 1990, and 1991. |
| Exceptions | The April 2018 and May 2022 reports form the winter observations `2018a` and `2022a`. |
| Combining reports | When several reports fall in one observation, their legislative revenue and outlay changes are summed. The deficit projection uses the latest report's horizon values and weights. |

### Sign and allocation corrections

The maintained revision series carries a small number of row-level
corrections that align the published CBO tables with a common sign convention
and half-year allocation. The pipeline applies them in
`apply_report_timing_adjustments` and `build_leg_regression_data`:

| Rows | Multiplier |
|---|---|
| August 1995 legislative revenue | −0.5 |
| August 1995 legislative outlays | +0.5 |
| December 1995 revenue (report year 1996) | −0.5 |
| January 1998 revenue | +0.5 |
| January 1998 outlays | −0.5 |
| August 1998 outlays | −1 |
| 1997 revenue, after aggregation to the observation | sign reversed |
| Deficit rows from 2012 onward | sign reversed |

### The 2025b and 2026a observations

CBO issued no outlook between January 2025 and February 2026. Following the
authors, the February 2026 changes are split into two observations: the One
Big Beautiful Bill Act and other revenue and mandatory-spending legislation go
to `2025b`, most discretionary-spending changes go to `2026a`, and remaining
economic and technical revisions are divided evenly. CBO records projected
tariff revenue from executive action as a technical change, so `2025b`
excludes it. The alternative series `cbo_revision_2026_tariffs_as_legislation.csv`
treats that revenue as legislated and produces the `2025b*` observation.

## Variables

**Outcome.** The legislated change in the primary surplus over the five fiscal
years beginning with the observation, each year's change divided by that
year's potential GDP and weighted toward the near term. Winter weights for
years t through t+4 are 16/31, 8/31, 4/31, 2/31, and 1/31. Summer observations
halve the year-t weight and scale the remaining weights up so that the five
weights still sum to one. When reports are combined within an observation,
the weights are averaged across the combined reports. Revenue changes enter
positively and outlay changes negatively.

**Regressor.** CBO's projected average annual change in the debt-to-GDP ratio
over the same five years, as of the prior observation. The projected path is
built from the prior report's baseline deficit projections and potential GDP
growth, starting from the debt ratio at the end of the year before the report.
Each observation's regressor is the value computed from the immediately
preceding observation in the series, so Congress's action in an interval is
graded against the outlook that preceded it.

**Output-gap control.** The output gap from the same half of the prior year,
taken from CBO's quarterly series (second quarter for winter observations,
fourth quarter for summer observations) and entered so that a positive value
means output below potential. Because the lag comes from the quarterly
series, `2023b` receives the 2022 fourth-quarter value although there is no
`2022b` observation.

**Standard specification.** The regression summary also reports the paper's
main specification, which replaces the projected debt-ratio change with the
prior observation's projected five-year primary surplus. Because there is no
`2022b` observation, `2023a` takes its projected surplus from the `2022a`
report.

## Samples

| Sample | Periods | Observations |
|---|---|---|
| Benchmark era | 1984b through 2003b | 39 |
| Comparison era | 2004a through 2024b, excluding constrained monetary policy | 21 |
| Highlighted | 2025a, 2025b, 2025b*, 2026a | 4 |

Periods of constrained monetary policy, 2008b through 2016a and 2020b through
2022a, are excluded from the regression, the scorecard, the figures, and the
percentile pools.

The paper estimates its benchmark on 1984b through 2003a, 38 observations,
and reports a coefficient of 0.144. The scorecard adds 2003b, the last
observation before the comparison era begins.

## Estimation and scoring

The benchmark regression is ordinary least squares of the outcome on the
projected debt-ratio change and the lagged output gap over the benchmark era,
with all three variables in percent of potential GDP. Standard errors are
heteroskedasticity-consistent (HC1). On the February 2026 vintage the
coefficient on the projected debt-ratio change is 0.1763 (standard error
0.0401) with an R-squared of 0.478. The standard specification gives −0.1437
(0.0324) with an R-squared of 0.458.

Every observation, including `2025b*`, receives a predicted deficit reduction
from the benchmark regression at its own regressor and output gap. The
`failure_value` is the predicted deficit reduction minus the actual
legislated deficit reduction, so a positive value means Congress enacted less
deficit reduction than the benchmark rule predicts.

Percentiles are the share of a fixed pool with a failure value less than or
equal to the observation's value. Three pools are reported: the benchmark
era, the comparison era, and both together. Highlighted observations are
never part of a pool.

## Figures

The scatter plot shows both variables net of the output gap, using the
benchmark-era regression of each variable on the lagged output gap and adding
back the benchmark-era mean. The fitted line is the benchmark-era regression
of the adjusted outcome on the adjusted regressor. The density and histogram
figures show the distribution of failure values by era with the highlighted
observations marked.

## Forward table

The forward table is a deterministic calculation that applies a fixed annual
fiscal-feedback coefficient to CBO's long-term baseline. It uses the paper's
minimum sustainable coefficient of 0.19, which the repository takes as an
input and does not estimate. The `DEFICIT_SCORECARD_FORWARD_C` environment
variable overrides it.

The baseline path combines the long-term budget outlook's Supplemental
Table 1 with the prior year's debt and debt-to-GDP ratio from the historical
budget data. The excess of the interest rate over the growth rate follows the
paper's autoregressive process with persistence 0.576 and a response of
0.00848 to the deviation of the debt ratio from baseline. Each year's required
primary-deficit reduction is the coefficient times the debt-ratio change that
would otherwise occur, and each reduction is permanent.

## Generated results

`scripts/run_pipeline.R` writes to `output/`:

- `empirical_regression_summary.csv`: both specifications' coefficients, HC1
  standard errors, t-statistics, sample sizes, output-gap coefficients, and
  R-squared values;
- `scorecard_unified.csv`: actual and predicted deficit reduction, failure
  values, era membership, and the three percentiles for every observation;
- the scatter, density, and histogram figures in PDF and PNG;
- `forward_deficit_reduction_table.csv`, `forward_table_detail.csv`,
  `forward_cbo_paths_2026.csv`, `forward_deterministic_feedback_path.csv`,
  and `panel_b_deficit_reduction.tex`.

Intermediate datasets are saved to `data/processed/`.
