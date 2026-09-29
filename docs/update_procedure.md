# Updating the scorecard for a new CBO vintage

CBO publishes a budget outlook roughly twice a year, in winter and in summer.
Each release adds one observation to the scorecard and replaces the baseline
behind the forward table. This procedure lists every file that changes.

## 1. Extend the revision series

The two files under `inputs/cbo/` named `cbo_revision_<year>*.csv` are the
maintained series of CBO baseline revisions. For each new outlook, append rows
transcribed from its table of changes since the previous baseline, with the
change for each of the next thirteen fiscal years in columns `t0` through
`t12`.

- `report_month` and `report_year` are the outlook's release month and year.
  The month determines the half-year observation (see the specification).
- The pipeline reads rows with `change_type` equal to `leg` (legislative
  changes) or `baseline` (projected deficit levels, from which the regressor
  is built) and `budget_line` equal to `rev`, `out`, or `def`. Economic,
  technical, and interest rows are kept in the file for completeness but do
  not enter the scorecard.
- The alternative file differs from the main file only where projected
  customs-duty revenue is reclassified from technical to legislative.
- When a release spans more than one half-year, allocate the changes across
  observations as the specification describes for 2025b and 2026a.

## 2. Replace the CBO source files

Download the new vintage of each file and place it under `inputs/`:

| File | Source | Used for |
|---|---|---|
| `historical/Annual_FY_<Month><Year>.csv` | CBO budget and economic data, potential GDP (fiscal year) | Potential GDP scaling |
| `historical/Quarterly_<Month><Year>.csv` | CBO budget and economic data, quarterly output gap | Output-gap control |
| `cbo/51134-<year>-<mm>-Historical-Budget-Data.xlsx` | CBO historical budget data | Historical debt and debt-to-GDP ratio, forward-table seed year |
| `cbo/51119-<year>-<mm>-LTBO-Budget.xlsx` | CBO long-term budget outlook | Forward-table baseline |

The potential-GDP series must extend at least four years past the new report
year; the pipeline stops with a message if it does not.

## 3. Update the configuration

`scorecard_vintage()` in `R/build_inputs.R` holds every vintage-specific value:
the label, the latest report year and half, and six input filenames (potential
GDP, output gap, historical budget workbook, long-term outlook workbook, and
the main and alternative revision files). The pipeline stops if the newest
observation in the data does not match the configured year and half.
`cbo_path_config()` in `R/cbo_paths.R` reads the two workbook filenames from
`scorecard_vintage()` and takes the seed year, the last completed fiscal year
before the projection, as the year before the latest report year. The forward
table's ten years also follow from the latest report year, through
`forward_table_years()` and `forward_table_windows()`. No other file names a
vintage.

Three Excel ranges are hard-coded, and the historical-budget ranges gain a row
each year:

| File | Sheet | Range | Code |
|---|---|---|---|
| Historical budget | `1. Rev, Outlays, Surplus, Debt` | `A9:H73` | `read_historical_debt()` in `R/build_inputs.R`; `read_previous_year_budget()` in `R/cbo_paths.R` |
| Historical budget | `1a. Rev, Outlays, Surplus (GDP)` | `A9:H73` | the same two functions |
| Long-term outlook | `Supplemental Table 1` | `A9:P40` | `cbo_path_config()` in `R/cbo_paths.R` |

Each range starts at the header row 9. The historical-budget ranges end at
row 73, the row for the last completed fiscal year (2025 in the February 2026
workbook), and the long-term outlook range ends at row 40, the row for 2056.
Check the sheet layouts in the new workbooks, since CBO occasionally moves
columns or renames headers.

The highlighted observations are named explicitly in `R/scorecard.R`: the
`group` labels in `prepare_scorecard_data()`, the period filter in
`plot_scatter()`, and the label list in `plot_distribution_histogram()`. The
alternative-series observation (`2025b*`) is also named there. Add the new observation to each list, and when
an observation ages out of the highlighted set, extend `later_era_end` in
`scorecard_periods()` so it joins the comparison era. The `test-scorecard.R`
and `test-golden.R` label lists change with it.

## 4. Refresh the manifest

Recompute the SHA-256 digest of every file under `inputs/` and update
`config/input_manifest.csv`:

```sh
sha256sum inputs/cbo/* inputs/historical/*
```

## 5. Run, review, and pin

```sh
Rscript scripts/run_pipeline.R
Rscript scripts/run_tests.R
```

The golden tests in `tests/testthat/test-golden.R` will fail because the
published values have changed. Compare the new `output/scorecard.csv`
and `output/regression_summary.csv` with the previous vintage,
confirm that the benchmark coefficient and the historical observations moved
only as much as the revised CBO history explains, and then replace the pinned
values. Record the vintage and its headline numbers in `CHANGELOG.md`.

## 6. Chart data

The pipeline writes the data behind the website charts to `output/chart_data/`
(`scorecard_scatter.csv`, `deviation_distribution.csv`, and
`required_deficit_reduction.csv`).
