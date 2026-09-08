# Input data

The repository includes the source files needed to reproduce the February 2026
scorecard and forward table.

`config/input_manifest.csv` records the expected SHA-256 digest for each file.
The test suite verifies those digests before it checks the model results.

## Sources

The Congressional Budget Office (CBO) files are from its February 2026
[budget and economic data](https://www.cbo.gov/data) and
[long-term budget outlook](https://www.cbo.gov/publication/62044). The Office
of Management and Budget (OMB) price-level series is from the Fiscal Year 2026
[Historical Tables](https://www.whitehouse.gov/omb/information-resources/budget/historical-tables/).

The two `cbo_revision_2026*.csv` files contain the maintained scorecard series
of CBO baseline revisions and legislative changes through February 2026. They
combine historical CBO releases and apply the timing allocations described in
`docs/empirical_scorecard_specification.md`. The alternative file differs only
in its treatment of projected customs-duty revenue for the 2025b observation.

## Updating the inputs

For a new scorecard vintage, update the two revision CSV files and replace the
current CBO and OMB source files. Then update `scorecard_vintage()` in
`R/build_inputs.R`, the manifest paths and digests, and the golden values only
after reviewing the resulting changes.
