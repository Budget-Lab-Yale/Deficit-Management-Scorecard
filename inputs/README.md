# Input data

The repository includes every source file needed to reproduce the February
2026 scorecard and forward table. `config/input_manifest.csv` records the
SHA-256 digest of each file, and the test suite verifies the digests before it
checks the results.

## Sources

| File | Source |
|---|---|
| `cbo/cbo_revision_2026.csv` | Budget Lab maintained series of CBO baseline revisions and legislative changes, January 1984 through February 2026, compiled from each outlook's "Changes in CBO's Baseline Projections" table |
| `cbo/cbo_revision_2026_tariffs_as_legislation.csv` | Same series with projected customs-duty revenue in the February 2026 outlook reclassified from technical to legislative |
| `cbo/51134-2026-02-Historical-Budget-Data.xlsx` | CBO, [Historical Budget Data](https://www.cbo.gov/data/budget-economic-data), February 2026 |
| `cbo/51119-2026-02-LTBO-Budget.xlsx` | CBO, [The Long-Term Budget Outlook: 2026 to 2056](https://www.cbo.gov/publication/62044), budget projections workbook |
| `historical/Annual_FY_February2026.csv` | CBO, potential GDP by fiscal year, February 2026 |
| `historical/Quarterly_February2026.csv` | CBO, quarterly output gap, February 2026 |

The revision series is the one input that CBO does not publish in this form.
Its rows are transcribed from each outlook and carry the sign and allocation
corrections listed in `docs/empirical_scorecard_specification.md`.

## Updating

`docs/update_procedure.md` lists every file and setting that changes with a
new CBO vintage.
