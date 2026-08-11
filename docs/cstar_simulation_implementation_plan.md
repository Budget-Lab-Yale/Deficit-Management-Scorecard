# R c-star Simulation Implementation Plan

## Objective

Implement the forward-looking `c*` estimation pipeline in R so the scorecard can compute the paper's deficit feedback value from raw/bootstrap inputs instead of relying on a manually chosen fixed `c` value. The implementation should preserve the existing empirical scorecard and fixed-`c` table while adding a reproducible simulation path that estimates:

- `s_u_hat`: the standard deviation of excess interest-rate shocks. **This is the only data-dependent calibration input** — everything else below is either a frozen constant or read from inside each LTBO workbook.
- `e_s_size`: the size of the Poisson debt shock. **In the production Stata masters this is a hardcoded constant `0.25`, not re-estimated per run** (see "Calibration inputs: what is actually used" below). The `m1` formula that derives it from potential GDP is vestigial.
- CBO long-term baseline paths for the 2024, 2025, and 2026 LTBO vintages. Each LTBO report is self-contained for path construction: nominal GDP and debt-to-GDP come from the report's own workbook, plus the prior year's debt from that vintage's Historical-Budget-Data file. No external/up-to-date potential-GDP series is needed to use a given vintage.
- Risk simulations over the Stata c-grid by LTBO vintage.
- `c*`: the first grid value where at least 95 percent of terminal simulated debt paths are below 250 percent of GDP.
- Deterministic deficit-reduction paths and the forward-looking table using the estimated `c*`.

The R implementation should be source-compatible with the local replication kit but should not commit raw inputs, processed data, simulation draws, or generated outputs.

## Current State

The repo is a lightweight R project, not a formal package. It already builds the empirical unified scorecard and a forward-looking deterministic table using a fixed `c` supplied by `DEFICIT_SCORECARD_FORWARD_C` with default `0.19`.

Relevant current files:

- `scripts/bootstrap_inputs.R`: copies required source files from `../final_repkit` into ignored `data-raw/`.
- `scripts/run_pipeline.R`: rebuilds processed data, appendix-style figures, scorecard CSV, and the fixed-`c` forward table.
- `R/forward_table.R`: builds the current 2026 deterministic forward table using an assumed `c`.
- `config/input_manifest.csv`: tracked source manifest; needs to be extended for the c-star inputs.
- `tests/testthat/`: existing empirical and fixed-table validation tests.

Do not replace the fixed-`c` pathway immediately. Add the c-star path beside it, validate it, then switch table generation to use estimated `c*` only after parity is acceptable.

## Stata Files Read and Confirmed

These Stata files define the source behavior to port:

- `../final_repkit/do/m1_sims_inputs.do`
  - Builds AR inputs and shock calibration.
  - Uses `dta/historicaldebtevolution_cleaned_updated_26.dta`.
  - Estimates `rho = rminusg / (1 + g_nom)` and `b = debt / gdp`.
  - Estimates the risk regression `reg rho L.rho L.b if year >= 1972, r`.
  - Stores the RMSE from that regression as `s_u_hat_risk`. **This is the input we actually need from `m1`.**
  - Computes `e_s_size = ((b_2020 - b_2019) + (b_2014 - b_2007)) / 2`, where `b = debt / potential_gdp` after merging `Annual_FY_June2024.csv`. **This computed value is written to `sims_inputs` (matrix column 7) but is never read by any simulation** — the masters override it with a hardcoded `0.25`. Treat the formula as documentation only.
  - The `restricted` indicator and the constrained `rho_lhs` regression in `m1` feed only the AR table / plot shading, not `s_u_hat_risk`. Ignore them for calibration.
- `../final_repkit/do/m0_prepare_cbo_data_scorecard24.do`
  - Builds `dta/cbo_paths_2024.dta`.
  - Reads `raw/cbo/51119-2024-03-LTBO-budget.xlsx`.
  - Reads previous-year 2023 debt and debt/GDP from `raw/cbo/51134-2024-02-Historical-Budget-Data.xlsx`.
  - Keeps `year`, `b_cbo`, `rho_cbo`, `s_cbo`, `m_cbo`, and `gdp`.
- `../final_repkit/do/m0_prepare_cbo_data_scorecard25.do`
  - Builds `dta/cbo_paths25.dta`.
  - Reads `raw/cbo/51119-2025-03-LTBO-budget.xlsx`.
  - Reads previous-year 2024 debt and debt/GDP from `raw/cbo/51134-2025-01-Historical-Budget-Data.xlsx`.
  - Keeps `s_tot_cbo` in addition to the 2024 variables.
- `../final_repkit/do/m0_prepare_cbo_data_scorecard26.do`
  - Builds `dta/cbo_paths26.dta`.
  - Reads `raw/cbo/51119-2026-02-LTBO-Budget.xlsx`, sheet `Supplemental Table 1`, range `A9:P40`.
  - Reads previous-year 2025 debt and debt/GDP from `raw/cbo/51134-2026-02-Historical-Budget-Data.xlsx`.
  - Computes `rho_cbo = (i - g) / (1 + g)`, `m_cbo = (debtchange - primarydeficit - interest) / gdp`, and extends each path to 103 model years.
- `../final_repkit/do/m0_programs_scorecard24.do`, `m0_programs_scorecard25.do`, `m0_programs_scorecard26.do`
  - Define `sim_full_cbo`.
  - Use fixed parameters `beta_1 = 0.576`, `beta_2 = 0.00848`, `lambda = 0.02`, `periods = 100`, `obs = 103`, `last_period = 101`.
  - Generate annual shocks `e_u ~ N(0, s_u)` and `e_s = e_s_size * Poisson(lambda)`.
  - Simulate deterministic, stochastic no-feedback, and feedback paths.
- `../final_repkit/do/m2_sims_master_scorecard24.do`
  - Runs 2024 LTBO deterministic simulations at `c = 0` and `c = 0.18`.
  - Runs risk simulations with 1,000 reps for `c = 0`, `0.17`, `0.18`, and `0.19`.
- `../final_repkit/do/m2_sims_master_scorecard25.do`
  - Runs 2025 deterministic simulations at `c = 0` and `c = 0.18`.
  - Runs risk simulations with 5,000 reps for `c = 0`, `0.17`, `0.18`, and `0.19`.
- `../final_repkit/do/m2_sims_master_scorecard26.do`
  - Runs 2026 deterministic simulations at `c = 0` and `c = 0.19`.
  - Runs risk simulations with 1,000 reps for `c = 0`, `0.18`, `0.19`, and `0.20`.
- `../final_repkit/do/m3_exhibits_prod_scorecard26.do`
  - Discovers available c-grid files.
  - Computes `c*` as the first `c` with share of terminal debt paths below `2.5` greater than or equal to `0.95`.
  - Uses `model_year == 101` as the terminal simulation point.
  - Uses `b_stoch` when `c == 0` and `b_feedback` when `c > 0`.
  - Exports `ltbo_cstar_values.csv` and `ltbo_pdchange_terminal_values.csv`.

The replication-kit `run_all.do` reports a full Stata runtime of 8-12 hours and `expected_sim_rep_calls = 28006`. That is mostly I/O and append overhead: 28,006 reps times 103 years is roughly 2.9 million annual state updates, which is small in vectorized R.

### Calibration inputs: what is actually used

Confirmed by reading all three `m2_sims_master_scorecard*.do` files (lines 17 and 20-21 in each):

- All three masters set `global e_s_size = 0.25` (hardcoded) and pass that constant to `sim_full_cbo`.
- All three masters read **only** `s_u_hat_risk` from `dta/sims_inputs_constrained.dta` (`global s_u_hat = s_u_hat_risk[_n==1]`).
- The `e_s_size` value `m1` computes from `Annual_FY_June2024.csv` is therefore dead — never consumed by a simulation.

**Implication for the R port:** `s_u_hat` is the single data-dependent calibration number. Hardcode `e_s_size = 0.25` to match production. Cross-check the frozen constants (`beta_1 = 0.576`, `beta_2 = 0.00848`, `lambda = 0.02`, `e_s_size = 0.25`, and the 250%-debt / 95%-probability sustainability definition) against the paper markdown in `docs/deficit_management_scorecard_paper.md` (and `..._from_latex.md`) so the implementation matches the published spec, not just the code.

## Historical Data Scope

The c-star simulation does not need the full pre-WWII historical dataset if the goal is to reproduce the simulation inputs used by the paper.

Confirmed requirements:

- `s_u_hat` uses the regression sample `year >= 1972` on an **annual** realized series (`year`, `rminusg`, `debt`, `g_nom`, `gdp`).
- `e_s_size` is **not** computed — it is hardcoded `0.25` (see "Calibration inputs"). The 2007/2014/2019/2020 potential-GDP debt ratios are not needed.
- The Stata script has older historical observations because `historicaldebtevolution_cleaned_updated_26.dta` supports other paper exhibits and alternate AR tables, but those pre-WWII observations are not needed for the simulation calibration used by `m2_sims_master_scorecard*.do`.

Implementation implication:

- Derive `debt`, `gdp`, `g_nom`, and `rminusg` from the existing R historical builder. `R/build_inputs.R::read_historical_debt_evolution()` **already reconstructs this annual series** (it computes `g_nom`, `g`, `rminusg`, `rho`); it just isn't persisted to `data/processed`. The c-star module should call that builder directly and run the AR regression on it. **Do not use `data/processed/dataset_for_regression_26.rds` for calibration** — it is semi-annual and starts in 1983, so it cannot supply the annual 1971+ sample the `year >= 1972` lagged regression needs.
- The exact Stata fixture `historicaldebtevolution_cleaned_updated_26.dta` is **not shipped in the repkit** (only the raw `raw/historical/historicaldebtevolution_updated.csv`, 1791-2023, exists). So there is no drop-in parity fixture; `s_u_hat` parity rests on the R reconstruction matching closely enough. If a boundary `c*` result later forces exact replication, reconstruct from the raw CSV (or request the cleaned `.dta` from Danny) — do not block the first c-star port on this.

## Required Inputs

Extend `config/input_manifest.csv` and `scripts/bootstrap_inputs.R` to copy these files from the replication kit into ignored `data-raw/`.

Already present or likely present for the current pipeline:

- `raw/cbo/51119-2026-02-LTBO-Budget.xlsx`
- `raw/cbo/51134-2026-02-Historical-Budget-Data.xlsx`
- historical files used by the current empirical pipeline to build recent `debt`, `gdp`, `g_nom`, and `rminusg`

Additional files needed for c-star:

- `raw/cbo/51119-2024-03-LTBO-budget.xlsx`
- `raw/cbo/51134-2024-02-Historical-Budget-Data.xlsx`
- `raw/cbo/51119-2025-03-LTBO-budget.xlsx`
- `raw/cbo/51134-2025-01-Historical-Budget-Data.xlsx`

**Not needed** (revised): `raw/historical/Annual_FY_June2024.csv` — it fed only the vestigial `e_s_size` computation, which production overrides with `0.25`. Do not add it to the manifest. (If you ever want to verify `0.25` is the right round number, bootstrap it ad hoc and check the formula offline; it is not a pipeline dependency.)

Parity input (not available):

- `dta/historicaldebtevolution_cleaned_updated_26.dta` is **not present in the repkit**, so it cannot be used as a drop-in fixture. The cleaned annual series is reconstructed by the R historical builder instead (see "Historical Data Scope"). The raw `raw/historical/historicaldebtevolution_updated.csv` is present if a closer reconstruction is ever required.

## New R Modules

Add small, pure-function modules. Keep script entrypoints thin.

### `R/simulation_inputs.R`

Responsibilities:

- `build_simulation_inputs(input_dir, processed_dir)`: returns a one-row tibble with `beta_1_risk`, `cons_risk`, `s_u_hat_risk`, and `e_s_size`. Source the annual series from `read_historical_debt_evolution()` (do **not** read the semi-annual regression RDS). Set `e_s_size = 0.25` as a constant to match the Stata masters.
- `estimate_rho_shock_sd(historical_debt_evolution)`: reproduces `reg rho L.rho L.b if year >= 1972` and returns the RMSE as `s_u_hat_risk`. This is the load-bearing function.
- `compute_poisson_shock_size(...)`: **optional, off the critical path.** Only implement if you want a sanity check that the historical formula ≈ 0.25; it must not feed the simulator.

Validation:

- Required columns: `year`, `rminusg`, `debt`, `g_nom`, `gdp` for the AR regression.
- Required annual observations: 1971 onward, so the `year >= 1972` regression has its lag.
- `s_u_hat_risk` is finite and positive.
- Hard-fail on duplicate years or missing values in required years.

### `R/cbo_paths.R`

Responsibilities:

- `build_cbo_path(input_dir, vintage)`: returns a 103-row path with `model_year`, `year`, `b_cbo`, `rho_cbo`, `s_cbo`, optional `s_tot_cbo`, `m_cbo`, and `gdp`.
- `read_ltbo_projection(input_dir, vintage)`: vintage-specific workbook/sheet/range/column mapping.
- `read_previous_year_budget(input_dir, vintage)`: previous-year debt and debt/GDP from the corresponding historical budget workbook.
- `extend_cbo_path(path, obs = 103)`: carries forward `b_cbo`, `rho_cbo`, `s_cbo`, `s_tot_cbo` when present, and sets `m_cbo = 0` for extension years.

Vintage mappings:

| vintage | LTBO file | LTBO sheet/range | previous historical file | previous year |
| --- | --- | --- | --- | --- |
| 2024 | `51119-2024-03-LTBO-budget.xlsx` | `1. Summary Ext Baseline`, `A10:AC41` | `51134-2024-02-Historical-Budget-Data.xlsx` | 2023 |
| 2025 | `51119-2025-03-LTBO-budget.xlsx` | `1. Summary Ext Baseline`, `A10:AC41` | `51134-2025-01-Historical-Budget-Data.xlsx` | 2024 |
| 2026 | `51119-2026-02-LTBO-Budget.xlsx` | `Supplemental Table 1`, `A9:P40` | `51134-2026-02-Historical-Budget-Data.xlsx` | 2025 |

Column mapping details:

- 2024 LTBO:
  - `Primarydeficitc` -> `primarydeficit_gdp`
  - `Federaldebtheldbythepublic` -> `b_cbo`
  - `Totaldeficit` -> `deficit_gdp`
  - `GDPtrillionsofdollars` -> `gdp`, multiply by 1000
  - `Netinterest` -> `interest_gdp`
  - `Fiscalyear` -> `year`
- 2025 LTBO:
  - `Primarydeficitc` -> `primarydeficit_gdp`
  - `Federaldebtheldbythepublic` -> `b_cbo`
  - `Totaldeficit` -> `deficit_tot`
  - `Grossdomesticproducttrillion` -> `gdp`, multiply by 1000
  - `Netinterest` -> `interest_gdp`
  - `Fiscalyear` -> `year`
- 2026 LTBO:
  - `Revenuesminustotalnoninterest` -> `primarydeficit_gdp`
  - `Federaldebtheldbythepublic` -> `b_cbo`
  - `Revenuesminustotalspendingb` -> `deficit_tot`
  - `GDPbillionsofdollars` -> `gdp`
  - `Netinterest` -> `interest_gdp`
  - `Fiscalyear` -> `year`

All vintages should compute:

```text
debt = b_cbo * gdp
interest = interest_gdp / 100 * gdp
i = interest / lag(debt)
g = gdp / lag(gdp) - 1
rho_cbo = (i - g) / (1 + g)
debtchange = debt - lag(debt)
m_cbo = (debtchange - primarydeficit - interest) / gdp
s_cbo = primary surplus as share of GDP
```

Be careful with signs. The Stata scripts store `s_cbo` as primary surplus, while CBO workbooks often label deficit concepts.

### `R/cstar_simulation.R`

Responsibilities:

- `simulate_cbo_paths(cbo_path, c, s_u, e_s_size, reps, shocks = TRUE, seed = 1000, return_paths = FALSE)`.
- `scan_cstar(cbo_path, c_grid, reps, s_u, e_s_size, threshold = 2.5, probability = 0.95)`.
- `build_cstar_summary(vintages = c(2024, 2025, 2026))`.
- `build_deterministic_feedback_path(cbo_path, c_star)`.
- `build_cstar_forward_table(cstar_summary, deterministic_paths)`.

Keep the simulator vectorized over reps. Use a year loop over 103 model years and numeric vectors or matrices over reps.

Simulation constants from Stata:

```text
beta_1 = 0.576
beta_2 = 0.00848
lambda = 0.02
periods = 100
obs = 103
last_period = 101
terminal_model_year = 101
debt_threshold = 2.5
target_probability = 0.95
seed = 1000
```

State initialization at `model_year == 1`:

```text
rho_det = rho_cbo
s_det = s_cbo
b_det = b_cbo

rho_stoch = rho_cbo
s_stoch = s_cbo
b_stoch = b_cbo

rho_feedback = rho_cbo
s_feedback = s_cbo
b_feedback = b_cbo
cumulative_adjustment = 0
```

For `model_year = 2..101`, with shocks enabled:

```text
e_u_t ~ Normal(0, s_u)
e_s_t = e_s_size * Poisson(lambda)

rho_det_t =
  rho_cbo_t +
  beta_1 * (rho_det_{t-1} - rho_cbo_{t-1}) +
  beta_2 * (b_det_{t-1} - b_cbo_{t-1})

s_det_t = s_cbo_t
b_det_t = b_det_{t-1} * (1 + rho_det_t) - s_det_t + m_cbo_t

rho_stoch_t =
  rho_cbo_t +
  beta_1 * (rho_stoch_{t-1} - rho_cbo_{t-1}) +
  beta_2 * (b_stoch_{t-1} - b_cbo_{t-1}) +
  e_u_t

s_stoch_t = s_cbo_t
b_stoch_t = b_stoch_{t-1} * (1 + rho_stoch_t) - s_stoch_t + m_cbo_t + e_s_t
```

If `c > 0`:

```text
rho_feedback_t =
  rho_cbo_t +
  beta_1 * (rho_feedback_{t-1} - rho_cbo_{t-1}) +
  beta_2 * (b_feedback_{t-1} - b_cbo_{t-1}) +
  e_u_t

delta_s_t =
  c * (rho_feedback_t * b_feedback_{t-1} + m_cbo_t -
       (s_cbo_t + cumulative_adjustment_{t-1}))

cumulative_adjustment_t = cumulative_adjustment_{t-1} + delta_s_t
s_feedback_t = s_cbo_t + cumulative_adjustment_t
b_feedback_t =
  b_feedback_{t-1} * (1 + rho_feedback_t) - s_feedback_t + m_cbo_t + e_s_t
```

If `c == 0`, copy the stochastic no-feedback path into the feedback fields.

For `model_year = 102..103`, set `e_u_t = 0` and `e_s_t = 0` and repeat the same transition formulas. Stata includes these extra periods for path stability even though c-star is evaluated at `model_year == 101`.

### `scripts/run_cstar_simulation.R`

Add a dedicated entrypoint first, then optionally call it from `scripts/run_pipeline.R` once stable.

Interface:

```sh
Rscript scripts/run_cstar_simulation.R
```

Environment variables:

- `DEFICIT_SCORECARD_INPUT_DIR`, default `data-raw`
- `DEFICIT_SCORECARD_DATA_DIR`, default `data/processed`
- `DEFICIT_SCORECARD_OUTPUT_DIR`, default `output`
- `DEFICIT_SCORECARD_CSTAR_REPS_SCALE`, default `1`
- `DEFICIT_SCORECARD_CSTAR_SAVE_DRAWS`, default `false`

Outputs:

- `data/processed/simulation_inputs.rds`
- `data/processed/cbo_paths_2024.rds`
- `data/processed/cbo_paths_2025.rds`
- `data/processed/cbo_paths_2026.rds`
- `output/ltbo_cstar_values.csv`
- `output/ltbo_pdchange_terminal_values.csv`
- `output/forward_table_cstar.csv`
- `output/forward_deterministic_feedback_path_cstar.csv`

Do not write full stochastic draws by default. If `DEFICIT_SCORECARD_CSTAR_SAVE_DRAWS=true`, save compressed RDS files under ignored `data/processed/simulation_draws/`.

## C-Star Grid

Use the Stata grids exactly for the first parity pass:

| vintage | risk reps | c-grid | deterministic feedback c in Stata | start year | terminal year |
| --- | ---: | --- | ---: | ---: | ---: |
| 2024 | 1000 | `0, 0.17, 0.18, 0.19` | 0.18 | 2025 | 2034 |
| 2025 | 5000 | `0, 0.17, 0.18, 0.19` | 0.18 | 2026 | 2035 |
| 2026 | 1000 | `0, 0.18, 0.19, 0.20` | 0.19 | 2027 | 2036 |

Compute `c*` independently from risk simulations:

```text
terminal_b = b_stoch at model_year 101 if c == 0
terminal_b = b_feedback at model_year 101 if c > 0
share_below_250 = mean(terminal_b < 2.5)
c_star = first c where share_below_250 >= 0.95
```

For the deterministic forward table, use deterministic paths at `c = 0` and `c = c_star`, not the Stata hardcoded deterministic feedback values. This is what `m3_exhibits_prod_scorecard26.do` does after it discovers `c*`.

## Randomness and Parity

Stata calls `set seed 1000` separately for each `c` run. R should do the same for deterministic comparability across grid points:

```r
set.seed(seed)
```

Exact draw-by-draw parity with Stata's RNG is not required for the first implementation. The acceptance target is parity in:

- formulas,
- input paths,
- sample sizes,
- c-grid,
- terminal threshold definition,
- selected `c*`.

If selected `c*` differs at a grid boundary, add a diagnostic output with `share_below_250` by vintage and c. Do not tune the RNG to force agreement unless the formulas and inputs have already been ruled out.

## Runtime Read

A vectorized R implementation should be much faster than Stata because the Stata code repeatedly saves and appends `.dta` files for each rep. The full parity workload is:

```text
2024: 4 c-values * 1,000 reps = 4,000 reps
2025: 4 c-values * 5,000 reps = 20,000 reps
2026: 4 c-values * 1,000 reps = 4,000 reps
deterministic: 6 one-rep paths
total: 28,006 reps
annual steps per rep: 103
total annual state updates: about 2.9 million
```

In R, this should be seconds to a few minutes on a normal laptop if implemented as vectorized year loops over rep vectors and summary-only output. The main cost should be Excel reading, not simulation.

Implementation guardrails:

- Do not store per-rep annual paths unless explicitly requested.
- For c-star, retain only terminal debt and summary statistics.
- For deterministic table/plots, retain one full deterministic path per needed vintage/c.
- Avoid row-binding inside rep loops.

## Implementation Sequence

### Step 1: Extend inputs and bootstrap

- Add the 2024/2025 LTBO files and 2024/2025 historical budget files to `config/input_manifest.csv`. **Do not add `Annual_FY_June2024.csv`** — it is not needed (`e_s_size` is hardcoded `0.25`).
- Update `scripts/bootstrap_inputs.R` to copy the new files.
- Add tests that required c-star inputs are either present after bootstrap or produce clear missing-input errors.

### Step 2: Build simulation calibration inputs

- Add `R/simulation_inputs.R`.
- Source the annual series from `read_historical_debt_evolution()` (the existing R historical builder), not the semi-annual regression RDS.
- Compute `s_u_hat_risk` from the unrestricted AR regression — this is the only number the masters consume:

```text
rho_t = alpha + beta_1 * rho_{t-1} + beta_2 * b_{t-1} + u_t
sample: year >= 1972
s_u_hat_risk = RMSE(u_t)
```

- Use `s_u_hat_risk` for simulations, matching the Stata master scripts.
- Set `e_s_size = 0.25` as a constant (matching `global e_s_size = 0.25` in all three `m2_*` masters). Do **not** derive it from potential GDP. The constrained `rho_lhs` regression and the `m1` `e_s_size` formula can be skipped entirely; implement them only if you want offline diagnostics.

### Step 3: Generalize CBO path construction

- Add `R/cbo_paths.R`.
- Port the three `m0_prepare_cbo_data_scorecard*.do` scripts.
- Validate each path has exactly 103 rows, unique `model_year`, unique `year`, no missing values in required simulation columns, and `model_year == 101` exists.
- Compare 2026 path output to current `R/forward_table.R` behavior and resolve any sign or scaling differences before proceeding.

### Step 4: Implement vectorized simulator

- Add `R/cstar_simulation.R`.
- Implement deterministic, stochastic, and feedback paths from `sim_full_cbo`.
- First verify no-shock deterministic `c = 0` reproduces the CBO baseline accounting path.
- Then verify no-shock deterministic `c = 0.19` reproduces the current fixed-`c` 2026 table within a tight tolerance.

### Step 5: Implement c-star scan and summaries

- Add `scan_cstar()` and `build_cstar_summary()`.
- Write `output/ltbo_cstar_values.csv` with:
  - `ltbo`
  - `risk_reps`
  - `grid_min`
  - `grid_max`
  - `c_star_250_95`
  - `at_lower_bound`
  - `at_upper_bound`
  - `share_below_250` diagnostics by c, either in the same file or a companion `output/ltbo_cstar_grid_scan.csv`
- Write `output/ltbo_pdchange_terminal_values.csv`. Semantics, read directly from `m3_exhibits_prod_scorecard26.do` (lines 150-174):
  - Columns: `ltbo`, `terminal_year`, `c_star_used`, `pd_change_terminal_raw`, `pd_change_terminal_abs`.
  - Build from the **deterministic, no-shock** paths (the `_no_poisson_no_rminusg` runs), one at `c = 0` and one at `c = c_star`, over the calendar window `[year_start, terminal_year]` per the C-Star Grid table.
  - `pd_base = -s_det * 100` (from the `c = 0` deterministic path); `pd_presc = -s_feedback * 100` (from the `c = c_star` deterministic path); `pd_change = pd_base - pd_presc`.
  - Report the value at `year == terminal_year` as `pd_change_terminal_raw`, and its absolute value as `pd_change_terminal_abs`.
  - Note: in R we generate the `c = c_star` deterministic path on demand via `build_deterministic_feedback_path()`, so this works for any selected `c_star` — unlike Stata, which could only read a pre-generated deterministic file at the hardcoded grid points.

### Step 6: Build c-star forward table

- Add a c-star version of the forward table that uses the estimated 2026 `c*`.
- Keep the current fixed-`c` table output until the c-star table is validated.
- Once validated, make the default forward table use estimated `c*`, with an environment override for paper/fixed values if useful.

### Step 7: Tests and parity checks

Add focused `testthat` coverage:

- `simulation_inputs`:
  - required calibration years exist (annual, 1971 onward);
  - duplicate years fail;
  - `s_u_hat_risk` is finite and positive;
  - `e_s_size` equals the constant `0.25` (the production value), not a recomputed formula.
- `cbo_paths`:
  - vintages 2024, 2025, 2026 build to 103 rows;
  - `model_year == 101` exists;
  - required columns have no missing values;
  - extension years carry forward baseline variables and set `m_cbo = 0`.
- `simulate_cbo_paths`:
  - no-shock `c = 0` path is deterministic and finite;
  - `c = 0` feedback fields equal stochastic fields;
  - no-shock `c > 0` has zero stochastic shocks and nonzero cumulative adjustment after the initial period;
  - terminal debt summary is invariant to `return_paths = FALSE` vs `TRUE`.
- `scan_cstar`:
  - returns the first passing grid value, not just any passing value;
  - includes lower/upper-bound flags;
  - for full inputs, 2026 `c*` should match the shipped paper/table value if the grid and inputs align.

Recommended command sequence:

```sh
Rscript scripts/bootstrap_inputs.R --repkit ../final_repkit
Rscript scripts/run_pipeline.R
Rscript scripts/run_cstar_simulation.R
Rscript -e "testthat::test_dir('tests/testthat')"
git status --short
```

## Acceptance Criteria

The implementation is complete when:

- All required c-star raw inputs are bootstrapped but not tracked.
- `scripts/run_cstar_simulation.R` runs from a clean checkout after bootstrap.
- The c-star scan produces 2024, 2025, and 2026 rows.
- The 2026 forward table can be generated from estimated `c*` without manually setting `DEFICIT_SCORECARD_FORWARD_C`.
- Existing empirical figures and scorecard outputs still build.
- Existing fixed-`c` functionality remains available during transition.
- Tests pass.
- `git status --short` shows only tracked code/docs/config changes and ignored runtime outputs.

## Open Questions for the Implementing Agent

- ~~Whether to use the optional `historicaldebtevolution_cleaned_updated_26.dta`~~ — resolved: that fixture is **not in the repkit**. Reconstruct the annual series from `read_historical_debt_evolution()`. The only residual risk is whether the R reconstruction reproduces the Stata `s_u_hat` closely enough; validate via the 2026 `c*` landing on `0.19` and the `share_below_250` grid diagnostic. If `c*` lands on a grid boundary, that is the trigger to reconstruct more carefully from the raw CSV or request the cleaned `.dta`.
- Whether to scan only the paper's coarse grid or add a finer grid after parity. Start with the paper grid.
- Whether exact Stata RNG parity is necessary. Start with formula and c-star parity; exact RNG parity is likely unnecessary for website production.
- Whether to keep `DEFICIT_SCORECARD_FORWARD_C` after c-star is default. It is useful as a paper-value override and should probably remain.
- Constants cross-check: confirm `beta_1`, `beta_2`, `lambda`, `e_s_size = 0.25`, and the 250%/95% sustainability definition against `docs/deficit_management_scorecard_paper.md` before locking the simulator.

