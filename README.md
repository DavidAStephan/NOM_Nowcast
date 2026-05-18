# nomnowcast — Nowcasting Australian NOM from public data

A reproducible R pipeline for nowcasting quarterly Australian Net Overseas
Migration (NOM) using only publicly available data. Built around three
modelling generations (univariate Kalman → multi-source state-space →
Bayesian hierarchical) and a vintage-aware backtest framework that compares
the model against the ABS preliminary estimate.

## Quick start

```r
# 1. Install renv and restore the locked library
install.packages("renv")
renv::restore()

# 2. Fetch raw data, build vintage store, run Phase 1 model, build reports
targets::tar_make()

# 3. Inspect the headline nowcast
targets::tar_read(nowcast_headline)

# 4. Open the reports
browseURL("reports/nowcast_report.html")
browseURL("reports/methodology.html")
browseURL("reports/backtest_report.html")
```

A full run from a cold cache takes roughly 30–60 minutes on a modern laptop
for Phase 1; Phase 3 (Stan) is the bottleneck and budgets up to 4 hours.

## What the pipeline does

1. **Ingest** publicly available migration indicators
   - ABS Overseas Arrivals and Departures (cat. 3401.0) via `{readabs}`
   - ABS NOM estimates (cat. 3101.0)
   - Department of Home Affairs visa grants
   - Department of Education international student commencements/enrolments
   - BITRE international airline activity
   - DHA temporary visa holder stocks and Working Holiday Maker statistics
2. **Vintage-track** every fetched value in a DuckDB store so future runs can
   reconstruct exactly what was knowable at any historical date.
3. **Estimate empirical classification probabilities** π<sub>c</sub> by
   comparing completed-cohort OAD long-term arrivals against the
   eventually-revised ABS NOM by visa category.
4. **Model latent arrivals** with a per-category univariate Kalman filter
   (Phase 1), extended in later phases to a multi-source state-space model
   and a full Bayesian hierarchical specification in Stan.
5. **Backtest** every model variant against the ABS preliminary estimate over
   pre-COVID, COVID and post-COVID regimes, by category and aggregate.
6. **Report** the current nowcast, methodology and backtest results in three
   self-contained Quarto documents.

See [reports/methodology.qmd](reports/methodology.qmd) for the full write-up
including all model equations.

## Project layout

```
nom_nowcast/
├── _targets.R                Pipeline DAG
├── config.yml                Paths, hyperparameters, eval windows
├── R/
│   ├── ingest/               One fetcher per source
│   ├── clean/                Harmonisation, category mapping
│   ├── vintages/             DuckDB vintage store interface
│   ├── pi/                   Classification-probability estimation
│   ├── models/               Kalman, Stan, bridge regression
│   ├── backtest/             Vintage simulation, scoring, benchmarks
│   ├── viz/                  Plotting
│   └── utils/                Cross-cutting helpers
├── stan/                     Stan source for Phase 3
├── data/
│   ├── raw/                  Immutable downloads (gitignored)
│   ├── vintages/             DuckDB vintage store (gitignored)
│   └── processed/            Analysis-ready data (gitignored)
├── reports/                  Quarto reports
└── tests/testthat/           Unit tests
```

## Configuration

All parameters live in [config.yml](config.yml). The pipeline reads it via
`{config}` so non-default environments can override values without editing
code. Notable knobs:

- `run.asof_date` — set to a past date to replay a historical vintage.
- `pi.smoother` — `loess`, `hp`, `locallinear` or `none`.
- `backtest.start` / `backtest.end` — evaluation window for the recursive
  scoring run.
- `models.stan.chains` etc. — Stan sampling controls.

## Vintage-aware backtesting

The framework recursively re-runs every model at each evaluation date
`T ∈ [backtest.start, backtest.end]`. For each `T` we:

1. Restrict each input series to observations whose publication date is
   `≤ T`. Publication lags are series-specific (see `config.yml`).
2. Restrict the ABS NOM target to the vintage available at `T`.
3. Re-estimate the model from scratch.
4. Produce nowcast (h=0), backcast (h=-2, -1) and forecast (h=+1).

Performance is reported as RMSE / MAE / bias / log score / hit rate, broken
down by regime (pre-COVID / COVID / post-COVID) and by category, against
both the eventually-revised final NOM and — most importantly — the ABS
preliminary estimate available at `T`.

## Reproducibility

- Dependencies pinned in `renv.lock`.
- All intermediate state lives in `_targets/` and the DuckDB vintage store.
- Raw downloads are immutable: filenames include the download timestamp;
  the pipeline never overwrites.
- No `setwd`, no `rm(list=ls())`, no manual data steps anywhere.

## Phase 1 performance (as at the v0.1 calibration)

On a live ABS pull (May 2026), the headline nowcast tracks the ABS
preliminary closely for the most recent eight completed quarters:

| Quarter  | Model    | ABS    | Error    | % |
|----------|---------:|-------:|---------:|--:|
| 2023-Q1  | 178,974  | 165,500| +13,474  | +8.1% |
| 2023-Q2  | 106,851  | 120,500| −13,649  | −11.3% |
| 2023-Q3  | 144,781  | 145,200|   −419   | −0.3% |
| 2023-Q4  | 93,677   | 99,500 | −5,823   | −5.9% |
| 2024-Q1  | 125,485  | 128,700| −3,215   | −2.5% |
| 2024-Q2  | 59,105   | 55,800 | +3,305   | +5.9% |
| 2024-Q3  | 77,992   | 81,600 | −3,608   | −4.4% |
| 2024-Q4  | 70,251   | 63,900 | +6,351   | +9.9% |

Out-of-sample (2025+) the Kalman currently over-shoots because ABS
revised NOM down through 2025 and the structural trend hasn't caught
up. This is the natural target of Phase 2's visa-grants leading
indicator.

## Limitations

- Without PLIDA-level stay-duration data, the project models
  classification probabilities indirectly. The π estimator uses
  completed cohorts only; uncertainty around extrapolating π forward
  is substantial during regime breaks.
- ABS NOM final values are themselves revised; "ground truth" in the
  backtest means the latest vintage available at the report run time.
- **ABS NOM by visa category is now only published annually**
  (catalogue 3407.0). Quarterly NOM is total only. The empirical π
  estimator falls back to aggregate-π
  (`NOM_total_q / net_long_term_flow_q`) and broadcasts it across
  categories. Category attribution is mechanical rather than
  structural — Phase-1 simplification, documented in the methodology
  report.
- **OAD has migrated to Excel data cubes.** The headline now uses
  Tables 1/2 (Permanent + Long-term movements, national totals) which
  is the canonical NOM-relevant flow. Tables 15/16 (by visa group)
  still feed the categorical decomposition but cover total movements
  rather than long-term only.
- **Secondary sources have largely migrated to Power BI dashboards.**
  DHA visa grants, Department of Education student data and BITRE
  airline activity no longer expose monthly CSV/XLSX downloads on
  their public landing pages — they now require either the
  data.gov.au open-data API or Power BI clients. The fetchers degrade
  gracefully (return empty tibbles) and the Phase 1 nowcast is
  unaffected, but wiring up these sources is a prerequisite for
  Phase 2.

## Adding a new data source

1. Write a fetcher under `R/ingest/`. It must return a tibble keyed by
   `series_id`, `period`, with one column per observation.
2. Register it in `_targets.R` as a target. Use `tar_target(..., format =
   "qs")`.
3. Add the publication-lag entry to `config.yml`.
4. Wire it into the vintage store via `vintages_write()`.
5. Optionally surface it as an observation block in the state-space model.

## Contributing

Code style: tidyverse style guide, run `styler::style_pkg()` and
`lintr::lint_package()` before committing. Roxygen on every exported
function. Tests in `tests/testthat/` for any function with non-trivial
logic.
