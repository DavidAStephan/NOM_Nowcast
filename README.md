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

## Phase 2 v5.0 — data-driven Gamma (α, β) via grid search

v4.0 fixed Gamma α=2, β=1. v5.0 picks (α, β) from a small grid by
maximising the panel log-likelihood, then refits the multivariate
Kalman with the chosen parameters. The number of free variance
parameters is unchanged across grid points so log-likelihood
ordering equals AIC ordering — a principled estimator for the
Gamma lag shape.

At the current full-data fit, grid search picks **α=1, β=2** (mass
on lag 0–1; mean lag = 0.5 quarters), substantially shorter than
the v4 default. Backtest performance is essentially tied with v4
fixed:

| Model                   | h=-2 | h=-1 | h=0 | h=+1 |
|-------------------------|----:|----:|----:|----:|
| v5 (grid search)        | 36,405 | 39,282 | 77,985 | 73,303 |
| v4 (fixed α=2, β=1)     | 36,183 | 38,330 | 78,773 | 73,970 |

The (α, β) likelihood surface is flat enough that nearby values
give equivalent fits — the grid-search-picked point varies modestly
across backtest as-of dates and cancels out on average. Operationally
v5.0 is mathematically correct but adds ~4s of overhead per fit for
~0% backtest improvement. Enabled by default in `config.yml` as a
diagnostic; turn off via `models.kalman_multi.gamma_lag.grid_search:
false` for production-speed fits at fixed (α, β).

## Phase 2 v4.0 — parametric Gamma lag for visa grants

The bivariate / trivariate Kalman in v1.0/v2.0 used a fixed
single-quarter lead from visa grants to arrivals — i.e. the
observation row for visa grants loaded only on the current latent
log-arrivals state. v4.0 implements the parametric Gamma lag
specification from the methodology document:

$$
V_{c,t} = \kappa_c \sum_{k=0}^{K} w_{c,k}(\alpha_c, \beta_c)\, A^*_{c,t+k} + \varepsilon^V_{c,t}
$$

with $w_k$ a discretised Gamma(α, β) lag distribution. Implementation
augments the state vector with K lagged copies of the level state
($\mu_{t-1}, \dots, \mu_{t-K}$), shifts the visa-grants observation
back by K quarters, and loads the row on the K+1 lagged μ states
with the Gamma weights. Defaults are α=2, β=1, K=4 (mode at 1q, four-
quarter tail). Configure via `models.kalman_multi.gamma_lag`.

### Head-to-head backtest (11 quarterly dates, 2023-Q1 → 2025-Q3)

| Model | h=-2 RMSE | h=-1 RMSE | h=0 RMSE | h=+1 RMSE |
|-------|----------:|----------:|---------:|----------:|
| bridge | **21,872** | **26,432** | – | – |
| **kalman_multi_v2 (Gamma lag)** | **36,183** | **38,330** | 78,773 | 73,970 |
| random walk | 39,188 | 46,857 | – | – |
| AR(1) | 48,150 | – | – | – |
| kalman_multi_pi (π-route, Gamma lag) | 55,573 | 61,813 | 81,976 | 80,595 |
| kalman_v1 (Phase 1) | 55,700 | 59,845 | 78,042 | 77,350 |

The state-augmented Gamma lag gives a clean **−3.6%/−5.9%** backcast
improvement on the v2.0 NOM-anchored headline (`kalman_multi_v2`).
The state-augmentation cost is 4 extra deterministic state dimensions
(9 total) and zero new MLE parameters — Gamma α and β are fixed
config knobs in v4.0.

## Phase 2 v3.0 exploration (negative + small positive)

Two extensions to v2.0 were tried in this iteration:

1. **Time-varying $\alpha_n$** — model the OAD-to-NOM log offset as a
   slow random-walk state (β_n) so the Kalman could adapt to π
   regime shifts. **Result: underperforms v2.0 across every horizon.**
   The state is weakly identified against the level on a 200-quarter
   panel; several reparameterisations (diffuse vs informative β_n
   prior, fixed vs estimated H[3,3], recent-period α_n calibration)
   all produced backtest RMSE worse than the static v2.0 baseline.
   Scaffold retained behind
   `models.kalman_multi.nom_alpha_time_varying: false` for a future
   attempt; see STATUS.md for design notes.

2. **OAD-proportional quarterly disaggregation** of FY-annual DHA
   visa grants. Replaces the equal-quarters broadcast with weights
   from category-level OAD long-term arrivals within each FY. **Modest
   improvement**: kalman_multi_pi h=-1 RMSE drops 75,848 → 59,135
   (-22%); other horizons within ±5%. Defaults to `proportional` via
   `panel.vg_disagg_strategy`. v2.0 headline (`kalman_multi_v2`)
   essentially unchanged.

## Phase 2 v2.0 — trivariate Kalman with NOM observation block

The Kalman now sees three observations on a shared latent
log-arrivals state $\mu_t$: OAD long-term arrivals, lagged DHA visa
grants, and ABS NOM itself. NOM observations enter as
$y_3(t) = \log(1+\mathrm{NOM}_t) - \alpha_n$ where $\alpha_n$ is the
empirical long-run mean log-gap between NOM and arrivals. The headline
NOM forecast comes directly from the multi-SSM via the inverse
transform $\exp(\mu_t + \alpha_n) - 1$, bypassing the empirical-π
extrapolation entirely.

### Head-to-head backtest (11 quarterly dates, 2023-Q1 → 2025-Q3)

| Model | h=-2 RMSE | h=-1 RMSE | h=0 RMSE | h=+1 RMSE |
|-------|----------:|----------:|---------:|----------:|
| bridge                          | **21,511** | **25,656** | – | – |
| **kalman_multi_v2** (NOM block) | **37,523** | **40,745** | 77,699 | 72,793 |
| random walk                     | 39,188    | 46,857    | – | – |
| AR(1)                           | 48,150    | –         | – | – |
| kalman_v1 (Phase 1, π-route)    | 55,700    | 59,845    | 78,042 | 77,350 |
| kalman_multi_pi (v1.0)          | 58,861    | 75,848    | 78,622 | 71,303 |

v2.0 cuts backcast RMSE by **33-40%** versus the v1 Kalman variants.
NOM observations directly anchor the latent state at past quarters
where ABS has already published, sidestepping the regime-shift
problem the π extrapolation kept hitting. At h=0/+1 NOM is
unobserved so all Kalmans converge to similar performance — that's
the territory where a multi-source SSM with leading indicators is
the only structural way to improve.

The bridge regression still wins backcasts because it directly fits
NOM on lagged flows with no log-Gaussian assumption.

Toggle the v2.0 path via `models.kalman_multi.include_nom` in
`config.yml`.

## Phase 2 v1.0 — multivariate Kalman with visa-grants block

The bivariate ancestor of v2.0: two observations (OAD long-term
arrivals + lagged visa grants) share a common trend with damped
slope. The headline NOM forecast still goes through the empirical-π
route. Retained as a baseline for the v2.0 comparison.

## Phase 2 v0.5 — what's new

- **Real DHA visa-grants data via data.gov.au CKAN.** The fetcher
  pulls Student, Visitor, Temporary Graduate, Skilled and Working
  Holiday Maker visa grant data from the CKAN open-data API at
  `data.gov.au/data/api/3/action/...`. Files are annual financial-year
  pivots; the panel builder broadcasts them equally across the four
  quarters of each FY.
- **Damped Kalman trend.** The default Kalman now uses a level-plus-
  AR(1)-slope structure (`phi = 0.85`) instead of the local-linear
  trend, which stabilises long-horizon forecasts. Configurable via
  `models.kalman.trend_type` ∈ {`damped`, `local_linear`,
  `level_only`}.
- **Robust π projection.** Forward projection of π past the cohort
  cutoff now uses the median of the last 8 quarters (configurable via
  `pi.projection` / `pi.projection_window`) instead of the latest
  value, which was too sensitive to noise / revisions.
- **Bridge regression with visa-grants leading indicator.** The
  benchmark now fits `nom_final ~ Σ β·OAD_arrivals_l(k) +
  Σ γ·visa_grants_l(k)`. With recent live data, in-sample R² ≈ 0.87
  and the bridge **outperforms the Kalman** by ~2.5× RMSE on
  backcasts, confirming that visa grants are a useful leading
  indicator.
- **Vintage-aware backtest runs end-to-end on real data.** Pseudo-
  real-time mode lets the backtest framework run before any historical
  vintages have been captured: at each evaluation date, sources are
  fetched live and restricted to what would have been observable then,
  using the configured publication lags.

### Indicative backtest performance (11 quarterly dates, 2023-Q1 → 2025-Q3)

| Model | h=-2 RMSE | h=-1 RMSE | h=0 RMSE |
|-------|----------:|----------:|---------:|
| bridge   | 21,511 | 25,656 | (truth NA) |
| random walk | 39,188 | 46,857 | (truth NA) |
| AR(1) | 48,150 | – | – |
| kalman_v1 | 57,432 | 60,585 | 78,949 |

Comparison to ABS preliminary is informational only in v0.5 — without a
multi-month history of vintages captured, the framework falls back to
treating the latest NOM observation as the preliminary value.

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
