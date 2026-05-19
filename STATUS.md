# nomnowcast — session status

**Last touched:** 2026-05-19
**Repo:** `/Users/davidstephan/Documents/NOM_Nowcast` (git, branch `main`)
**Latest commit:** Phase 2 v4.0 — parametric Gamma lag from visa
  grants to arrivals (state augmentation; 4-6% backcast lift)
**R version pinned:** 4.5.3 via `renv.lock` (123 packages)
**Tests:** 78 passing

## TL;DR — what you have

A working R / `{targets}` pipeline that nowcasts quarterly Australian NOM
from live ABS + DHA public data, with vintage-aware backtesting,
benchmarks (RW, AR(1), bridge), and three Quarto reports. All 35 tests
pass.

**Headline backtest result** (pseudo-real-time, 11 quarterly dates,
2023-Q1 → 2025-Q3, target = ABS quarterly NOM total):

| Model                          | h=-2 RMSE | h=-1 RMSE | h=0 RMSE | h=+1 RMSE |
|--------------------------------|----------:|----------:|---------:|----------:|
| **bridge** (OAD + visa grants) | **21,511** | **25,656** | – | – |
| **kalman_multi_v2** (NOM block) | **37,523** | **40,745** | 77,699 | 72,793 |
| random walk                    | 39,188    | 46,857    | –        | –         |
| AR(1)                          | 48,150    | –         | –        | –         |
| kalman_v1 (Phase 1, π-route)   | 55,700    | 59,845    | 78,042   | 77,350    |
| kalman_multi_pi (v1.0 bivariate)| 58,861   | 75,848    | 78,622   | 71,303    |

Phase 2 v2.0 cuts backcast RMSE by **33-40%** versus the v1 Kalman
variants. The NOM observation row directly anchors the latent state
at past quarters where ABS has already published — sidestepping the
π extrapolation that broke under the 2025 regime shift. At h=0/+1
NOM is unobserved so all Kalman variants converge to similar
performance.

Bridge regression still wins backcasts (h<0) because it regresses
NOM directly on lagged flows with no log-Gaussian assumption.

In-sample headline performance against ABS NOM, latest 8 completed
quarters: errors within ±13%. Out-of-sample 2025: model over-shoots
by ~75-90% because ABS revised NOM down faster than the Kalman trend
could adapt (no leading indicators in the v0.5 Kalman).

## Phase status

| Phase | Status | Notes |
|-------|--------|-------|
| **Phase 0** scaffold | ✅ done | DESCRIPTION, renv, _targets.R, config.yml, dirs |
| **Phase 1** univariate Kalman | ✅ done + calibrated against live ABS |
| **Phase 2 v0.5** | ✅ done | damped trend, CKAN visa grants, bridge regression, real backtest |
| **Phase 2 v1.0** | ✅ done | bivariate KFAS SSM, fixed-quarter visa-grants lead |
| **Phase 2 v2.0** | ✅ done | trivariate SSM, NOM as third observation row, π-free headline, 33-40% backcast lift |
| **Phase 2 v3.0** | ⚠️ explored | time-varying α_n scaffolded but underperforms v2.0 (weak identification); OAD-proportional visa-grant disaggregation modestly helps π-route backcasts |
| **Phase 2 v4.0** | ✅ done | parametric Gamma lag via state augmentation; 4-6% backcast RMSE lift |
| **Phase 2 v5.0** | ⏳ open | jointly estimate Gamma (α, β); student-enrolments block; cross-category partial pooling |
| **Phase 3** | ⏳ scaffolded | `stan/hierarchical_nom.stan` compiles but not calibrated |
| **Phase 4** | ✅ effectively done | bridge regression is the benchmark |

## How to run things

```r
# From the project root, ensure the renv library is active:
renv::restore()           # one-time, ~10 min

# Run the pipeline end-to-end:
targets::tar_make()       # fetches ABS + DHA, runs all models, builds reports

# Quick smoke tests (no targets cache):
source(".tmp_run_tests.R")    # testthat suite (regenerate from STATUS.md if cleaned up)
```

Sanity-check the headline against ABS:

```r
files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
for (f in files) sys.source(f, envir = globalenv())
cfg <- config::get(file = "config.yml")
oad <- fetch_oad(cfg, Sys.Date())
nom <- fetch_nom(cfg, Sys.Date())
panel <- build_quarterly_panel(clean_oad(oad, cfg), clean_nom(nom, cfg),
                               empty_vg_clean(), empty_students_clean(), cfg)
pi_emp <- estimate_pi_empirical(panel, cfg)
pi_sm  <- smooth_pi(pi_emp, cfg)
fits   <- fit_kalman_univariate(panel, cfg)
fc     <- forecast_kalman_univariate(fits, Sys.Date(), cfg)
cats   <- build_nowcast_categories(fc, pi_sm, cfg)
head   <- build_nowcast_headline(cats, cfg)
print(head |> tail(8))
```

## Architecture map

```
R/
├── ingest/
│   ├── fetch_oad.R               # ABS OAD data cubes (Tables 1/2 + 15/16)
│   ├── fetch_nom.R               # ABS quarterly NOM + annual NOM-by-visa
│   ├── fetch_dha_ckan.R          # data.gov.au CKAN visa-grant XLSXs
│   ├── fetch_visa_grants.R       # delegates to CKAN; HTML fallback
│   ├── fetch_student_data.R      # Department of Education — currently 404s
│   ├── fetch_bitre.R             # BITRE aviation — currently 404s
│   └── fetch_temp_visa_holders.R # DHA TWH/WHM stocks
├── clean/
│   ├── category_map.R            # canonical visa-category regex table
│   ├── clean_*.R                 # one cleaner per source
│   └── panel.R                   # build_quarterly_panel — the modelling input
├── vintages/
│   ├── store.R                   # DuckDB vintage store interface
│   └── reenrich.R                # re-derive category/direction from series_id
├── pi/
│   ├── empirical_pi.R            # NOM / OAD-net long-term ratio
│   ├── smooth_pi.R               # LOESS/HP/local-linear smoothing with regime breaks
│   └── project_pi.R              # forward projection (currently inlined in nowcast_assembly)
├── models/
│   ├── kalman_univariate.R       # Phase 1 + damped trend (default)
│   ├── kalman_multi.R            # Phase 2 v1.0 scaffold
│   ├── bridge_regression.R       # OAD + visa grants → NOM benchmark
│   ├── stan_hierarchical.R       # Phase 3 wrapper around cmdstanr
│   └── nowcast_assembly.R        # π × (arrivals - departures) + uncertainty
├── backtest/
│   ├── vintage_simulation.R      # run_backtest (+ pseudo-real-time mode)
│   ├── benchmark_models.R        # RW, AR(1), ABS-preliminary
│   └── score_forecasts.R         # RMSE / MAE / bias / hit rate / log score
├── viz/plots.R
└── utils/                        # dates, http, logging
stan/
└── hierarchical_nom.stan         # Phase 3 model (compiles, not yet calibrated)
tests/testthat/                   # 35 tests
reports/                          # nowcast / methodology / backtest Quarto
```

## Key technical context — read this before resuming

### 1. ABS migrated away from time-series spreadsheets (2025-ish)

`readabs::read_abs(cat_no = "3401.0", ...)` no longer works — it raises
"Cannot find valid entry in the ABS Time Series Directory". Everything
now ships as Excel "data cubes". Slug-based identifiers replace numeric
catalogues:

- `overseas-arrivals-and-departures-australia` — OAD
- `national-state-and-territory-population` — quarterly NOM total
- `overseas-migration` — annual NOM by (4) broad visa groups

We use `readabs::download_abs_data_cube()` and parse the XLSXs
ourselves. See [R/ingest/fetch_oad.R](R/ingest/fetch_oad.R) and
[R/ingest/fetch_nom.R](R/ingest/fetch_nom.R).

### 2. ABS doesn't publish quarterly NOM by visa anymore

The detailed quarterly visa breakdown is gone. NOM-by-visa is now only
in the annual `overseas-migration` cube and only with 4 categories
(NZ / permanent / temporary / Australian citizens). The aggregate-π
estimator broadcasts a single π across categories
([R/pi/empirical_pi.R:25](R/pi/empirical_pi.R)).

### 3. DHA visa grants live at data.gov.au, NOT homeaffairs.gov.au

DHA has retired the static-XLSX download pages. The actual data lives
on data.gov.au's CKAN API:

```
https://data.gov.au/data/api/3/action/package_search?q=Student+visa+program
```

(NB: only the `/data/` path works; `data.gov.au/api/3/...` and
`data.gov.au/api/...` both return 404.)

See [R/ingest/fetch_dha_ckan.R](R/ingest/fetch_dha_ckan.R). Files are
annual financial-year pivot tables; we broadcast them equally across
quarters for now.

### 4. Department of Education + BITRE are currently 404

`education.gov.au` and `bitre.gov.au` both return Stream errors or 404s
on the URLs in `config.yml`. The fetchers degrade gracefully to empty
tibbles. Wiring these up is open work — possibly the data is on
data.gov.au too.

### 5. KFAS formula quirks

`KFAS::SSModel(y ~ KFAS::SSMtrend(...))` does **not** work — the
formula walker doesn't accept `pkg::fn` syntax. Workarounds:

- Import locally: `SSMtrend <- KFAS::SSMtrend` inside the function
- For `SSMcustom`, matrices must be 3D arrays `(p × m × n)` with
  `n = 1` for time-invariant
- See [R/models/kalman_univariate.R](R/models/kalman_univariate.R) —
  `build_damped_ssmodel()` works around both quirks

### 6. lubridate gotchas

`lubridate::quarters()` does **not** exist (only `years()`, `months()`,
etc.). Use `seq.Date(..., by = "-3 months", ...)` for quarter
arithmetic. `lubridate::my()` is permissive enough to match "May 10"
inside a footnote string — use a strict header-row detector (see
`detect_oad_header_row()` in [R/ingest/fetch_oad.R](R/ingest/fetch_oad.R)).

### 7. `nn_info` / `nn_warn` and `cli` envir

`cli::cli_alert_info("... {var}")` interpolates from
`parent.frame()`. If you wrap it (`nn_info <- function(...) cli::cli_alert_info(...)`)
you lose one frame and `var` becomes invisible. The current code
passes `.envir = parent.frame()` explicitly — preserve that pattern.

## Open work — prioritised

### Negative results from v3.0 — preserved for context

- **Time-varying $\alpha_n$ as a random-walk state.** Implemented in
  [R/models/kalman_multi.R](R/models/kalman_multi.R) behind the
  `models.kalman_multi.nom_alpha_time_varying` flag (default `false`).
  The state is weakly identified against the level state on the
  full ~200-quarter panel; H[3,3] inflates or β_n absorbs all
  quarterly noise depending on hyperparameter seeding. Several
  variants tried — diffuse vs informative prior on β_n, fixed vs
  estimated Q[β_n], fixed vs estimated H[3,3], recent-period
  α_n calibration — none beat v2.0 cleanly. The scaffold remains in
  place (with Q[β_n] = 0.01 and H[3,3] fixed at 0.005) for a future
  attempt that re-frames the identification problem (e.g.
  hierarchical prior on β_n innovation variance pooled across
  categories, or a 2-state piecewise-constant α with hard
  breakpoints at regime-shift dates).

### High-impact next steps

1. **Joint MLE of Gamma (α, β).** v4.0 fixes the Gamma lag shape at
   α=2, β=1 (mode at 1q, 4q tail). Profiling the panel likelihood
   over a small (α, β) grid would estimate the actual peak lag from
   data. Pure config-time work — no architecture changes needed.
   Implementation sketch: wrap `fit_kalman_multi()` in a grid search,
   pick the (α, β) with highest log-likelihood.

2. **Quarterly disaggregation of DHA grants.** Currently the annual
   FY pivots are broadcast as equal quarters. The pivot files'
   "Financial Year Quarter" filter is set to `(All)` — there's
   probably a way to download per-quarter views, or we could fit a
   monthly-via-OAD disaggregation. See
   [R/clean/panel.R](R/clean/panel.R) `spread_annual_to_quarters()`.

3. **Real ABS release-metadata read for `nom_classify_status`.**
   Currently age-based heuristic
   ([R/ingest/fetch_nom.R](R/ingest/fetch_nom.R) `nom_classify_status`).
   ABS release notes give the true preliminary/revised/final markers.
   This matters for proper vintage-aware backtest scoring.

4. **Validate Stan model end-to-end.** The model in
   [stan/hierarchical_nom.stan](stan/hierarchical_nom.stan) compiles
   but has never been sampled against the v1 panel. Needs cmdstanr
   installation (CRAN doesn't have it — pull from
   `stan-dev.r-universe.dev`), then run a small chain count and check
   divergences. Bayesian credible intervals on the headline would be
   a real upgrade.

### Lower-impact but useful

5. **Re-wire Department of Education + BITRE fetchers** to whatever
   their actual current URLs are. data.gov.au is likely.
6. **Capture vintages going forward.** The DuckDB store is set up but
   only contains today's data. Once you run the pipeline weekly for
   a few months you'll have real vintages and the pseudo-real-time
   backtest can transition to a true vintage-aware backtest.
7. **Granular NOM-by-visa annual π.** The annual NOM-by-visa cube
   gives 4 visa groups (NZ / permanent / temporary / Aust citizens).
   The aggregate-π fallback ignores this — using the annual values to
   refine category-level π is straightforward.
8. **`benchmark_ar1` doesn't forecast forward.** It only emits
   in-sample fitted values, so backtest h=0/+1 are NA for AR(1). Add
   `predict()` for 4 steps ahead.
9. **AR(1) on quarter-on-quarter growth currently uses `stats::arima`**
   — `fitted.Arima` is in `{forecast}` not `{stats}`. Use
   `g - residuals(fit)`, already done in
   [R/backtest/benchmark_models.R](R/backtest/benchmark_models.R).
10. **Re-add the 4 `.tmp_*.R` diagnostic scripts** if you find them
    useful — they got cleaned up at commit time. Specifically a small
    standalone "fetch + headline" runner is handy for sanity checks
    independent of `{targets}`.

### Known but acceptable limitations

- π extrapolation past the completion cutoff uses a regime-aware
  rolling median of the last 8 quarters
  ([R/models/nowcast_assembly.R](R/models/nowcast_assembly.R)
  `project_pi_value`). Robust to noise; not robust to genuine regime
  shifts like the 2025 ABS NOM revision. Stan / hierarchical π will
  do better here.
- OAD Tables 15/16 (by visa) are total movements (short + long-term
  + permanent), not long-term only. The aggregate "total" series from
  Tables 1/2 IS Permanent + Long-term, which is the right NOM-relevant
  flow — but only at the national, no-category-breakdown level.
- The `secondary_ci` (95%) intervals are based on the model's internal
  uncertainty only — they don't account for π misspecification at
  regime breaks. Treat them as conservative.

## Recent commits

```
b717c3a  Phase 2 v0.5: damped trend, CKAN visa grants, bridge w/ leading indicator
29cbe65  Phase 1 calibration: aggregate long-term path; sensible nowcasts
3046ec7  Live-validate Phase 1 against current ABS releases; lock deps
1151ea6  Scaffold NOM nowcast pipeline (v0.1)
```

## What to do first when you resume

1. `git log --oneline -5` — confirm you're on `b717c3a` (or later).
2. `Rscript -e 'renv::status()'` — confirm the library matches the
   lockfile.
3. Re-run the headline sanity check (snippet at top) to confirm the
   pipeline still flows end-to-end (ABS / data.gov.au URLs change
   without warning).
4. Decide between (a) Phase 2 v1.0 multivariate SSM or (b) Phase 3
   Stan calibration as the next big push.

Either path will produce a much better Kalman uncertainty
characterisation than v0.5 has today.
