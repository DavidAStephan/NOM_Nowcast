#!/usr/bin/env Rscript
## scripts/refresh_headline_bayes.R
##
## Fit the production headline using bayes_kalman (Phase 3 v0.6) on
## the current panel and write the forecast + a small meta file to
## data/. The index.qmd page reads these files at render time, so
## the landing page can show the Bayesian headline without doing a
## Stan compile + HMC sample on every render.
##
## bayes_kalman replaced bayes_gamma as the production model after
## the calibration analysis (see backtest.qmd "Calibration"): its
## 80% / 95% credible intervals approximately match nominal coverage
## in post-COVID quarters, while bayes_gamma is significantly
## over-confident there. Point-RMSE at h=0 is essentially tied.
##
## Triggered manually (`Rscript scripts/refresh_headline_bayes.R`) or
## via the `Refresh headline` GitHub Action.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
})

files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
for (f in files) sys.source(f, envir = globalenv())

cfg <- config::get(file = "config.yml")
cfg$models$bayes_kalman$enabled <- TRUE
asof <- Sys.Date()

cli::cli_alert_info("Fetching upstream sources at as-of {format(asof)}")
oad         <- fetch_oad(cfg, asof)
nom         <- fetch_nom(cfg, asof)
visa_grants <- fetch_visa_grants(cfg, asof)

panel <- build_quarterly_panel(clean_oad(oad, cfg), clean_nom(nom, cfg),
                               clean_visa_grants(visa_grants, cfg), cfg)
cli::cli_alert_info("Fitting bayes_kalman headline")
t0 <- Sys.time()
fc <- fit_bayes_kalman(panel, asof, cfg)
elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)

if (!nrow(fc)) {
  cli::cli_abort("bayes_kalman returned an empty forecast")
}

# Regime-aware bias + coverage correction (see
# R/backtest/nowcast_adjustment.R and backtest.qmd). The structural
# nowcast under-predicts NOM at the live edge post-COVID by a large but
# stable, model-invariant margin; we centre and re-cover the live
# horizons (h >= 0) from a backtest-calibrated adjustment. Backcasts
# (h < 0) are already near-unbiased and left untouched.
#
# OPT-IN (default off). The adjustment was calibrated on a backtest
# whose h=0 quarter carried only 1 of 3 OAD months summed as a full
# quarter (see aggregate_oad_quarters()). With that panel bug fixed the
# calibration is stale and must be re-estimated before it is ever
# switched on. Horizon convention when it is: h=0 is the LAST COMPLETED
# quarter, exactly as in run_backtest() (ref_q = quarter before the
# origin), never the in-progress quarter.
apply_adj <- cfg$models$bayes_kalman$apply_adjustment %||% FALSE
adj_path  <- "data/nowcast_adjustment.rds"
fc$adjusted <- FALSE
if (isTRUE(apply_adj) && file.exists(adj_path)) {
  ref_q <- nn_last_completed_quarter(asof)
  fc$horizon <- quarters_between(fc$period, ref_q)
  fc <- apply_nowcast_adjustment(fc, readRDS(adj_path))
  cli::cli_alert_info(
    "Applied nowcast adjustment to {sum(fc$adjusted)} live-horizon rows")
  fc$horizon <- NULL
} else if (isTRUE(apply_adj)) {
  cli::cli_alert_warning("apply_adjustment on but {adj_path} missing; headline left raw")
}

fs::dir_create("data")
saveRDS(fc, "data/headline_bayes.rds")
saveRDS(
  list(
    refreshed_at  = Sys.time(),
    asof          = asof,
    model         = "bayes_kalman",
    elapsed_seconds = elapsed,
    panel_periods = range(panel$period, na.rm = TRUE),
    # Transparency on the live edge: last OAD quarter with any data and
    # how many of its months were published at fit time.
    last_oad_quarter = {
      tot <- panel[panel$category == "total" & !is.na(panel$oad_lt_arrivals), ]
      if (nrow(tot)) list(period = max(tot$period),
                          months = tot$oad_months[which.max(tot$period)]) else NULL
    },
    oad_partial_quarter = cfg$panel$oad_partial_quarter %||% "scale",
    gamma_lag     = cfg$models$bayes_kalman$gamma_lag
  ),
  "data/headline_bayes_meta.rds"
)
cli::cli_alert_success("Wrote data/headline_bayes.rds (fit took {elapsed}s)")
