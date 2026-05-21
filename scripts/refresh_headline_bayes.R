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

fs::dir_create("data")
saveRDS(fc, "data/headline_bayes.rds")
saveRDS(
  list(
    refreshed_at  = Sys.time(),
    asof          = asof,
    model         = "bayes_kalman",
    elapsed_seconds = elapsed,
    panel_periods = range(panel$period, na.rm = TRUE),
    gamma_lag     = cfg$models$bayes_kalman$gamma_lag
  ),
  "data/headline_bayes_meta.rds"
)
cli::cli_alert_success("Wrote data/headline_bayes.rds (fit took {elapsed}s)")
