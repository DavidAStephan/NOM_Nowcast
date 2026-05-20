#!/usr/bin/env Rscript
## scripts/refresh_backtest.R
##
## Run the full pseudo-real-time backtest grid and write the scored
## tibble + a tiny manifest to data/. The Quarto site reads these
## files at render time, so the page stays cheap on every push.
##
## Trigger this manually (`Rscript scripts/refresh_backtest.R`) or via
## the `Refresh backtest` GitHub Action.

suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes)
  library(dplyr)
  library(tibble)
  library(lubridate)
})

files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
for (f in files) sys.source(f, envir = globalenv())

cfg <- config::get(file = "config.yml")

# Enable the Bayesian model when NN_REFRESH_BAYES=1 in the environment.
# Off by default so a refresh stays cheap; the Bayes fit window is
# controlled by cfg$models$bayes_headline$backtest_window.
if (identical(Sys.getenv("NN_REFRESH_BAYES"), "1")) {
  cfg$models$bayes_headline$enabled <- TRUE
  cli::cli_alert_info("Bayesian model: enabled (last {cfg$models$bayes_headline$backtest_window %||% 12} quarters of grid)")
} else {
  cli::cli_alert_info("Bayesian model: disabled (set NN_REFRESH_BAYES=1 to include)")
}

# Use a throwaway DuckDB so we don't litter the repo's vintage store.
cfg$paths$duckdb <- tempfile(fileext = ".duckdb")
db <- vintages_init(cfg)

backtest_grid <- build_backtest_grid(cfg)
cli::cli_alert_info("Backtest grid: {length(backtest_grid)} as-of dates")

runs <- lapply(seq_along(backtest_grid), function(i) {
  d <- backtest_grid[[i]]
  cli::cli_alert_info("[{i}/{length(backtest_grid)}] backtest at {format(d)}")
  tryCatch(run_backtest(d, db, cfg), error = function(e) {
    cli::cli_alert_warning("  failed: {conditionMessage(e)}")
    NULL
  })
})
runs <- Filter(Negate(is.null), runs)
scored <- score_backtest(runs, db, cfg)

fs::dir_create("data")
saveRDS(scored, "data/backtest_scored.rds")
saveRDS(
  list(
    refreshed_at = Sys.time(),
    n_asof_dates = length(backtest_grid),
    n_successful = length(runs),
    grid_from    = min(backtest_grid),
    grid_to      = max(backtest_grid)
  ),
  "data/backtest_meta.rds"
)
cli::cli_alert_success("Wrote data/backtest_scored.rds and data/backtest_meta.rds")
