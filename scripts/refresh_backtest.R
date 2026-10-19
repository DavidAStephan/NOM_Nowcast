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

# Enable the Bayesian models when NN_REFRESH_BAYES=1 in the environment.
# Off by default so a refresh stays cheap; the Bayes fit window is
# controlled by cfg$models$<model>$backtest_window. NN_BAYES_MODELS
# (comma-separated, default all four) narrows the set, e.g.
# NN_BAYES_MODELS=bayes_kalman for a ~15-minute production-only run.
bayes_all <- c("bayes_headline", "bayes_gamma", "bayes_gamma_learned", "bayes_kalman")
if (identical(Sys.getenv("NN_REFRESH_BAYES"), "1")) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    cli::cli_abort("NN_REFRESH_BAYES=1 but cmdstanr is not installed; the committed snapshot would lose the production model's rows")
  }
  wanted <- Sys.getenv("NN_BAYES_MODELS", "")
  wanted <- if (nzchar(wanted)) trimws(strsplit(wanted, ",")[[1]]) else bayes_all
  for (m in intersect(bayes_all, wanted)) cfg$models[[m]]$enabled <- TRUE
  cli::cli_alert_info("Bayesian models: {paste(intersect(bayes_all, wanted), collapse = ', ')} (last {cfg$models$bayes_kalman$backtest_window %||% 12} quarters of grid)")
} else {
  cli::cli_alert_info("Bayesian models: disabled (set NN_REFRESH_BAYES=1 to include)")
}

# Use a throwaway DuckDB so we don't litter the repo's vintage store,
# then seed it with the real NOM vintages captured weekly by the
# `Capture NOM vintage` workflow. Backtest origins at or after the first
# capture then read a genuine vintage (and score against a genuine ABS
# preliminary); earlier origins fall back to pseudo-real-time.
cfg$paths$duckdb <- tempfile(fileext = ".duckdb")
db <- vintages_init(cfg)
hist_path <- "data/vintages/nom_history.csv"
if (file.exists(hist_path)) {
  hist <- readr::read_csv(hist_path, show_col_types = FALSE)
  vds  <- sort(unique(as.Date(hist$vintage_date)))
  for (i in seq_along(vds)) {
    rows <- hist[as.Date(hist$vintage_date) == vds[i], ]
    vintages_write(db, "nom", tibble::tibble(
      series_id   = paste0("nom_quarterly:", rows$category),
      period      = as.Date(rows$period),
      period_unit = "quarter",
      value       = rows$value,
      unit        = "persons",
      metadata    = as.character(jsonlite::toJSON(
        list(series = "Net overseas migration ; Australia ;",
             source = basename(hist_path)), auto_unbox = TRUE))
    ), vds[i])
  }
  cli::cli_alert_info("Loaded {length(vds)} real NOM vintages ({format(min(vds))} to {format(max(vds))}) into the backtest store")
}

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
    grid_to      = max(backtest_grid),
    models       = sort(unique(scored$model)),
    n_real_vintage_origins = sum(vapply(runs, function(r) {
      nrow(r) > 0L && "nom_vintage" %in% names(r) && r$nom_vintage[1] == "real"
    }, logical(1))),
    oad_partial_quarter = cfg$panel$oad_partial_quarter %||% "scale"
  ),
  "data/backtest_meta.rds"
)
cli::cli_alert_success("Wrote data/backtest_scored.rds and data/backtest_meta.rds")
