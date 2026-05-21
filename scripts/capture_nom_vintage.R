#!/usr/bin/env Rscript
## scripts/capture_nom_vintage.R
##
## Fetch ABS NOM (cat. 3101.0) as it stands today and append the
## resulting (vintage_date, period, category, value) rows to
## data/vintages/nom_history.csv. Idempotent — if a row already
## exists for today's vintage_date / period / category, leave it.
##
## Over time the CSV accumulates ABS' quarterly revisions, which is
## what enables a *real* (not pseudo-real-time) vintage backtest.
## See backtest.qmd "Methodology" for why this matters.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
})

files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
for (f in files) sys.source(f, envir = globalenv())

cfg  <- config::get(file = "config.yml")
asof <- Sys.Date()

cli::cli_alert_info("Fetching ABS NOM at vintage_date = {format(asof)}")
nom_raw   <- fetch_nom(cfg, asof)
nom_clean <- clean_nom(nom_raw, cfg)

if (!nrow(nom_clean)) {
  cli::cli_abort("NOM fetch returned no rows; nothing to record")
}

new_rows <- nom_clean |>
  dplyr::filter(.data$period_unit == "quarter", !is.na(.data$value)) |>
  dplyr::group_by(.data$period, .data$category) |>
  dplyr::summarise(value = sum(.data$value, na.rm = TRUE),
                   .groups = "drop") |>
  dplyr::transmute(
    vintage_date = asof,
    .data$period,
    .data$category,
    .data$value
  )

vintage_path <- "data/vintages/nom_history.csv"
fs::dir_create("data/vintages")
existing <- if (file.exists(vintage_path)) {
  readr::read_csv(vintage_path, show_col_types = FALSE)
} else {
  tibble::tibble(vintage_date = as.Date(character()),
                 period       = as.Date(character()),
                 category     = character(),
                 value        = numeric())
}

# Dedupe by (vintage_date, period, category) — last write wins, so a
# re-run on the same day overwrites the row.
combined <- dplyr::bind_rows(
    existing |> dplyr::anti_join(new_rows,
      by = c("vintage_date", "period", "category")),
    new_rows
  ) |>
  dplyr::arrange(.data$vintage_date, .data$period, .data$category)

readr::write_csv(combined, vintage_path)
cli::cli_alert_success(
  "Wrote {nrow(combined)} rows to {vintage_path} ({nrow(new_rows)} new at vintage_date = {format(asof)})"
)
