## nomnowcast — pipeline definition
##
## Each section corresponds to a stage in the dependency graph. Targets
## that hit the network are intentionally cached aggressively: rebuilds
## only fire when the upstream file hash or the as-of date changes.

library(targets)
library(tarchetypes)

# Load every R/ file. Targets discovers function definitions only via
# `tar_source()` — package-style namespacing would also work but keeps
# iteration slower.
tar_source("R")

tar_option_set(
  packages = c(
    "dplyr", "tidyr", "tibble", "purrr", "stringr", "lubridate",
    "tsibble", "readr", "fs", "glue", "cli", "rlang", "yaml"
  ),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE,
  error = "continue",
  workspace_on_error = TRUE
)

# Per-project config snapshot. Anything that depends on it will rebuild
# when config.yml changes.
cfg <- config::get(file = "config.yml")
asof <- nn_asof(cfg)

list(
  # ----------------------------------------------------------------- config
  tar_target(config_file,      "config.yml", format = "file"),
  tar_target(cfg_target,       config::get(file = config_file)),
  tar_target(asof_date,        nn_asof(cfg_target)),

  # ----------------------------------------------------------------- vintages
  tar_target(vintage_db,       vintages_init(cfg_target), format = "file"),

  # ----------------------------------------------------------------- ingest
  # Each fetcher writes raw artifacts to `data/raw/<source>/<asof>/...`
  # and returns a tibble. They check for cached downloads first.
  tar_target(oad_raw,          fetch_oad(cfg_target, asof_date)),
  tar_target(nom_raw,          fetch_nom(cfg_target, asof_date)),
  tar_target(visa_grants_raw,  fetch_visa_grants(cfg_target, asof_date)),
  tar_target(students_raw,     fetch_student_data(cfg_target, asof_date)),
  tar_target(bitre_raw,        fetch_bitre(cfg_target, asof_date)),
  tar_target(twh_raw,          fetch_temp_visa_holders(cfg_target, asof_date)),

  # Write each source into the vintage store. Returns nrows written.
  tar_target(oad_vintaged,         vintages_write(vintage_db, "oad",         oad_raw,         asof_date)),
  tar_target(nom_vintaged,         vintages_write(vintage_db, "nom",         nom_raw,         asof_date)),
  tar_target(visa_grants_vintaged, vintages_write(vintage_db, "visa_grants", visa_grants_raw, asof_date)),
  tar_target(students_vintaged,    vintages_write(vintage_db, "students",    students_raw,    asof_date)),
  tar_target(bitre_vintaged,       vintages_write(vintage_db, "bitre",       bitre_raw,       asof_date)),
  tar_target(twh_vintaged,         vintages_write(vintage_db, "twh",         twh_raw,         asof_date)),

  # ----------------------------------------------------------------- clean
  tar_target(oad_clean,         clean_oad(oad_raw, cfg_target)),
  tar_target(nom_clean,         clean_nom(nom_raw, cfg_target)),
  tar_target(visa_grants_clean, clean_visa_grants(visa_grants_raw, cfg_target)),
  tar_target(students_clean,    clean_students(students_raw, cfg_target)),

  # Quarterly aggregated panel by category, ready for modelling.
  tar_target(panel_quarterly,   build_quarterly_panel(
    oad_clean, nom_clean, visa_grants_clean, students_clean, cfg_target
  )),

  # ----------------------------------------------------------------- pi
  tar_target(pi_empirical,      estimate_pi_empirical(panel_quarterly, cfg_target)),
  tar_target(pi_smoothed,       smooth_pi(pi_empirical, cfg_target)),

  # ----------------------------------------------------------------- Phase 1 Kalman
  tar_target(kalman_fits,       fit_kalman_univariate(panel_quarterly, cfg_target)),
  tar_target(kalman_forecasts,  forecast_kalman_univariate(kalman_fits, asof_date, cfg_target)),

  # ----------------------------------------------------------------- nowcast
  tar_target(nowcast_categories, build_nowcast_categories(kalman_forecasts, pi_smoothed, cfg_target)),
  tar_target(nowcast_headline,   build_nowcast_headline(nowcast_categories, cfg_target)),

  # ----------------------------------------------------------------- benchmarks
  tar_target(bench_rw,    benchmark_random_walk(panel_quarterly, cfg_target)),
  tar_target(bench_ar1,   benchmark_ar1(panel_quarterly, cfg_target)),
  tar_target(bench_bridge, benchmark_bridge(panel_quarterly, cfg_target)),

  # ----------------------------------------------------------------- backtest
  tar_target(backtest_dates,  build_backtest_grid(cfg_target)),
  tar_target(backtest_runs,   run_backtest(backtest_dates, vintage_db, cfg_target),
             pattern = map(backtest_dates), iteration = "list"),
  tar_target(backtest_scored, score_backtest(backtest_runs, vintage_db, cfg_target)),

  # ----------------------------------------------------------------- reports
  # Skipped automatically when the quarto CLI isn't on the PATH so the
  # manifest still builds in CI / minimal environments.
  if (nzchar(Sys.which("quarto"))) {
    list(
      tar_quarto(report_nowcast,     path = "reports/nowcast_report.qmd"),
      tar_quarto(report_methodology, path = "reports/methodology.qmd"),
      tar_quarto(report_backtest,    path = "reports/backtest_report.qmd")
    )
  } else {
    NULL
  }
)
