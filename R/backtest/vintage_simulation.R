#' Restrict a tibble of observations to what was observable at a date
#'
#' Given a series with monthly or quarterly periodicity and a known
#' publication lag, returns only the rows whose `(period + period_length
#' + lag_days)` is `<=` the as-of date.
#'
#' This is the workhorse of vintage-aware backtesting: leakage through
#' "the model saw data it shouldn't have" is the most common error in
#' nowcast evaluation, and centralising the check here is how we prevent
#' it.
#'
#' @param df Tibble with `period` and `period_unit` columns.
#' @param asof As-of date.
#' @param lag_days Publication lag in days.
#' @return Filtered tibble.
#' @export
restrict_to_asof <- function(df, asof, lag_days) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  if (!"period_unit" %in% names(df)) df$period_unit <- "month"
  asof <- as.Date(asof)
  cutoff <- nn_first_observable(df$period, df$period_unit[1], lag_days)
  df[cutoff <= asof, , drop = FALSE]
}

#' Build the list of backtest evaluation dates
#'
#' Quarterly dates between `cfg$backtest$start` and `cfg$backtest$end`
#' (defaulting to the last completed quarter at `asof`).
#'
#' @param cfg Project config.
#' @return Vector of `Date` (quarter starts).
#' @export
build_backtest_grid <- function(cfg) {
  start <- as.Date(cfg$backtest$start %||% "2010-01-01")
  end <- cfg$backtest$end
  end <- if (is.null(end) || isTRUE(is.na(end)) || identical(end, "")) {
    nn_last_completed_quarter(Sys.Date())
  } else as.Date(end)
  nn_quarter_seq(start, end)
}

#' Run a single backtest at a given as-of date
#'
#' Workflow:
#'
#'   1. Read each source as-of `asof_date` from the vintage store.
#'   2. Clean, build the quarterly panel.
#'   3. Estimate empirical π and smooth.
#'   4. Fit the univariate Kalman per category.
#'   5. Assemble nowcast and forecast horizons.
#'
#' The function is deliberately self-contained so that it can be called
#' by `{targets}`'s `pattern = map()` across a vector of dates.
#'
#' @param asof_date Date.
#' @param db_path Path to the DuckDB vintage store.
#' @param cfg Project config.
#' @return Tibble with one row per `(period, category, horizon, model)`.
#' @export
run_backtest <- function(asof_date, db_path, cfg) {
  nn_info("Backtest run: as-of {format(asof_date)}")
  vintages <- list(
    oad         = vintages_read_asof(db_path, "oad",         asof_date),
    nom         = vintages_read_asof(db_path, "nom",         asof_date),
    visa_grants = vintages_read_asof(db_path, "visa_grants", asof_date),
    students    = vintages_read_asof(db_path, "students",    asof_date)
  )
  # Pseudo-real-time mode: when no actual historical vintages have
  # been captured yet (vintage store empty for this source at
  # asof_date), fall back to the current data and filter by
  # publication lag. This loses true revision-vintage realism but is
  # the standard workaround for first-time backtesting before any
  # vintages have accumulated.
  pseudo_rt <- cfg$backtest$pseudo_real_time
  if (is.null(pseudo_rt)) pseudo_rt <- TRUE
  if (isTRUE(pseudo_rt)) {
    if (!nrow(vintages$oad))  vintages$oad <- pseudo_vintage_read(cfg, "oad")
    if (!nrow(vintages$nom))  vintages$nom <- pseudo_vintage_read(cfg, "nom")
    if (!nrow(vintages$visa_grants))
      vintages$visa_grants <- pseudo_vintage_read(cfg, "visa_grants")
    if (!nrow(vintages$students))
      vintages$students <- pseudo_vintage_read(cfg, "students")
  }
  oad_lag <- cfg$abs$publication_lag_days$oad %||% 35
  nom_lag <- cfg$abs$publication_lag_days$nom %||% 183
  vg_lag  <- cfg$homeaffairs$publication_lag_days %||% 45
  stu_lag <- cfg$education$publication_lag_days %||% 35

  oad_clean    <- clean_oad(reenrich_oad(restrict_to_asof(vintages$oad, asof_date, oad_lag)), cfg)
  nom_clean    <- clean_nom(reenrich_nom(restrict_to_asof(vintages$nom, asof_date, nom_lag)), cfg)
  vg_clean     <- clean_visa_grants(reenrich_visa_grants(restrict_to_asof(vintages$visa_grants, asof_date, vg_lag)), cfg)
  stu_clean    <- clean_students(reenrich_students(restrict_to_asof(vintages$students, asof_date, stu_lag)), cfg)

  panel <- build_quarterly_panel(oad_clean, nom_clean, vg_clean, stu_clean, cfg)
  if (nrow(panel) < 8L) {
    nn_warn("Insufficient panel data at {format(asof_date)} (n={nrow(panel)})")
    return(empty_backtest_run())
  }

  pi_emp <- estimate_pi_empirical(panel, cfg)
  pi_sm  <- smooth_pi(pi_emp, cfg)
  fits   <- fit_kalman_univariate(panel, cfg)
  fcs_uni <- forecast_kalman_univariate(fits, asof_date, cfg)
  fcs <- fcs_uni
  if (isTRUE(cfg$models$kalman_multi$enabled %||% TRUE)) {
    fit_multi <- tryCatch(fit_kalman_multi(panel, cfg),
                          error = function(e) NULL)
    fcs_multi <- if (is.null(fit_multi)) tibble::tibble()
                 else tryCatch(forecast_kalman_multi(fit_multi, asof_date, cfg),
                               error = function(e) tibble::tibble())
    fcs <- combine_kalman_forecasts(fcs_uni, fcs_multi, cfg)
  }
  cats   <- build_nowcast_categories(fcs, pi_sm, cfg)
  head   <- build_nowcast_headline(cats, cfg)

  # If multi-SSM is enabled we also produce a "kalman_v1" stream from
  # the pure-univariate forecast so the backtest can quantify the
  # marginal value of the multivariate observation block.
  head_v1 <- if (isTRUE(cfg$models$kalman_multi$enabled %||% TRUE)) {
    cats_v1 <- build_nowcast_categories(fcs_uni, pi_sm, cfg)
    build_nowcast_headline(cats_v1, cfg)
  } else NULL

  abs_prelim <- benchmark_abs_preliminary(panel, cfg)
  rw         <- benchmark_random_walk(panel, cfg)
  ar1        <- benchmark_ar1(panel, cfg)
  bridge_fits <- tryCatch(benchmark_bridge(panel, cfg), error = function(e) list())
  bridge_fc <- if (length(bridge_fits)) forecast_bridge(bridge_fits, panel, cfg) else tibble::tibble()

  horizons <- cfg$backtest$horizons %||% c(-2, -1, 0, 1)
  ref_q <- nn_quarter_start(asof_date - lubridate::days(1))

  keeper <- horizons_at(ref_q, horizons)

  multi_active <- isTRUE(cfg$models$kalman_multi$enabled %||% TRUE)
  headline_model <- if (multi_active) "kalman_multi" else "kalman_v1"

  rows <- list(
    head |>
      dplyr::mutate(model = headline_model, category = "total") |>
      dplyr::semi_join(keeper, by = "period"),
    cats |>
      dplyr::filter(.data$category != "total") |>
      dplyr::mutate(model = headline_model) |>
      dplyr::semi_join(keeper, by = "period"),
    abs_prelim   |> dplyr::semi_join(keeper, by = "period") |>
                    dplyr::mutate(model = "abs_preliminary"),
    rw           |> dplyr::semi_join(keeper, by = "period") |>
                    dplyr::mutate(model = "random_walk"),
    ar1          |> dplyr::semi_join(keeper, by = "period") |>
                    dplyr::mutate(model = "ar1"),
    bridge_fc    |> dplyr::semi_join(keeper, by = "period") |>
                    dplyr::mutate(model = "bridge")
  )
  if (multi_active && !is.null(head_v1)) {
    rows <- c(rows, list(
      head_v1 |>
        dplyr::mutate(model = "kalman_v1", category = "total") |>
        dplyr::semi_join(keeper, by = "period")
    ))
  }

  dplyr::bind_rows(rows) |>
    dplyr::mutate(asof_date = asof_date,
                  horizon   = quarters_between(.data$period, ref_q))
}

#' Fall back to current source data when the vintage store has none
#' for the requested as-of date. Returns a tibble in the same schema
#' [vintages_read_asof()] would emit.
#' @keywords internal
pseudo_vintage_read <- function(cfg, source) {
  fetched <- switch(source,
    oad         = fetch_oad(cfg, Sys.Date()),
    nom         = fetch_nom(cfg, Sys.Date()),
    visa_grants = fetch_visa_grants(cfg, Sys.Date()),
    students    = fetch_student_data(cfg, Sys.Date()),
    tibble::tibble()
  )
  if (!nrow(fetched)) return(tibble::tibble())
  # Coerce to the vintage-store schema (period, period_unit, value,
  # metadata + the original rich columns so reenrich is a no-op).
  fetched
}

#' @keywords internal
empty_backtest_run <- function() {
  tibble::tibble(
    period = as.Date(character()), category = character(),
    nom_mean = numeric(), nom_se = numeric(),
    lower_80 = numeric(), upper_80 = numeric(),
    lower_95 = numeric(), upper_95 = numeric(),
    model = character(), asof_date = as.Date(character()),
    horizon = integer()
  )
}

#' @keywords internal
horizons_at <- function(ref_q, horizons) {
  periods <- vapply(horizons, function(h) {
    seq.Date(ref_q, by = sprintf("%d months", 3L * h), length.out = 2L)[2L]
  }, FUN.VALUE = as.Date(NA))
  tibble::tibble(period = as.Date(periods, origin = "1970-01-01"),
                 horizon = horizons)
}

#' @keywords internal
quarters_between <- function(periods, ref_q) {
  as.integer(round(as.numeric(periods - ref_q) / 91.25))
}
