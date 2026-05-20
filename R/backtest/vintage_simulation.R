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
    visa_grants = vintages_read_asof(db_path, "visa_grants", asof_date)
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
  }
  oad_lag <- cfg$abs$publication_lag_days$oad %||% 35
  nom_lag <- cfg$abs$publication_lag_days$nom %||% 183
  vg_lag  <- cfg$homeaffairs$publication_lag_days %||% 45

  oad_clean    <- clean_oad(reenrich_oad(restrict_to_asof(vintages$oad, asof_date, oad_lag)), cfg)
  nom_clean    <- clean_nom(reenrich_nom(restrict_to_asof(vintages$nom, asof_date, nom_lag)), cfg)
  vg_clean     <- clean_visa_grants(reenrich_visa_grants(restrict_to_asof(vintages$visa_grants, asof_date, vg_lag)), cfg)

  panel <- build_quarterly_panel(oad_clean, nom_clean, vg_clean, cfg)
  if (nrow(panel) < 8L) {
    nn_warn("Insufficient panel data at {format(asof_date)} (n={nrow(panel)})")
    return(empty_backtest_run())
  }

  pi_emp <- estimate_pi_empirical(panel, cfg)
  pi_sm  <- smooth_pi(pi_emp, cfg)
  fits   <- fit_kalman_univariate(panel, cfg)
  fcs_uni <- forecast_kalman_univariate(fits, asof_date, cfg)
  fcs <- fcs_uni
  fit_multi <- NULL
  fcs_multi <- tibble::tibble()
  if (isTRUE(cfg$models$kalman_multi$enabled %||% TRUE)) {
    # Don't re-run the v5 Gamma grid search inside each backtest
    # branch — at ~60s per branch × ~60 quarters it blows the
    # workflow timeout. The production fit (in the main pipeline)
    # already searched; reuse its alpha/beta defaults from cfg.
    cfg_bt <- cfg
    cfg_bt$models$kalman_multi$gamma_lag$grid_search <- FALSE
    fit_multi <- tryCatch(fit_kalman_multi(panel, cfg_bt),
                          error = function(e) NULL)
    fcs_multi <- if (is.null(fit_multi)) tibble::tibble()
                 else tryCatch(forecast_kalman_multi(fit_multi, asof_date, cfg),
                               error = function(e) tibble::tibble())
    fcs <- combine_kalman_forecasts(fcs_uni, fcs_multi, cfg)
  }
  cats   <- build_nowcast_categories(fcs, pi_sm, cfg)
  # Headline: prefer the direct NOM forecast from the trivariate
  # multi-SSM (no pi); fall back to the pi-based assembly when v2.0
  # isn't active or the v2.0 forecast is empty.
  head_v2 <- build_nowcast_headline_from_multi(fcs_multi, cfg)
  head <- if (nrow(head_v2) > 0L) head_v2 else build_nowcast_headline(cats, cfg)

  # When multi-SSM is enabled we also produce a "kalman_v1" stream
  # from the pure-univariate forecast (pi-based) so the backtest can
  # quantify the marginal value of the multivariate observation block.
  head_v1 <- if (isTRUE(cfg$models$kalman_multi$enabled %||% TRUE)) {
    cats_v1 <- build_nowcast_categories(fcs_uni, pi_sm, cfg)
    build_nowcast_headline(cats_v1, cfg)
  } else NULL
  # Also expose the v1.0 (bivariate, pi-based) headline as its own
  # stream when v2.0 (trivariate, NOM-direct) is active, so the
  # backtest measures the marginal effect of the NOM observation row
  # specifically (separately from the visa-grants row).
  head_pi <- if (isTRUE(cfg$models$kalman_multi$include_nom %||% TRUE) &&
                 isTRUE(cfg$models$kalman_multi$enabled %||% TRUE)) {
    build_nowcast_headline(cats, cfg)
  } else NULL

  abs_prelim <- benchmark_abs_preliminary(panel, cfg)
  rw         <- benchmark_random_walk(panel, cfg)
  ar1        <- benchmark_ar1(panel, cfg)
  bridge_fits <- tryCatch(benchmark_bridge(panel, cfg), error = function(e) list())
  bridge_fc <- if (length(bridge_fits)) forecast_bridge(bridge_fits, panel, cfg) else tibble::tibble()

  # Phase 3 — Bayesian variants. Each fit is ~20-30s; we only run them
  # on the most recent `backtest_window` quarters of the grid to keep
  # the refresh cheap.
  bayes_window_floor <- function(window) {
    cutoff <- nn_quarter_start(Sys.Date()) - lubridate::days(1L)
    nn_quarter_start(cutoff) - months(3L * (window - 1L))
  }
  bcfg <- cfg$models$bayes_headline %||% list(enabled = FALSE)
  bh_window <- bcfg$backtest_window %||% 12L
  bayes_fc <- if (isTRUE(bcfg$enabled %||% FALSE) &&
                  asof_date >= bayes_window_floor(bh_window)) {
    tryCatch(fit_bayes_headline(panel, asof_date, cfg),
             error = function(e) {
               nn_warn("bayes_headline failed at {format(asof_date)}: {conditionMessage(e)}")
               tibble::tibble()
             })
  } else tibble::tibble()

  # Phase 3 v0.2 — Gamma-lag Bayesian headline.
  gcfg <- cfg$models$bayes_gamma %||% list(enabled = FALSE)
  bg_window <- gcfg$backtest_window %||% 12L
  bayes_gamma_fc <- if (isTRUE(gcfg$enabled %||% FALSE) &&
                       asof_date >= bayes_window_floor(bg_window)) {
    tryCatch(fit_bayes_gamma(panel, asof_date, cfg),
             error = function(e) {
               nn_warn("bayes_gamma failed at {format(asof_date)}: {conditionMessage(e)}")
               tibble::tibble()
             })
  } else tibble::tibble()

  # Phase 3 v0.3 — Gamma-lag with learned (alpha, beta).
  glcfg <- cfg$models$bayes_gamma_learned %||% list(enabled = FALSE)
  bgl_window <- glcfg$backtest_window %||% 12L
  bayes_gamma_learned_fc <- if (isTRUE(glcfg$enabled %||% FALSE) &&
                              asof_date >= bayes_window_floor(bgl_window)) {
    tryCatch(fit_bayes_gamma_learned(panel, asof_date, cfg),
             error = function(e) {
               nn_warn("bayes_gamma_learned failed at {format(asof_date)}: {conditionMessage(e)}")
               tibble::tibble()
             })
  } else tibble::tibble()

  # Phase 3 v0.6 — Kalman-marginal-likelihood Bayesian SSM.
  bkcfg <- cfg$models$bayes_kalman %||% list(enabled = FALSE)
  bk_window <- bkcfg$backtest_window %||% 12L
  bayes_kalman_fc <- if (isTRUE(bkcfg$enabled %||% FALSE) &&
                        asof_date >= bayes_window_floor(bk_window)) {
    tryCatch(fit_bayes_kalman(panel, asof_date, cfg),
             error = function(e) {
               nn_warn("bayes_kalman failed at {format(asof_date)}: {conditionMessage(e)}")
               tibble::tibble()
             })
  } else tibble::tibble()

  horizons <- cfg$backtest$horizons %||% c(-2, -1, 0, 1)
  ref_q <- nn_quarter_start(asof_date - lubridate::days(1))

  keeper <- horizons_at(ref_q, horizons)

  multi_active <- isTRUE(cfg$models$kalman_multi$enabled %||% TRUE)
  v2_active    <- multi_active &&
                  isTRUE(cfg$models$kalman_multi$include_nom %||% TRUE) &&
                  nrow(head_v2) > 0L
  headline_model <- if (v2_active) "kalman_multi_v2" else
                    if (multi_active) "kalman_multi" else "kalman_v1"

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
  if (v2_active && !is.null(head_pi)) {
    rows <- c(rows, list(
      head_pi |>
        dplyr::mutate(model = "kalman_multi_pi", category = "total") |>
        dplyr::semi_join(keeper, by = "period")
    ))
  }
  if (nrow(bayes_fc) > 0L) {
    rows <- c(rows, list(
      bayes_fc |>
        dplyr::mutate(category = "total") |>
        dplyr::semi_join(keeper, by = "period")
    ))
  }
  if (nrow(bayes_gamma_fc) > 0L) {
    rows <- c(rows, list(
      bayes_gamma_fc |>
        dplyr::mutate(category = "total") |>
        dplyr::semi_join(keeper, by = "period")
    ))
  }
  if (nrow(bayes_gamma_learned_fc) > 0L) {
    rows <- c(rows, list(
      bayes_gamma_learned_fc |>
        dplyr::mutate(category = "total") |>
        dplyr::semi_join(keeper, by = "period")
    ))
  }
  if (nrow(bayes_kalman_fc) > 0L) {
    rows <- c(rows, list(
      bayes_kalman_fc |>
        dplyr::mutate(category = "total") |>
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
#'
#' Session-cached: each `source` is fetched at most once per R session,
#' regardless of how many backtest dates call into it. Without this,
#' a flaky endpoint (e.g. education.gov.au returning HTTP/2 framing
#' errors) burns the full retry-with-backoff chain on every branch of
#' the `pattern = map(backtest_dates)` target — that's how CI ran out
#' the 45-min job timeout.
#' @keywords internal
.pseudo_vintage_cache <- new.env(parent = emptyenv())

pseudo_vintage_read <- function(cfg, source) {
  if (exists(source, envir = .pseudo_vintage_cache, inherits = FALSE)) {
    return(get(source, envir = .pseudo_vintage_cache, inherits = FALSE))
  }
  fetched <- switch(source,
    oad         = fetch_oad(cfg, Sys.Date()),
    nom         = fetch_nom(cfg, Sys.Date()),
    visa_grants = fetch_visa_grants(cfg, Sys.Date()),
    tibble::tibble()
  )
  out <- if (!nrow(fetched)) tibble::tibble() else fetched
  assign(source, out, envir = .pseudo_vintage_cache)
  out
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
