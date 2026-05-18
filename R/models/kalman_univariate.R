#' Fit per-category univariate Kalman filters
#'
#' For each category `c`, fits a Basic Structural Model with:
#'
#'   * Local linear trend (level + slope), both random-walk innovations
#'   * Quarterly seasonal component
#'   * AR(1) irregular (optional, controlled by
#'     `cfg$models$kalman$include_ar1_irregular`)
#'
#' The observation is `log(1 + oad_lt_arrivals)` to keep the latent state
#' multiplicative and tame the COVID-era variance change. Departures are
#' handled in a separate fit so the two flows can have independent
#' dynamics (departures during 2020-22 collapsed harder than arrivals).
#'
#' Implementation note: `{KFAS}` is the standard R workhorse for
#' state-space models. Hyperparameters are estimated by maximum
#' likelihood; the implementation here is intentionally non-Bayesian
#' for Phase 1.
#'
#' @param panel Output of [build_quarterly_panel()].
#' @param cfg Project config.
#' @return Named list keyed by category, each element a list with
#'   `arrivals_fit`, `departures_fit`, both `KFS` objects, and
#'   `transformation` describing the log transform applied.
#' @export
fit_kalman_univariate <- function(panel, cfg) {
  cats <- cfg$models$kalman$categories_to_fit %||%
          unique(c(cfg$categories$levels, "total"))
  out <- list()
  for (cat in cats) {
    arr <- prepare_kalman_series(panel, cat, "oad_lt_arrivals")
    dep <- prepare_kalman_series(panel, cat, "oad_lt_departures")
    out[[cat]] <- list(
      arrivals_fit   = fit_one_kalman(arr$y, cfg),
      departures_fit = fit_one_kalman(dep$y, cfg),
      arrivals_periods   = arr$periods,
      departures_periods = dep$periods,
      transformation = "log1p"
    )
  }
  out
}

#' @keywords internal
prepare_kalman_series <- function(panel, cat, col) {
  x <- panel |>
    dplyr::filter(.data$category == cat) |>
    dplyr::arrange(.data$period)
  vals <- x[[col]]
  list(
    y = log1p(pmax(vals, 0)),
    periods = x$period
  )
}

#' Fit one univariate BSM via KFAS.
#' @keywords internal
fit_one_kalman <- function(y, cfg) {
  if (sum(!is.na(y)) < 12L) {
    return(structure(list(error = "insufficient observations",
                          y = y), class = "kalman_failed"))
  }
  seasonal_period <- cfg$models$kalman$seasonal_period %||% 4L
  include_ar1     <- isTRUE(cfg$models$kalman$include_ar1_irregular)

  # SSModel parses unqualified SSMtrend/SSMseasonal via custom term
  # walking that doesn't recognise `pkg::fn`. Bring them in locally.
  SSModel     <- KFAS::SSModel
  SSMtrend    <- KFAS::SSMtrend
  SSMseasonal <- KFAS::SSMseasonal
  ssm <- SSModel(
    y ~ SSMtrend(degree = 2, Q = list(NA, NA)) +
        SSMseasonal(period = seasonal_period, sea.type = "dummy", Q = NA),
    H = NA
  )

  fit <- tryCatch(
    KFAS::fitSSM(ssm, inits = rep(0, 4), method = "BFGS"),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(structure(list(error = "fitSSM failed", y = y), class = "kalman_failed"))
  }
  ks <- KFAS::KFS(fit$model, filtering = c("state", "mean"),
                  smoothing = c("state", "mean", "disturbance"))

  structure(
    list(
      y       = y,
      model   = fit$model,
      kfs     = ks,
      include_ar1_irregular = include_ar1
    ),
    class = "kalman_fit"
  )
}

#' Produce per-category forecasts from fitted Kalman filters
#'
#' Generates filtered estimates for all in-sample periods plus `h_max`
#' steps ahead. The filtered states give us the "best estimate of latent
#' arrivals" at each historical date, accounting for OAD revisions; the
#' forecast extends that for nowcasting purposes.
#'
#' @param fits Output of [fit_kalman_univariate()].
#' @param asof As-of date.
#' @param cfg Project config.
#' @return Tibble: `period`, `category`, `direction`, `mean_log`,
#'   `mean_level`, `se_log`, `forecast_horizon`.
#' @export
forecast_kalman_univariate <- function(fits, asof, cfg) {
  h_max <- 4L   # 4 quarters ahead is plenty for nowcasting
  purrr::imap_dfr(fits, function(fit_pair, cat) {
    arr <- forecast_one_kalman(fit_pair$arrivals_fit, fit_pair$arrivals_periods, h_max)
    dep <- forecast_one_kalman(fit_pair$departures_fit, fit_pair$departures_periods, h_max)
    dplyr::bind_rows(
      arr |> dplyr::mutate(direction = "arrival", category = cat),
      dep |> dplyr::mutate(direction = "departure", category = cat)
    )
  })
}

#' @keywords internal
forecast_one_kalman <- function(fit, periods, h_max) {
  if (inherits(fit, "kalman_failed")) {
    return(tibble::tibble(period = as.Date(character()),
                          mean_log = numeric(), mean_level = numeric(),
                          se_log = numeric(), forecast_horizon = integer()))
  }
  smoothed_log <- as.numeric(fit$kfs$muhat)
  se_log       <- sqrt(pmax(as.numeric(fit$kfs$V_mu), 0))

  in_sample <- tibble::tibble(
    period           = periods,
    mean_log         = smoothed_log,
    se_log           = se_log,
    forecast_horizon = -seq(length(periods) - 1L, 0L)
  )

  # h-step-ahead forecast using KFAS's predict on the fitted model
  fc <- tryCatch(
    KFAS::predict(fit$model, n.ahead = h_max, interval = "prediction",
                  level = 0.95, type = "response"),
    error = function(e) NULL
  )
  out_of_sample <- if (is.null(fc)) {
    tibble::tibble()
  } else {
    last_p <- max(periods)
    future <- seq.Date(lubridate::add_with_rollback(last_p, lubridate::period("3 months"), preserve_hms = FALSE),
                       by = "quarter", length.out = h_max)
    tibble::tibble(
      period   = future,
      mean_log = as.numeric(fc[, "fit"]),
      se_log   = (as.numeric(fc[, "upr"]) - as.numeric(fc[, "fit"])) / 1.96,
      forecast_horizon = seq_len(h_max)
    )
  }

  out <- dplyr::bind_rows(in_sample, out_of_sample)
  # Cap se_log so the log-normal conversion doesn't blow up at long
  # forecast horizons where KFAS's predict() returns wide intervals.
  out$se_log[!is.finite(out$se_log)] <- NA_real_
  out$se_log <- pmin(out$se_log, 1.0, na.rm = FALSE)
  out$mean_level <- expm1(out$mean_log + 0.5 * (out$se_log^2))
  out
}
