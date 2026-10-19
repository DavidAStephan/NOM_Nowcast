#' Regime-aware nowcast bias + coverage adjustment
#'
#' The structural-model nowcast under-predicts NOM at the live edge in
#' the post-COVID regime: every model family (bridge, pi-Kalman,
#' multivariate Kalman, all Bayesian SSMs) shows a large, one-signed
#' h = 0 bias (~-50k to -100k) that does NOT respond to model-internal
#' changes (visa-lag convention, state-innovation scale, an extra
#' onshore-stock covariate were all tried and failed). The bias is
#' therefore treated as a property of the data, not the model, and
#' corrected post-hoc.
#'
#' Because the bias is large but *stable and one-signed*, a two-scalar
#' calibration per (horizon, regime) removes it cleanly and restores
#' interval coverage. Leave-one-out validation on the post-COVID h = 0
#' backtest cut RMSE ~39% (62k -> 38k), drove bias to ~0 and lifted
#' cov80 from 0.50 toward 0.80.
#'
#' Calibration is multiplicative on the log scale (NOM is positive and
#' the SSM is log-based; this can never produce a negative bound):
#'
#'   * `factor` = mean(actual) / mean(pred)  — ratio-of-means centring,
#'     which makes the in-sample mean bias exactly zero.
#'   * `k80`, `k95` = inflation of the implied log-sd so the empirical
#'     coverage of the widened band hits its nominal level.
#'
#' Only the live horizons are corrected (h >= 0); the settled backcasts
#' (h < 0) are already near-unbiased and are left untouched.
#'
#' @name nowcast_adjustment
NULL

#' Calibrate the adjustment from backtest forecasts vs truth.
#'
#' @param runs Per-origin backtest forecasts (one concatenated tibble
#'   or a list) with columns `model, horizon, period, nom_mean,
#'   lower_80, upper_80, lower_95, upper_95`.
#' @param truth Tibble `period, category, nom_truth` (e.g.
#'   [latest_truth_nom()]).
#' @param cfg Project config (for regime classification).
#' @param model Which model stream to calibrate (default
#'   `"bayes_kalman"`).
#' @param regime Regime to calibrate within (default `"post_covid"`).
#' @param horizons Horizons to correct (default `c(0, 1)`).
#' @return Tibble `horizon, regime, n, factor, k80, k95`.
#' @export
calibrate_nowcast_adjustment <- function(runs, truth, cfg,
                                         model = "bayes_kalman",
                                         regime = "post_covid",
                                         horizons = c(0, 1)) {
  bt <- if (is.list(runs) && !is.data.frame(runs)) dplyr::bind_rows(runs) else runs
  z90 <- stats::qnorm(0.90); z975 <- stats::qnorm(0.975)

  df <- bt |>
    dplyr::filter(.data$model == !!model, .data$category == "total",
                  .data$horizon %in% horizons) |>
    dplyr::left_join(truth, by = c("period", "category")) |>
    dplyr::mutate(regime = classify_regime(.data$period, cfg)) |>
    dplyr::filter(.data$regime == !!regime,
                  !is.na(.data$nom_truth), .data$nom_mean > 0,
                  .data$lower_80 > 0, .data$upper_80 > 0)

  df |>
    dplyr::group_by(.data$horizon) |>
    dplyr::summarise(
      n      = dplyr::n(),
      factor = sum(.data$nom_truth) / sum(.data$nom_mean),
      k80 = {
        s  <- (log(.data$upper_80) - log(.data$lower_80)) / (2 * z90)
        f  <- sum(.data$nom_truth) / sum(.data$nom_mean)
        r  <- (log(.data$nom_truth) - (log(.data$nom_mean) + log(f))) / s
        as.numeric(stats::quantile(abs(r), 0.80, names = FALSE)) / z90
      },
      k95 = {
        s  <- (log(.data$upper_80) - log(.data$lower_80)) / (2 * z90)
        f  <- sum(.data$nom_truth) / sum(.data$nom_mean)
        r  <- (log(.data$nom_truth) - (log(.data$nom_mean) + log(f))) / s
        as.numeric(stats::quantile(abs(r), 0.95, names = FALSE)) / z975
      },
      .groups = "drop"
    ) |>
    dplyr::mutate(regime = regime, .after = "horizon")
}

#' Apply a calibrated adjustment to a forecast tibble.
#'
#' Rows are matched to the adjustment by horizon (h >= max correctable
#' horizon reuse the furthest calibrated horizon; h < 0 are returned
#' unchanged). The point is scaled by `factor`; the 80/95 bands are
#' recentred on the corrected point and widened by `k80`/`k95` in log
#' space.
#'
#' @param fc Forecast tibble with `period, nom_mean, lower_80, upper_80,
#'   lower_95, upper_95` and a `horizon` column (relative to the current
#'   quarter).
#' @param adj Output of [calibrate_nowcast_adjustment()].
#' @return `fc` with adjusted `nom_mean` and interval columns and an
#'   added logical `adjusted` flag.
#' @export
apply_nowcast_adjustment <- function(fc, adj) {
  if (is.null(adj) || !nrow(adj) || !nrow(fc)) {
    fc$adjusted <- FALSE
    return(fc)
  }
  z90 <- stats::qnorm(0.90); z975 <- stats::qnorm(0.975)
  hmax <- max(adj$horizon)
  pick <- function(h) {
    if (h < min(adj$horizon)) return(NULL)
    adj[adj$horizon == min(h, hmax), , drop = FALSE]
  }
  out <- fc
  out$adjusted <- FALSE
  for (i in seq_len(nrow(out))) {
    a <- pick(out$horizon[i])
    if (is.null(a) || !nrow(a)) next
    lo80 <- out$lower_80[i]; hi80 <- out$upper_80[i]; m0 <- out$nom_mean[i]
    if (is.na(lo80) || is.na(hi80) || is.na(m0) || lo80 <= 0 || hi80 <= 0 || m0 <= 0) next
    s   <- (log(hi80) - log(lo80)) / (2 * z90)            # implied log-sd
    m   <- log(m0) + log(a$factor)                        # corrected log-centre
    out$nom_mean[i] <- exp(m)
    out$lower_80[i] <- exp(m - z90  * a$k80 * s)
    out$upper_80[i] <- exp(m + z90  * a$k80 * s)
    out$lower_95[i] <- exp(m - z975 * a$k95 * s)
    out$upper_95[i] <- exp(m + z975 * a$k95 * s)
    if ("nom_se" %in% names(out)) out$nom_se[i] <- out$nom_se[i] * a$factor * a$k80
    out$adjusted[i] <- TRUE
  }
  out
}
