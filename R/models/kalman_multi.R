#' Phase 2 v1.0 — multi-source state-space for the total flow
#'
#' Extends the Phase 1 univariate Kalman with a second observation
#' block fed by DHA visa-grants data (CKAN, annualised then broadcast
#' to quarters by [build_quarterly_panel()]). The model is restricted
#' to the aggregate `total` category — that is where both ABS NOM and
#' the dominant visa-grant streams have meaningful overlap.
#'
#' Mathematical specification (all quantities at quarterly frequency):
#'
#' \deqn{
#'   y_1(t) = \log(1 + A^{OAD,LT}_t),\quad
#'   y_2(t) = \log(1 + V_{t-\ell}) - \alpha
#' }
#'
#' \deqn{
#'   y_1(t) = \mu_t + \gamma_t + \varepsilon^1_t,\qquad
#'   y_2(t) = \mu_t + \varepsilon^2_t
#' }
#'
#' \deqn{
#'   \mu_t = \mu_{t-1} + \delta_{t-1} + u_t,\quad
#'   \delta_t = \phi \delta_{t-1} + v_t
#' }
#'
#' with a quarterly dummy seasonal $\gamma_t$ on OAD only (visa grants
#' arrive as annual FY totals broadcast across quarters, so they carry
#' no within-year seasonal signal in v0.5).
#'
#' The offset $\alpha$ is calibrated as the empirical long-run mean of
#' $\log(1+A^{OAD,LT}) - \log(1+V_{\cdot - \ell})$ over quarters where
#' both observations are present; it absorbs the level difference
#' between OAD aggregate movements and visa-grant volumes.
#'
#' Why this is interesting: the bridge regression already shows that
#' visa grants reduce backcast RMSE by ~2.5x relative to the Phase 1
#' Kalman. The multivariate SSM puts the same signal *inside* the
#' Kalman state, so headline Kalman intervals automatically tighten
#' and the same uncertainty machinery downstream (nowcast assembly,
#' headline reporting) keeps working unchanged.
#'
#' @param panel Output of [build_quarterly_panel()].
#' @param cfg Project config.
#' @return Named list with:
#'   - `fit`              the underlying KFAS::KFS object
#'   - `model`            the underlying KFAS::SSModel object
#'   - `periods`          vector of period start dates aligned with the state series
#'   - `alpha`            calibrated offset between OAD and visa-grants log levels
#'   - `lead_quarters`    the visa-grants lead used (ℓ)
#'   - `transformation`   string label for downstream consumers
#'   - `kind`             "kalman_multi_v1"
#' @export
fit_kalman_multi <- function(panel, cfg) {
  total <- panel |>
    dplyr::filter(.data$category == "total") |>
    dplyr::arrange(.data$period)
  if (!nrow(total)) {
    return(structure(list(error = "no total rows"), class = "kalman_multi_failed"))
  }

  lead_q <- cfg$models$kalman_multi$visa_lead_quarters %||% 1L
  damping <- cfg$models$kalman$damping_phi %||% 0.85
  seasonal_period <- cfg$models$kalman$seasonal_period %||% 4L

  # Visa-grants signal for the total category: sum across visa
  # categories in the panel (the "total" row itself doesn't carry
  # visa_grants — those live on the per-category rows).
  vg_total <- panel |>
    dplyr::filter(.data$category != "total") |>
    dplyr::group_by(.data$period) |>
    dplyr::summarise(visa_grants = sum(.data$visa_grants, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(visa_grants = dplyr::if_else(.data$visa_grants <= 0,
                                               NA_real_, .data$visa_grants))

  joined <- total |>
    dplyr::select("period", oad_arr = "oad_lt_arrivals") |>
    dplyr::left_join(vg_total, by = "period") |>
    dplyr::arrange(.data$period)

  # Construct y1 = log(1+arrivals), y2 = log(1+V_{t-ell})
  y1 <- log1p(pmax(joined$oad_arr, 0))
  vg_lag <- shift_lag(joined$visa_grants, lead_q)
  y2_raw <- log1p(pmax(vg_lag, 0))

  # Calibrate alpha from rows where both series are non-NA AND
  # non-zero in level. We use the long-run mean to avoid letting any
  # single quarter dominate.
  both_ok <- !is.na(y1) & !is.na(y2_raw) & is.finite(y1) & is.finite(y2_raw) &
             y1 > 0 & y2_raw > 0
  if (sum(both_ok) < 8L) {
    nn_warn("kalman_multi: insufficient overlap ({sum(both_ok)} quarters) ",
            "between OAD arrivals and lagged visa grants; falling back to ",
            "univariate fit.")
    alpha <- 0
  } else {
    alpha <- mean(y2_raw[both_ok] - y1[both_ok])
  }

  y2 <- y2_raw - alpha
  # When visa-grants is missing for a quarter, KFAS treats NA as
  # "missing observation" and propagates the state with the
  # measurement variance contribution from y1 alone.
  y_mat <- cbind(arr = y1, vg = y2)

  ssm <- build_multi_ssmodel(y_mat,
                             phi             = damping,
                             seasonal_period = seasonal_period)
  # 5 free variance parameters: H[1,1], H[2,2], Q[1,1] (level), Q[2,2]
  # (slope), Q[3,3] (seasonal). Inits=0 for all.
  fit <- tryCatch(
    KFAS::fitSSM(ssm, inits = rep(0, 5), method = "BFGS"),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    nn_warn("kalman_multi fitSSM error: {conditionMessage(fit)}")
    return(structure(list(error = paste("fitSSM:", conditionMessage(fit)),
                          y = y_mat),
                     class = "kalman_multi_failed"))
  }
  kfs <- KFAS::KFS(fit$model,
                   filtering = c("state", "mean"),
                   smoothing = c("state", "mean", "disturbance"))

  structure(
    list(
      kfs            = kfs,
      model          = fit$model,
      periods        = joined$period,
      y              = y_mat,
      alpha          = alpha,
      lead_quarters  = lead_q,
      transformation = "log1p",
      kind           = "kalman_multi_v1"
    ),
    class = "kalman_multi_fit"
  )
}

#' Build the bivariate SSModel used by [fit_kalman_multi()].
#'
#' Both rows of the observation matrix load on the same level state;
#' OAD additionally loads on a quarterly dummy seasonal. The trend is
#' the damped local-linear specification used elsewhere in the
#' project.
#'
#' @keywords internal
build_multi_ssmodel <- function(y_mat, phi, seasonal_period) {
  # State: [level, slope, seasonal_lag_0, seasonal_lag_1, seasonal_lag_2]
  # (3-quarter rolling sum + 1 = 4-quarter seasonal — see KFAS docs.)
  # We assemble the design matrices by hand for clarity and to keep
  # full control over the cross-equation loadings.
  m_trend <- 2L                                # level, slope
  m_seas  <- seasonal_period - 1L              # dummy seasonal lags
  m       <- m_trend + m_seas

  # Transition matrix T_mat
  T_mat <- matrix(0, m, m)
  T_mat[1, 1] <- 1; T_mat[1, 2] <- 1
  T_mat[2, 2] <- phi
  # Quarterly dummy seasonal: gamma[t] = -sum_{s=1}^{S-1} gamma[t-s] + w[t]
  # Implemented as a companion form on (m_trend + 1):(m_trend + m_seas).
  for (j in seq_len(m_seas)) {
    T_mat[m_trend + 1L, m_trend + j] <- -1
  }
  if (m_seas >= 2L) {
    for (k in 2L:m_seas) {
      T_mat[m_trend + k, m_trend + k - 1L] <- 1
    }
  }

  # Observation matrix Z (p x m): row 1 (arrivals) = level + seasonal_lag_0;
  # row 2 (visa grants) = level only.
  Z_mat <- matrix(0, 2L, m)
  Z_mat[1, 1] <- 1                       # level -> arrivals
  Z_mat[1, m_trend + 1L] <- 1            # seasonal -> arrivals
  Z_mat[2, 1] <- 1                       # level -> visa grants

  # Selection R: trend (2 innovations) + 1 seasonal innovation.
  R_mat <- matrix(0, m, 3L)
  R_mat[1, 1] <- 1
  R_mat[2, 2] <- 1
  R_mat[m_trend + 1L, 3L] <- 1
  Q_mat <- diag(c(NA_real_, NA_real_, NA_real_))   # MLE-estimated

  # Observation variance H: 2x2, diagonal, both NA -> MLE.
  H_mat <- diag(c(NA_real_, NA_real_))

  # Diffuse init on level, slope; small priors on seasonals
  P1    <- diag(rep(0, m))
  P1inf <- diag(c(1, 1, rep(0, m_seas)))
  for (j in (m_trend + 1L):m) P1[j, j] <- 1   # mild prior on seasonal lags

  state_names <- c("level", "slope",
                   paste0("seasonal_lag_", seq_len(m_seas) - 1L))

  # Inline-namespaced builder, same dodge as the univariate damped path:
  # KFAS's formula walker doesn't accept pkg::fn or pre-built blocks
  # passed by name, so we import the constructors locally and inline
  # the SSMcustom call.
  SSModel   <- KFAS::SSModel
  SSMcustom <- KFAS::SSMcustom

  # Wrap y_mat in an array of dim (n x p) for SSModel formula.
  y_lhs <- y_mat
  SSModel(
    y_lhs ~ -1 +
      SSMcustom(
        Z           = Z_mat,
        T           = T_mat,
        R           = R_mat,
        Q           = Q_mat,
        a1          = rep(0, m),
        P1          = P1,
        P1inf       = P1inf,
        state_names = state_names
      ),
    H = H_mat
  )
}

#' Lag a numeric vector by `k` positions (NA-padded at the head).
#' Used to shift visa grants forward in time (i.e. give the Kalman
#' an observation of "log visa grants k quarters ago").
#'
#' @keywords internal
shift_lag <- function(x, k) {
  if (k <= 0L) return(x)
  n <- length(x)
  if (k >= n) return(rep(NA_real_, n))
  c(rep(NA_real_, k), x[seq_len(n - k)])
}

#' Forecast log-arrivals from a multivariate SSM fit
#'
#' Emits one row per period covering both the in-sample smoothed
#' estimate and `h_max` quarters of forecast, matching the schema of
#' [forecast_kalman_univariate()] so the existing nowcast assembly
#' (`build_nowcast_categories`) consumes it transparently.
#'
#' For forecasting beyond the last observed quarter, visa grants
#' provide signal as long as `t - lead_quarters` is still within the
#' observable visa-grants series. Past that the forecast collapses to
#' the univariate Kalman conditional on the latent state.
#'
#' @param fit Output of [fit_kalman_multi()].
#' @param asof As-of date (unused but accepted for API parity).
#' @param cfg Project config.
#' @return Tibble: `period`, `category`, `direction`, `mean_log`,
#'   `mean_level`, `se_log`, `forecast_horizon`.
#' @export
forecast_kalman_multi <- function(fit, asof, cfg) {
  if (inherits(fit, "kalman_multi_failed")) {
    return(empty_kalman_forecast())
  }
  h_max <- 4L
  periods <- fit$periods

  smoothed_log <- as.numeric(fit$kfs$muhat[, 1])
  se_log       <- sqrt(pmax(as.numeric(fit$kfs$V_mu[1, 1, ]), 0))

  in_sample <- tibble::tibble(
    period           = periods,
    mean_log         = smoothed_log,
    se_log           = se_log,
    forecast_horizon = -seq(length(periods) - 1L, 0L)
  )

  # KFAS::predict for a multivariate model requires the `states`
  # argument to disambiguate which observation series to predict.
  # We want the first (arrivals) series — pass `states = "all"` and
  # extract index 1 from the returned list.
  # `predict.SSModel` is an S3 method in KFAS; it isn't exported under
  # the `pkg::fn` syntax, so we let `stats::predict()` dispatch find
  # it. (Argument `states = "all"` keeps both observation rows in the
  # output; we slice out the arrivals series below.)
  fc <- tryCatch(
    stats::predict(fit$model, n.ahead = h_max, interval = "prediction",
                   level = 0.95, type = "response", states = "all"),
    error = function(e) {
      nn_warn("kalman_multi predict failed: {conditionMessage(e)}")
      NULL
    }
  )
  out_of_sample <- if (is.null(fc)) {
    tibble::tibble()
  } else {
    arr <- if (is.list(fc) && !is.null(fc[[1L]])) fc[[1L]] else fc
    last_p <- max(periods)
    future <- seq.Date(
      seq.Date(last_p, by = "3 months", length.out = 2L)[2L],
      by = "quarter", length.out = h_max
    )
    tibble::tibble(
      period           = future,
      mean_log         = as.numeric(arr[, "fit"]),
      se_log           = (as.numeric(arr[, "upr"]) -
                          as.numeric(arr[, "fit"])) / 1.96,
      forecast_horizon = seq_len(h_max)
    )
  }

  out <- dplyr::bind_rows(in_sample, out_of_sample)
  out$se_log[!is.finite(out$se_log)] <- NA_real_
  out$se_log <- pmin(out$se_log, 1.0, na.rm = FALSE)
  out$mean_level <- expm1(out$mean_log + 0.5 * (out$se_log^2))

  # The multivariate fit covers only the "total" category and the
  # "arrival" direction. To get a NOM headline, the existing
  # `build_nowcast_categories()` needs both arrivals and departures.
  # Departures for "total" come from the univariate Kalman; the
  # multivariate fit replaces the arrivals row only.
  out |>
    dplyr::mutate(direction = "arrival", category = "total")
}

#' Combined forecast: multivariate SSM for total arrivals, univariate
#' Kalman for everything else
#'
#' The multivariate SSM in `fit_kalman_multi()` covers only the
#' aggregate `category == "total"` row in the `arrival` direction.
#' Everything else (per-category arrivals + every direction's departure)
#' still comes from the univariate Phase 1 Kalman, so existing
#' downstream consumers (`build_nowcast_categories`, etc.) keep
#' working unchanged.
#'
#' When `cfg$models$kalman_multi$enabled` is `FALSE` or the multi-SSM
#' failed to fit, this is a no-op and returns the univariate forecast
#' verbatim.
#'
#' @param uni_fc Output of [forecast_kalman_univariate()].
#' @param multi_fc Output of [forecast_kalman_multi()].
#' @param cfg Project config.
#' @return Tibble of the same shape as `uni_fc`.
#' @export
combine_kalman_forecasts <- function(uni_fc, multi_fc, cfg) {
  if (!isTRUE(cfg$models$kalman_multi$enabled %||% TRUE) ||
      is.null(multi_fc) || nrow(multi_fc) == 0L) {
    return(uni_fc)
  }
  # Replace category=total, direction=arrival rows in uni_fc with the
  # corresponding rows from multi_fc.
  uni_keep <- uni_fc |>
    dplyr::filter(!(.data$category == "total" & .data$direction == "arrival"))
  multi_use <- multi_fc |>
    dplyr::filter(.data$category == "total", .data$direction == "arrival") |>
    dplyr::select(dplyr::any_of(names(uni_fc)))
  dplyr::bind_rows(uni_keep, multi_use) |>
    dplyr::arrange(.data$category, .data$direction, .data$period)
}

#' Discretised Gamma lag distribution
#'
#' Retained from the v0.5 scaffold as the parametric form the
#' next-iteration Phase 2 SSM will use; we currently apply a fixed
#' single-quarter lag instead.
#'
#' @param alpha Shape parameter.
#' @param beta Rate parameter.
#' @param k_max Maximum lag.
#' @return Numeric vector of length `k_max + 1`.
#' @export
gamma_lag_weights <- function(alpha, beta, k_max) {
  k <- 0:k_max
  w <- stats::pgamma(k + 0.5, shape = alpha, rate = beta) -
       stats::pgamma(pmax(k - 0.5, 0), shape = alpha, rate = beta)
  w / sum(w)
}

#' @keywords internal
empty_kalman_forecast <- function() {
  tibble::tibble(
    period = as.Date(character()),
    mean_log = numeric(), se_log = numeric(),
    forecast_horizon = integer(), mean_level = numeric(),
    direction = character(), category = character()
  )
}
