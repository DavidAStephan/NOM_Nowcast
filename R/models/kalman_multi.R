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
  include_nom <- isTRUE(cfg$models$kalman_multi$include_nom %||% TRUE)
  nom_alpha_tv <- isTRUE(cfg$models$kalman_multi$nom_alpha_time_varying %||% TRUE)

  # Parametric Gamma lag: when `gamma_lag.enabled` is TRUE, visa
  # grants observation y_2(t) is set to log(V_{t-K}) where K is the
  # max forward lag, and the Z matrix loads on K+1 lagged copies of
  # the level state with discretised Gamma(alpha, beta) weights.
  gl_cfg <- cfg$models$kalman_multi$gamma_lag %||% list(enabled = FALSE)
  use_gamma_lag <- isTRUE(gl_cfg$enabled %||% FALSE)
  gl_alpha <- gl_cfg$alpha %||% 2.0
  gl_beta  <- gl_cfg$beta  %||% 1.0
  K_max    <- gl_cfg$k_max %||% 4L
  visa_gamma_weights <- if (use_gamma_lag) {
    gamma_lag_weights(gl_alpha, gl_beta, K_max)
  } else NULL
  # When Gamma lag is active, override the single-quarter lead with
  # a K-quarter shift so V_{t-K} aligns with the state-augmented
  # window the Z matrix loads on.
  effective_lead <- if (use_gamma_lag) K_max else lead_q

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
    dplyr::select("period", oad_arr = "oad_lt_arrivals",
                  nom_final = "nom_final") |>
    dplyr::left_join(vg_total, by = "period") |>
    dplyr::arrange(.data$period)

  y1 <- log1p(pmax(joined$oad_arr, 0))
  vg_lag <- shift_lag(joined$visa_grants, effective_lead)
  y2_raw <- log1p(pmax(vg_lag, 0))
  alpha_v <- calibrate_offset(y2_raw, y1)
  y2 <- y2_raw - alpha_v

  alpha_n <- 0
  y3 <- NULL
  if (include_nom) {
    y3_raw <- log1p(pmax(joined$nom_final, 0))
    alpha_n <- calibrate_offset(y3_raw, y1)
    y3 <- y3_raw - alpha_n
  }

  y_mat <- if (include_nom) cbind(arr = y1, vg = y2, nom = y3)
           else             cbind(arr = y1, vg = y2)

  ssm <- build_multi_ssmodel(y_mat,
                             phi             = damping,
                             seasonal_period = seasonal_period,
                             include_nom     = include_nom,
                             nom_alpha_time_varying = nom_alpha_tv && include_nom,
                             visa_gamma_weights = visa_gamma_weights)
  # Free variance parameters:
  #   bivariate  (no NOM):          5 = 2 H + 3 Q
  #   trivariate static-offset:     6 = 3 H + 3 Q
  #   trivariate time-varying alpha 7 = 3 H + 3 Q + Q[beta_n]
  #
  # Initial values for the log-variance hyperparameters. With three
  # slope-1 observations on a shared level state the optimiser is
  # prone to collapse to an all-zero variance corner; seeding the
  # state-innovation variances at log(0.01)=-4.6 and the observation
  # variances at log(0.1)=-2.3 keeps it in the interior. beta_n's
  # innovation should be tiny (log(1e-4) = -9.2) to keep it slow.
  inits <- if (include_nom && nom_alpha_tv) {
    # v3 path: H[3,3] AND Q[beta_n] are both fixed (in the SSModel
    # itself), so we only estimate 5 params: H[1,1], H[2,2] and the
    # 3 state-innovation variances Q[level], Q[slope], Q[seasonal].
    c(-2.3, -2.3, -4.6, -4.6, -4.6)
  } else if (include_nom) {
    c(-2.3, -2.3, -2.3, -4.6, -4.6, -4.6)
  } else {
    c(-2.3, -2.3,        -4.6, -4.6, -4.6)
  }
  fit <- tryCatch(
    KFAS::fitSSM(ssm, inits = inits, method = "BFGS"),
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

  kind <- if (!include_nom) "kalman_multi_v1"
          else if (use_gamma_lag) "kalman_multi_v4"
          else if (nom_alpha_tv) "kalman_multi_v3"
          else "kalman_multi_v2"
  structure(
    list(
      kfs                 = kfs,
      model               = fit$model,
      periods             = joined$period,
      y                   = y_mat,
      alpha_v             = alpha_v,
      alpha_n             = alpha_n,
      include_nom         = include_nom,
      nom_alpha_tv        = nom_alpha_tv && include_nom,
      lead_quarters       = effective_lead,
      use_gamma_lag       = use_gamma_lag,
      visa_gamma_weights  = visa_gamma_weights,
      transformation      = "log1p",
      kind                = kind
    ),
    class = "kalman_multi_fit"
  )
}

#' Calibrate the mean log-gap between two series over overlapping
#' non-missing, non-zero quarters.
#'
#' Used to demean the visa-grants and NOM observation rows so they
#' line up with the OAD log-arrivals state at slope 1.
#'
#' @keywords internal
calibrate_offset <- function(y_target, y_reference, min_overlap = 8L) {
  ok <- !is.na(y_target) & !is.na(y_reference) &
        is.finite(y_target) & is.finite(y_reference) &
        y_target > 0 & y_reference > 0
  if (sum(ok) < min_overlap) {
    nn_warn("calibrate_offset: insufficient overlap ({sum(ok)} obs); ",
            "using zero offset.")
    return(0)
  }
  mean(y_target[ok] - y_reference[ok])
}

#' Build the bivariate SSModel used by [fit_kalman_multi()].
#'
#' Both rows of the observation matrix load on the same level state;
#' OAD additionally loads on a quarterly dummy seasonal. The trend is
#' the damped local-linear specification used elsewhere in the
#' project.
#'
#' @keywords internal
build_multi_ssmodel <- function(y_mat, phi, seasonal_period,
                                include_nom = FALSE,
                                nom_alpha_time_varying = FALSE,
                                visa_gamma_weights = NULL) {
  # State layout:
  #   [level, slope, seasonal_lag_0 ... seasonal_lag_(S-2),
  #    (mu_lag_1 ... mu_lag_K)?, (beta_n)?]
  #
  # The optional mu_lag block carries lagged copies of the level
  # state, so the visa-grants observation can load on (mu_{t-K},
  # mu_{t-K+1}, ..., mu_t) with Gamma weights — implementing the
  # parametric-Gamma-lag spec from the methodology doc.
  #
  # beta_n is the v3 time-varying NOM-offset state (off by default).
  m_trend <- 2L
  m_seas  <- seasonal_period - 1L
  m_alpha <- if (include_nom && nom_alpha_time_varying) 1L else 0L
  m_mulag <- if (!is.null(visa_gamma_weights)) length(visa_gamma_weights) - 1L
             else 0L
  m       <- m_trend + m_seas + m_mulag + m_alpha
  p       <- if (include_nom) 3L else 2L
  # State-vector index helpers
  i_level <- 1L
  i_slope <- 2L
  i_seas  <- m_trend + 1L
  i_mulag_start <- m_trend + m_seas + 1L
  i_mulag_end   <- m_trend + m_seas + m_mulag
  i_beta_n      <- if (m_alpha == 1L) m else NA_integer_

  T_mat <- matrix(0, m, m)
  T_mat[i_level, i_level] <- 1
  T_mat[i_level, i_slope] <- 1
  T_mat[i_slope, i_slope] <- phi
  for (j in seq_len(m_seas)) {
    T_mat[i_seas, m_trend + j] <- -1
  }
  if (m_seas >= 2L) {
    for (k in 2L:m_seas) {
      T_mat[m_trend + k, m_trend + k - 1L] <- 1
    }
  }
  # Lagged level chain. mu_lag_1[t+1] = level[t]; mu_lag_k[t+1] =
  # mu_lag_{k-1}[t] for k >= 2. Deterministic shifts, no innovation.
  if (m_mulag >= 1L) {
    T_mat[i_mulag_start, i_level] <- 1
    if (m_mulag >= 2L) {
      for (k in 2L:m_mulag) {
        T_mat[i_mulag_start + k - 1L, i_mulag_start + k - 2L] <- 1
      }
    }
  }
  if (m_alpha == 1L) {
    T_mat[i_beta_n, i_beta_n] <- 1            # beta_n random walk
  }

  Z_mat <- matrix(0, p, m)
  Z_mat[1, i_level] <- 1                     # level -> arrivals
  Z_mat[1, i_seas]  <- 1                     # seasonal -> arrivals
  if (m_mulag == 0L) {
    Z_mat[2, i_level] <- 1                   # level -> visa grants (v1.0/v2.0)
  } else {
    # Visa-grants row uses Gamma weights across mu_{t-K}, ..., mu_t.
    # By the V_{t-K} = sum_k w_k A_{t-K+k} derivation:
    #   mu_t (i_level)         -> w_K (last weight)
    #   mu_lag_1 (i_mulag_start)-> w_{K-1}
    #   ...
    #   mu_lag_K (i_mulag_end)  -> w_0
    w <- visa_gamma_weights
    Z_mat[2, i_level] <- w[length(w)]
    for (k in seq_len(m_mulag)) {
      Z_mat[2, i_mulag_start + k - 1L] <- w[length(w) - k]
    }
  }
  if (include_nom) {
    Z_mat[3, i_level] <- 1
    if (m_alpha == 1L) Z_mat[3, i_beta_n] <- 1
  }

  # Innovations: trend (2), seasonal (1), and optionally beta_n (1).
  # beta_n's innovation variance is FIXED rather than MLE-estimated;
  # if left free, the optimiser either lets it absorb every quarter's
  # NOM noise (degenerate perfect fit) or shrinks it to 0 and uses
  # H[3,3] >> 1 to "ignore" NOM. SD ≈ 0.1 per quarter (Q = 0.01) lets
  # the OAD-to-NOM log ratio drift ~0.5 over a couple of years —
  # roughly the magnitude observed in the 2020-2025 regime shift.
  # Innovations come from trend (level, slope) and seasonal only —
  # the mu-lag chain is purely deterministic shifts of the level
  # state, with no fresh disturbance. beta_n (if present) has its
  # own innovation.
  n_innov <- 3L + m_alpha
  R_mat <- matrix(0, m, n_innov)
  R_mat[i_level, 1L] <- 1
  R_mat[i_slope, 2L] <- 1
  R_mat[i_seas,  3L] <- 1
  if (m_alpha == 1L) R_mat[i_beta_n, 4L] <- 1
  Q_diag <- if (m_alpha == 1L) c(NA_real_, NA_real_, NA_real_, 0.01)
            else               c(NA_real_, NA_real_, NA_real_)
  Q_mat <- diag(Q_diag)

  # H matrix: observation variances. In v3 (time-varying alpha_n), fix
  # H[3,3] to a realistic NOM measurement noise (SD ~0.07 ~ 7% on log
  # scale, the rough magnitude of ABS NOM revisions). Otherwise the
  # MLE inflates it to "ignore" NOM and beta_n never moves.
  fix_h3 <- isTRUE(p == 3L) &&
            isTRUE(m_alpha == 1L)
  H_diag <- if (fix_h3) c(NA_real_, NA_real_, 0.005)
            else        rep(NA_real_, p)
  H_mat <- diag(H_diag)

  P1    <- diag(rep(0, m))
  # Diffuse init on level + slope (+ beta_n if used). The mu-lag
  # states get a LARGE FINITE prior rather than diffuse init — with
  # only 3 observations per period, having 2 + K diffuse states
  # overspecifies the diffuse filtering and KFAS warns "Number of
  # nonzero elements in Finf is not equal to the number of diffuse
  # states". A finite SD ~10 in log space is effectively uninformative.
  diff_init <- c(1, 1,                                       # level, slope
                 rep(0, m_seas),                             # seasonal
                 if (m_mulag > 0L) rep(0, m_mulag) else integer(0),
                 if (m_alpha == 1L) 1 else integer(0))
  P1inf <- diag(diff_init)
  for (j in (m_trend + 1L):(m_trend + m_seas)) P1[j, j] <- 1
  if (m_mulag >= 1L) {
    for (j in i_mulag_start:i_mulag_end) P1[j, j] <- 100   # SD = 10
  }

  state_names <- c("level", "slope",
                   paste0("seasonal_lag_", seq_len(m_seas) - 1L),
                   if (m_mulag > 0L) paste0("mu_lag_", seq_len(m_mulag)) else character(),
                   if (m_alpha == 1L) "beta_n" else character())

  SSModel   <- KFAS::SSModel
  SSMcustom <- KFAS::SSMcustom

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

  # When NOM is in the observation block, also pull the smoothed
  # predicted log-NOM (= mu_t + alpha_n + beta_n_t in v3 / = mu_t
  # alone in v2 with offset applied below).
  has_nom_row <- isTRUE(fit$include_nom)
  if (has_nom_row && dim(fit$kfs$muhat)[2L] >= 3L) {
    smoothed_nom_log <- as.numeric(fit$kfs$muhat[, 3])
    se_nom_log       <- sqrt(pmax(as.numeric(fit$kfs$V_mu[3, 3, ]), 0))
  } else {
    smoothed_nom_log <- rep(NA_real_, length(periods))
    se_nom_log       <- rep(NA_real_, length(periods))
  }

  in_sample <- tibble::tibble(
    period           = periods,
    mean_log         = smoothed_log,
    se_log           = se_log,
    nom_log_smoothed = smoothed_nom_log,
    nom_se_log_smoothed = se_nom_log,
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
    # KFAS::predict returns a *list* of matrices for multivariate
    # models, one per observation series. fc[[1]] = arrivals (y1);
    # fc[[3]] = NOM (y3) if present.
    fc_list <- if (is.list(fc)) fc else list(fc)
    arr <- fc_list[[1L]]
    last_p <- max(periods)
    future <- seq.Date(
      seq.Date(last_p, by = "3 months", length.out = 2L)[2L],
      by = "quarter", length.out = h_max
    )
    nom_log_oos <- if (has_nom_row && length(fc_list) >= 3L) {
      as.numeric(fc_list[[3L]][, "fit"])
    } else rep(NA_real_, h_max)
    nom_se_oos <- if (has_nom_row && length(fc_list) >= 3L) {
      (as.numeric(fc_list[[3L]][, "upr"]) -
       as.numeric(fc_list[[3L]][, "fit"])) / 1.96
    } else rep(NA_real_, h_max)
    tibble::tibble(
      period              = future,
      mean_log            = as.numeric(arr[, "fit"]),
      se_log              = (as.numeric(arr[, "upr"]) -
                             as.numeric(arr[, "fit"])) / 1.96,
      nom_log_smoothed    = nom_log_oos,
      nom_se_log_smoothed = nom_se_oos,
      forecast_horizon    = seq_len(h_max)
    )
  }

  out <- dplyr::bind_rows(in_sample, out_of_sample)
  out$se_log[!is.finite(out$se_log)] <- NA_real_
  out$se_log <- pmin(out$se_log, 1.0, na.rm = FALSE)
  out$mean_level <- expm1(out$mean_log + 0.5 * (out$se_log^2))

  # NOM forecast.
  #   v2.0 (static offset):    nom_log = mu + alpha_n (constant)
  #   v3.0 (drift + static):   nom_log = (mu + beta_n) + alpha_n where
  #                            muhat[, 3] is precisely the demeaned
  #                            log-NOM signal mu + beta_n.
  if (isTRUE(fit$include_nom %||% FALSE)) {
    alpha_n_static <- fit$alpha_n %||% 0
    if (isTRUE(fit$nom_alpha_tv %||% FALSE)) {
      out$nom_log    <- out$nom_log_smoothed + alpha_n_static
      out$nom_se_log <- out$nom_se_log_smoothed
    } else {
      out$nom_log    <- out$mean_log + alpha_n_static
      out$nom_se_log <- out$se_log
    }
    out$nom_se_log[!is.finite(out$nom_se_log)] <- NA_real_
    out$nom_se_log <- pmin(out$nom_se_log, 1.0, na.rm = FALSE)
    out$nom_level  <- expm1(out$nom_log + 0.5 * (out$nom_se_log^2))
  } else {
    out$nom_log    <- NA_real_
    out$nom_level  <- NA_real_
    out$nom_se_log <- NA_real_
  }
  # Don't surface the internal smoothed helpers further downstream.
  out$nom_log_smoothed    <- NULL
  out$nom_se_log_smoothed <- NULL

  # The multivariate fit covers only the "total" category and the
  # "arrival" direction. To get a NOM headline via the pi-route, the
  # existing `build_nowcast_categories()` needs both arrivals and
  # departures. The univariate Kalman covers departures and per-
  # category arrivals; the multivariate fit replaces the total /
  # arrival row only (and, if NOM is in the block, exposes a direct
  # NOM forecast via the *_nom columns).
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

#' Build a NOM headline directly from a trivariate (v2) multi-SSM fit
#'
#' When `cfg$models$kalman_multi$include_nom` is TRUE, the multi-SSM
#' carries log(NOM) as a third observation row and produces a NOM
#' forecast directly from the latent state via the calibrated
#' offset $\alpha_n$. This bypasses the empirical-pi extrapolation,
#' which is the principal weakness of the v1.0 pi-based assembly
#' under regime shifts.
#'
#' The headline schema matches [build_nowcast_headline()] so existing
#' downstream consumers (reports, backtest scoring) work unchanged.
#'
#' @param multi_fc Output of [forecast_kalman_multi()] for a v2 fit.
#' @param cfg Project config.
#' @return Tibble: `period`, `nom_mean`, `nom_se`, `lower_80`,
#'   `upper_80`, `lower_95`, `upper_95`. Empty when the input lacks
#'   a NOM column.
#' @export
build_nowcast_headline_from_multi <- function(multi_fc, cfg) {
  empty <- tibble::tibble(
    period = as.Date(character()), nom_mean = numeric(),
    nom_se = numeric(),
    lower_80 = numeric(), upper_80 = numeric(),
    lower_95 = numeric(), upper_95 = numeric()
  )
  if (is.null(multi_fc) || !nrow(multi_fc) ||
      !"nom_level" %in% names(multi_fc) ||
      all(is.na(multi_fc$nom_level))) {
    return(empty)
  }
  ci80 <- cfg$reporting$headline_ci  %||% 0.80
  ci95 <- cfg$reporting$secondary_ci %||% 0.95
  z80 <- stats::qnorm(0.5 + ci80 / 2)
  z95 <- stats::qnorm(0.5 + ci95 / 2)

  multi_fc |>
    dplyr::filter(.data$category == "total", .data$direction == "arrival",
                  !is.na(.data$nom_level)) |>
    dplyr::transmute(
      .data$period,
      nom_mean = .data$nom_level,
      # Log-normal SD: SD[X] = mu * sqrt(exp(sigma^2) - 1)
      nom_se   = .data$nom_level *
                 sqrt(pmax(expm1((.data$nom_se_log)^2), 0)),
      lower_80 = pmax(0, .data$nom_level - z80 * .data$nom_level *
                     sqrt(pmax(expm1((.data$nom_se_log)^2), 0))),
      upper_80 = .data$nom_level + z80 * .data$nom_level *
                 sqrt(pmax(expm1((.data$nom_se_log)^2), 0)),
      lower_95 = pmax(0, .data$nom_level - z95 * .data$nom_level *
                     sqrt(pmax(expm1((.data$nom_se_log)^2), 0))),
      upper_95 = .data$nom_level + z95 * .data$nom_level *
                 sqrt(pmax(expm1((.data$nom_se_log)^2), 0))
    )
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
