#' Phase 3 v0.6 — Bayesian headline SSM with Kalman-filter likelihood.
#'
#' Same observation structure as [fit_bayes_gamma()] but the latent
#' state trajectory is integrated out via a forward Kalman filter,
#' so only ~10 static parameters are sampled. This eliminates the
#' sigma_oad ↔ sigma_mu identifiability funnel that saturates HMC
#' tree depth in the latent-state versions.
#'
#' One linearisation: the bayes_gamma visa equation
#'   y_visa[t] = log_sum_exp(log_w[k] + mu[t + k]) + alpha_v + eps
#' is replaced with its first-order Taylor approximation
#'   y_visa[t] ≈ sum_k w_k * mu[t + k] + alpha_v + eps
#' This is exact when mu is flat across the lag window and close in
#' practice; it makes the visa observation linear-Gaussian, hence
#' Kalman-compatible.
#'
#' Only forward forecasts (t = T_obs + 1 .. T_obs + H) are emitted;
#' for in-sample smoothing, run bayes_gamma or build a Kalman
#' smoother (Phase 3 v0.7 follow-up).
#'
#' @param panel Output of [build_quarterly_panel()].
#' @param asof  Reference date.
#' @param cfg   Project config.
#' @return Headline forecast tibble (one row per period, model =
#'   "bayes_kalman").
#' @export
fit_bayes_kalman <- function(panel, asof, cfg) {
  bcfg <- cfg$models$bayes_kalman %||% list(enabled = FALSE)
  if (!isTRUE(bcfg$enabled %||% FALSE)) {
    return(empty_bayes_headline())
  }
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    nn_warn("bayes_kalman: cmdstanr not installed; skipping")
    return(empty_bayes_headline())
  }

  total <- panel |>
    dplyr::filter(.data$category == "total") |>
    dplyr::arrange(.data$period)
  if (!nrow(total)) return(empty_bayes_headline())

  vg_total <- panel |>
    dplyr::filter(.data$category != "total") |>
    dplyr::group_by(.data$period) |>
    dplyr::summarise(visa_grants = sum(.data$visa_grants, na.rm = TRUE),
                     .groups = "drop")
  total <- total |>
    dplyr::select(-dplyr::any_of("visa_grants")) |>
    dplyr::left_join(vg_total, by = "period")

  warmup_q <- bcfg$pre_nom_quarters %||% 8L
  first_nom <- which(!is.na(total$nom_final))[1L]
  if (length(first_nom) && !is.na(first_nom)) {
    start <- max(1L, first_nom - warmup_q)
    total <- total[start:nrow(total), , drop = FALSE]
  }

  K_max <- bcfg$gamma_lag$k_max %||% 4L
  n_w   <- K_max + 1L
  gl_a  <- bcfg$gamma_lag$alpha %||% 2.0
  gl_b  <- bcfg$gamma_lag$beta  %||% 1.0
  weights <- gamma_lag_weights(gl_a, gl_b, K_max)
  H <- bcfg$forecast_horizons %||% 4L

  obs_periods <- total$period
  future_periods <- seq.Date(
    seq.Date(max(obs_periods), by = "3 months", length.out = 2L)[2L],
    by = "quarter", length.out = H
  )
  all_periods <- c(obs_periods, future_periods)

  log_or_na <- function(x) {
    out <- suppressWarnings(log(x))
    out[!is.finite(out)] <- NA_real_
    out
  }
  y_oad  <- log_or_na(total$oad_lt_arrivals)
  y_visa <- log_or_na(total$visa_grants)
  y_nom  <- log_or_na(total$nom_final)

  stan_data <- list(
    T_obs    = length(obs_periods),
    H        = as.integer(H),
    K_max    = as.integer(K_max),
    n_w      = as.integer(n_w),
    y_oad    = ifelse(is.na(y_oad),  0, y_oad),
    has_oad  = as.integer(!is.na(y_oad)),
    y_visa   = ifelse(is.na(y_visa), 0, y_visa),
    has_visa = as.integer(!is.na(y_visa)),
    y_nom    = ifelse(is.na(y_nom),  0, y_nom),
    has_nom  = as.integer(!is.na(y_nom)),
    gamma_weights      = weights,
    mu0_loc            = bcfg$priors$mu0_loc           %||% 11.0,
    mu0_scale          = bcfg$priors$mu0_scale         %||% 1.5,
    slope0_scale       = bcfg$priors$slope0_scale      %||% 0.1,
    sigma_obs_scale    = bcfg$priors$sigma_obs_scale   %||% 0.3,
    sigma_state_scale  = bcfg$priors$sigma_state_scale %||% 0.1,
    alpha_scale        = bcfg$priors$alpha_scale       %||% 1.0,
    phi_loc            = bcfg$priors$phi_loc           %||% 0.85,
    phi_conc           = bcfg$priors$phi_conc          %||% 10,
    prior_only         = 0L
  )

  model <- bayes_kalman_compile()
  fit <- tryCatch(
    model$sample(
      data            = stan_data,
      seed            = bcfg$seed         %||% 20260520,
      chains          = bcfg$chains       %||% 2L,
      parallel_chains = bcfg$chains       %||% 2L,
      iter_warmup     = bcfg$iter_warmup  %||% 500L,
      iter_sampling   = bcfg$iter_sample  %||% 500L,
      adapt_delta     = bcfg$adapt_delta  %||% 0.9,
      max_treedepth   = bcfg$max_treedepth %||% 10L,
      refresh         = 0,
      show_messages   = FALSE,
      show_exceptions = FALSE
    ),
    error = function(e) {
      nn_warn("bayes_kalman sample failed: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(fit)) return(empty_bayes_headline())

  draws <- tryCatch(fit$draws("nom_hat", format = "draws_matrix"),
                    error = function(e) NULL)
  log_mean_draws <- tryCatch(fit$draws("nom_hat_log_mean", format = "draws_matrix"),
                             error = function(e) NULL)
  log_sd_draws   <- tryCatch(fit$draws("nom_hat_log_sd", format = "draws_matrix"),
                             error = function(e) NULL)
  if (is.null(draws) || is.null(log_mean_draws)) return(empty_bayes_headline())
  draws          <- as.matrix(draws)
  log_mean_draws <- as.matrix(log_mean_draws)
  log_sd_draws   <- if (is.null(log_sd_draws)) NULL else as.matrix(log_sd_draws)

  # Build interval via log-normal sampling: simulate from each
  # posterior draw of (log_mean, log_sd) to get a draw of nom_hat,
  # then take quantiles across all sims.
  n_periods <- ncol(log_mean_draws)
  set.seed(20260520)
  nsims_per_draw <- 20L
  sim_draws <- matrix(NA_real_, nrow(log_mean_draws) * nsims_per_draw, n_periods)
  for (h in seq_len(n_periods)) {
    mu  <- log_mean_draws[, h]
    sd_ <- if (is.null(log_sd_draws)) rep(0, length(mu)) else log_sd_draws[, h]
    sim_draws[, h] <- exp(rnorm(length(mu) * nsims_per_draw,
                                mean = rep(mu, each = nsims_per_draw),
                                sd   = rep(sd_, each = nsims_per_draw)))
  }
  q <- function(p) apply(sim_draws, 2L, stats::quantile, probs = p, na.rm = TRUE)
  tibble::tibble(
    period   = all_periods,
    category = "total",
    nom_mean = apply(draws, 2L, mean),
    nom_se   = apply(sim_draws, 2L, sd),
    lower_80 = q(0.10),
    upper_80 = q(0.90),
    lower_95 = q(0.025),
    upper_95 = q(0.975),
    model    = "bayes_kalman"
  )
}

.bayes_kalman_model <- NULL

bayes_kalman_compile <- function(stan_file = "stan/headline_kalman_ssm.stan") {
  if (!is.null(.bayes_kalman_model)) return(.bayes_kalman_model)
  m <- cmdstanr::cmdstan_model(stan_file, compile = TRUE)
  assign(".bayes_kalman_model", m, envir = topenv())
  m
}
