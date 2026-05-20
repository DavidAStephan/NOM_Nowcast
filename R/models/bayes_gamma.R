#' Phase 3 v0.2 — Bayesian headline SSM with Gamma-lag visa observation.
#'
#' Architecturally identical to [fit_bayes_headline()] except the
#' visa-grants observation row uses a log-sum-exp of forward-lagged
#' latent log-arrivals (weighted by a Gamma(alpha, beta) pmf) instead
#' of a single lagged copy. Matches kalman_v4's parametric Gamma lag
#' but with full posterior.
#'
#' The Gamma weights are *fixed* at the cfg-supplied (alpha, beta) —
#' learning them inside HMC is a Phase 3 v0.3 follow-up (would need
#' to evaluate Gamma at sampled (alpha, beta) and renormalise inside
#' the model block).
#'
#' @param panel Output of [build_quarterly_panel()].
#' @param asof  Reference date.
#' @param cfg   Project config.
#' @return Headline forecast tibble, one row per period, model = "bayes_gamma".
#' @export
fit_bayes_gamma <- function(panel, asof, cfg) {
  bcfg <- cfg$models$bayes_gamma %||% list(enabled = FALSE)
  if (!isTRUE(bcfg$enabled %||% FALSE)) {
    return(empty_bayes_headline())
  }
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    nn_warn("bayes_gamma: cmdstanr not installed; skipping")
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

  h_max  <- bcfg$forecast_horizons %||% 4L
  gl_a   <- bcfg$gamma_lag$alpha   %||% 2.0
  gl_b   <- bcfg$gamma_lag$beta    %||% 1.0
  K_max  <- bcfg$gamma_lag$k_max   %||% 4L
  weights <- gamma_lag_weights(gl_a, gl_b, K_max)   # length K_max + 1
  n_w     <- length(weights)

  obs_periods <- total$period
  # Latent state must extend far enough that every visa observation
  # has its full Gamma window in-bounds. Pad with max(h_max, n_w - 1)
  # future periods; we only report h_max in the output.
  pad <- max(h_max, n_w - 1L)
  future_all <- seq.Date(
    seq.Date(max(obs_periods), by = "3 months", length.out = 2L)[2L],
    by = "quarter", length.out = pad
  )
  all_periods <- c(obs_periods, future_all)
  T      <- length(all_periods)
  T_obs  <- length(obs_periods)

  log_or_na <- function(x) {
    out <- suppressWarnings(log(x))
    out[!is.finite(out)] <- NA_real_
    out
  }
  y_oad  <- log_or_na(total$oad_lt_arrivals)
  y_visa <- log_or_na(total$visa_grants)
  y_nom  <- log_or_na(total$nom_final)

  stan_data <- list(
    T = T, T_obs = T_obs, n_w = n_w,
    y_oad    = ifelse(is.na(y_oad),  0, y_oad),
    has_oad  = as.integer(!is.na(y_oad)),
    y_visa   = ifelse(is.na(y_visa), 0, y_visa),
    has_visa = as.integer(!is.na(y_visa)),
    y_nom    = ifelse(is.na(y_nom),  0, y_nom),
    has_nom  = as.integer(!is.na(y_nom)),
    gamma_weights = weights,
    mu0_loc           = bcfg$priors$mu0_loc           %||% 11.0,
    mu0_scale         = bcfg$priors$mu0_scale         %||% 1.5,
    slope0_scale      = bcfg$priors$slope0_scale      %||% 0.1,
    sigma_obs_scale   = bcfg$priors$sigma_obs_scale   %||% 0.3,
    sigma_state_scale = bcfg$priors$sigma_state_scale %||% 0.1,
    alpha_scale       = bcfg$priors$alpha_scale       %||% 1.0,
    phi_loc           = bcfg$priors$phi_loc           %||% 0.85,
    phi_conc          = bcfg$priors$phi_conc          %||% 10,
    prior_only        = 0L
  )

  model <- bayes_gamma_compile()
  fit <- tryCatch(
    model$sample(
      data            = stan_data,
      seed            = bcfg$seed         %||% 20260520,
      chains          = bcfg$chains       %||% 2L,
      parallel_chains = bcfg$chains       %||% 2L,
      iter_warmup     = bcfg$iter_warmup  %||% 300L,
      iter_sampling   = bcfg$iter_sample  %||% 300L,
      adapt_delta     = bcfg$adapt_delta  %||% 0.9,
      max_treedepth   = bcfg$max_treedepth %||% 10L,
      refresh         = 0,
      show_messages   = FALSE,
      show_exceptions = FALSE
    ),
    error = function(e) {
      nn_warn("bayes_gamma sample failed: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(fit)) return(empty_bayes_headline())

  draws <- tryCatch(fit$draws("nom_hat", format = "draws_matrix"),
                    error = function(e) NULL)
  if (is.null(draws)) return(empty_bayes_headline())
  draws <- as.matrix(draws)

  nom_mean <- apply(draws, 2L, mean)
  nom_sd   <- apply(draws, 2L, sd)
  q <- function(p) apply(draws, 2L, stats::quantile, probs = p, na.rm = TRUE)
  out <- tibble::tibble(
    period   = all_periods,
    category = "total",
    nom_mean = as.numeric(nom_mean),
    nom_se   = as.numeric(nom_sd),
    lower_80 = as.numeric(q(0.10)),
    upper_80 = as.numeric(q(0.90)),
    lower_95 = as.numeric(q(0.025)),
    upper_95 = as.numeric(q(0.975)),
    model    = "bayes_gamma"
  )
  # Drop the extra K_max forecast quarters past h_max
  out[seq_len(T_obs + h_max), , drop = FALSE]
}

.bayes_gamma_model <- NULL

bayes_gamma_compile <- function(stan_file = "stan/headline_gamma_ssm.stan") {
  if (!is.null(.bayes_gamma_model)) return(.bayes_gamma_model)
  m <- cmdstanr::cmdstan_model(stan_file, compile = TRUE)
  assign(".bayes_gamma_model", m, envir = topenv())
  m
}
