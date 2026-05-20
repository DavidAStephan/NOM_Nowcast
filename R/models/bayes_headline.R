#' Phase 3 v0.1 — Bayesian headline state-space (total NOM only).
#'
#' Mirrors the trivariate observation structure of
#' [fit_kalman_multi_one()] but fits via cmdstanr HMC, yielding a full
#' posterior over the latent log-arrivals state.  Headline-only for
#' now (no per-category hierarchy); the bigger model is Phase 3 v0.2.
#'
#' Behaves like the other forecast wrappers: in/out a tibble with one
#' row per period, columns `period, category, nom_mean, nom_se,
#' lower_80, upper_80, lower_95, upper_95, model`.
#'
#' Triggered only when `cfg$models$bayes_headline$enabled` is TRUE.
#' Compilation is cached by cmdstanr (`exe_file` defaults to a path
#' under the Stan model file's directory), so subsequent fits skip
#' the C++ compile.
#'
#' @param panel Output of [build_quarterly_panel()].
#' @param asof  Reference as-of date.
#' @param cfg   Project config.
#' @return Headline forecast tibble.  Empty when Bayes is disabled or
#'   cmdstanr is unavailable.
#' @export
fit_bayes_headline <- function(panel, asof, cfg) {
  bcfg <- cfg$models$bayes_headline %||% list(enabled = FALSE)
  if (!isTRUE(bcfg$enabled %||% FALSE)) {
    return(empty_bayes_headline())
  }
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    nn_warn("bayes_headline: cmdstanr not installed; skipping")
    return(empty_bayes_headline())
  }

  total <- panel |>
    dplyr::filter(.data$category == "total") |>
    dplyr::arrange(.data$period)
  if (!nrow(total)) return(empty_bayes_headline())

  # Visa-grants live on per-category rows; aggregate to the headline.
  vg_total <- panel |>
    dplyr::filter(.data$category != "total") |>
    dplyr::group_by(.data$period) |>
    dplyr::summarise(visa_grants = sum(.data$visa_grants, na.rm = TRUE),
                     .groups = "drop")
  total <- total |>
    dplyr::select(-dplyr::any_of("visa_grants")) |>
    dplyr::left_join(vg_total, by = "period")

  # Trim history to a sensible window before fitting. OAD goes back
  # to 1976 but NOM starts in the early 2000s; running the latent
  # state on 50 years of essentially OAD-only data inflates T from
  # ~90 to ~200 and saturates HMC's tree depth. Cut to a configurable
  # number of quarters before the first NOM observation (default 8q
  # = 2 years of OAD-only warmup for level/trend identification).
  warmup_q <- bcfg$pre_nom_quarters %||% 8L
  first_nom <- which(!is.na(total$nom_final))[1L]
  if (length(first_nom) && !is.na(first_nom)) {
    start <- max(1L, first_nom - warmup_q)
    total <- total[start:nrow(total), , drop = FALSE]
  }

  h_max  <- bcfg$forecast_horizons %||% 4L
  lead_q <- cfg$models$kalman_multi$visa_lead_quarters %||% 1L

  obs_periods <- total$period
  future <- seq.Date(
    seq.Date(max(obs_periods), by = "3 months", length.out = 2L)[2L],
    by = "quarter", length.out = h_max
  )
  all_periods <- c(obs_periods, future)
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
    T        = T,
    T_obs    = T_obs,
    lead_q   = as.integer(lead_q),
    y_oad    = ifelse(is.na(y_oad),  0, y_oad),
    has_oad  = as.integer(!is.na(y_oad)),
    y_visa   = ifelse(is.na(y_visa), 0, y_visa),
    has_visa = as.integer(!is.na(y_visa)),
    y_nom    = ifelse(is.na(y_nom),  0, y_nom),
    has_nom  = as.integer(!is.na(y_nom)),
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

  model <- bayes_headline_compile()
  fit <- tryCatch(
    model$sample(
      data            = stan_data,
      seed            = bcfg$seed         %||% 20260520,
      chains          = bcfg$chains       %||% 2L,
      parallel_chains = bcfg$chains       %||% 2L,
      iter_warmup     = bcfg$iter_warmup  %||% 500L,
      iter_sampling   = bcfg$iter_sample  %||% 500L,
      adapt_delta     = bcfg$adapt_delta  %||% 0.95,
      max_treedepth   = bcfg$max_treedepth %||% 10L,
      refresh         = 0,
      show_messages   = FALSE,
      show_exceptions = FALSE
    ),
    error = function(e) {
      nn_warn("bayes_headline sample failed: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(fit)) return(empty_bayes_headline())

  # Extract posterior draws of nom_hat (already exp-transformed in Stan)
  draws <- tryCatch(fit$draws("nom_hat", format = "draws_matrix"),
                    error = function(e) NULL)
  if (is.null(draws)) return(empty_bayes_headline())
  draws <- as.matrix(draws)        # rows = draws, cols = periods 1..T

  nom_mean <- apply(draws, 2L, mean)
  nom_sd   <- apply(draws, 2L, sd)
  q <- function(p) apply(draws, 2L, stats::quantile, probs = p, na.rm = TRUE)
  l80 <- q(0.10); u80 <- q(0.90)
  l95 <- q(0.025); u95 <- q(0.975)

  tibble::tibble(
    period   = all_periods,
    category = "total",
    nom_mean = as.numeric(nom_mean),
    nom_se   = as.numeric(nom_sd),
    lower_80 = as.numeric(l80),
    upper_80 = as.numeric(u80),
    lower_95 = as.numeric(l95),
    upper_95 = as.numeric(u95),
    model    = "bayes_headline"
  )
}

#' @keywords internal
empty_bayes_headline <- function() {
  tibble::tibble(
    period   = as.Date(character()),
    category = character(),
    nom_mean = numeric(),
    nom_se   = numeric(),
    lower_80 = numeric(),
    upper_80 = numeric(),
    lower_95 = numeric(),
    upper_95 = numeric(),
    model    = character()
  )
}

#' Compile the Bayesian headline Stan model.
#'
#' Cached at the session level; cmdstanr keeps the compiled exe next
#' to the .stan file so repeat sessions reuse it as well.
#' @keywords internal
.bayes_headline_model <- NULL

bayes_headline_compile <- function(stan_file = "stan/headline_ssm.stan") {
  if (!is.null(.bayes_headline_model)) return(.bayes_headline_model)
  m <- cmdstanr::cmdstan_model(stan_file, compile = TRUE)
  assign(".bayes_headline_model", m, envir = topenv())
  m
}
