#' Phase 3 — Bayesian hierarchical model (scaffold)
#'
#' Fits the hierarchical model in `stan/hierarchical_nom.stan` using
#' `{cmdstanr}`. Partial pooling across categories on the trend-
#' innovation precision and a Gaussian-process prior on the time-varying
#' π handle the small-N-per-category problem. Vintage-aware NOM
#' observation uses the variance scaling
#' \eqn{\mathrm{Var}(\mathrm{NOM}^{ABS}_{q,v}) = \sigma^2_r(v-q)} so the
#' likelihood downweights preliminary releases.
#'
#' This file wires up the cmdstanr call; the Stan source lives in
#' `stan/hierarchical_nom.stan`. The function returns a `posterior_draws`
#' object suitable for use with `{posterior}` and `{bayesplot}`.
#'
#' @param panel Output of [build_quarterly_panel()].
#' @param cfg Project config.
#' @return A `posterior::draws_df`. Currently raises `not_implemented`
#'   pending Stan model verification — see methodology report Section 6
#'   for status.
#' @export
fit_stan_hierarchical <- function(panel, cfg) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    rlang::abort(
      "cmdstanr not installed; install via install.packages('cmdstanr', ",
      "repos = c('https://stan-dev.r-universe.dev', getOption('repos')))",
      class = "not_implemented"
    )
  }
  stan_path <- "stan/hierarchical_nom.stan"
  if (!file.exists(stan_path)) {
    rlang::abort(glue::glue("Stan model not found at {stan_path}"),
                 class = "not_implemented")
  }
  data <- build_stan_data(panel, cfg)
  mod <- cmdstanr::cmdstan_model(stan_path)
  fit <- mod$sample(
    data            = data,
    chains          = cfg$models$stan$chains          %||% 4,
    iter_warmup     = cfg$models$stan$iter_warmup     %||% 1000,
    iter_sampling   = cfg$models$stan$iter_sampling   %||% 1000,
    adapt_delta     = cfg$models$stan$adapt_delta     %||% 0.95,
    max_treedepth   = cfg$models$stan$max_treedepth   %||% 12,
    parallel_chains = cfg$run$parallel_workers        %||% 4
  )
  posterior::as_draws_df(fit$draws())
}

#' Build the data list passed to Stan
#' @keywords internal
build_stan_data <- function(panel, cfg) {
  cats <- cfg$categories$levels
  panel_cat <- panel |> dplyr::filter(.data$category %in% cats) |>
    dplyr::arrange(.data$category, .data$period)
  n_cat <- length(cats)
  n_t   <- length(unique(panel_cat$period))
  list(
    N_cat = n_cat,
    N_t   = n_t,
    cat_idx = match(panel_cat$category, cats),
    t_idx   = match(panel_cat$period, sort(unique(panel_cat$period))),
    y_log_arrivals = log1p(pmax(panel_cat$oad_lt_arrivals, 0)),
    y_visa_grants  = panel_cat$visa_grants,
    y_nom_obs      = panel_cat$nom_preliminary,
    vintage_age    = pmax(
      as.integer(round(as.numeric(Sys.Date() - panel_cat$period) / 91.25)),
      0L
    )
  )
}
