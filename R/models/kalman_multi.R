#' Phase 2 — multi-source state-space model (scaffold)
#'
#' Extends the Phase 1 univariate Kalman filter with additional
#' observation blocks for DHA visa grants (with a parametric Gamma lag)
#' and Department of Education student enrolments (for the student
#' category only). Cross-category dependencies enter through partial
#' pooling on the trend-innovation variance.
#'
#' This file is a working scaffold: the design is documented and the
#' KFAS-compatible state structure is sketched, but estimation requires
#' joint MLE over many hyperparameters and is left for the v0.5
#' milestone. Use [fit_kalman_univariate()] in the meantime.
#'
#' Specification reminder:
#'
#' Latent log-arrivals state:
#' \deqn{\log A^*_{c,t} = \mu_{c,t} + \gamma_{c,t} + \eta_{c,t}}
#'
#' Observation blocks:
#' \deqn{A^{OAD,LT}_{c,t} = A^*_{c,t} + \varepsilon^{OAD}_{c,t}}
#' \deqn{V_{c,t} = \kappa_c \sum_{k=0}^{K_c} w_{c,k}(\alpha_c, \beta_c)\, A^*_{c,t+k} + \varepsilon^V_{c,t}}
#' \deqn{E_t = \kappa_E\, A^*_{c=student,t-\ell} + \varepsilon^E_t}
#'
#' @param panel Output of [build_quarterly_panel()].
#' @param cfg Project config.
#' @return Currently raises a `not_implemented` condition. The function
#'   exists so the targets DAG can include the node and downstream
#'   reports degrade gracefully.
#' @export
fit_kalman_multi <- function(panel, cfg) {
  cli::cli_alert_info(
    "Phase 2 multi-source SSM is scaffolded only. See R/models/kalman_multi.R."
  )
  rlang::abort("not_implemented: Phase 2 SSM not yet enabled.",
               class = "not_implemented")
}

#' Discretised Gamma lag distribution
#'
#' Helper used by the visa-grant observation block. Returns the discrete
#' PMF of `Gamma(alpha, beta)` over integer support `0:k_max`,
#' normalised so weights sum to 1.
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
