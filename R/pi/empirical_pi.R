#' Empirical classification probability π
#'
#' For each quarter `q` that has been observed for at least
#' `cfg$pi$completion_quarters` quarters (so ABS NOM has had time to
#' settle), and each category `c`, compute
#'
#' \deqn{\hat{\pi}_{c,q} = \frac{NOM_{c,q}^{ABS,final}}{\sum_{t \in q} A^{OAD,LT}_{c,t}}}
#'
#' where `A^{OAD,LT}` is the OAD long-term arrivals series (the panel
#' uses already-summed quarterly values; departures are handled
#' symmetrically as part of the modelling pipeline, not the π estimator).
#'
#' Rationale: π is the fraction of long-term arrivals that *eventually*
#' satisfy the 12/16 rule and count as migrants. It varies by visa class
#' (students stay longer than working-holiday makers, etc.) and shifts
#' across policy regimes. We can only estimate it from cohorts that have
#' had enough time to be classified — hence the `completion_quarters`
#' restriction.
#'
#' Values outside `[cfg$pi$floor, cfg$pi$ceiling]` are coerced into the
#' interval (the ceiling slightly above 1 accommodates the rare case
#' where the NOM cohort contains arrivals classified as short-term in
#' OAD). NAs propagate.
#'
#' @param panel Output of [build_quarterly_panel()].
#' @param cfg Project config.
#' @return Tibble: `period`, `category`, `pi_hat`, `n_arrivals`,
#'   `nom_final`.
#' @export
estimate_pi_empirical <- function(panel, cfg) {
  cq <- cfg$pi$completion_quarters %||% 6L
  floor_v   <- cfg$pi$floor   %||% 0
  ceiling_v <- cfg$pi$ceiling %||% 1.5
  cutoff <- max(panel$period, na.rm = TRUE) - lubridate::quarters(cq)

  panel |>
    dplyr::filter(.data$period <= cutoff,
                  !is.na(.data$oad_lt_arrivals),
                  !is.na(.data$nom_final),
                  .data$oad_lt_arrivals > 0) |>
    dplyr::transmute(
      .data$period, .data$category,
      n_arrivals = .data$oad_lt_arrivals,
      nom_final  = .data$nom_final,
      pi_hat     = pmin(pmax(.data$nom_final / .data$oad_lt_arrivals,
                             floor_v), ceiling_v)
    )
}
