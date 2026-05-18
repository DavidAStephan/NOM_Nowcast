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
  cutoff <- seq.Date(max(panel$period, na.rm = TRUE),
                     by = sprintf("-%d months", 3L * cq),
                     length.out = 2L)[2L]

  by_cat <- panel |>
    dplyr::filter(.data$period <= cutoff,
                  .data$category != "total",
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
  if (nrow(by_cat) > 0L) return(by_cat)

  # Aggregate-pi fallback: ABS quarterly NOM is published as a total
  # only (no category breakdown). Compute the total classification
  # ratio for each completed quarter and emit one row per category by
  # broadcasting the total — this lets the modelling pipeline downstream
  # treat every category uniformly while keeping the estimator stable.
  totals <- panel |>
    dplyr::filter(.data$category == "total", !is.na(.data$nom_final)) |>
    dplyr::select("period", nom_total = "nom_final") |>
    dplyr::filter(.data$period <= cutoff)
  net_by_q <- panel |>
    dplyr::filter(.data$category == "total") |>
    dplyr::select("period",
                  total_net      = "oad_lt_net",
                  total_arrivals = "oad_lt_arrivals")
  agg <- totals |>
    dplyr::inner_join(net_by_q, by = "period") |>
    dplyr::filter(!is.na(.data$total_net), .data$total_net > 0) |>
    dplyr::transmute(
      .data$period,
      # π is defined here as NOM / net long-term flow so that the
      # downstream `π × (arrivals - departures)` reconstruction
      # algebraically recovers the headline. We use the dedicated
      # "total" row, which carries the ABS Permanent+Long-term flows
      # from Tables 1/2 — these are the canonical NOM-relevant flows
      # and are always positive on a quarterly basis (the by-visa
      # tables include short-term visitors and can flip sign).
      pi_hat = pmin(pmax(.data$nom_total / .data$total_net,
                         floor_v), ceiling_v),
      nom_final  = .data$nom_total,
      n_arrivals = .data$total_arrivals
    )
  cats <- unique(c(cfg$categories$levels, "total"))
  tidyr::expand_grid(category = cats, agg) |>
    dplyr::select("period", "category", "n_arrivals", "nom_final", "pi_hat")
}
