#' Project smoothed π forward to cover incomplete cohorts
#'
#' For quarters more recent than the empirical-π cutoff, carry forward
#' the last smoothed value within the most recent regime, optionally with
#' a small AR(1)-style mean-reversion to the regime mean.
#'
#' @param pi_smoothed Output of [smooth_pi()].
#' @param panel_periods All quarter starts in the modelling panel.
#' @param cfg Project config.
#' @return Tibble with `period`, `category`, `pi_proj`, `pi_se` for every
#'   `(period, category)` in `panel_periods`.
#' @export
project_pi <- function(pi_smoothed, panel_periods, cfg) {
  cats <- unique(pi_smoothed$category)
  if (!length(cats)) {
    return(tibble::tibble(period = panel_periods, category = "total",
                          pi_proj = NA_real_, pi_se = NA_real_))
  }
  purrr::map_dfr(cats, function(cat) {
    src <- pi_smoothed |> dplyr::filter(.data$category == cat) |>
      dplyr::arrange(.data$period)
    last_val <- utils::tail(src$pi_smoothed, 1)
    last_se  <- utils::tail(src$pi_se, 1)
    out <- tibble::tibble(
      period   = panel_periods,
      category = cat
    ) |>
      dplyr::left_join(src |> dplyr::select("period", "pi_smoothed", "pi_se"),
                       by = "period") |>
      dplyr::mutate(
        pi_proj = dplyr::coalesce(.data$pi_smoothed, last_val),
        pi_se   = dplyr::coalesce(.data$pi_se, last_se)
      ) |>
      dplyr::select(-"pi_smoothed")
    out
  })
}
