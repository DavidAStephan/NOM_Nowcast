#' Clean OAD into the analysis-ready long form
#'
#' Aggregates over country/age/sex to a single monthly observation per
#' `(category, direction)`, retaining only long-term arrivals/departures
#' (the relevant flow for NOM under the 12/16 rule).
#'
#' @param oad_raw Output of [fetch_oad()].
#' @param cfg Project config.
#' @return Tibble with columns `period`, `period_unit`, `category`,
#'   `direction`, `arrivals_or_departures` (sum), and a derived
#'   `net_lt_flow` for convenience at the monthly frequency.
#' @export
clean_oad <- function(oad_raw, cfg) {
  if (is.null(oad_raw) || nrow(oad_raw) == 0L) {
    return(empty_oad_clean())
  }
  oad_raw |>
    dplyr::filter(!is.na(.data$category), !is.na(.data$direction)) |>
    dplyr::group_by(.data$period, .data$category, .data$direction) |>
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(period_unit = "month")
}

#' @keywords internal
empty_oad_clean <- function() {
  tibble::tibble(
    period = as.Date(character()), category = character(),
    direction = character(), value = numeric(), period_unit = character()
  )
}
