#' Clean DHA visa grants into the analysis-ready long form
#'
#' Aggregates over country and onshore/offshore. Filters to known
#' categories (drops `other` to keep model inputs interpretable).
#'
#' @param visa_grants_raw Output of [fetch_visa_grants()].
#' @param cfg Project config.
#' @return Tibble: `period`, `category`, `value`, `period_unit`.
#' @export
clean_visa_grants <- function(visa_grants_raw, cfg) {
  if (is.null(visa_grants_raw) || nrow(visa_grants_raw) == 0L) {
    return(empty_vg_clean())
  }
  unit <- if ("period_unit" %in% names(visa_grants_raw)) {
    unique(stats::na.omit(visa_grants_raw$period_unit))[1] %||% "month"
  } else "month"
  visa_grants_raw |>
    dplyr::filter(!is.na(.data$category)) |>
    dplyr::group_by(.data$period, .data$category) |>
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(period_unit = unit)
}

#' @keywords internal
empty_vg_clean <- function() {
  tibble::tibble(
    period = as.Date(character()), category = character(),
    value = numeric(), period_unit = character()
  )
}
