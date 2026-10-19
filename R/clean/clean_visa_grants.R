#' Clean DHA visa grants into the analysis-ready long form
#'
#' Aggregates over country and onshore/offshore. Drops the categories
#' listed in `cfg$panel$visa_grant_exclude` (default: `visitor`, which
#' is short-term tourism and would otherwise dominate the summed
#' visa signal ~5:1).
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
  exclude <- cfg$panel$visa_grant_exclude
  if (is.null(exclude)) exclude <- "visitor"   # explicit empty list => keep all
  visa_grants_raw |>
    dplyr::filter(!is.na(.data$category),
                  !.data$category %in% exclude) |>
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
