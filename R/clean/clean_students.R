#' Clean Department of Education student data
#'
#' Aggregates commencements/enrolments over countries, retains the sector
#' dimension so we can use higher-ed commencements as a leading indicator
#' separately from VET. Returns monthly observations.
#'
#' @param students_raw Output of [fetch_student_data()].
#' @param cfg Project config.
#' @return Tibble: `period`, `metric`, `sector`, `value`, `period_unit`.
#' @export
clean_students <- function(students_raw, cfg) {
  if (is.null(students_raw) || nrow(students_raw) == 0L) {
    return(empty_students_clean())
  }
  students_raw |>
    dplyr::group_by(.data$period, .data$metric, .data$sector) |>
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(period_unit = "month")
}

#' @keywords internal
empty_students_clean <- function() {
  tibble::tibble(
    period = as.Date(character()), metric = character(),
    sector = character(), value = numeric(), period_unit = character()
  )
}
