#' Clean ABS NOM into the analysis-ready quarterly panel
#'
#' Returns `(period, category, value, vintage_status)`. The classification
#' of preliminary/revised/final is preserved so the backtesting framework
#' can use the right benchmark at each evaluation date.
#'
#' @param nom_raw Output of [fetch_nom()].
#' @param cfg Project config.
#' @return Tibble.
#' @export
clean_nom <- function(nom_raw, cfg) {
  if (is.null(nom_raw) || nrow(nom_raw) == 0L) return(empty_nom_clean())
  nom_raw |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::group_by(.data$period, .data$category, .data$vintage_status) |>
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(period_unit = "quarter")
}

#' @keywords internal
empty_nom_clean <- function() {
  tibble::tibble(
    period = as.Date(character()), category = character(),
    vintage_status = character(), value = numeric(),
    period_unit = character()
  )
}
