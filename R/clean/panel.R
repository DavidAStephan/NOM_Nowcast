#' Build the quarterly modelling panel
#'
#' Joins all cleaned sources into a single tidy panel at the quarterly
#' frequency, keyed by `(period, category)`. The panel is the canonical
#' input to every model (Phase 1–3) and every benchmark.
#'
#' Columns:
#'
#'   * `period`              quarter start `Date`
#'   * `category`            one of `config.yml::categories.levels`
#'   * `oad_lt_arrivals`     OAD long-term arrivals, summed within quarter
#'   * `oad_lt_departures`   ... and departures
#'   * `oad_lt_net`          arrivals - departures
#'   * `visa_grants`         DHA visa grants (sum within quarter)
#'   * `student_commencements` commencements where category == student
#'   * `student_enrolments`    enrolments where category == student
#'   * `nom_preliminary`     ABS preliminary value at the relevant vintage
#'   * `nom_final`           ABS latest-vintage NOM (only available after
#'                           sufficient revisions have settled; NA
#'                           otherwise)
#'
#' Missing values propagate; they are not imputed.
#'
#' @param oad_clean,nom_clean,visa_grants_clean,students_clean Cleaned
#'   source tibbles.
#' @param cfg Project config.
#' @return Quarterly panel tibble.
#' @export
#' Expand a (period = FY-start, value) row into 4 equal-quarter rows
#'
#' Helper used by [build_quarterly_panel()] to handle annual visa-grant
#' inputs. Each FY-start date (1 July YYYY) becomes four quarterly
#' periods within that FY (Jul, Oct, Jan, Apr), each carrying 1/4 of
#' the annual value.
#' @keywords internal
spread_annual_to_quarters <- function(df) {
  if (!nrow(df)) return(tibble::tibble(period = as.Date(character()),
                                       value = numeric()))
  out <- purrr::map_dfr(seq_len(nrow(df)), function(i) {
    fy_start <- as.Date(df$period[i])
    qs <- seq.Date(fy_start, by = "3 months", length.out = 4L)
    tibble::tibble(period = qs, value = df$value[i] / 4)
  })
  out
}

build_quarterly_panel <- function(oad_clean, nom_clean,
                                  visa_grants_clean, students_clean, cfg) {
  levels <- unique(c(cfg$categories$levels, "total"))

  oad_q <- oad_clean |>
    dplyr::mutate(quarter = nn_quarter_start(.data$period)) |>
    dplyr::group_by(.data$quarter, .data$category, .data$direction) |>
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "direction",
                       values_from = "value",
                       values_fill = NA_real_) |>
    dplyr::rename(period = "quarter")
  if (!"arrival" %in% names(oad_q))   oad_q$arrival <- NA_real_
  if (!"departure" %in% names(oad_q)) oad_q$departure <- NA_real_
  oad_q <- oad_q |>
    dplyr::transmute(
      .data$period, .data$category,
      oad_lt_arrivals   = .data$arrival,
      oad_lt_departures = .data$departure,
      oad_lt_net        = .data$arrival - .data$departure
    )

  nom_q <- nom_clean |>
    dplyr::filter(.data$category %in% c(levels, "total")) |>
    tidyr::pivot_wider(
      id_cols = c("period", "category"),
      names_from = "vintage_status",
      values_from = "value",
      values_fn = function(x) mean(x, na.rm = TRUE)
    ) |>
    dplyr::rename_with(.fn = ~ paste0("nom_", .),
                       .cols = dplyr::any_of(c("preliminary", "revised", "final")))
  for (col in c("nom_preliminary", "nom_revised", "nom_final")) {
    if (!col %in% names(nom_q)) nom_q[[col]] <- NA_real_
  }
  nom_q <- nom_q |>
    dplyr::mutate(nom_final = dplyr::coalesce(.data$nom_final,
                                              .data$nom_revised,
                                              .data$nom_preliminary))

  # Visa grants may be monthly OR annual (data.gov.au DHA pivots are
  # annual). Convert annual values to a quarterly profile by spreading
  # evenly across the 4 financial-year quarters; monthly values sum to
  # quarters in the usual way.
  vg_unit <- if (nrow(visa_grants_clean) &&
                 "period_unit" %in% names(visa_grants_clean)) {
    unique(stats::na.omit(visa_grants_clean$period_unit))[1] %||% "month"
  } else "month"
  vg_q <- if (identical(vg_unit, "year")) {
    visa_grants_clean |>
      dplyr::group_by(.data$category) |>
      dplyr::group_modify(~ spread_annual_to_quarters(.x)) |>
      dplyr::ungroup() |>
      dplyr::rename(visa_grants = "value")
  } else {
    visa_grants_clean |>
      dplyr::mutate(quarter = nn_quarter_start(.data$period)) |>
      dplyr::group_by(.data$quarter, .data$category) |>
      dplyr::summarise(visa_grants = sum(.data$value, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::rename(period = "quarter")
  }

  stu_q <- students_clean |>
    dplyr::filter(.data$sector %in% c("Higher Education", "Higher Ed", "VET",
                                      "Schools", "ELICOS", "Non-award")) |>
    dplyr::mutate(quarter = nn_quarter_start(.data$period)) |>
    dplyr::group_by(.data$quarter, .data$metric) |>
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "metric", values_from = "value") |>
    dplyr::rename(period = "quarter")
  if (!"commencements" %in% names(stu_q)) stu_q$commencements <- NA_real_
  if (!"enrolments"    %in% names(stu_q)) stu_q$enrolments    <- NA_real_
  stu_q <- stu_q |>
    dplyr::transmute(
      .data$period, category = "student",
      student_commencements = .data$commencements,
      student_enrolments    = .data$enrolments
    )

  # Build the (period x category) grid spanning the union of inputs
  periods <- sort(unique(c(oad_q$period, nom_q$period, vg_q$period, stu_q$period)))
  grid <- tidyr::expand_grid(period = periods, category = levels)

  out <- grid |>
    dplyr::left_join(oad_q, by = c("period", "category")) |>
    dplyr::left_join(nom_q, by = c("period", "category")) |>
    dplyr::left_join(vg_q, by = c("period", "category")) |>
    dplyr::left_join(stu_q, by = c("period", "category"))

  # Preserve column ordering for stability
  must <- c("period", "category", "oad_lt_arrivals", "oad_lt_departures",
            "oad_lt_net", "visa_grants", "student_commencements",
            "student_enrolments", "nom_preliminary", "nom_revised", "nom_final")
  for (m in must) if (!m %in% names(out)) out[[m]] <- NA_real_
  out[, must]
}
