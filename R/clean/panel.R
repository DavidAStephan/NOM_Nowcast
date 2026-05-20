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
#'   * `nom_preliminary`     ABS preliminary value at the relevant vintage
#'   * `nom_final`           ABS latest-vintage NOM (only available after
#'                           sufficient revisions have settled; NA
#'                           otherwise)
#'
#' Missing values propagate; they are not imputed.
#'
#' @param oad_clean,nom_clean,visa_grants_clean Cleaned source tibbles.
#' @param cfg Project config.
#' @return Quarterly panel tibble.
#' @export
#' Expand annual (period = FY-start, value) rows into quarterly rows
#'
#' Helper used by [build_quarterly_panel()] for annual visa-grant
#' inputs. Each FY-start date (1 July YYYY) becomes four quarterly
#' periods (Jul, Oct, Jan, Apr).
#'
#' Two disaggregation strategies are supported:
#'
#'   * **equal** (default fallback): each quarter gets one quarter
#'     of the annual value.
#'   * **proportional**: weights derived from a quarterly weighting
#'     series (typically OAD long-term arrivals for the same visa
#'     category) are used to allocate the annual total across the
#'     four quarters of the FY. Within-year seasonality of grants
#'     then mirrors the seasonality of the underlying flow.
#'
#' When `weights_df` is provided but doesn't cover a particular FY's
#' four quarters with non-zero values, that FY falls back to equal
#' allocation.
#'
#' @param df   Annual data: at minimum columns `period` (FY start)
#'   and `value`.
#' @param weights_df Optional quarterly weights tibble with columns
#'   `period` and `weight` (typically the OAD long-term arrivals
#'   series for the same visa category).
#' @keywords internal
spread_annual_to_quarters <- function(df, weights_df = NULL) {
  if (!nrow(df)) {
    return(tibble::tibble(period = as.Date(character()), value = numeric()))
  }
  purrr::map_dfr(seq_len(nrow(df)), function(i) {
    fy_start <- as.Date(df$period[i])
    qs <- seq.Date(fy_start, by = "3 months", length.out = 4L)
    annual <- df$value[i]
    w <- if (!is.null(weights_df)) {
      wm <- weights_df$weight[match(qs, weights_df$period)]
      if (any(!is.na(wm)) && sum(wm, na.rm = TRUE) > 0) {
        wm[is.na(wm) | wm < 0] <- 0
        wm / sum(wm)
      } else rep(0.25, 4L)
    } else rep(0.25, 4L)
    tibble::tibble(period = qs, value = annual * w)
  })
}

build_quarterly_panel <- function(oad_clean, nom_clean,
                                  visa_grants_clean, cfg) {
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
    # Disaggregation strategy. The "proportional" default uses OAD
    # long-term arrivals for the same visa category as quarterly
    # weights within each FY; equal-quarter fallback is used when
    # OAD weights are missing.
    strategy <- cfg$panel$vg_disagg_strategy %||% "proportional"
    oad_weights_by_cat <- if (identical(strategy, "proportional")) {
      oad_q |>
        dplyr::select("period", "category",
                      weight = "oad_lt_arrivals") |>
        dplyr::filter(!is.na(.data$weight))
    } else NULL
    visa_grants_clean |>
      dplyr::group_by(.data$category) |>
      dplyr::group_modify(function(.x, .y) {
        w <- if (!is.null(oad_weights_by_cat)) {
          oad_weights_by_cat |>
            dplyr::filter(.data$category == .y$category) |>
            dplyr::select("period", "weight")
        } else NULL
        spread_annual_to_quarters(.x, w)
      }) |>
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

  # Build the (period x category) grid spanning the union of inputs
  periods <- sort(unique(c(oad_q$period, nom_q$period, vg_q$period)))
  grid <- tidyr::expand_grid(period = periods, category = levels)

  out <- grid |>
    dplyr::left_join(oad_q, by = c("period", "category")) |>
    dplyr::left_join(nom_q, by = c("period", "category")) |>
    dplyr::left_join(vg_q, by = c("period", "category"))

  # Preserve column ordering for stability
  must <- c("period", "category", "oad_lt_arrivals", "oad_lt_departures",
            "oad_lt_net", "visa_grants",
            "nom_preliminary", "nom_revised", "nom_final")
  for (m in must) if (!m %in% names(out)) out[[m]] <- NA_real_
  out[, must]
}
