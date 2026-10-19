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

  oad_q <- aggregate_oad_quarters(
    oad_clean,
    strategy    = cfg$panel$oad_partial_quarter %||% "scale",
    share_years = cfg$panel$oad_share_years %||% 5L
  ) |>
    tidyr::pivot_wider(names_from = "direction",
                       values_from = c("value", "n_months"),
                       values_fill = NA_real_) |>
    dplyr::rename(period = "quarter")
  for (col in c("value_arrival", "value_departure",
                "n_months_arrival", "n_months_departure")) {
    if (!col %in% names(oad_q)) oad_q[[col]] <- NA_real_
  }
  oad_q <- oad_q |>
    dplyr::transmute(
      .data$period, .data$category,
      oad_lt_arrivals   = .data$value_arrival,
      oad_lt_departures = .data$value_departure,
      oad_lt_net        = .data$value_arrival - .data$value_departure,
      oad_months        = pmin(.data$n_months_arrival,
                               .data$n_months_departure, na.rm = TRUE)
    )

  nom_q <- if (!is.null(nom_clean) && nrow(nom_clean)) {
    nom_clean |>
      dplyr::filter(.data$category %in% c(levels, "total")) |>
      tidyr::pivot_wider(
        id_cols = c("period", "category"),
        names_from = "vintage_status",
        values_from = "value",
        values_fn = function(x) mean(x, na.rm = TRUE)
      )
  } else {
    tibble::tibble(period = as.Date(character()), category = character())
  }
  for (st in c("preliminary", "revised", "final")) {
    if (st %in% names(nom_q)) names(nom_q)[names(nom_q) == st] <- paste0("nom_", st)
  }
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
    drop_incomplete_visa_years(visa_grants_clean) |>
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
            "oad_lt_net", "oad_months", "visa_grants",
            "nom_preliminary", "nom_revised", "nom_final")
  for (m in must) if (!m %in% names(out)) out[[m]] <- NA_real_
  out[, must]
}

#' Drop financial years that are missing programs
#'
#' DHA programs publish on their own schedules, so at any moment the
#' latest financial year may exist for one program (say Temporary
#' Graduate) but not yet for Student, Skilled or Working Holiday. The
#' models sum grants across categories, so such a year would look like a
#' ~90% collapse in the leading indicator. A year is kept only when it
#' carries every category that the preceding year carried.
#'
#' @param vg Cleaned annual visa grants (`period`, `category`, `value`).
#' @return `vg` without incomplete years.
#' @keywords internal
drop_incomplete_visa_years <- function(vg) {
  if (is.null(vg) || !nrow(vg)) return(vg)
  ok <- !is.na(vg$value)
  cats_by_year <- lapply(split(vg$category[ok],
                               format(vg$period[ok], "%Y-%m-%d")), unique)
  years <- sort(names(cats_by_year))
  if (length(years) < 2L) return(vg)
  drop <- character()
  for (i in 2:length(years)) {
    prev <- cats_by_year[[years[i - 1L]]]
    cur  <- cats_by_year[[years[i]]]
    if (length(setdiff(prev, cur))) drop <- c(drop, years[i])
  }
  if (length(drop)) {
    nn_info("visa grants: dropping FY starting {paste(drop, collapse = ', ')} (missing programs vs prior year)")
    vg <- vg[!format(vg$period, "%Y-%m-%d") %in% drop, , drop = FALSE]
  }
  vg
}

#' Aggregate monthly OAD flows to quarters, handling incomplete quarters
#'
#' The live edge of the OAD series is always a partial quarter: with a
#' ~35-day publication lag the in-progress quarter has 0-2 of its 3
#' months published, and at a quarter-start backtest origin the
#' just-completed quarter has exactly 1 of 3. Summing those months and
#' presenting the result as a full quarter tells every downstream model
#' that arrivals collapsed by a third to two-thirds. That single
#' artefact produced the large, one-signed, model-invariant h = 0
#' under-prediction seen in every backtest before this fix.
#'
#' Strategies (`cfg$panel$oad_partial_quarter`):
#'
#'   * `"scale"` (default) — grossed up to a full-quarter equivalent
#'     using each calendar month's median share of its quarter over the
#'     last `share_years` complete years, per (category, direction).
#'     Falls back to `3 / n_months` when no history exists.
#'   * `"drop"` — partial quarters become `NA` (pure forecast state).
#'   * `"raw"`  — legacy behaviour (plain sum); kept for comparison only.
#'
#' @param oad_clean Output of [clean_oad()] (monthly rows).
#' @param strategy One of `"scale"`, `"drop"`, `"raw"`.
#' @param share_years Years of complete history used for month shares.
#' @return Tibble `quarter, category, direction, value, n_months`.
#' @keywords internal
aggregate_oad_quarters <- function(oad_clean, strategy = "scale",
                                   share_years = 5L) {
  empty <- tibble::tibble(quarter = as.Date(character()),
                          category = character(), direction = character(),
                          value = numeric(), n_months = integer())
  if (is.null(oad_clean) || !nrow(oad_clean)) return(empty)
  strategy <- match.arg(strategy, c("scale", "drop", "raw"))

  monthly <- oad_clean |>
    dplyr::filter(!is.na(.data$period)) |>
    dplyr::mutate(quarter = nn_quarter_start(.data$period),
                  month   = as.integer(format(.data$period, "%m")))

  q <- monthly |>
    dplyr::group_by(.data$quarter, .data$category, .data$direction) |>
    dplyr::summarise(value    = sum(.data$value, na.rm = TRUE),
                     n_months = dplyr::n_distinct(.data$period),
                     .groups  = "drop")
  if (strategy == "raw") return(q)

  partial <- q$n_months < 3L
  if (!any(partial)) return(q)

  if (strategy == "drop") {
    q$value[partial] <- NA_real_
    return(q)
  }

  # "scale": month shares from complete quarters in the recent window.
  complete_q <- q |> dplyr::filter(.data$n_months == 3L, .data$value > 0)
  latest_complete <- suppressWarnings(max(complete_q$quarter))
  window_start <- if (is.finite(latest_complete)) {
    latest_complete - lubridate::years(share_years)
  } else as.Date("1900-01-01")
  shares <- monthly |>
    dplyr::inner_join(
      complete_q |>
        dplyr::filter(.data$quarter >= window_start) |>
        dplyr::select("quarter", "category", "direction", q_total = "value"),
      by = c("quarter", "category", "direction")
    ) |>
    dplyr::mutate(share = .data$value / .data$q_total) |>
    dplyr::group_by(.data$category, .data$direction, .data$month) |>
    dplyr::summarise(share = stats::median(.data$share, na.rm = TRUE),
                     .groups = "drop")

  observed_share <- monthly |>
    dplyr::semi_join(q[partial, c("quarter", "category", "direction")],
                     by = c("quarter", "category", "direction")) |>
    dplyr::left_join(shares, by = c("category", "direction", "month")) |>
    dplyr::group_by(.data$quarter, .data$category, .data$direction) |>
    dplyr::summarise(obs_share = sum(.data$share),
                     n_obs     = dplyr::n(),
                     .groups   = "drop") |>
    dplyr::mutate(obs_share = dplyr::if_else(
      is.na(.data$obs_share) | .data$obs_share <= 0,
      .data$n_obs / 3, .data$obs_share))

  q |>
    dplyr::left_join(observed_share,
                     by = c("quarter", "category", "direction")) |>
    dplyr::mutate(value = dplyr::if_else(.data$n_months < 3L & !is.na(.data$obs_share),
                                         .data$value / .data$obs_share,
                                         .data$value)) |>
    dplyr::select("quarter", "category", "direction", "value", "n_months")
}
