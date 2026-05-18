#' Fetch ABS NOM estimates (cat. 3101.0)
#'
#' Quarterly NOM totals and breakdown by visa class. Published with
#' National, State and Territory Population approximately 6 months after
#' the reference quarter.
#'
#' Two distinct outputs need to be captured:
#'
#'   1. **Headline NOM** by quarter (final / preliminary distinction
#'      preserved via metadata).
#'   2. **NOM by visa group** when available — ABS publishes this in the
#'      "Migration, Australia" sub-product on an annual cadence (cat.
#'      3412.0). When the quarterly breakdown isn't published, we proxy
#'      with the annual figure proportioned by OAD.
#'
#' @inheritParams fetch_oad
#' @return Tibble: `series_id`, `period`, `period_unit`, `value`,
#'   `category`, `vintage_status` ("preliminary" / "revised" / "final"),
#'   `unit`, `metadata`.
#' @export
fetch_nom <- function(cfg, asof) {
  cat_no <- cfg$abs$nom_cat
  tables <- cfg$abs$nom_tables
  table_ids <- unname(unlist(tables))

  nn_info("NOM: requesting cat {cat_no}, tables {paste(table_ids, collapse=', ')}")
  raw <- tryCatch(
    readabs::read_abs(
      cat_no = cat_no,
      tables = table_ids,
      path   = fs::path(cfg$paths$raw, "nom", format(asof, "%Y-%m-%d")),
      show_progress_bars = FALSE,
      retain_files = TRUE
    ),
    error = function(e) {
      nn_warn("NOM fetch failed via {{readabs}}: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(raw)) return(fetch_nom_fallback(cfg, asof))
  parse_nom(raw, cfg, asof)
}

#' @keywords internal
parse_nom <- function(raw, cfg, asof) {
  out <- raw |>
    dplyr::filter(.data$frequency == "Quarter",
                  .data$series_type == "Original") |>
    dplyr::mutate(
      period      = lubridate::floor_date(.data$date, "quarter"),
      period_unit = "quarter",
      category    = nom_classify_category(.data$series),
      vintage_status = nom_classify_status(.data$series, .data$period, asof),
      metadata    = jsonlite::toJSON(
        list(table_no = .data$table_no, series = .data$series),
        auto_unbox = TRUE
      )
    ) |>
    dplyr::filter(stringr::str_detect(.data$series, "(?i)net overseas migration|NOM")) |>
    dplyr::transmute(
      .data$series_id, .data$period, .data$period_unit, .data$value,
      .data$category, .data$vintage_status, .data$unit, .data$metadata
    )

  # Default category to "total" where we can't classify
  out$category[is.na(out$category)] <- "total"
  out
}

#' @keywords internal
nom_classify_category <- function(series_desc) {
  m <- nn_category_map()
  out <- rep(NA_character_, length(series_desc))
  for (cat in names(m$nom)) {
    pats <- m$nom[[cat]]
    hits <- vapply(pats, function(p) stringr::str_detect(series_desc, p), logical(length(series_desc)))
    if (is.null(dim(hits))) hits <- matrix(hits, nrow = length(series_desc))
    any_hit <- apply(hits, 1, any)
    out[any_hit & is.na(out)] <- cat
  }
  out
}

#' Heuristic classification of a NOM value as preliminary / revised /
#' final.
#'
#' ABS labels these via release metadata, not always via the series name.
#' Until we have the release metadata wired up, this uses age in quarters
#' since the reference quarter relative to `asof`:
#'
#'   * < 3 quarters: preliminary
#'   * 3–6 quarters: revised
#'   * >= 6 quarters: final
#'
#' This is consistent with ABS's stated revision practice but should be
#' replaced with a hard read of the release notes when available.
#'
#' @keywords internal
nom_classify_status <- function(series_desc, period, asof) {
  age_q <- as.numeric((as.Date(asof) - period) / 90)
  dplyr::case_when(
    age_q < 3  ~ "preliminary",
    age_q < 6  ~ "revised",
    TRUE       ~ "final"
  )
}

#' @keywords internal
fetch_nom_fallback <- function(cfg, asof) {
  cache_dir <- fs::path(cfg$paths$raw, "nom")
  if (!fs::dir_exists(cache_dir)) {
    cli::cli_abort("No cached NOM data and live fetch failed.")
  }
  recent <- fs::dir_ls(cache_dir, type = "directory") |> sort(decreasing = TRUE)
  if (!length(recent)) cli::cli_abort("No cached NOM data and live fetch failed.")
  files <- fs::dir_ls(recent[[1]], regexp = "\\.parquet$")
  if (!length(files)) cli::cli_abort("Cached NOM dir is empty: {recent[[1]]}")
  nn_warn("NOM: using cached vintage {basename(recent[[1]])}")
  arrow::read_parquet(files[[1]])
}
