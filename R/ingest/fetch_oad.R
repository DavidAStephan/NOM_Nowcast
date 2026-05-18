#' Fetch ABS Overseas Arrivals and Departures (OAD; cat. 3401.0)
#'
#' Pulls the monthly OAD time series. The primary path uses
#' [readabs::read_abs()] which handles ABS catalogue lookups, time series
#' download facility URLs and revision tracking. We pull:
#'
#'   * arrivals and departures by visa class (long-term)
#'   * arrivals by country of stay / birth (for cross-checks)
#'
#' If `{readabs}` lookup fails (ABS occasionally reissues tables and the
#' canonical IDs change), we fall back to the cached download from the
#' previous as-of and log a warning. Hard failure only occurs when no
#' cached data is available at all.
#'
#' @param cfg Project config.
#' @param asof As-of date for vintage assignment.
#' @return Tibble with columns `series_id`, `period`, `period_unit`,
#'   `value`, `category`, `direction`, `unit`, `metadata`.
#' @export
fetch_oad <- function(cfg, asof) {
  cat_no <- cfg$abs$oad_cat
  tables <- cfg$abs$oad_tables
  out <- tryCatch(
    fetch_oad_via_readabs(cat_no, tables, cfg, asof),
    error = function(e) {
      nn_warn("OAD fetch failed via {{readabs}}: {conditionMessage(e)}")
      fetch_oad_fallback(cfg, asof)
    }
  )
  if (is.null(out) || nrow(out) == 0L) {
    cli::cli_abort("OAD fetch returned no rows.")
  }
  out
}

#' @keywords internal
fetch_oad_via_readabs <- function(cat_no, tables, cfg, asof) {
  # readabs::read_abs caches downloads under tools::R_user_dir; we mirror
  # the artifact into data/raw for full reproducibility.
  table_ids <- unname(unlist(tables))
  nn_info("OAD: requesting cat {cat_no}, tables {paste(table_ids, collapse=', ')}")

  raw <- readabs::read_abs(
    cat_no  = cat_no,
    tables  = table_ids,
    path    = fs::path(cfg$paths$raw, "oad", format(asof, "%Y-%m-%d")),
    show_progress_bars = FALSE,
    retain_files = TRUE
  )

  parse_oad(raw, cfg)
}

#' Parse the OAD long-format tibble returned by `{readabs}`.
#'
#' @keywords internal
parse_oad <- function(raw, cfg) {
  # Each row already has columns: series_id, series, table_no, date, value,
  # frequency, series_type ("Original"|"Seasonally Adjusted"|"Trend"), unit.
  raw |>
    dplyr::filter(.data$frequency == "Month",
                  .data$series_type == "Original") |>
    dplyr::mutate(
      period      = lubridate::floor_date(.data$date, "month"),
      period_unit = "month",
      category    = oad_classify_category(.data$series, cfg),
      direction   = oad_classify_direction(.data$series),
      metadata    = jsonlite::toJSON(
        list(table_no = .data$table_no,
             series    = .data$series,
             series_type = .data$series_type),
        auto_unbox = TRUE
      )
    ) |>
    dplyr::filter(!is.na(.data$category)) |>
    dplyr::transmute(
      .data$series_id, .data$period, .data$period_unit, .data$value,
      .data$category, .data$direction, .data$unit, .data$metadata
    )
}

#' Map a raw OAD `series` description to a canonical category.
#'
#' Conservative: anything not matched is dropped. The category map is in
#' [R/clean/category_map.R][nn_category_map].
#'
#' @keywords internal
oad_classify_category <- function(series_desc, cfg) {
  m <- nn_category_map()
  out <- rep(NA_character_, length(series_desc))
  for (cat in names(m$oad)) {
    pats <- m$oad[[cat]]
    hits <- vapply(pats, function(p) stringr::str_detect(series_desc, p), logical(length(series_desc)))
    if (is.null(dim(hits))) hits <- matrix(hits, nrow = length(series_desc))
    any_hit <- apply(hits, 1, any)
    out[any_hit & is.na(out)] <- cat
  }
  out
}

#' @keywords internal
oad_classify_direction <- function(series_desc) {
  dplyr::case_when(
    stringr::str_detect(series_desc, "(?i)arrival") ~ "arrival",
    stringr::str_detect(series_desc, "(?i)departure") ~ "departure",
    TRUE ~ NA_character_
  )
}

#' Cached / fallback fetch
#'
#' Returns the most-recent locally cached OAD parquet, if any. Used when
#' the live fetch fails so an offline run still has *some* signal.
#'
#' @keywords internal
fetch_oad_fallback <- function(cfg, asof) {
  cache_dir <- fs::path(cfg$paths$raw, "oad")
  if (!fs::dir_exists(cache_dir)) {
    cli::cli_abort("No cached OAD data and live fetch failed.")
  }
  recent <- fs::dir_ls(cache_dir, type = "directory") |> sort(decreasing = TRUE)
  if (!length(recent)) cli::cli_abort("No cached OAD data and live fetch failed.")
  files <- fs::dir_ls(recent[[1]], regexp = "\\.parquet$")
  if (!length(files)) cli::cli_abort("Cached OAD dir is empty: {recent[[1]]}")
  nn_warn("OAD: using cached vintage {basename(recent[[1]])}")
  arrow::read_parquet(files[[1]])
}
