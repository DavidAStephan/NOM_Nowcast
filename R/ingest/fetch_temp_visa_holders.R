#' Fetch DHA temporary visa holders (stocks) and WHM grant statistics
#'
#' Stock estimates of temporary visa holders in Australia at a point in
#' time, published quarterly by DHA. Useful for:
#'
#'   * cross-validating implied stocks from the flow-based nowcast,
#'   * sanity-checking working-holiday and student categories.
#'
#' This fetcher scrapes the DHA index pages and downloads the most recent
#' CSV/XLSX, applying the same defensive pattern as the visa grants
#' fetcher.
#'
#' @inheritParams fetch_oad
#' @return Tibble: `series_id`, `period`, `period_unit`, `value`,
#'   `category`, `unit`, `metadata`.
#' @export
fetch_temp_visa_holders <- function(cfg, asof) {
  out <- tryCatch(
    fetch_twh_scrape(cfg, asof),
    error = function(e) {
      nn_warn("TWH fetch failed: {conditionMessage(e)}")
      fetch_twh_fallback(cfg)
    }
  )
  if (is.null(out) || nrow(out) == 0L) return(empty_twh())
  out
}

#' @keywords internal
empty_twh <- function() {
  tibble::tibble(
    series_id = character(), period = as.Date(character()),
    period_unit = character(), value = numeric(),
    category = character(), unit = character(), metadata = character()
  )
}

#' @keywords internal
fetch_twh_scrape <- function(cfg, asof) {
  index_urls <- c(cfg$homeaffairs$twh_index, cfg$homeaffairs$wh_index)
  all_files <- purrr::flatten_chr(purrr::map(index_urls, function(url) {
    page <- tryCatch(
      nn_request(url, cfg) |> httr2::req_perform() |> httr2::resp_body_html(),
      error = function(e) {
        nn_warn("TWH fetch: HTTP error on {url}: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(page)) return(character())
    links <- page |>
      rvest::html_elements("a") |>
      rvest::html_attr("href") |>
      Filter(f = function(x) !is.na(x) &&
               stringr::str_detect(x, "(?i)\\.(csv|xlsx|xls)$"))
    if (!length(links)) return(character())
    links <- ifelse(stringr::str_detect(links, "^https?://"),
                    links,
                    paste0("https://www.homeaffairs.gov.au", links))
    vapply(unique(links), function(u) {
      ext <- paste0(".", tools::file_ext(u))
      tryCatch(nn_download(u, "twh", cfg, asof, ext = ext),
               error = function(e) NA_character_)
    }, character(1))
  }))
  all_files <- all_files[!is.na(all_files)]
  if (!length(all_files)) {
    nn_warn("No TWH/WHM files found across configured DHA URLs.")
    return(empty_twh())
  }
  purrr::map_dfr(all_files, parse_twh_file)
}

#' @keywords internal
parse_twh_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  raw <- switch(ext,
    csv  = readr::read_csv(path, show_col_types = FALSE),
    xlsx = readxl::read_excel(path, .name_repair = "minimal"),
    xls  = readxl::read_excel(path, .name_repair = "minimal"),
    NULL
  )
  if (is.null(raw) || nrow(raw) == 0L) return(empty_twh())

  # Heuristic columnar parse. TWH files vary heavily across releases.
  date_col <- names(raw)[grepl("(?i)date|period|month|quarter|as.at", names(raw))][1]
  val_col  <- names(raw)[grepl("(?i)count|number|persons|total", names(raw))][1]
  sub_col  <- names(raw)[grepl("(?i)subclass|visa|category", names(raw))][1]
  if (is.na(date_col) || is.na(val_col)) return(empty_twh())

  period_parsed <- parse_dha_period(raw[[date_col]])
  tibble::tibble(
    period      = period_parsed,
    value       = suppressWarnings(as.numeric(raw[[val_col]])),
    subclass    = if (!is.na(sub_col)) as.character(raw[[sub_col]]) else "total"
  ) |>
    dplyr::filter(!is.na(.data$period), !is.na(.data$value)) |>
    dplyr::mutate(
      period_unit = ifelse(lubridate::day(.data$period) == 1L &
                             lubridate::month(.data$period) %in% c(1, 4, 7, 10),
                           "quarter", "month"),
      category    = map_subclass_to_category(.data$subclass),
      series_id   = paste0("twh:", .data$subclass),
      unit        = "persons",
      metadata    = jsonlite::toJSON(list(file = basename(path),
                                          subclass = .data$subclass),
                                     auto_unbox = TRUE)
    ) |>
    dplyr::select("series_id", "period", "period_unit", "value",
                  "category", "unit", "metadata")
}

#' @keywords internal
fetch_twh_fallback <- function(cfg) {
  cache_dir <- fs::path(cfg$paths$raw, "twh")
  if (!fs::dir_exists(cache_dir)) return(empty_twh())
  recent <- fs::dir_ls(cache_dir, type = "directory") |> sort(decreasing = TRUE)
  if (!length(recent)) return(empty_twh())
  files <- fs::dir_ls(recent[[1]], regexp = "\\.(csv|xlsx|xls)$")
  if (!length(files)) return(empty_twh())
  nn_warn("TWH: using cached vintage {basename(recent[[1]])}")
  purrr::map_dfr(files, parse_twh_file)
}
