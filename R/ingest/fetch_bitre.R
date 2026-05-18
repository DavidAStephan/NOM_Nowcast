#' Fetch BITRE international airline activity (monthly)
#'
#' BITRE publishes monthly international airline activity at
#' <https://www.bitre.gov.au/statistics/aviation/international_airline_activity_monthly>,
#' with passenger movements by route. We use total inbound/outbound
#' passengers as a capacity constraint indicator: it dominates short-run
#' OAD movements at the monthly frequency and survived COVID as the most
#' reliable real-time signal of arrivals capacity.
#'
#' @inheritParams fetch_oad
#' @return Tibble: `series_id`, `period`, `period_unit`, `value`,
#'   `direction` ("inbound"/"outbound"), `unit`, `metadata`.
#' @export
fetch_bitre <- function(cfg, asof) {
  index_url <- cfg$bitre$airline_activity_index
  out <- tryCatch(
    fetch_bitre_scrape(index_url, cfg, asof),
    error = function(e) {
      nn_warn("BITRE fetch failed: {conditionMessage(e)}")
      fetch_bitre_fallback(cfg)
    }
  )
  if (is.null(out) || nrow(out) == 0L) return(empty_bitre())
  out
}

#' @keywords internal
empty_bitre <- function() {
  tibble::tibble(
    series_id = character(), period = as.Date(character()),
    period_unit = character(), value = numeric(),
    direction = character(), unit = character(), metadata = character()
  )
}

#' @keywords internal
fetch_bitre_scrape <- function(index_url, cfg, asof) {
  page <- tryCatch(
    nn_request(index_url, cfg) |> httr2::req_perform() |> httr2::resp_body_html(),
    error = function(e) {
      nn_warn("BITRE fetch: HTTP error on {index_url}: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(page)) return(empty_bitre())
  links <- page |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    Filter(f = function(x) !is.na(x) &&
             stringr::str_detect(x, "(?i)international.*airline.*\\.xlsx$|airline.*activity.*\\.xlsx$"))
  if (!length(links)) {
    nn_warn("BITRE: no airline activity XLSX found on {index_url}")
    return(empty_bitre())
  }
  links <- ifelse(stringr::str_detect(links, "^https?://"),
                  links,
                  paste0("https://www.bitre.gov.au", links))
  file <- tryCatch(
    nn_download(unique(links)[[1]], "bitre", cfg, asof, ext = ".xlsx"),
    error = function(e) NULL
  )
  if (is.null(file)) return(empty_bitre())
  parse_bitre_workbook(file)
}

#' @keywords internal
parse_bitre_workbook <- function(path) {
  sheets <- readxl::excel_sheets(path)
  pax_sheet <- sheets[grepl("(?i)passenger|pax", sheets)][1]
  if (is.na(pax_sheet)) return(empty_bitre())
  raw <- tryCatch(
    readxl::read_excel(path, sheet = pax_sheet, .name_repair = "minimal"),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) == 0L) return(empty_bitre())

  # Look for columns that look like dates/months for the time axis, plus
  # "inbound"/"outbound" totals. BITRE layouts vary; this is a defensive
  # extractor.
  date_col <- names(raw)[vapply(raw, function(x) inherits(x, "Date") ||
                                  inherits(x, "POSIXt"), logical(1))][1]
  if (is.na(date_col)) {
    # Try parsing any character column as month-year
    char_cols <- names(raw)[vapply(raw, is.character, logical(1))]
    for (cc in char_cols) {
      parsed <- suppressWarnings(lubridate::my(raw[[cc]]))
      if (sum(!is.na(parsed)) > nrow(raw) * 0.5) {
        raw[[cc]] <- parsed
        date_col <- cc
        break
      }
    }
  }
  if (is.na(date_col)) return(empty_bitre())

  in_col  <- names(raw)[grepl("(?i)inbound|arriv", names(raw))][1]
  out_col <- names(raw)[grepl("(?i)outbound|depart", names(raw))][1]
  if (is.na(in_col) && is.na(out_col)) return(empty_bitre())

  out <- tibble::tibble()
  if (!is.na(in_col)) {
    out <- dplyr::bind_rows(out, tibble::tibble(
      period = lubridate::floor_date(as.Date(raw[[date_col]]), "month"),
      value  = suppressWarnings(as.numeric(raw[[in_col]])),
      direction = "inbound"
    ))
  }
  if (!is.na(out_col)) {
    out <- dplyr::bind_rows(out, tibble::tibble(
      period = lubridate::floor_date(as.Date(raw[[date_col]]), "month"),
      value  = suppressWarnings(as.numeric(raw[[out_col]])),
      direction = "outbound"
    ))
  }
  out |>
    dplyr::filter(!is.na(.data$period), !is.na(.data$value)) |>
    dplyr::mutate(
      series_id   = paste0("bitre:", .data$direction),
      period_unit = "month",
      unit        = "passengers",
      metadata    = jsonlite::toJSON(list(file = basename(path)), auto_unbox = TRUE)
    ) |>
    dplyr::select("series_id", "period", "period_unit", "value",
                  "direction", "unit", "metadata")
}

#' @keywords internal
fetch_bitre_fallback <- function(cfg) {
  cache_dir <- fs::path(cfg$paths$raw, "bitre")
  if (!fs::dir_exists(cache_dir)) return(empty_bitre())
  recent <- fs::dir_ls(cache_dir, type = "directory") |> sort(decreasing = TRUE)
  if (!length(recent)) return(empty_bitre())
  files <- fs::dir_ls(recent[[1]], regexp = "\\.xlsx$")
  if (!length(files)) return(empty_bitre())
  nn_warn("BITRE: using cached vintage {basename(recent[[1]])}")
  parse_bitre_workbook(files[[1]])
}
