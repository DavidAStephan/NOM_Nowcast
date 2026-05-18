#' Fetch Department of Home Affairs visa grant statistics
#'
#' DHA publishes monthly visa grants by visa class, source country and
#' onshore/offshore. The publication format has changed several times
#' (CSV, XLSX, then a Power BI-backed download interface). We scrape the
#' "Visa grants" index page, identify the most recent monthly XLSX/CSV
#' download links, fetch them and parse.
#'
#' This fetcher is *defensive*: when the page structure changes it raises
#' an informative warning and falls back to the most recent cached
#' artifact rather than crashing the pipeline.
#'
#' @inheritParams fetch_oad
#' @return Tibble: `series_id`, `period`, `period_unit`, `value`,
#'   `category`, `onshore`, `unit`, `metadata`.
#' @export
fetch_visa_grants <- function(cfg, asof) {
  index_url <- cfg$homeaffairs$visa_grants_index
  out <- tryCatch(
    fetch_visa_grants_scrape(index_url, cfg, asof),
    error = function(e) {
      nn_warn("DHA visa grants scrape failed: {conditionMessage(e)}")
      fetch_visa_grants_fallback(cfg)
    }
  )
  if (is.null(out) || nrow(out) == 0L) {
    nn_warn("DHA visa grants: no data, returning empty tibble.")
    return(empty_visa_grants())
  }
  out
}

#' @keywords internal
empty_visa_grants <- function() {
  tibble::tibble(
    series_id = character(), period = as.Date(character()),
    period_unit = character(), value = numeric(),
    category = character(), onshore = logical(),
    unit = character(), metadata = character()
  )
}

#' @keywords internal
fetch_visa_grants_scrape <- function(index_url, cfg, asof) {
  page <- tryCatch(
    nn_request(index_url, cfg) |> httr2::req_perform() |> httr2::resp_body_html(),
    error = function(e) {
      nn_warn("DHA visa grants: HTTP error on {index_url}: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(page)) return(empty_visa_grants())

  # Crawl both the index page and any documented sub-pages (visit /
  # study / live / work etc.) for visa-grant CSV/XLSX links.
  candidate_pages <- character()
  candidate_pages <- c(candidate_pages, index_url)
  sub_keys <- cfg$homeaffairs$sub_page_keywords %||% character()
  if (length(sub_keys)) {
    sub_links <- page |>
      rvest::html_elements("a") |>
      rvest::html_attr("href")
    sub_links <- sub_links[!is.na(sub_links)]
    keep <- vapply(sub_links, function(u) {
      any(stringr::str_detect(u, paste0("(?i)", sub_keys, "(/|$)")))
    }, logical(1))
    sub_links <- unique(sub_links[keep])
    sub_links <- ifelse(stringr::str_detect(sub_links, "^https?://"),
                        sub_links,
                        paste0("https://www.homeaffairs.gov.au", sub_links))
    candidate_pages <- unique(c(candidate_pages, sub_links))
  }

  links <- purrr::flatten_chr(purrr::map(candidate_pages, function(p) {
    sub_page <- tryCatch(
      nn_request(p, cfg) |> httr2::req_perform() |> httr2::resp_body_html(),
      error = function(e) NULL
    )
    if (is.null(sub_page)) return(character())
    sub_page |>
      rvest::html_elements("a") |>
      rvest::html_attr("href") |>
      Filter(f = function(x) !is.na(x) &&
               stringr::str_detect(x, "(?i)\\.(xlsx|csv|xls)$"))
  }))
  links <- unique(links)
  if (!length(links)) {
    nn_warn("DHA visa grants: no XLSX/CSV downloads found across {length(candidate_pages)} candidate pages. ",
            "(DHA has migrated to Power BI dashboards; see README.)")
    return(empty_visa_grants())
  }
  links <- ifelse(stringr::str_detect(links, "^https?://"),
                  links,
                  paste0("https://www.homeaffairs.gov.au", links))
  files <- vapply(unique(links), function(u) {
    ext <- paste0(".", tools::file_ext(u))
    tryCatch(nn_download(u, "visa_grants", cfg, asof, ext = ext),
             error = function(e) NA_character_)
  }, character(1))
  files <- files[!is.na(files)]
  if (!length(files)) return(empty_visa_grants())
  purrr::map_dfr(files, parse_visa_grants_file)
}

#' Parse a single visa grants CSV/XLSX into long form
#'
#' Historical DHA files have wide layouts: rows = visa subclass, columns
#' = financial year months. We pivot to long and infer the visa category
#' via `nn_category_map()$visa_subclass`.
#'
#' @keywords internal
parse_visa_grants_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  raw <- switch(ext,
    csv  = readr::read_csv(path, show_col_types = FALSE),
    xlsx = readxl::read_excel(path, .name_repair = "minimal"),
    xls  = readxl::read_excel(path, .name_repair = "minimal"),
    NULL
  )
  if (is.null(raw)) return(empty_visa_grants())

  # The DHA tables nearly always have a "Visa subclass" or "Subclass"
  # column. Detect it; everything to its right is treated as monthly
  # observations.
  raw <- janitor_clean(raw)
  subclass_col <- detect_subclass_column(raw)
  if (is.na(subclass_col)) return(empty_visa_grants())

  obs_cols <- setdiff(names(raw), subclass_col)
  obs_cols <- obs_cols[grepl("[0-9]{4}", obs_cols)]
  if (!length(obs_cols)) return(empty_visa_grants())

  long <- raw |>
    dplyr::select(dplyr::all_of(c(subclass_col, obs_cols))) |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(subclass_col),
      names_to = "period_raw",
      values_to = "value",
      values_transform = list(value = as.numeric)
    ) |>
    dplyr::rename(subclass = !!subclass_col) |>
    dplyr::mutate(
      period      = parse_dha_period(.data$period_raw),
      period_unit = "month",
      onshore     = NA,  # not always present; set in caller if column found
      category    = map_subclass_to_category(.data$subclass),
      series_id   = paste0("dha_visa_grants:", .data$subclass),
      unit        = "persons",
      metadata    = jsonlite::toJSON(list(file = basename(path),
                                          subclass = .data$subclass),
                                     auto_unbox = TRUE)
    ) |>
    dplyr::filter(!is.na(.data$period), !is.na(.data$value)) |>
    dplyr::transmute(
      .data$series_id, .data$period, .data$period_unit, .data$value,
      .data$category, .data$onshore, .data$unit, .data$metadata
    )
  long
}

#' @keywords internal
detect_subclass_column <- function(df) {
  candidates <- c("Visa subclass", "Subclass", "Visa Subclass", "subclass",
                  "Visa stream", "Stream")
  hit <- intersect(candidates, names(df))
  if (length(hit)) return(hit[[1]])
  # Fall back: first character column with realistic cardinality
  char_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (cc in char_cols) {
    if (dplyr::n_distinct(df[[cc]]) > 3 && dplyr::n_distinct(df[[cc]]) < nrow(df)) {
      return(cc)
    }
  }
  NA_character_
}

#' Best-effort parse of DHA period labels (e.g. "Jul 2023", "2023-07",
#' "July 2023 - Jun 2024", financial year strings).
#' @keywords internal
parse_dha_period <- function(x) {
  x <- as.character(x)
  out <- suppressWarnings(lubridate::my(x))
  na_mask <- is.na(out)
  if (any(na_mask)) {
    out[na_mask] <- suppressWarnings(lubridate::ym(x[na_mask]))
  }
  na_mask <- is.na(out)
  if (any(na_mask)) {
    out[na_mask] <- suppressWarnings(as.Date(x[na_mask]))
  }
  lubridate::floor_date(out, "month")
}

#' @keywords internal
map_subclass_to_category <- function(subclass) {
  m <- nn_category_map()$visa_subclass
  out <- vapply(subclass, function(s) {
    if (is.na(s)) return(NA_character_)
    matches <- vapply(names(m), function(cat) {
      any(stringr::str_detect(s, m[[cat]]))
    }, logical(1))
    hits <- names(m)[matches]
    if (length(hits)) hits[[1]] else "other"
  }, character(1))
  unname(out)
}

#' Light alternative to {janitor}::clean_names() that we keep inline to
#' avoid the extra dependency.
#' @keywords internal
janitor_clean <- function(df) {
  nms <- names(df)
  nms <- stringr::str_trim(nms)
  names(df) <- nms
  df
}

#' @keywords internal
fetch_visa_grants_fallback <- function(cfg) {
  cache_dir <- fs::path(cfg$paths$raw, "visa_grants")
  if (!fs::dir_exists(cache_dir)) return(empty_visa_grants())
  recent <- fs::dir_ls(cache_dir, type = "directory") |> sort(decreasing = TRUE)
  if (!length(recent)) return(empty_visa_grants())
  files <- fs::dir_ls(recent[[1]], regexp = "\\.(csv|xlsx|xls)$")
  if (!length(files)) return(empty_visa_grants())
  nn_warn("Visa grants: using cached vintage {basename(recent[[1]])}")
  purrr::map_dfr(files, parse_visa_grants_file)
}
