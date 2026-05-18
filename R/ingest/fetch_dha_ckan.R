#' Fetch DHA visa-grant data via the data.gov.au CKAN API
#'
#' data.gov.au hosts the Department of Home Affairs "BP00" visa
#' program reports. Each visa stream has its own CKAN dataset:
#'
#'   * Student visa program
#'   * Visitor visa program
#'   * Temporary Graduate visa program
#'   * Temporary Work (skilled) visa program
#'   * Working Holiday Maker visa program
#'
#' The published XLSX files are pivot-table reports (filters at the
#' top, then a wide table with applicant type / sector / financial
#' year). They are released "locked at" the latest quarter-end and
#' usually carry quarterly granularity in the underlying data even
#' though the default pivot rolls up to financial-year totals.
#'
#' This fetcher discovers each dataset via the CKAN search API,
#' downloads the "granted" XLSX(s), and parses out grant counts at
#' whatever granularity the pivot exposes. When the XLSX is annual
#' the values land in the panel as `period_unit = "year"`; the
#' multi-source SSM downstream knows how to handle mixed-frequency
#' inputs.
#'
#' The CKAN endpoint is fixed at `data.gov.au/data/api/3/action/...`;
#' alternative sub-domains return 404.
#'
#' @inheritParams fetch_oad
#' @return Tibble: `series_id`, `period`, `period_unit`, `value`,
#'   `category`, `unit`, `metadata`.
#' @export
fetch_dha_ckan <- function(cfg, asof) {
  out <- tryCatch(
    fetch_dha_ckan_impl(cfg, asof),
    error = function(e) {
      nn_warn("DHA CKAN fetch failed: {conditionMessage(e)}")
      empty_visa_grants()
    }
  )
  if (is.null(out) || nrow(out) == 0L) return(empty_visa_grants())
  out
}

#' @keywords internal
fetch_dha_ckan_impl <- function(cfg, asof) {
  ep <- cfg$datagov$ckan_endpoint %||%
        "https://data.gov.au/data/api/3/action/package_search"
  queries <- cfg$datagov$dha_queries %||%
             c("Student visa program",
               "Visitor visa program",
               "Temporary Graduate visa program",
               "Temporary Work (skilled) visa program",
               "Working Holiday Maker visa program")
  resources <- purrr::flatten(purrr::map(queries, ckan_dataset_resources,
                                         endpoint = ep, cfg = cfg))
  if (!length(resources)) {
    nn_warn("DHA CKAN: no visa-program datasets returned by CKAN.")
    return(empty_visa_grants())
  }
  # Pick "granted" XLSX resources only (the lodged-but-not-granted
  # series adds noise for our use).
  granted <- Filter(function(r) {
    is_xlsx <- identical(tolower(r$format %||% ""), "xlsx")
    is_grant <- grepl("(?i)granted", r$name %||% "") ||
                grepl("(?i)granted", r$url  %||% "")
    is_xlsx && is_grant
  }, resources)
  if (!length(granted)) {
    nn_warn("DHA CKAN: no 'granted' XLSX resources found.")
    return(empty_visa_grants())
  }

  purrr::map_dfr(granted, function(res) {
    path <- tryCatch(
      nn_download(res$url, "dha_ckan", cfg, asof, ext = ".xlsx"),
      error = function(e) NULL
    )
    if (is.null(path)) return(empty_visa_grants())
    parse_dha_ckan_xlsx(path, dataset_title = res$dataset_title %||% "")
  })
}

#' Query CKAN for datasets matching `query` and return a list of
#' resource records (each with `url`, `format`, `name` and the parent
#' `dataset_title`).
#' @keywords internal
ckan_dataset_resources <- function(query, endpoint, cfg) {
  r <- tryCatch(
    nn_request(endpoint, cfg) |>
      httr2::req_url_query(q = query, rows = 5L) |>
      httr2::req_perform(),
    error = function(e) NULL)
  if (is.null(r)) return(list())
  body <- tryCatch(httr2::resp_body_json(r), error = function(e) NULL)
  if (is.null(body) || !isTRUE(body$success)) return(list())
  out <- list()
  for (pkg in body$result$results) {
    for (res in pkg$resources) {
      res$dataset_title <- pkg$title
      out <- c(out, list(res))
    }
  }
  out
}

#' Parse a single DHA pivot-table XLSX
#'
#' The pivot files share a layout: rows 1-12 are pivot filters, row 13
#' carries the metric label ("Sum of Total"), row 14 has the row /
#' column dimensions ("Applicant Type", "Sector", then financial-year
#' or quarter labels), row 15 onwards are data.
#'
#' We dynamically detect the header row by scanning for the row that
#' contains "Applicant Type" (or "Visa Subclass") in column A.
#'
#' @keywords internal
parse_dha_ckan_xlsx <- function(path, dataset_title) {
  sheets <- readxl::excel_sheets(path)
  # Prefer a sheet whose name implies actual data, in order of
  # preference: "Granted" > "Lodged" > "Grants" > anything that isn't
  # Overview / Notes / Glossary / Terminology.
  pick <- function(pat) sheets[grepl(pat, sheets, ignore.case = TRUE)][1]
  data_sheet <- pick("(?i)^granted")
  if (is.na(data_sheet)) data_sheet <- pick("(?i)granted")
  if (is.na(data_sheet)) data_sheet <- pick("(?i)grants?")
  if (is.na(data_sheet)) data_sheet <- pick("(?i)lodged")
  if (is.na(data_sheet)) {
    data_sheet <- sheets[!grepl("(?i)overview|note|explanat|gloss|filter|cover|terminol|item",
                                sheets)][1]
  }
  if (is.na(data_sheet)) data_sheet <- sheets[1]
  raw <- tryCatch(
    readxl::read_excel(path, sheet = data_sheet, col_names = FALSE,
                       .name_repair = "minimal"),
    error = function(e) NULL)
  if (is.null(raw) || nrow(raw) < 15L) return(empty_visa_grants())

  hdr_idx <- detect_dha_pivot_header(raw)
  if (is.na(hdr_idx)) return(empty_visa_grants())

  header <- as.character(unlist(raw[hdr_idx, ]))
  # The first 1-2 columns are dimension labels (Applicant Type, Sector,
  # ...). Everything after the last dimension column is a year/quarter
  # value column.
  dim_cols <- which(!is.na(header) & nzchar(header) &
                    !grepl("^\\d{4}", header))
  dim_cols <- dim_cols[seq_len(max(1L, min(3L, length(dim_cols))))]
  val_cols <- which(grepl("^\\d{4}([-]\\d{2})?(\\s*Q[1-4])?$", header))
  if (!length(val_cols)) return(empty_visa_grants())

  body <- raw[(hdr_idx + 1L):nrow(raw), , drop = FALSE]
  # Drop the obvious total / blank rows
  body <- body[!is.na(body[[dim_cols[1L]]]), , drop = FALSE]

  # Fill-down on any dimension column with merged cells
  for (j in dim_cols) {
    v <- as.character(body[[j]])
    for (i in seq_along(v)) {
      if (is.na(v[i]) && i > 1L) v[i] <- v[i - 1L]
    }
    body[[j]] <- v
  }

  long <- purrr::map_dfr(val_cols, function(j) {
    tibble::tibble(
      period_label = header[j],
      applicant    = if (length(dim_cols) >= 1L) as.character(body[[dim_cols[1L]]]) else NA_character_,
      sector       = if (length(dim_cols) >= 2L) as.character(body[[dim_cols[2L]]]) else NA_character_,
      stream       = if (length(dim_cols) >= 3L) as.character(body[[dim_cols[3L]]]) else NA_character_,
      value        = suppressWarnings(as.numeric(body[[j]]))
    )
  })

  long |>
    # Drop subtotal / grand-total rows so caller-side summation by
    # (period, category) doesn't double-count.
    dplyr::filter(
      !is.na(.data$applicant),
      !grepl("(?i)total", .data$applicant)
    ) |>
    dplyr::mutate(
      period      = parse_dha_period_label(.data$period_label),
      period_unit = period_unit_from_label(.data$period_label),
      category    = classify_dha_dataset(dataset_title),
      series_id   = paste0("dha_grants:", .data$category, ":",
                           dplyr::coalesce(.data$applicant, "")),
      unit        = "persons",
      metadata    = vapply(seq_len(dplyr::n()), function(i) {
        as.character(jsonlite::toJSON(
          list(dataset = dataset_title,
               applicant = .data$applicant[i],
               sector    = .data$sector[i],
               stream    = .data$stream[i]),
          auto_unbox = TRUE))
      }, character(1))
    ) |>
    dplyr::filter(!is.na(.data$period), !is.na(.data$value),
                  !is.na(.data$category)) |>
    dplyr::group_by(.data$series_id, .data$period, .data$period_unit,
                    .data$category, .data$unit, .data$metadata) |>
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::select("series_id", "period", "period_unit", "value",
                  "category", "unit", "metadata")
}

#' @keywords internal
detect_dha_pivot_header <- function(raw) {
  # The pivot file lists each filter dimension on its own row at the
  # top ("Visa Subclass | (All)", etc.) and then the actual header is
  # a separate row whose column-2 entry is a real value (not "(All)"
  # and not NA). The most reliable signature is column-1 = "Applicant
  # Type" with column-2 not being "(All)".
  col1 <- as.character(raw[[1L]])
  col2 <- if (ncol(raw) >= 2L) as.character(raw[[2L]]) else rep(NA, length(col1))
  for (i in seq_len(min(40L, length(col1)))) {
    if (is.na(col1[i])) next
    is_applicant <- grepl("(?i)^\\s*Applicant\\s*Type\\s*$", col1[i])
    looks_like_pivot_filter <- !is.na(col2[i]) &&
      grepl("^\\s*\\(All\\)\\s*$", col2[i])
    if (is_applicant && !looks_like_pivot_filter) return(i)
  }
  # Secondary: row with "Visa Subclass" as col 1 AND col 2 is not "(All)"
  for (i in seq_len(min(40L, length(col1)))) {
    if (is.na(col1[i])) next
    if (grepl("(?i)Visa Subclass|Visa Type|Citizenship", col1[i]) &&
        !(is.na(col2[i]) || grepl("^\\s*\\(All\\)\\s*$", col2[i]))) {
      return(i)
    }
  }
  NA_integer_
}

#' @keywords internal
parse_dha_period_label <- function(x) {
  x <- as.character(x)
  # Financial year "2024-25" -> period start of FY (1 July 2024).
  fy <- stringr::str_match(x, "^(\\d{4})-(\\d{2})$")
  out <- rep(as.Date(NA_character_), length(x))
  fy_yr <- suppressWarnings(as.integer(fy[, 2L]))
  ok_fy <- !is.na(fy_yr)
  out[ok_fy] <- as.Date(sprintf("%d-07-01", fy_yr[ok_fy]))
  # Calendar year "2024"
  cy <- suppressWarnings(as.integer(stringr::str_extract(x, "^\\d{4}$")))
  ok_cy <- !is.na(cy) & is.na(out)
  out[ok_cy] <- as.Date(sprintf("%d-01-01", cy[ok_cy]))
  # FY + quarter "2024-25 Q2" -> Q-start
  fq <- stringr::str_match(x, "^(\\d{4})-\\d{2}\\s*Q([1-4])$")
  ok_fq <- !is.na(fq[, 2L]) & is.na(out)
  out[ok_fq] <- as.Date(sprintf("%d-%02d-01",
                                as.integer(fq[ok_fq, 2L]) +
                                  ifelse(as.integer(fq[ok_fq, 3L]) >= 3, 1L, 0L),
                                ((as.integer(fq[ok_fq, 3L]) - 1L) * 3L + 7L) %% 12L + 1L))
  out
}

#' @keywords internal
period_unit_from_label <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    grepl("Q[1-4]$", x) ~ "quarter",
    grepl("^\\d{4}-\\d{2}$", x) ~ "year",   # financial year
    grepl("^\\d{4}$", x) ~ "year",
    TRUE ~ "year"
  )
}

#' Map a CKAN dataset title to a canonical visa category.
#' @keywords internal
classify_dha_dataset <- function(title) {
  title <- as.character(title)
  out <- dplyr::case_when(
    grepl("(?i)^student",                 title) ~ "student",
    grepl("(?i)^visitor",                 title) ~ "other_temp",
    grepl("(?i)working\\s*holiday",       title) ~ "working_holiday",
    grepl("(?i)temporary graduate",       title) ~ "other_temp",
    grepl("(?i)temporary work \\(skilled\\)", title) ~ "skilled",
    grepl("(?i)temporary\\s*resident.*skilled|^skilled", title) ~ "skilled",
    grepl("(?i)^family|partner",          title) ~ "family",
    grepl("(?i)bridging",                 title) ~ "bridging",
    grepl("(?i)^permanent",               title) ~ "permanent",
    TRUE ~ NA_character_
  )
  unname(out)
}
