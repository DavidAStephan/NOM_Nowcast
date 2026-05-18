#' Fetch Department of Education international student data
#'
#' Department of Education publishes monthly commencements and enrolments
#' by source country and education sector at
#' <https://www.education.gov.au/international-education-data-and-research/international-student-data>.
#' Files are XLSX with a wide layout (months as columns) and one sheet
#' per sector (Higher Ed, VET, Schools, ELICOS, Non-award).
#'
#' We scrape the index page for the most recent "Monthly Summary"
#' workbook, download it, and parse all sheets into long form.
#'
#' @inheritParams fetch_oad
#' @return Tibble: `series_id`, `period`, `period_unit`, `value`,
#'   `metric` ("commencements" or "enrolments"), `sector`,
#'   `category` (always `"student"`), `unit`, `metadata`.
#' @export
fetch_student_data <- function(cfg, asof) {
  index_url <- cfg$education$students_index
  out <- tryCatch(
    fetch_student_data_scrape(index_url, cfg, asof),
    error = function(e) {
      nn_warn("Education student fetch failed: {conditionMessage(e)}")
      fetch_student_data_fallback(cfg)
    }
  )
  if (is.null(out) || nrow(out) == 0L) return(empty_students())
  out
}

#' @keywords internal
empty_students <- function() {
  tibble::tibble(
    series_id = character(), period = as.Date(character()),
    period_unit = character(), value = numeric(),
    metric = character(), sector = character(), category = character(),
    unit = character(), metadata = character()
  )
}

#' @keywords internal
fetch_student_data_scrape <- function(index_url, cfg, asof) {
  page <- nn_request(index_url, cfg) |> httr2::req_perform() |> httr2::resp_body_html()
  links <- page |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    Filter(f = function(x) !is.na(x) &&
             stringr::str_detect(x, "(?i)monthly.*summary.*\\.xlsx$"))
  if (!length(links)) {
    cli::cli_abort("Could not locate education monthly summary XLSX on {index_url}")
  }
  links <- ifelse(stringr::str_detect(links, "^https?://"),
                  links,
                  paste0("https://www.education.gov.au", links))
  # Take the most recently-named link by parsed month
  links <- unique(links)
  file <- nn_download(links[[1]], "students", cfg, asof, ext = ".xlsx")
  parse_student_workbook(file)
}

#' Parse a Department of Education monthly summary workbook.
#'
#' Each sheet contains a header block plus a long matrix of
#' country/sector × month observations. We melt all sheets.
#'
#' @keywords internal
parse_student_workbook <- function(path) {
  sheets <- readxl::excel_sheets(path)
  sheets <- sheets[grepl("(?i)commence|enrol", sheets)]
  if (!length(sheets)) {
    nn_warn("Student workbook {basename(path)} has no commencements/enrolments sheets")
    return(empty_students())
  }

  purrr::map_dfr(sheets, function(sh) {
    raw <- tryCatch(
      readxl::read_excel(path, sheet = sh, .name_repair = "minimal"),
      error = function(e) NULL
    )
    if (is.null(raw) || nrow(raw) == 0L) return(empty_students())

    metric <- if (grepl("(?i)commence", sh)) "commencements" else "enrolments"
    sector <- stringr::str_replace_all(sh, "(?i)\\s*(commencements|enrolments)\\s*", "") |>
      stringr::str_squish()

    long <- pivot_student_sheet(raw)
    if (nrow(long) == 0L) return(empty_students())

    long |>
      dplyr::mutate(
        period_unit = "month",
        metric      = metric,
        sector      = sector,
        category    = "student",
        series_id   = paste0("edu_students:", metric, ":", sector, ":", .data$country),
        unit        = "persons",
        metadata    = jsonlite::toJSON(list(file = basename(path),
                                            sheet = sh,
                                            country = .data$country),
                                       auto_unbox = TRUE)
      ) |>
      dplyr::transmute(
        .data$series_id, .data$period, .data$period_unit, .data$value,
        .data$metric, .data$sector, .data$category, .data$unit, .data$metadata
      )
  })
}

#' @keywords internal
pivot_student_sheet <- function(df) {
  # Heuristic: the first column is country; remaining numeric-looking
  # columns whose headers parse to a month are observations.
  if (ncol(df) < 2) return(empty_students())
  names(df)[1] <- "country"
  obs_cols <- names(df)[-1]
  parsed <- suppressWarnings(lubridate::my(obs_cols))
  parsed_alt <- suppressWarnings(lubridate::ym(obs_cols))
  parsed[is.na(parsed)] <- parsed_alt[is.na(parsed)]
  keep <- !is.na(parsed)
  if (!any(keep)) return(empty_students())

  out <- df |>
    dplyr::select(dplyr::all_of(c("country", obs_cols[keep]))) |>
    tidyr::pivot_longer(
      cols = -"country",
      names_to = "period_raw",
      values_to = "value",
      values_transform = list(value = function(x) suppressWarnings(as.numeric(x)))
    )
  out$period <- parsed[match(out$period_raw, obs_cols)]
  out$period <- lubridate::floor_date(out$period, "month")
  out |> dplyr::filter(!is.na(.data$period), !is.na(.data$value))
}

#' @keywords internal
fetch_student_data_fallback <- function(cfg) {
  cache_dir <- fs::path(cfg$paths$raw, "students")
  if (!fs::dir_exists(cache_dir)) return(empty_students())
  recent <- fs::dir_ls(cache_dir, type = "directory") |> sort(decreasing = TRUE)
  if (!length(recent)) return(empty_students())
  files <- fs::dir_ls(recent[[1]], regexp = "\\.xlsx$")
  if (!length(files)) return(empty_students())
  nn_warn("Students: using cached vintage {basename(recent[[1]])}")
  parse_student_workbook(files[[1]])
}
