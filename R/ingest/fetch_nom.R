#' Fetch ABS Net Overseas Migration estimates
#'
#' NOM is now published across two ABS releases, both as data cubes:
#'
#'   * **Quarterly headline** lives in the
#'     `national-state-and-territory-population` release
#'     (Table 310101.xlsx). It contains population components of change
#'     including the NOM series at quarterly frequency. The published
#'     value labelled "Net overseas migration" is the relevant target.
#'
#'   * **Annual by visa group** lives in the `overseas-migration`
#'     release (catalogue 3407.0), data cube `34070DO004_<fy>.xlsx`.
#'     This file gives arrivals and departures by visa group at
#'     financial-year frequency. The π estimator uses the implied
#'     ratios but only at annual frequency until a quarterly visa
#'     breakdown is re-published by the ABS.
#'
#' The fetcher pulls both files and emits long-form observations from
#' each. `vintage_status` is classified by age (see
#' [nom_classify_status()]) until we wire up reading release notes.
#'
#' @inheritParams fetch_oad
#' @return Tibble: `series_id`, `period`, `period_unit`, `value`,
#'   `category`, `vintage_status`, `unit`, `metadata`.
#' @export
fetch_nom <- function(cfg, asof) {
  out <- tryCatch(
    fetch_nom_quarterly(cfg, asof),
    error = function(e) {
      nn_warn("NOM quarterly fetch failed: {conditionMessage(e)}")
      tibble::tibble()
    }
  )
  ann <- tryCatch(
    fetch_nom_annual_visa(cfg, asof),
    error = function(e) {
      nn_warn("NOM annual-visa fetch failed: {conditionMessage(e)}")
      tibble::tibble()
    }
  )
  out <- dplyr::bind_rows(out, ann)
  if (is.null(out) || nrow(out) == 0L) {
    return(fetch_nom_fallback(cfg, asof))
  }
  out
}

#' Download Table 310101.xlsx and extract the quarterly NOM series.
#'
#' Sheet `Data1` contains the wide layout: column A = quarter end date,
#' subsequent columns are components of change including NOM. We melt
#' to long, keep only the NOM column, classify category as `"total"`.
#'
#' @keywords internal
fetch_nom_quarterly <- function(cfg, asof) {
  slug <- cfg$abs$nom_slug_quarterly %||% "national-state-and-territory-population"
  file <- cfg$abs$nom_quarterly_file  %||% "310101.xlsx"
  out_dir <- fs::path(cfg$paths$raw, "nom", format(asof, "%Y-%m-%d"))
  fs::dir_create(out_dir)

  nn_info("NOM: downloading quarterly {file}")
  path <- readabs::download_abs_data_cube(
    catalogue_string = slug, cube = file, path = out_dir
  )
  parse_nom_quarterly_cube(path, asof)
}

#' Parse an ABS quarterly population data cube.
#'
#' These files use the standard ABS time-series spreadsheet layout
#' (also seen in the legacy time-series download facility):
#'
#'   * Column A: 9 metadata rows ("Unit", "Series Type", ... "Series ID")
#'     then quarter-end dates (as Excel serials).
#'   * Columns B+: one column per series. Row 1 is the descriptive
#'     label ("Net overseas migration ; Australia ;"). Row 10 is the
#'     ABS series ID ("A2133252X" etc.).
#'
#' We melt to long, retain only series labelled `(?i)Net overseas
#' migration`, and emit (period, value) for each.
#'
#' @keywords internal
parse_nom_quarterly_cube <- function(path, asof) {
  sheets <- readxl::excel_sheets(path)
  data_sheet <- sheets[grepl("(?i)^Data", sheets)][1]
  if (is.na(data_sheet)) data_sheet <- sheets[1]

  # Read once with no header to expose the descriptive labels (row 1)
  # and series IDs (row 10).
  raw <- readxl::read_excel(path, sheet = data_sheet, col_names = FALSE,
                            .name_repair = "minimal")
  if (nrow(raw) < 11L) {
    cli::cli_abort("NOM: {basename(path)}::{data_sheet} has < 11 rows")
  }
  series_labels <- as.character(unlist(raw[1L, ]))
  series_units  <- as.character(unlist(raw[2L, ]))
  series_ids    <- as.character(unlist(raw[10L, ]))
  # Identify NOM series by descriptive label
  nom_cols <- which(grepl("(?i)net\\s*overseas\\s*migration", series_labels))
  nom_cols <- setdiff(nom_cols, 1L)  # drop the metadata column
  if (!length(nom_cols)) {
    cli::cli_abort("NOM: no Net Overseas Migration series in {basename(path)}::{data_sheet}")
  }

  # Build the long output by taking the period column and each NOM
  # series column. Periods start at row 11.
  period_serial <- suppressWarnings(as.numeric(unlist(raw[11:nrow(raw), 1L])))
  period <- as.Date(period_serial, origin = "1899-12-30")

  purrr::map_dfr(nom_cols, function(col_idx) {
    vals <- suppressWarnings(as.numeric(unlist(raw[11:nrow(raw), col_idx])))
    scale <- if (grepl("000", series_units[col_idx], fixed = TRUE)) 1000 else 1
    tibble::tibble(
      series_id      = series_ids[col_idx] %||% paste0("nom_col", col_idx),
      period         = lubridate::floor_date(period, "quarter"),
      period_unit    = "quarter",
      value          = vals * scale,
      category       = "total",
      vintage_status = nom_classify_status(series_labels[col_idx], period, asof),
      unit           = "persons",
      metadata       = as.character(jsonlite::toJSON(
        list(file = basename(path), sheet = data_sheet,
             series = series_labels[col_idx],
             raw_unit = series_units[col_idx]),
        auto_unbox = TRUE))
    ) |>
      dplyr::filter(!is.na(.data$period), !is.na(.data$value))
  })
}

#' Download the annual NOM-by-visa data cube.
#' @keywords internal
fetch_nom_annual_visa <- function(cfg, asof) {
  slug <- cfg$abs$nom_slug_annual         %||% "overseas-migration"
  file <- cfg$abs$nom_annual_visa_file
  if (is.null(file) || !nzchar(file)) {
    nn_warn("NOM annual visa file not configured; skipping")
    return(tibble::tibble())
  }
  out_dir <- fs::path(cfg$paths$raw, "nom_annual", format(asof, "%Y-%m-%d"))
  fs::dir_create(out_dir)
  nn_info("NOM: downloading annual visa {file}")
  path <- tryCatch(
    readabs::download_abs_data_cube(
      catalogue_string = slug, cube = file, path = out_dir
    ),
    error = function(e) {
      nn_warn("NOM annual visa download failed: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(path)) return(tibble::tibble())
  parse_nom_annual_visa_cube(path, asof)
}

#' @keywords internal
parse_nom_annual_visa_cube <- function(path, asof) {
  # Annual NOM-by-visa cubes use one "Table 4.x" sheet per
  # state/territory. Each is a wide layout: Direction in column A
  # (merged), Visa Group in column B, financial-year columns from
  # column D onward. We parse the Australia-wide sheet (Table 4.9).
  sheets <- readxl::excel_sheets(path)
  national_sheet <- sheets[grepl("4\\.9$|Australia", sheets, ignore.case = TRUE)][1]
  if (is.na(national_sheet)) {
    nn_warn("NOM annual visa: could not identify the Australia sheet")
    return(tibble::tibble())
  }
  raw <- tryCatch(
    readxl::read_excel(path, sheet = national_sheet, col_names = FALSE,
                       .name_repair = "minimal"),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) < 16L) return(tibble::tibble())

  # Find the header row: the one containing "Direction" in column A
  # or "Visa Group" in column B.
  hdr_idx <- NA_integer_
  for (i in seq_len(min(30L, nrow(raw)))) {
    a <- as.character(raw[[1L]][i]); b <- as.character(raw[[2L]][i])
    if (!is.na(a) && grepl("(?i)^direction", a)) { hdr_idx <- i; break }
    if (!is.na(b) && grepl("(?i)visa.*group", b)) { hdr_idx <- i; break }
  }
  if (is.na(hdr_idx)) return(tibble::tibble())

  header <- as.character(unlist(raw[hdr_idx, ]))
  # Match financial-year columns ("2004-05" etc.) in the header
  yr_cols <- which(grepl("^\\d{4}[-]\\d{2}$", header))
  if (!length(yr_cols)) return(tibble::tibble())
  year_labels <- header[yr_cols]

  body <- raw[(hdr_idx + 1L):nrow(raw), , drop = FALSE]
  body <- body[!is.na(body[[1L]]) | !is.na(body[[2L]]), , drop = FALSE]

  # Direction is in col 1 (with merges => fill down). Visa group is col 2.
  direction_raw <- as.character(body[[1L]])
  for (i in seq_along(direction_raw)) {
    if (is.na(direction_raw[i]) && i > 1L) direction_raw[i] <- direction_raw[i - 1L]
  }
  visa_group <- as.character(body[[2L]])

  long <- purrr::map_dfr(seq_along(yr_cols), function(j) {
    col_idx <- yr_cols[j]
    tibble::tibble(
      year_label = year_labels[j],
      direction  = direction_raw,
      visa_group = visa_group,
      value      = suppressWarnings(as.numeric(body[[col_idx]]))
    )
  })

  long <- long |>
    dplyr::mutate(
      direction = dplyr::case_when(
        grepl("(?i)arriv", direction) ~ "arrival",
        grepl("(?i)depart", direction) ~ "departure",
        grepl("(?i)net",   direction) ~ "net",
        TRUE                          ~ NA_character_
      ),
      category = oad_classify_visa_group(visa_group)
    ) |>
    dplyr::filter(!is.na(.data$value), !is.na(.data$direction),
                  !is.na(.data$category))
  if (nrow(long) == 0L) return(tibble::tibble())

  long |>
    dplyr::mutate(
      period         = parse_fy_label(.data$year_label),
      period_unit    = "year",
      vintage_status = nom_classify_status(.data$visa_group, .data$period, asof),
      series_id      = paste0("nom_annual:", .data$category, ":", .data$direction),
      unit           = "persons",
      metadata       = as.character(jsonlite::toJSON(
        list(file = basename(path)), auto_unbox = TRUE))
    ) |>
    dplyr::filter(!is.na(.data$period)) |>
    dplyr::group_by(.data$period, .data$category, .data$direction,
                    .data$vintage_status, .data$series_id,
                    .data$period_unit, .data$unit, .data$metadata) |>
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") |>
    dplyr::select("series_id", "period", "period_unit", "value",
                  "category", "vintage_status", "unit", "metadata")
}

#' Parse an ABS financial-year label (`"2023-24"`) into the FY-end date
#' (`2024-06-30`, normalised to `2024-07-01` as the period start).
#' @keywords internal
parse_fy_label <- function(x) {
  x_chr <- as.character(x)
  m <- stringr::str_match(x_chr, "^(\\d{4})-?(\\d{2})$")
  yr <- suppressWarnings(as.integer(m[, 2]))
  ok <- !is.na(yr)
  out <- rep(as.Date(NA_character_), length(x_chr))
  out[ok] <- as.Date(sprintf("%d-07-01", yr[ok] + 1L))
  out
}

#' Classify a NOM observation's vintage status as preliminary / revised
#' / final based on the age of the reference period at the as-of date.
#'
#' This is a heuristic — until we read ABS release notes the only
#' signal we have is "how long ago was the reference period". The
#' thresholds match published ABS revision practice (preliminary for
#' ~3 quarters, revised through ~6 quarters, treated as final after).
#'
#' @keywords internal
nom_classify_status <- function(series_desc, period, asof) {
  if (length(period) == 0L) return(character())
  age_q <- as.numeric((as.Date(asof) - as.Date(period)) / 90)
  dplyr::case_when(
    age_q < 3  ~ "preliminary",
    age_q < 6  ~ "revised",
    TRUE       ~ "final"
  )
}

#' Map an ABS NOM `series` description to a canonical category. Used by
#' [reenrich_nom()] when reading from the vintage store.
#' @keywords internal
nom_classify_category <- function(series_desc) {
  oad_classify_visa_group(series_desc)
}

#' @keywords internal
fetch_nom_fallback <- function(cfg, asof) {
  cache_dir <- fs::path(cfg$paths$raw, "nom")
  if (!fs::dir_exists(cache_dir)) {
    nn_warn("No cached NOM data and live fetch failed.")
    return(tibble::tibble())
  }
  recent <- fs::dir_ls(cache_dir, type = "directory") |> sort(decreasing = TRUE)
  if (!length(recent)) return(tibble::tibble())
  files <- fs::dir_ls(recent[[1]], regexp = "\\.xlsx$")
  if (!length(files)) return(tibble::tibble())
  nn_warn("NOM: using cached vintage {basename(recent[[1]])}")
  parse_nom_quarterly_cube(files[[1]], asof)
}
