#' Fetch ABS Overseas Arrivals and Departures (OAD) data cubes
#'
#' ABS has migrated OAD from time-series spreadsheets to Excel data
#' cubes. We use `{readabs}::download_abs_data_cube()` to retrieve the
#' national tables that are most relevant for nowcasting:
#'
#'   * **Table 15.9** — Total arrivals by visa group, Australia
#'   * **Table 16.9** — Total departures by visa group, Australia
#'
#' Both contain monthly observations from Jul-2004 to the latest release
#' month. Each row is a month; columns are visa groups (Permanent,
#' Skilled, Student, Working Holiday, Visitor, Other Temporary,
#' Australian Citizens etc.). The first ~16 rows are metadata; we
#' auto-detect the header row by scanning column A for the first
#' parseable date.
#'
#' The legacy time-series API (`readabs::read_abs(cat_no="3401.0")`) no
#' longer works for this catalogue — readabs raises "Cannot find valid
#' entry in the ABS Time Series Directory" because OAD time-series
#' spreadsheets were retired.
#'
#' On fetch failure (network, parser breakage after an ABS layout
#' change) we fall back to the most recent cached XLSX in
#' `data/raw/oad/`.
#'
#' @inheritParams fetch_oad_via_readabs
#' @return Tibble: `series_id`, `period`, `period_unit`, `value`,
#'   `category`, `direction`, `unit`, `metadata`.
#' @export
fetch_oad <- function(cfg, asof) {
  out <- tryCatch(
    fetch_oad_data_cubes(cfg, asof),
    error = function(e) {
      nn_warn("OAD fetch failed: {conditionMessage(e)}")
      fetch_oad_fallback(cfg, asof)
    }
  )
  if (is.null(out) || nrow(out) == 0L) {
    cli::cli_abort("OAD fetch returned no rows.")
  }
  out
}

#' Download OAD data cubes and parse them into long form
#'
#' Two streams come out of this fetcher:
#'
#'   * **Aggregate long-term** — Tables 1 and 2 of cat. 3401.0
#'     (`340101.xlsx`, `340102.xlsx`) follow the standard ABS time
#'     series layout and contain Permanent, Long-term and Short-term
#'     arrivals/departures. We pull Permanent+Long-term as the
#'     canonical NOM-relevant flow.
#'
#'   * **By visa group** — Tables 15.9 and 16.9 (`3401015.xlsx`,
#'     `3401016.xlsx`) give arrivals/departures by visa group for
#'     Australia, but cover *total* movements (short + long + permanent
#'     combined). Used for category-level diagnostics and Phase 2.
#'
#' Both streams are returned in a single tibble; `category == "total"`
#' marks the aggregate long-term rows.
#'
#' @param cfg Project config.
#' @param asof As-of date for cache pathing.
#' @keywords internal
fetch_oad_data_cubes <- function(cfg, asof) {
  slug <- cfg$abs$oad_slug %||% "overseas-arrivals-and-departures-australia"
  out_dir <- fs::path(cfg$paths$raw, "oad", format(asof, "%Y-%m-%d"))
  fs::dir_create(out_dir)

  aggregate <- fetch_oad_aggregate_long_term(slug, out_dir)

  by_visa_specs <- list(
    list(file = "3401015.xlsx", direction = "arrival",   sheet = "Table 15.9"),
    list(file = "3401016.xlsx", direction = "departure", sheet = "Table 16.9")
  )
  by_visa <- purrr::map_dfr(by_visa_specs, function(spec) {
    nn_info("OAD: downloading {spec$file}")
    path <- tryCatch(
      readabs::download_abs_data_cube(
        catalogue_string = slug,
        cube             = spec$file,
        path             = out_dir
      ),
      error = function(e) NULL
    )
    if (is.null(path) || !file.exists(path)) {
      nn_warn("OAD: failed to download {spec$file}")
      return(empty_oad())
    }
    parse_oad_cube(path, spec$sheet, spec$direction)
  })

  dplyr::bind_rows(aggregate, by_visa)
}

#' Download Tables 1/2 and extract the canonical
#' "Permanent and Long-term" national flow series.
#'
#' These series are stored in the standard ABS time-series spreadsheet
#' layout (sheet "Data1": metadata in rows 1-10, monthly observations
#' from row 11). We hard-code the series IDs because they are stable
#' identifiers; if ABS reissues a table with new IDs the fallback
#' filters by series description.
#'
#' @keywords internal
fetch_oad_aggregate_long_term <- function(slug, out_dir) {
  table_specs <- list(
    list(file = "340101.xlsx", direction = "arrival",
         sid = "A85232568L",
         label_pat = "(?i)Permanent and Long.?term Arrivals"),
    list(file = "340102.xlsx", direction = "departure",
         sid = "A85232558J",
         label_pat = "(?i)Permanent and Long.?term Departures")
  )
  purrr::map_dfr(table_specs, function(spec) {
    nn_info("OAD: downloading {spec$file} (aggregate long-term)")
    path <- tryCatch(
      readabs::download_abs_data_cube(
        catalogue_string = slug,
        cube             = spec$file,
        path             = out_dir
      ),
      error = function(e) NULL
    )
    if (is.null(path) || !file.exists(path)) {
      nn_warn("OAD: failed to download {spec$file}")
      return(empty_oad())
    }
    parse_oad_aggregate_sheet(path, "Data1", spec)
  })
}

#' @keywords internal
parse_oad_aggregate_sheet <- function(path, sheet, spec) {
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                            .name_repair = "minimal")
  if (nrow(raw) < 11L) return(empty_oad())
  labels <- as.character(unlist(raw[1L,  ]))
  ids    <- as.character(unlist(raw[10L, ]))
  # Prefer match by series ID; fall back to label regex.
  col <- which(ids == spec$sid)
  if (!length(col)) col <- which(grepl(spec$label_pat, labels))
  if (!length(col)) {
    nn_warn("OAD aggregate: could not find series {spec$sid} ({spec$label_pat}) in {basename(path)}")
    return(empty_oad())
  }
  col <- col[1L]
  period_serial <- suppressWarnings(as.numeric(unlist(raw[11:nrow(raw), 1L])))
  period <- as.Date(period_serial, origin = "1899-12-30")
  vals <- suppressWarnings(as.numeric(unlist(raw[11:nrow(raw), col])))
  tibble::tibble(
    series_id   = paste0("oad_lt:", spec$direction, ":total"),
    period      = lubridate::floor_date(period, "month"),
    period_unit = "month",
    value       = vals,
    category    = "total",
    direction   = spec$direction,
    unit        = "persons",
    metadata    = as.character(jsonlite::toJSON(
      list(file = basename(path), sheet = sheet,
           abs_series_id = ids[col], series = labels[col]),
      auto_unbox = TRUE))
  ) |>
    dplyr::filter(!is.na(.data$period), !is.na(.data$value))
}

#' Parse a single OAD data-cube sheet
#'
#' The sheet has ~15 lines of preamble (title, footnotes), one or two
#' header lines containing visa-group names, then monthly rows with the
#' month in column A.
#'
#' @param path Path to the XLSX.
#' @param sheet Sheet name (e.g. `"Table 15.9"`).
#' @param direction `"arrival"` or `"departure"`.
#' @keywords internal
parse_oad_cube <- function(path, sheet, direction) {
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                            .name_repair = "minimal")
  hdr_row <- detect_oad_header_row(raw)
  if (is.na(hdr_row)) {
    nn_warn("OAD: could not detect header row in {basename(path)}::{sheet}")
    return(empty_oad())
  }
  body <- readxl::read_excel(path, sheet = sheet, skip = hdr_row - 1L,
                             .name_repair = "minimal")
  # Drop columns with NA/empty names — ABS templates often have spacer
  # columns that confuse pivot_longer.
  keep_cols <- !is.na(names(body)) & nzchar(names(body))
  body <- body[, keep_cols, drop = FALSE]
  # First column is the date label
  names(body)[1] <- "period_raw"
  body$period <- parse_oad_month(body[["period_raw"]])
  body <- body[!is.na(body$period), , drop = FALSE]

  visa_cols <- names(body)
  visa_cols <- setdiff(visa_cols, c("period_raw", "period"))
  visa_cols <- visa_cols[!grepl("^Total$|^[Tt]otal", visa_cols)]

  long <- body |>
    dplyr::select(dplyr::all_of(c("period", visa_cols))) |>
    tidyr::pivot_longer(
      cols = -"period", names_to = "visa_group", values_to = "value",
      values_transform = list(value = function(x) suppressWarnings(as.numeric(x)))
    ) |>
    dplyr::filter(!is.na(.data$value))

  cfg <- config::get(file = "config.yml")
  long |>
    dplyr::mutate(
      period_unit = "month",
      category    = oad_classify_visa_group(.data$visa_group),
      direction   = direction,
      series_id   = paste0("oad:", .data$category, ":", direction),
      unit        = "persons",
      metadata    = vapply(.data$visa_group, function(v) {
        as.character(jsonlite::toJSON(
          list(file = basename(path), sheet = sheet, visa_group = v),
          auto_unbox = TRUE))
      }, character(1))
    ) |>
    dplyr::filter(!is.na(.data$category)) |>
    dplyr::select("series_id", "period", "period_unit", "value",
                  "category", "direction", "unit", "metadata")
}

#' Detect the header row in an OAD data-cube sheet.
#'
#' Heuristic: scan column A for the first run of at least three
#' consecutive parseable Excel-serial date values. The row immediately
#' before that run is the header row.
#'
#' Using a "strict" date check (Excel serial in a plausible range, or
#' a bare month-year string with nothing else) is essential — the
#' footnote text on these sheets contains substrings like "May 10"
#' that the permissive [parse_oad_month()] would happily parse as a
#' date.
#' @keywords internal
detect_oad_header_row <- function(raw) {
  col1 <- raw[[1]]
  is_strict_date <- vapply(col1, function(v) {
    if (is.na(v)) return(FALSE)
    if (inherits(v, c("Date", "POSIXt"))) return(TRUE)
    s <- stringr::str_trim(as.character(v))
    # Bare Excel serial in plausible range (~1990 to ~2050)
    n <- suppressWarnings(as.numeric(s))
    if (!is.na(n) && n >= 30000 && n <= 60000) return(TRUE)
    # Bare month-year only
    if (grepl("^[A-Za-z]{3,}[ -]\\d{4}$", s)) return(TRUE)
    if (grepl("^\\d{4}-\\d{2}(-\\d{2})?$", s)) return(TRUE)
    FALSE
  }, logical(1))
  # Find the start of the first run of >= 3 consecutive strict dates
  n <- length(is_strict_date)
  if (n < 3L) return(NA_integer_)
  for (i in seq_len(n - 2L)) {
    if (is_strict_date[i] && is_strict_date[i + 1L] && is_strict_date[i + 2L]) {
      return(max(i - 1L, 1L))
    }
  }
  NA_integer_
}

#' Parse an OAD month label, accepting Excel serial numbers, "Jul-2004"
#' style strings, "Jul 2004" strings, and ISO date strings.
#' @keywords internal
parse_oad_month <- function(x) {
  if (length(x) == 0L) return(as.Date(integer(0), origin = "1970-01-01"))
  if (inherits(x, c("Date", "POSIXt"))) {
    return(lubridate::floor_date(as.Date(x), "month"))
  }
  x_chr <- as.character(x)
  parsed <- suppressWarnings(lubridate::my(x_chr))                  # Jul-2004
  na1 <- is.na(parsed)
  parsed[na1] <- suppressWarnings(lubridate::ym(x_chr[na1]))         # 2004-07
  na2 <- is.na(parsed)
  parsed[na2] <- suppressWarnings(lubridate::ymd(x_chr[na2]))        # 2004-07-01
  # Excel numerics
  na3 <- is.na(parsed)
  num <- suppressWarnings(as.numeric(x_chr[na3]))
  date_from_num <- as.Date(num, origin = "1899-12-30")
  parsed[na3] <- date_from_num
  lubridate::floor_date(parsed, "month")
}

#' Map an ABS OAD visa-group header to a canonical category
#'
#' OAD Tables 15/16 use visa-group labels with "Permanent" / "Temporary"
#' qualifiers (e.g. "Temporary Student Visas - Higher Educ.(h)",
#' "Permanent Skilled Visas", "Temporary Working Visas"). Subtotal
#' rows ("Permanent Visas - Total", "Temporary Visas - Total") are
#' dropped to avoid double-counting.
#' @keywords internal
oad_classify_visa_group <- function(group) {
  trim <- stringr::str_trim(as.character(group))
  out <- dplyr::case_when(
    # Drop subtotals — caller filters NA, so this prevents
    # double-counting permanent + temporary + total + ...
    stringr::str_detect(trim, "(?i)Total|^Total$") &
      !stringr::str_detect(trim, "(?i)Postgraduate Research") ~ NA_character_,
    # Special Category Visa subclass 444 = NZ citizens
    stringr::str_detect(trim, "(?i)444|special category") ~ "nz_citizen",
    stringr::str_detect(trim, "(?i)student")               ~ "student",
    stringr::str_detect(trim, "(?i)skill")                 ~ "skilled",
    stringr::str_detect(trim, "(?i)working")               ~ "working_holiday",
    stringr::str_detect(trim, "(?i)working\\s*holiday")    ~ "working_holiday",
    stringr::str_detect(trim, "(?i)family|partner")        ~ "family",
    stringr::str_detect(trim, "(?i)new\\s*zealand")        ~ "nz_citizen",
    stringr::str_detect(trim, "(?i)bridging")              ~ "bridging",
    stringr::str_detect(trim, "(?i)visitor")               ~ "other_temp",
    stringr::str_detect(trim, "(?i)australian\\s*citizen|aust.*resident") ~ "returning_au",
    # Catch-all temporary / permanent buckets, after specific categories
    stringr::str_detect(trim, "(?i)temporary")             ~ "other_temp",
    stringr::str_detect(trim, "(?i)permanent")             ~ "permanent",
    stringr::str_detect(trim, "(?i)other")                 ~ "other",
    TRUE                                                    ~ NA_character_
  )
  unname(out)
}

#' @keywords internal
empty_oad <- function() {
  tibble::tibble(
    series_id = character(), period = as.Date(character()),
    period_unit = character(), value = numeric(),
    category = character(), direction = character(),
    unit = character(), metadata = character()
  )
}

#' Legacy entry point kept for backwards compatibility with vintage
#' replay code that imports it by name. Delegates to the data-cube
#' implementation.
#' @keywords internal
fetch_oad_via_readabs <- function(cat_no, tables, cfg, asof) {
  fetch_oad_data_cubes(cfg, asof)
}

#' Map a raw OAD `series` description to a canonical category (used by
#' [reenrich_oad()] when reading from the vintage store).
#'
#' @keywords internal
oad_classify_category <- function(series_desc, cfg) {
  if (length(series_desc) == 0L) return(character(0))
  oad_classify_visa_group(series_desc)
}

#' @keywords internal
oad_classify_direction <- function(series_desc) {
  dplyr::case_when(
    stringr::str_detect(series_desc, "(?i)arrival") ~ "arrival",
    stringr::str_detect(series_desc, "(?i)departure") ~ "departure",
    TRUE ~ NA_character_
  )
}

#' Fallback using the most recently cached OAD XLSX(s).
#' @keywords internal
fetch_oad_fallback <- function(cfg, asof) {
  cache_dir <- fs::path(cfg$paths$raw, "oad")
  if (!fs::dir_exists(cache_dir)) {
    cli::cli_abort("No cached OAD data and live fetch failed.")
  }
  recent <- fs::dir_ls(cache_dir, type = "directory") |> sort(decreasing = TRUE)
  if (!length(recent)) cli::cli_abort("No cached OAD data and live fetch failed.")
  xlsx <- fs::dir_ls(recent[[1]], regexp = "\\.xlsx$")
  if (!length(xlsx)) cli::cli_abort("Cached OAD dir is empty: {recent[[1]]}")
  nn_warn("OAD: using cached vintage {basename(recent[[1]])}")

  purrr::map_dfr(xlsx, function(p) {
    sheets <- readxl::excel_sheets(p)
    nat_arr <- sheets[grepl("15\\.9", sheets)][1]
    nat_dep <- sheets[grepl("16\\.9", sheets)][1]
    out <- empty_oad()
    if (!is.na(nat_arr)) out <- dplyr::bind_rows(out, parse_oad_cube(p, nat_arr, "arrival"))
    if (!is.na(nat_dep)) out <- dplyr::bind_rows(out, parse_oad_cube(p, nat_dep, "departure"))
    out
  })
}
