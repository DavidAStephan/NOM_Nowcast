#' Re-attach category / direction columns lost when round-tripping
#' through the vintage store
#'
#' The vintage store deliberately uses a narrow schema —
#' `(source, series_id, period, value, metadata)` — so it can hold any
#' source. The downstream `clean_*` functions expect the richer
#' tibbles produced by the original fetchers, with explicit `category`,
#' `direction`, `metric`, etc. columns.
#'
#' The fetchers all encode that structure into `series_id` using a
#' simple `name:dim1:dim2:...` convention (e.g. `oad:student:arrival`,
#' `dha_visa_grants:500`, `edu_students:commencements:VET:CHN`). These
#' helpers reverse the encoding so vintage replay works.
#'
#' When the source uses a different `series_id` convention (e.g.
#' ABS series IDs like `A2304585J`) we fall back to parsing the
#' `metadata` JSON, which the fetchers populate with the original
#' descriptive labels.
#' @name vintage_reenrich
#' @keywords internal
NULL

#' @rdname vintage_reenrich
reenrich_oad <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  if (all(c("category", "direction") %in% names(df))) return(df)
  meta <- purrr::map(df$metadata %||% rep(NA_character_, nrow(df)), safe_fromJSON)
  series_desc <- vapply(meta, function(m) {
    v <- m$series %||% NA_character_
    if (length(v) != 1L) NA_character_ else as.character(v)
  }, character(1))
  cfg <- config::get(file = "config.yml")
  df$category  <- oad_classify_category(series_desc, cfg)
  df$direction <- oad_classify_direction(series_desc)
  # Fallback: parse "oad:<category>:<direction>" pattern from series_id
  parts <- stringr::str_split_fixed(df$series_id, ":", n = 3L)
  df$category[is.na(df$category)]   <- parts[is.na(df$category), 2L]
  df$direction[is.na(df$direction)] <- parts[is.na(df$direction), 3L]
  df$category[df$category   == ""]  <- NA_character_
  df$direction[df$direction == ""]  <- NA_character_
  df
}

#' @rdname vintage_reenrich
reenrich_nom <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  if (all(c("category", "vintage_status") %in% names(df))) return(df)
  meta <- purrr::map(df$metadata %||% rep(NA_character_, nrow(df)), safe_fromJSON)
  series_desc <- vapply(meta, function(m) {
    v <- m$series %||% NA_character_
    if (length(v) != 1L) NA_character_ else as.character(v)
  }, character(1))
  df$category <- nom_classify_category(series_desc)
  parts <- stringr::str_split_fixed(df$series_id, ":", n = 2L)
  df$category[is.na(df$category)] <- parts[is.na(df$category), 2L]
  df$category[df$category == ""]  <- "total"
  df$vintage_status <- nom_classify_status(series_desc, df$period,
                                           max(df$vintage_date, na.rm = TRUE))
  df
}

#' @rdname vintage_reenrich
reenrich_visa_grants <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  if ("category" %in% names(df)) return(df)
  parts <- stringr::str_split_fixed(df$series_id, ":", n = 2L)
  subclass <- parts[, 2L]
  df$category <- map_subclass_to_category(subclass)
  df
}

#' @rdname vintage_reenrich
reenrich_students <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  if (all(c("metric", "sector") %in% names(df))) return(df)
  parts <- stringr::str_split_fixed(df$series_id, ":", n = 4L)
  df$metric   <- parts[, 2L]
  df$sector   <- parts[, 3L]
  df$category <- "student"
  df
}
