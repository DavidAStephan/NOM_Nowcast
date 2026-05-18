#' Initialise the DuckDB vintage store
#'
#' Creates the database file if it does not exist and ensures the
#' canonical schema is in place. Returns the path to the database, so that
#' downstream `{targets}` nodes that depend on it can declare a file
#' dependency.
#'
#' The vintage table is:
#'
#' ```
#' vintages (
#'   source        VARCHAR  NOT NULL,
#'   series_id     VARCHAR  NOT NULL,
#'   period        DATE     NOT NULL,
#'   period_unit   VARCHAR  NOT NULL,          -- "month" or "quarter"
#'   vintage_date  DATE     NOT NULL,
#'   value         DOUBLE,
#'   unit          VARCHAR,
#'   metadata      VARCHAR,                    -- JSON blob
#'   PRIMARY KEY (source, series_id, period, vintage_date)
#' )
#' ```
#'
#' Multiple vintages of the same observation are stored side by side, never
#' overwritten. Updates within the same `(source, series_id, period,
#' vintage_date)` tuple replace the row (idempotent re-runs on the same
#' day).
#'
#' @param cfg The project config.
#' @return Path to the DuckDB file.
#' @export
vintages_init <- function(cfg) {
  path <- cfg$paths$duckdb
  fs::dir_create(fs::path_dir(path))
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS vintages (
      source        VARCHAR  NOT NULL,
      series_id     VARCHAR  NOT NULL,
      period        DATE     NOT NULL,
      period_unit   VARCHAR  NOT NULL,
      vintage_date  DATE     NOT NULL,
      value         DOUBLE,
      unit          VARCHAR,
      metadata      VARCHAR,
      PRIMARY KEY (source, series_id, period, vintage_date)
    )
  ")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_vintages_source_period
      ON vintages (source, period)
  ")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_vintages_vintage_date
      ON vintages (vintage_date)
  ")
  path
}

#' Connect to the vintage store
#'
#' @param db_path Path to the DuckDB file.
#' @param read_only Open in read-only mode? Defaults to `TRUE` so callers
#'   that only need to read won't block writers.
#' @return A DBI connection. Caller is responsible for disconnecting.
#' @export
vintages_connect <- function(db_path, read_only = TRUE) {
  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
}

#' Write a tibble of observations into the vintage store
#'
#' The input tibble must have columns `series_id`, `period` (Date),
#' `period_unit` ("month" / "quarter") and `value`. `unit` and `metadata`
#' are optional. The `vintage_date` is set from `asof`.
#'
#' @param db_path Path to the DuckDB file.
#' @param source Short source identifier (e.g. `"oad"`).
#' @param df Tibble of observations.
#' @param asof The vintage date to attach.
#' @return Number of rows written.
#' @export
vintages_write <- function(db_path, source, df, asof) {
  if (is.null(df) || nrow(df) == 0L) {
    nn_warn("vintages_write({source}): empty data, skipping.")
    return(0L)
  }
  required <- c("series_id", "period", "period_unit", "value")
  missing  <- setdiff(required, names(df))
  if (length(missing)) {
    cli::cli_abort("vintages_write: missing required columns: {missing}")
  }
  if (!"unit"     %in% names(df)) df$unit     <- NA_character_
  if (!"metadata" %in% names(df)) df$metadata <- NA_character_

  payload <- tibble::tibble(
    source       = source,
    series_id    = as.character(df$series_id),
    period       = as.Date(df$period),
    period_unit  = as.character(df$period_unit),
    vintage_date = as.Date(asof),
    value        = as.numeric(df$value),
    unit         = as.character(df$unit),
    metadata     = as.character(df$metadata)
  )

  con <- vintages_connect(db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbWriteTable(con, "vintages_stage", payload,
                    temporary = TRUE, overwrite = TRUE)
  DBI::dbExecute(con, "
    INSERT OR REPLACE INTO vintages
      (source, series_id, period, period_unit, vintage_date, value, unit, metadata)
    SELECT source, series_id, period, period_unit, vintage_date, value, unit, metadata
    FROM vintages_stage
  ")
  nrow(payload)
}

#' Read the as-of-`vintage_date` view of a source
#'
#' For each `(series_id, period)` returns the most recent vintage at or
#' before `vintage_date`. This is what callers should use during vintage
#' replay to avoid information leakage.
#'
#' @param db_path Path to the DuckDB file.
#' @param source Source identifier.
#' @param vintage_date Date or `NULL` for latest.
#' @return Tibble with `series_id`, `period`, `period_unit`, `value`,
#'   `unit`, `metadata`, `vintage_date`.
#' @export
vintages_read_asof <- function(db_path, source, vintage_date = NULL) {
  con <- vintages_connect(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  vintage_date <- if (is.null(vintage_date)) Sys.Date() else as.Date(vintage_date)
  sql <- "
    SELECT *
    FROM (
      SELECT v.*,
             ROW_NUMBER() OVER (
               PARTITION BY series_id, period
               ORDER BY vintage_date DESC
             ) AS rn
      FROM vintages v
      WHERE source = ?
        AND vintage_date <= ?
    )
    WHERE rn = 1
  "
  res <- DBI::dbGetQuery(con, sql, params = list(source, vintage_date))
  res$rn <- NULL
  tibble::as_tibble(res)
}

#' List all vintages observed for a series
#'
#' Useful for revision tracking and the methodology report.
#'
#' @param db_path Path to the DuckDB file.
#' @param source Source identifier.
#' @param series_id Specific series.
#' @return Long tibble: one row per `(period, vintage_date, value)`.
#' @export
vintages_history <- function(db_path, source, series_id) {
  con <- vintages_connect(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  sql <- "
    SELECT period, vintage_date, value, unit, metadata
    FROM vintages
    WHERE source = ? AND series_id = ?
    ORDER BY period, vintage_date
  "
  tibble::as_tibble(DBI::dbGetQuery(con, sql, params = list(source, series_id)))
}
