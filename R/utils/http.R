#' Build a project-standard `httr2` request
#'
#' Applies retry, exponential backoff and a project user-agent to all
#' outbound requests. The retry policy is read from `cfg$run`.
#'
#' @param url The URL to request.
#' @param cfg The project config.
#' @return An `httr2_request`.
#' @export
nn_request <- function(url, cfg) {
  retries <- cfg$run$http_retries %||% 4
  ua <- cfg$run$user_agent %||% "nomnowcast"
  httr2::request(url) |>
    httr2::req_user_agent(ua) |>
    httr2::req_retry(
      max_tries = retries,
      backoff = function(i) {
        backoff <- cfg$run$http_backoff_seconds %||% c(1, 4, 16, 64)
        backoff[min(i, length(backoff))]
      },
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(408, 425, 429, 500, 502, 503, 504)
      }
    ) |>
    httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400)
}

#' Download a remote file to the immutable raw store
#'
#' Filenames include the as-of date and a sha1 of the source URL so that
#' repeated downloads from the same source on the same day reuse the cached
#' artifact, but a future as-of triggers a fresh fetch.
#'
#' @param url Source URL.
#' @param subdir Subdirectory under `data/raw/` (e.g. `"oad"`).
#' @param cfg Project config.
#' @param asof As-of date.
#' @param ext File extension including the leading dot (e.g. `".xlsx"`).
#' @return Path to the downloaded file.
#' @export
nn_download <- function(url, subdir, cfg, asof, ext = NULL) {
  raw_root <- cfg$paths$raw %||% "data/raw"
  ext <- ext %||% paste0(".", tools::file_ext(url))
  if (identical(ext, ".")) ext <- ".bin"
  dest_dir <- fs::path(raw_root, subdir, format(asof, "%Y-%m-%d"))
  fs::dir_create(dest_dir)
  hash <- substr(digest::digest(url, algo = "sha1"), 1, 10)
  dest <- fs::path(dest_dir, glue::glue("{hash}{ext}"))
  if (fs::file_exists(dest) && fs::file_info(dest)$size > 0) {
    cli::cli_alert_info("Using cached download: {.path {dest}}")
    return(as.character(dest))
  }
  cli::cli_alert_info("Downloading {.url {url}} -> {.path {dest}}")
  resp <- nn_request(url, cfg) |> httr2::req_perform()
  writeBin(httr2::resp_body_raw(resp), dest)
  as.character(dest)
}

#' Null-coalescing operator (re-exported for internal use).
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
