#' Cli wrappers
#'
#' Thin shims so the entire codebase has a single place to switch logging
#' backend without rippling. Use these instead of bare `cli::cli_*`.
#' @name nn_log
#' @keywords internal
NULL

#' @rdname nn_log
#' @export
nn_info <- function(...) cli::cli_alert_info(...)

#' @rdname nn_log
#' @export
nn_success <- function(...) cli::cli_alert_success(...)

#' @rdname nn_log
#' @export
nn_warn <- function(...) cli::cli_alert_warning(...)

#' @rdname nn_log
#' @export
nn_danger <- function(...) cli::cli_alert_danger(...)

#' Time and log a code block
#'
#' @param label A short label, included in the start/finish messages.
#' @param expr The expression to run.
#' @return The value of `expr`.
#' @export
nn_time <- function(label, expr) {
  t0 <- Sys.time()
  nn_info("{label} ...")
  out <- force(expr)
  nn_success("{label} done in {format(round(Sys.time() - t0, 1))}.")
  out
}
