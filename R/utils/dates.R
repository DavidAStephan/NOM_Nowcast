#' Project as-of date
#'
#' Resolves the run as-of date from config. When `run$asof_date` is `NULL`
#' (the default for production runs) returns `Sys.Date()`. When it is set
#' (e.g. for a vintage replay) returns the parsed date.
#'
#' @param cfg The list returned by [config::get()].
#' @return A `Date`.
#' @export
nn_asof <- function(cfg) {
  raw <- cfg$run$asof_date
  if (is.null(raw) || isTRUE(is.na(raw)) || identical(raw, "")) {
    return(Sys.Date())
  }
  as.Date(raw)
}

#' Quarter floor for a `Date`
#'
#' Returns the first day of the calendar quarter containing `x`.
#'
#' @param x A vector coercible to `Date`.
#' @return A `Date`.
#' @export
nn_quarter_start <- function(x) {
  d <- as.Date(x)
  lubridate::floor_date(d, unit = "quarter")
}

#' Last completed quarter at a reference date
#'
#' "Completed" means: the quarter has fully elapsed. e.g. on 2025-05-18 the
#' last completed quarter is 2025-Q1 starting 2025-01-01.
#'
#' @param asof Reference date.
#' @return A `Date` representing the start of that quarter.
#' @export
nn_last_completed_quarter <- function(asof) {
  this_q <- nn_quarter_start(asof)
  this_q - lubridate::days(1)  # somewhere in previous quarter
  nn_quarter_start(this_q - lubridate::days(1))
}

#' All quarter starts in `[from, to]` inclusive
#'
#' @param from,to `Date`-coercible bounds.
#' @return Vector of quarter-start `Date`s.
#' @export
nn_quarter_seq <- function(from, to) {
  from_q <- nn_quarter_start(from)
  to_q   <- nn_quarter_start(to)
  seq.Date(from_q, to_q, by = "quarter")
}

#' Add a publication lag to determine observability
#'
#' A series with period `t` and publication lag `lag_days` is only
#' observable from `t + period_length + lag_days` onwards.
#'
#' @param period A `Date` (start of the reference period).
#' @param period_unit One of `"month"` or `"quarter"`.
#' @param lag_days Numeric publication lag in days.
#' @return A `Date` — the first date at which the value is observable.
#' @export
nn_first_observable <- function(period, period_unit = c("month", "quarter"),
                                lag_days = 0) {
  period_unit <- match.arg(period_unit)
  period_end <- if (period_unit == "month") {
    lubridate::ceiling_date(period, unit = "month") - lubridate::days(1)
  } else {
    lubridate::ceiling_date(period, unit = "quarter") - lubridate::days(1)
  }
  period_end + lubridate::days(lag_days) + lubridate::days(1)
}

#' Yearquarter helper using `{tsibble}` semantics
#'
#' @param x A `Date` vector.
#' @return A `tsibble::yearquarter`.
#' @export
nn_yq <- function(x) {
  tsibble::yearquarter(as.Date(x))
}
