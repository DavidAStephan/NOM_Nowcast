#' Random-walk benchmark
#'
#' Forecast at horizon h is the most recent observed NOM_final at the
#' evaluation date.
#'
#' @param panel Modelling panel.
#' @param cfg Project config.
#' @return Tibble: `period`, `category`, `nom_mean`, `model`.
#' @export
benchmark_random_walk <- function(panel, cfg) {
  panel |>
    dplyr::filter(.data$category == "total") |>
    dplyr::arrange(.data$period) |>
    dplyr::mutate(nom_mean = dplyr::lag(.data$nom_final),
                  model = "random_walk") |>
    dplyr::transmute(.data$period, .data$category, .data$nom_mean, .data$model)
}

#' AR(1)-on-growth benchmark
#'
#' Forecasts quarterly NOM via an AR(1) on quarter-on-quarter growth.
#' Fitted recursively for backtests; here we just return the in-sample
#' fitted values.
#'
#' @param panel Modelling panel.
#' @param cfg Project config.
#' @return Tibble: `period`, `category`, `nom_mean`, `model`.
#' @export
benchmark_ar1 <- function(panel, cfg) {
  totals <- panel |>
    dplyr::filter(.data$category == "total", !is.na(.data$nom_final)) |>
    dplyr::arrange(.data$period)
  if (nrow(totals) < 6L) {
    return(tibble::tibble(period = as.Date(character()),
                          category = character(), nom_mean = numeric(),
                          model = character()))
  }
  y <- totals$nom_final
  g <- diff(y) / y[-length(y)]
  fit <- tryCatch(stats::arima(g, order = c(1, 0, 0)), error = function(e) NULL)
  if (is.null(fit)) {
    return(tibble::tibble(period = totals$period,
                          category = "total",
                          nom_mean = y, model = "ar1"))
  }
  fitted_g <- as.numeric(fitted(fit))
  fitted_y <- y[-length(y)] * (1 + fitted_g)
  tibble::tibble(
    period = totals$period[-1],
    category = "total",
    nom_mean = fitted_y,
    model = "ar1"
  )
}

#' ABS-preliminary "benchmark"
#'
#' At each backtest date, take the ABS preliminary value as the model's
#' forecast. This is the most important benchmark — for the project to
#' have any utility, the structural model needs to outperform it.
#'
#' @param panel Modelling panel.
#' @param cfg Project config.
#' @return Tibble: `period`, `category`, `nom_mean`, `model`.
#' @export
benchmark_abs_preliminary <- function(panel, cfg) {
  panel |>
    dplyr::filter(.data$category == "total") |>
    dplyr::transmute(.data$period, .data$category,
                     nom_mean = .data$nom_preliminary,
                     model = "abs_preliminary")
}
