#' Convert Kalman forecasts + smoothed π into category-level NOM
#'
#' For each `(period, category)`:
#'
#'   NOM_hat_c,q = pi_c,q * arrivals_hat_c,q - pi_dep_c,q * departures_hat_c,q
#'
#' Treats π identically on both sides for simplicity; the methodology
#' note flags this as a Phase-1 approximation. Returned variance uses a
#' first-order delta method on the product `π * A*` and ignores the
#' covariance between arrivals and departures (a conservative
#' simplification at the quarterly horizon).
#'
#' @param kalman_forecasts Output of [forecast_kalman_univariate()].
#' @param pi_smoothed Output of [smooth_pi()] (or its projection).
#' @param cfg Project config.
#' @return Tibble: `period`, `category`, `nom_mean`, `nom_se`,
#'   `lower_80`, `upper_80`, `lower_95`, `upper_95`.
#' @export
build_nowcast_categories <- function(kalman_forecasts, pi_smoothed, cfg) {
  if (nrow(kalman_forecasts) == 0L) {
    return(empty_nowcast())
  }
  # Forward projection of π for quarters past the completion cutoff.
  # Phase 1 raw π is noisy (quarterly ABS NOM revisions ± 30%), so the
  # latest single value is a poor extrapolation. Use the median of the
  # last N quarters within each category by default.
  proj_method <- cfg$pi$projection %||% "median_n"
  proj_n      <- cfg$pi$projection_window %||% 8L
  pi_last <- pi_smoothed |>
    dplyr::arrange(.data$period) |>
    dplyr::group_by(.data$category) |>
    dplyr::summarise(
      pi_latest = project_pi_value(.data$pi_smoothed, proj_method, proj_n),
      se_latest = project_pi_value(.data$pi_se,       proj_method, proj_n),
      .groups   = "drop"
    )
  pi_period <- pi_smoothed |>
    dplyr::select("period", "category", "pi_smoothed", "pi_se")

  fc_wide <- kalman_forecasts |>
    tidyr::pivot_wider(
      id_cols    = c("period", "category"),
      names_from = "direction",
      values_from = c("mean_level", "se_log")
    )
  needed <- c("mean_level_arrival", "mean_level_departure",
              "se_log_arrival", "se_log_departure")
  for (n in needed) if (!n %in% names(fc_wide)) fc_wide[[n]] <- NA_real_

  finite_or_na <- function(x) {
    x[!is.finite(x)] <- NA_real_
    x
  }
  joined <- fc_wide |>
    dplyr::left_join(pi_period, by = c("period", "category")) |>
    dplyr::left_join(pi_last,   by = "category") |>
    dplyr::mutate(
      mean_level_arrival   = finite_or_na(.data$mean_level_arrival),
      mean_level_departure = finite_or_na(.data$mean_level_departure),
      se_log_arrival       = finite_or_na(.data$se_log_arrival),
      se_log_departure     = finite_or_na(.data$se_log_departure),
      pi_use     = dplyr::coalesce(.data$pi_smoothed, .data$pi_latest),
      pi_se_use  = finite_or_na(dplyr::coalesce(.data$pi_se, .data$se_latest)),
      nom_mean   = .data$pi_use *
                   (.data$mean_level_arrival - .data$mean_level_departure),
      var_term   = (.data$pi_use^2) *
                     ((.data$mean_level_arrival * .data$se_log_arrival)^2 +
                      (.data$mean_level_departure * .data$se_log_departure)^2) +
                   (.data$pi_se_use^2) *
                     (.data$mean_level_arrival - .data$mean_level_departure)^2,
      var_term   = finite_or_na(.data$var_term),
      nom_se     = sqrt(pmax(.data$var_term, 0, na.rm = TRUE))
    )

  ci80 <- cfg$reporting$headline_ci %||% 0.80
  ci95 <- cfg$reporting$secondary_ci %||% 0.95
  z80 <- stats::qnorm(0.5 + ci80 / 2)
  z95 <- stats::qnorm(0.5 + ci95 / 2)

  joined |>
    dplyr::transmute(
      .data$period, .data$category,
      .data$nom_mean, .data$nom_se,
      lower_80 = .data$nom_mean - z80 * .data$nom_se,
      upper_80 = .data$nom_mean + z80 * .data$nom_se,
      lower_95 = .data$nom_mean - z95 * .data$nom_se,
      upper_95 = .data$nom_mean + z95 * .data$nom_se
    )
}

#' Aggregate category-level NOM to a headline series
#'
#' Prefers the dedicated `category == "total"` Kalman fit (which is
#' driven by the ABS Permanent+Long-term aggregate) when it is
#' available, because the aggregate series has lower noise than the
#' sum of category-level fits. Falls back to summing across the
#' canonical visa categories otherwise.
#'
#' @param nowcast_categories Output of [build_nowcast_categories()].
#' @param cfg Project config.
#' @return Tibble: `period`, `nom_mean`, `nom_se`, `lower_80`,
#'   `upper_80`, `lower_95`, `upper_95`.
#' @export
build_nowcast_headline <- function(nowcast_categories, cfg) {
  if (nrow(nowcast_categories) == 0L) {
    return(empty_headline())
  }
  ci80 <- cfg$reporting$headline_ci %||% 0.80
  ci95 <- cfg$reporting$secondary_ci %||% 0.95
  z80 <- stats::qnorm(0.5 + ci80 / 2)
  z95 <- stats::qnorm(0.5 + ci95 / 2)

  total_rows <- nowcast_categories |>
    dplyr::filter(.data$category == "total", !is.na(.data$nom_mean))
  base <- if (nrow(total_rows) > 0L) {
    total_rows |>
      dplyr::transmute(.data$period, .data$nom_mean, .data$nom_se)
  } else {
    nowcast_categories |>
      dplyr::filter(.data$category != "total") |>
      dplyr::group_by(.data$period) |>
      dplyr::summarise(
        nom_mean = sum(.data$nom_mean, na.rm = TRUE),
        nom_se   = sqrt(sum(.data$nom_se^2, na.rm = TRUE)),
        .groups  = "drop"
      )
  }

  base |>
    dplyr::mutate(
      lower_80 = .data$nom_mean - z80 * .data$nom_se,
      upper_80 = .data$nom_mean + z80 * .data$nom_se,
      lower_95 = .data$nom_mean - z95 * .data$nom_se,
      upper_95 = .data$nom_mean + z95 * .data$nom_se
    )
}

#' @keywords internal
empty_headline <- function() {
  tibble::tibble(
    period = as.Date(character()), nom_mean = numeric(),
    nom_se = numeric(),
    lower_80 = numeric(), upper_80 = numeric(),
    lower_95 = numeric(), upper_95 = numeric()
  )
}

#' Project π forward past the empirical-cohort cutoff
#'
#' Robust to noisy quarterly π by using a rolling summary over the
#' last `n` non-NA values (default: median of last 8 quarters).
#' @keywords internal
project_pi_value <- function(x, method = "median_n", n = 8L) {
  x <- stats::na.omit(x)
  if (!length(x)) return(NA_real_)
  recent <- utils::tail(x, n)
  switch(method,
    latest   = utils::tail(x, 1L),
    mean_n   = mean(recent),
    median_n = stats::median(recent),
    stats::median(recent)
  )
}

#' @keywords internal
empty_nowcast <- function() {
  tibble::tibble(
    period = as.Date(character()), category = character(),
    nom_mean = numeric(), nom_se = numeric(),
    lower_80 = numeric(), upper_80 = numeric(),
    lower_95 = numeric(), upper_95 = numeric()
  )
}
