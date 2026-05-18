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
  # Latest available smoothed π per category — used as fallback for
  # quarters not represented in pi_smoothed (e.g. recent periods past
  # the completion cutoff).
  pi_last <- pi_smoothed |>
    dplyr::arrange(.data$period) |>
    dplyr::group_by(.data$category) |>
    dplyr::summarise(
      pi_latest = dplyr::last(stats::na.omit(.data$pi_smoothed)),
      se_latest = dplyr::last(stats::na.omit(.data$pi_se)),
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
#' Aggregates by summing means and combining variances (assumed
#' independent across categories — a Phase-1 simplification documented
#' in the methodology note).
#'
#' @param nowcast_categories Output of [build_nowcast_categories()].
#' @param cfg Project config.
#' @return Tibble: `period`, `nom_mean`, `nom_se`, `lower_80`,
#'   `upper_80`, `lower_95`, `upper_95`.
#' @export
build_nowcast_headline <- function(nowcast_categories, cfg) {
  if (nrow(nowcast_categories) == 0L) {
    return(tibble::tibble(
      period = as.Date(character()), nom_mean = numeric(),
      nom_se = numeric(),
      lower_80 = numeric(), upper_80 = numeric(),
      lower_95 = numeric(), upper_95 = numeric()
    ))
  }
  ci80 <- cfg$reporting$headline_ci %||% 0.80
  ci95 <- cfg$reporting$secondary_ci %||% 0.95
  z80 <- stats::qnorm(0.5 + ci80 / 2)
  z95 <- stats::qnorm(0.5 + ci95 / 2)

  nowcast_categories |>
    dplyr::group_by(.data$period) |>
    dplyr::summarise(
      nom_mean = sum(.data$nom_mean, na.rm = TRUE),
      nom_var  = sum(.data$nom_se^2, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      nom_se   = sqrt(.data$nom_var),
      lower_80 = .data$nom_mean - z80 * .data$nom_se,
      upper_80 = .data$nom_mean + z80 * .data$nom_se,
      lower_95 = .data$nom_mean - z95 * .data$nom_se,
      upper_95 = .data$nom_mean + z95 * .data$nom_se
    ) |>
    dplyr::select(-"nom_var")
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
