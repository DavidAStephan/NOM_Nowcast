#' Headline nowcast plot
#'
#' Shows the model's NOM nowcast with 80%/95% credible intervals, overlaid
#' with the ABS preliminary and (when available) final series.
#'
#' @param headline Output of [build_nowcast_headline()].
#' @param panel Output of [build_quarterly_panel()].
#' @return A `ggplot`.
#' @export
plot_nowcast_headline <- function(headline, panel) {
  truth <- panel |>
    dplyr::filter(.data$category == "total") |>
    dplyr::select("period", "nom_preliminary", "nom_final") |>
    tidyr::pivot_longer(c("nom_preliminary", "nom_final"),
                        names_to = "series", values_to = "value") |>
    dplyr::filter(!is.na(.data$value))

  ggplot2::ggplot(headline, ggplot2::aes(.data$period, .data$nom_mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_95, ymax = .data$upper_95),
                         alpha = 0.15, fill = "steelblue") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_80, ymax = .data$upper_80),
                         alpha = 0.30, fill = "steelblue") +
    ggplot2::geom_line(colour = "steelblue", linewidth = 0.7) +
    ggplot2::geom_line(data = truth,
                       ggplot2::aes(.data$period, .data$value, linetype = .data$series),
                       colour = "grey30", inherit.aes = FALSE) +
    ggplot2::scale_linetype_manual(values = c(nom_preliminary = "dashed",
                                              nom_final = "solid")) +
    ggplot2::labs(x = NULL, y = "NOM (persons per quarter)",
                  linetype = "ABS series",
                  title = "NOM nowcast versus ABS")
}

#' Category decomposition plot
#'
#' @param categories Output of [build_nowcast_categories()].
#' @return A `ggplot`.
#' @export
plot_nowcast_categories <- function(categories) {
  ggplot2::ggplot(categories,
                  ggplot2::aes(.data$period, .data$nom_mean, fill = .data$category)) +
    ggplot2::geom_col(width = 80) +
    ggplot2::labs(x = NULL, y = "NOM contribution",
                  fill = NULL,
                  title = "Category decomposition of model NOM")
}

#' π estimates plot
#'
#' @param pi_smoothed Output of [smooth_pi()].
#' @param pi_empirical Output of [estimate_pi_empirical()].
#' @return A `ggplot`.
#' @export
plot_pi <- function(pi_smoothed, pi_empirical) {
  ggplot2::ggplot() +
    ggplot2::geom_point(data = pi_empirical,
                        ggplot2::aes(.data$period, .data$pi_hat),
                        alpha = 0.5, size = 0.8) +
    ggplot2::geom_line(data = pi_smoothed,
                       ggplot2::aes(.data$period, .data$pi_smoothed),
                       colour = "firebrick", linewidth = 0.6) +
    ggplot2::facet_wrap(~ category, scales = "free_y") +
    ggplot2::labs(x = NULL, y = expression(hat(pi)[c]),
                  title = "Empirical and smoothed classification probabilities")
}

#' Backtest performance summary plot
#'
#' @param scored Output of [score_backtest()].
#' @return A `ggplot`.
#' @export
plot_backtest_rmse <- function(scored) {
  scored |>
    dplyr::filter(.data$category == "total") |>
    ggplot2::ggplot(ggplot2::aes(.data$horizon, .data$rmse, colour = .data$model)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ regime) +
    ggplot2::labs(x = "Horizon (quarters; 0 = nowcast)",
                  y = "RMSE vs final NOM",
                  colour = NULL,
                  title = "Backtest RMSE by model and horizon")
}
