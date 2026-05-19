#' Production-quality plot helpers used by the Quarto site
#'
#' All functions return `ggplot` objects so the site author can
#' customise via `+` operations. A shared theme keeps the visual
#' language consistent across pages.

#' Project plotting theme
#'
#' Light minimal grid, slightly larger title/axis text, clean
#' panel margins that work in 9-inch HTML embed widths.
#' @keywords internal
nn_theme <- function() {
  ggplot2::theme_minimal(base_size = 12, base_family = "") +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey92"),
      panel.grid.major.y = ggplot2::element_line(colour = "grey92"),
      axis.title.x       = ggplot2::element_blank(),
      axis.title.y       = ggplot2::element_text(size = 10, colour = "grey30"),
      plot.title         = ggplot2::element_text(size = 13, face = "bold",
                                                 colour = "grey15"),
      plot.subtitle      = ggplot2::element_text(size = 10, colour = "grey35",
                                                 margin = ggplot2::margin(b = 8)),
      plot.caption       = ggplot2::element_text(size = 8, colour = "grey45",
                                                 hjust = 0,
                                                 margin = ggplot2::margin(t = 6)),
      legend.position    = "top",
      legend.title       = ggplot2::element_text(size = 9, colour = "grey30"),
      legend.text        = ggplot2::element_text(size = 9),
      legend.key.width   = grid::unit(1.4, "lines"),
      plot.margin        = ggplot2::margin(8, 12, 8, 8)
    )
}

#' Project palette
#' @keywords internal
nn_palette <- list(
  model    = "#1f4e79",
  ribbon80 = "#5b9bd5",
  ribbon95 = "#5b9bd5",
  abs      = "#c62828",
  prelim   = "#9c9c9c",
  bridge   = "#2e7d32",
  rw       = "#888888",
  ar1      = "#bb8a3d"
)

#' Format Y axis as "Xk" thousands
#' @keywords internal
nn_y_thousands <- function() {
  ggplot2::scale_y_continuous(
    labels = function(x) paste0(scales::number(x / 1000, accuracy = 1), "k")
  )
}

#' Headline plot: model nowcast over time vs ABS final NOM
#'
#' Shows the headline tibble's `nom_mean` with 80% / 95% intervals,
#' overlaid with ABS final NOM as a thick line. A vertical reference
#' line marks the as-of date.
#'
#' @param headline Output of [build_nowcast_headline()] or
#'   [build_nowcast_headline_from_multi()].
#' @param panel Output of [build_quarterly_panel()].
#' @param asof Reference as-of date for the vertical marker.
#' @param min_period Start of plot window. Default: 5 years before
#'   the latest period in `headline`.
#' @return A `ggplot`.
#' @export
plot_nowcast_headline <- function(headline, panel, asof = Sys.Date(),
                                  min_period = NULL) {
  if (!nrow(headline)) {
    return(ggplot2::ggplot() + nn_theme() +
           ggplot2::labs(title = "Nowcast not available"))
  }
  if (is.null(min_period)) {
    min_period <- max(headline$period, na.rm = TRUE) - lubridate::days(5L * 365L)
  }
  hl <- headline |> dplyr::filter(.data$period >= min_period)
  abs_obs <- panel |>
    dplyr::filter(.data$category == "total",
                  !is.na(.data$nom_final),
                  .data$period >= min_period) |>
    dplyr::select("period", value = "nom_final")

  ggplot2::ggplot(hl, ggplot2::aes(.data$period, .data$nom_mean)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower_95, ymax = .data$upper_95),
      fill = nn_palette$ribbon95, alpha = 0.18
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower_80, ymax = .data$upper_80),
      fill = nn_palette$ribbon80, alpha = 0.30
    ) +
    ggplot2::geom_line(colour = nn_palette$model, linewidth = 0.85) +
    ggplot2::geom_line(data = abs_obs,
                       ggplot2::aes(.data$period, .data$value),
                       colour = nn_palette$abs, linewidth = 1.0,
                       inherit.aes = FALSE) +
    ggplot2::geom_point(data = abs_obs,
                        ggplot2::aes(.data$period, .data$value),
                        colour = nn_palette$abs, size = 1.6,
                        inherit.aes = FALSE) +
    ggplot2::geom_vline(xintercept = nn_quarter_start(asof - 1L),
                        linetype = "dashed", colour = "grey60") +
    ggplot2::annotate("text",
                      x = nn_quarter_start(asof - 1L), y = max(c(hl$upper_95, abs_obs$value), na.rm = TRUE),
                      label = "as-of", vjust = 1.2, hjust = -0.1,
                      colour = "grey55", size = 3) +
    nn_y_thousands() +
    ggplot2::labs(
      title    = "Headline NOM: model vs ABS",
      subtitle = "Blue line = multivariate Kalman nowcast (with 80% / 95% intervals); red line = ABS published final NOM",
      y        = "NOM per quarter (persons)",
      caption  = paste0("Source: ABS catalogues 3401.0, 3101.0, 3407.0; DHA visa-grant data via data.gov.au CKAN. Updated ",
                        format(asof, "%d %B %Y"), ".")
    ) +
    nn_theme()
}

#' Bar plot of category-level NOM contributions for a single period
#'
#' @param categories Output of [build_nowcast_categories()].
#' @param period Optional date — defaults to the latest period.
#' @return A `ggplot`.
#' @export
plot_nowcast_categories <- function(categories, period = NULL) {
  if (!nrow(categories)) {
    return(ggplot2::ggplot() + nn_theme() +
           ggplot2::labs(title = "No category nowcast"))
  }
  period <- period %||% max(categories$period, na.rm = TRUE)
  d <- categories |>
    dplyr::filter(.data$period == .env$period, .data$category != "total") |>
    dplyr::arrange(.data$nom_mean) |>
    dplyr::mutate(category = factor(.data$category, levels = .data$category))

  ggplot2::ggplot(d, ggplot2::aes(.data$nom_mean, .data$category)) +
    ggplot2::geom_col(fill = nn_palette$model, width = 0.7, alpha = 0.85) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(scales::number(x / 1000, accuracy = 1), "k")
    ) +
    ggplot2::labs(
      title    = sprintf("Category contributions to NOM, %s",
                         format(period, "%Y · Q%q")),
      subtitle = "Per-visa-category contribution from the univariate Kalman",
      y        = NULL
    ) +
    nn_theme()
}

#' π trajectory plot (empirical + smoothed)
#'
#' @param pi_smoothed Output of [smooth_pi()].
#' @param pi_empirical Output of [estimate_pi_empirical()].
#' @return A `ggplot`.
#' @export
plot_pi <- function(pi_smoothed, pi_empirical) {
  ggplot2::ggplot() +
    ggplot2::geom_point(data = pi_empirical,
                        ggplot2::aes(.data$period, .data$pi_hat),
                        alpha = 0.4, size = 0.9, colour = "grey45") +
    ggplot2::geom_line(data = pi_smoothed,
                       ggplot2::aes(.data$period, .data$pi_smoothed),
                       colour = nn_palette$model, linewidth = 0.7) +
    ggplot2::facet_wrap(~ category, scales = "free_y", ncol = 3) +
    ggplot2::labs(
      title    = expression("Empirical and smoothed " * pi[c]),
      subtitle = "Classification probability — ABS NOM divided by OAD net long-term flow, smoothed within regime",
      y        = expression(pi[c])
    ) +
    nn_theme() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "grey95",
                                                            colour = NA))
}

#' Backtest RMSE comparison plot
#'
#' One panel per regime if multiple; otherwise a single panel.
#' Line + point per model, sorted within each horizon by RMSE.
#'
#' @param scored Output of [score_backtest()].
#' @return A `ggplot`.
#' @export
plot_backtest_rmse <- function(scored) {
  if (!nrow(scored)) {
    return(ggplot2::ggplot() + nn_theme() +
           ggplot2::labs(title = "Backtest unavailable"))
  }
  d <- scored |>
    dplyr::filter(.data$category == "total", is.finite(.data$rmse)) |>
    dplyr::group_by(.data$model, .data$horizon) |>
    dplyr::summarise(rmse = mean(.data$rmse, na.rm = TRUE), .groups = "drop")
  if (!nrow(d)) {
    return(ggplot2::ggplot() + nn_theme() +
           ggplot2::labs(title = "Backtest produced no finite RMSEs"))
  }
  ggplot2::ggplot(d, ggplot2::aes(.data$horizon, .data$rmse, colour = .data$model)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_x_continuous(
      breaks = sort(unique(d$horizon)),
      labels = function(x) paste0("h = ", x)
    ) +
    nn_y_thousands() +
    ggplot2::labs(
      title    = "Backtest RMSE by horizon",
      subtitle = "Lower is better. h=0 is the current-quarter nowcast.",
      y        = "RMSE vs ABS NOM",
      colour   = NULL
    ) +
    ggplot2::scale_colour_manual(values = c(
      kalman_multi_v2  = nn_palette$model,
      kalman_multi_pi  = "#7b8a99",
      kalman_v1        = "#a3b8c9",
      bridge           = nn_palette$bridge,
      random_walk      = nn_palette$rw,
      ar1              = nn_palette$ar1,
      abs_preliminary  = nn_palette$prelim
    ), na.value = "grey60") +
    nn_theme()
}
