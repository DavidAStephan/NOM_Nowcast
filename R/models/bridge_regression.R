#' Bridge regression benchmark (Phase 4)
#'
#' Transparent OLS benchmark linking ABS NOM by category to lagged
#' OAD long-term arrivals and DHA visa grants. The intent is *not* to
#' compete with the state-space model — the bridge regression exists as
#' an interpretable baseline that the structural model has to beat. If
#' it doesn't, that's an honest finding worth reporting.
#'
#' Specification, per category `c`:
#'
#' \deqn{NOM_{c,q} = \alpha_c + \sum_{k=0}^{K} \beta_{c,k}\,A^{OAD,LT}_{c,q-k} + \sum_{k=0}^{K} \gamma_{c,k}\,V_{c,q-k} + \varepsilon_{c,q}}
#'
#' where `K = cfg$models$bridge$max_lag_quarters`. Fits use `{fixest}`'s
#' `feols()` so we get robust standard errors easily.
#'
#' @param panel Output of [build_quarterly_panel()].
#' @param cfg Project config.
#' @return Named list keyed by category, each element a `fixest` fit.
#' @export
benchmark_bridge <- function(panel, cfg) {
  k <- cfg$models$bridge$max_lag_quarters %||% 4L
  # Categories that have any NOM observations. With ABS no longer
  # publishing quarterly NOM by visa, this is typically only "total".
  cats_with_nom <- panel |>
    dplyr::filter(!is.na(.data$nom_final)) |>
    dplyr::pull(.data$category) |>
    unique()
  if (!length(cats_with_nom)) return(list())

  out <- list()
  for (cat in cats_with_nom) {
    df_self <- panel |>
      dplyr::filter(.data$category == cat) |>
      dplyr::arrange(.data$period) |>
      build_bridge_features(k)
    if (cat == "total") {
      # The "total" row carries OAD aggregate long-term flows but no
      # visa_grants column (visa grants live on the visa-category
      # rows). Aggregate visa_grants across categories before joining.
      vg_total <- panel |>
        dplyr::filter(.data$category != "total") |>
        dplyr::group_by(.data$period) |>
        dplyr::summarise(visa_grants_agg = sum(.data$visa_grants, na.rm = TRUE),
                         .groups = "drop")
      df_self <- df_self |>
        dplyr::select(-dplyr::any_of("visa_grants")) |>
        dplyr::left_join(vg_total, by = "period") |>
        dplyr::rename(visa_grants = "visa_grants_agg") |>
        build_bridge_features(k)
    }
    df <- df_self |> dplyr::filter(!is.na(.data$nom_final))
    if (nrow(df) < (k + 4)) {
      out[[cat]] <- NULL
      next
    }
    rhs <- c(paste0("oad_lt_arrivals_l", 0:k),
             paste0("visa_grants_l",     0:k))
    rhs <- intersect(rhs, names(df))
    f <- stats::as.formula(paste("nom_final ~", paste(rhs, collapse = " + ")))
    out[[cat]] <- tryCatch(fixest::feols(f, data = df),
                           error = function(e) NULL)
  }
  out
}

#' @keywords internal
build_bridge_features <- function(df, k) {
  for (lag in 0:k) {
    df[[paste0("oad_lt_arrivals_l", lag)]] <- dplyr::lag(df$oad_lt_arrivals, lag)
    df[[paste0("visa_grants_l",     lag)]] <- dplyr::lag(df$visa_grants,     lag)
  }
  df
}

#' Forecast NOM from fitted bridge regressions
#'
#' @param fits Output of [benchmark_bridge()].
#' @param panel Modelling panel.
#' @param cfg Project config.
#' @return Tibble with one forecast per `(period, category)`.
#' @export
forecast_bridge <- function(fits, panel, cfg) {
  k <- cfg$models$bridge$max_lag_quarters %||% 4L
  purrr::imap_dfr(fits, function(fit, cat) {
    if (is.null(fit)) return(tibble::tibble())
    df <- panel |>
      dplyr::filter(.data$category == cat) |>
      dplyr::arrange(.data$period)
    if (cat == "total") {
      vg_total <- panel |>
        dplyr::filter(.data$category != "total") |>
        dplyr::group_by(.data$period) |>
        dplyr::summarise(visa_grants_agg = sum(.data$visa_grants, na.rm = TRUE),
                         .groups = "drop")
      df <- df |>
        dplyr::select(-dplyr::any_of("visa_grants")) |>
        dplyr::left_join(vg_total, by = "period") |>
        dplyr::rename(visa_grants = "visa_grants_agg")
    }
    df <- build_bridge_features(df, k)
    yhat <- tryCatch(stats::predict(fit, newdata = df),
                     error = function(e) rep(NA_real_, nrow(df)))
    tibble::tibble(period = df$period, category = cat, nom_mean = yhat)
  })
}
