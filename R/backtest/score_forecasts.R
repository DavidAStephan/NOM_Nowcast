#' Score backtest output against eventually-revised NOM and ABS prelim
#'
#' Computes per-`(model, horizon, regime, category)` metrics:
#'
#'   * `rmse`       — versus `nom_final` (latest available vintage)
#'   * `mae`
#'   * `bias`
#'   * `rmse_vs_prelim`  — versus the ABS preliminary at the eval date
#'   * `hit_rate_qoq`    — directional accuracy of QoQ change
#'   * `log_score`       — only computed when SE is available (Phase 1
#'                         provides this; benchmarks don't)
#'
#' This is the project's headline performance summary. The structural
#' model needs to outperform `model == "abs_preliminary"` to be useful.
#'
#' @param backtest_runs A list of tibbles (one per backtest date) from
#'   [run_backtest()], or a single concatenated tibble.
#' @param db_path Path to the DuckDB vintage store (used to look up the
#'   latest-vintage NOM_final).
#' @param cfg Project config.
#' @return Tibble of metrics.
#' @export
score_backtest <- function(backtest_runs, db_path, cfg) {
  bt <- if (is.list(backtest_runs) && !is.data.frame(backtest_runs)) {
    dplyr::bind_rows(backtest_runs)
  } else backtest_runs
  if (nrow(bt) == 0L) return(empty_scored())

  truth <- latest_truth_nom(db_path)
  prelim <- prelim_at_asof(db_path, bt$asof_date)
  # When the vintage store is empty (e.g. pseudo-real-time mode on the
  # first backtest), fall back to using the latest-vintage NOM as the
  # "preliminary" reference. Reviewers should treat the
  # `rmse_vs_prelim` column as informational under those conditions.
  if (is.null(prelim) || nrow(prelim) == 0L) {
    if (nrow(bt) && nrow(truth)) {
      prelim <- tidyr::expand_grid(asof_date = unique(bt$asof_date),
                                   truth) |>
        dplyr::rename(nom_prelim = "nom_truth")
    } else {
      prelim <- tibble::tibble(asof_date = as.Date(character()),
                               period = as.Date(character()),
                               category = character(),
                               nom_prelim = numeric())
    }
  }

  # Fallback truth when the vintage store has no NOM either — use the
  # latest NOM observed within the panel produced by run_backtest.
  if (is.null(truth) || nrow(truth) == 0L) {
    panel_truth <- bt |>
      dplyr::filter(.data$model == "abs_preliminary",
                    !is.na(.data$nom_mean)) |>
      dplyr::transmute(.data$period, .data$category,
                       nom_truth = .data$nom_mean) |>
      dplyr::distinct()
    truth <- panel_truth
  }

  scored <- bt |>
    dplyr::left_join(truth, by = c("period", "category"),
                     suffix = c("", "_truth")) |>
    dplyr::left_join(prelim,
                     by = c("asof_date", "period", "category"),
                     suffix = c("", "_prelim")) |>
    dplyr::mutate(
      regime = classify_regime(.data$period, cfg)
    )

  by_keys <- c("model", "horizon", "regime", "category")

  scored |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by_keys))) |>
    dplyr::summarise(
      n              = sum(!is.na(.data$nom_mean) & !is.na(.data$nom_truth)),
      rmse           = rmse(.data$nom_mean - .data$nom_truth),
      mae            = mean(abs(.data$nom_mean - .data$nom_truth), na.rm = TRUE),
      bias           = mean(.data$nom_mean - .data$nom_truth, na.rm = TRUE),
      rmse_vs_prelim = rmse(.data$nom_mean - .data$nom_prelim),
      hit_rate_qoq   = hit_rate(.data$nom_mean, .data$nom_truth, .data$period),
      log_score      = mean(log_score_point(.data$nom_mean, .data$nom_se, .data$nom_truth),
                            na.rm = TRUE),
      .groups = "drop"
    )
}

#' Look up the latest-vintage final NOM for every `(period, category)`
#'
#' @param db_path Path to DuckDB.
#' @return Tibble: `period`, `category`, `nom_truth`.
#' @export
latest_truth_nom <- function(db_path) {
  raw <- vintages_read_asof(db_path, "nom", Sys.Date())
  if (nrow(raw) == 0L) {
    # Pseudo-real-time fallback: read NOM live and treat it as truth.
    nom_live <- tryCatch(fetch_nom(config::get(file = "config.yml"),
                                   Sys.Date()),
                         error = function(e) NULL)
    if (is.null(nom_live) || !nrow(nom_live)) {
      return(tibble::tibble(period = as.Date(character()),
                            category = character(), nom_truth = numeric()))
    }
    return(
      nom_live |>
        dplyr::filter(.data$period_unit == "quarter") |>
        dplyr::group_by(.data$period, .data$category) |>
        dplyr::summarise(nom_truth = sum(.data$value, na.rm = TRUE),
                         .groups = "drop")
    )
  }
  meta <- purrr::map(raw$metadata, safe_fromJSON)
  raw$series <- vapply(meta, function(m) m$series %||% NA_character_, character(1))
  raw |>
    dplyr::mutate(category = nom_classify_category(.data$series)) |>
    dplyr::mutate(category = ifelse(is.na(.data$category), "total", .data$category)) |>
    dplyr::group_by(.data$period, .data$category) |>
    dplyr::summarise(nom_truth = sum(.data$value, na.rm = TRUE), .groups = "drop")
}

#' Look up the ABS preliminary value as it stood at each as-of date
#'
#' @keywords internal
prelim_at_asof <- function(db_path, asof_dates) {
  dates <- unique(as.Date(asof_dates))
  purrr::map_dfr(dates, function(d) {
    snap <- vintages_read_asof(db_path, "nom", d)
    if (nrow(snap) == 0L) return(tibble::tibble())
    meta <- purrr::map(snap$metadata, safe_fromJSON)
    snap$series <- vapply(meta, function(m) m$series %||% NA_character_, character(1))
    snap |>
      dplyr::mutate(category = nom_classify_category(.data$series)) |>
      dplyr::mutate(category = ifelse(is.na(.data$category), "total", .data$category),
                    asof_date = d) |>
      dplyr::group_by(.data$asof_date, .data$period, .data$category) |>
      dplyr::summarise(nom_prelim = sum(.data$value, na.rm = TRUE), .groups = "drop")
  })
}

#' @keywords internal
safe_fromJSON <- function(x) {
  if (is.na(x)) return(list())
  tryCatch(jsonlite::fromJSON(x), error = function(e) list())
}

#' @keywords internal
classify_regime <- function(period, cfg) {
  regs <- cfg$backtest$regimes
  if (is.null(regs)) return(rep("all", length(period)))
  out <- rep(NA_character_, length(period))
  for (nm in names(regs)) {
    rng <- as.Date(regs[[nm]])
    out[is.na(out) & period >= rng[1] & period <= rng[2]] <- nm
  }
  out
}

#' @keywords internal
rmse <- function(err) {
  err <- err[!is.na(err)]
  if (!length(err)) return(NA_real_)
  sqrt(mean(err^2))
}

#' @keywords internal
hit_rate <- function(pred, actual, period) {
  d <- tibble::tibble(period, pred, actual) |> dplyr::arrange(.data$period)
  if (nrow(d) < 2L) return(NA_real_)
  d$dp <- c(NA, diff(d$pred))
  d$da <- c(NA, diff(d$actual))
  mean(sign(d$dp) == sign(d$da), na.rm = TRUE)
}

#' @keywords internal
log_score_point <- function(mu, sd, y) {
  ok <- !is.na(mu) & !is.na(sd) & sd > 0 & !is.na(y)
  out <- rep(NA_real_, length(mu))
  out[ok] <- stats::dnorm(y[ok], mean = mu[ok], sd = sd[ok], log = TRUE)
  out
}

#' @keywords internal
empty_scored <- function() {
  tibble::tibble(
    model = character(), horizon = integer(), regime = character(),
    category = character(), n = integer(), rmse = numeric(),
    mae = numeric(), bias = numeric(), rmse_vs_prelim = numeric(),
    hit_rate_qoq = numeric(), log_score = numeric()
  )
}
