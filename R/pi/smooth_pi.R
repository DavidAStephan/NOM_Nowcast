#' Smooth empirical π over time, with regime breaks
#'
#' Applies one of `{loess}`, an HP filter, a local-linear (random-walk
#' plus drift) state-space filter, or no smoothing, separately within
#' each regime defined by `cfg$pi$regime_breaks`. Smoothed series are
#' then extrapolated to cover every quarter in the panel — the last
#' smoothed value (or a small drift continuation) is carried forward.
#'
#' @param pi_empirical Output of [estimate_pi_empirical()].
#' @param cfg Project config.
#' @return Tibble: `period`, `category`, `pi_smoothed`, `pi_se`,
#'   `regime`.
#' @export
smooth_pi <- function(pi_empirical, cfg) {
  if (nrow(pi_empirical) == 0L) {
    return(tibble::tibble(
      period = as.Date(character()), category = character(),
      pi_smoothed = numeric(), pi_se = numeric(), regime = character()
    ))
  }
  smoother <- cfg$pi$smoother %||% "loess"
  breaks <- as.Date(cfg$pi$regime_breaks %||% character())

  pi_empirical |>
    dplyr::mutate(regime = assign_regime(.data$period, breaks)) |>
    dplyr::group_by(.data$category, .data$regime) |>
    dplyr::group_modify(~ smooth_pi_one(.x, smoother, cfg)) |>
    dplyr::ungroup()
}

#' @keywords internal
assign_regime <- function(period, breaks) {
  if (!length(breaks)) return(rep("all", length(period)))
  edges <- c(as.Date("1900-01-01"), sort(breaks), as.Date("2099-12-31"))
  cut(period, breaks = edges, labels = paste0("R", seq_along(edges)[-1]),
      right = FALSE) |> as.character()
}

#' Smooth a single (category, regime) slice.
#' @keywords internal
smooth_pi_one <- function(df, smoother, cfg) {
  ok <- !is.na(df$pi_hat) & is.finite(df$pi_hat)
  df_ok <- df[ok, , drop = FALSE]
  sd_fallback <- function(v) {
    s <- stats::sd(v, na.rm = TRUE)
    if (is.na(s) || !is.finite(s)) 0 else s
  }
  if (nrow(df_ok) < 5L || smoother == "none") {
    return(tibble::tibble(
      period = df$period, pi_smoothed = df$pi_hat,
      pi_se = sd_fallback(df$pi_hat)
    ))
  }
  x_full <- as.numeric(df$period)
  x  <- as.numeric(df_ok$period)
  y  <- df_ok$pi_hat

  switch(smoother,
    loess = {
      span <- cfg$pi$loess_span %||% 0.3
      fit <- tryCatch(
        stats::loess(y ~ x, span = span),
        error = function(e) NULL
      )
      pred <- if (!is.null(fit)) {
        tryCatch(stats::predict(fit, newdata = data.frame(x = x_full),
                                se = TRUE),
                 error = function(e) NULL)
      } else NULL
      if (is.null(pred)) {
        return(tibble::tibble(period = df$period, pi_smoothed = df$pi_hat,
                              pi_se = sd_fallback(y)))
      }
      tibble::tibble(period = df$period,
                     pi_smoothed = as.numeric(pred$fit),
                     pi_se       = as.numeric(pred$se.fit))
    },
    hp = {
      lambda <- cfg$pi$hp_lambda %||% 1600
      y_full <- df$pi_hat
      y_imp  <- impute_for_filter(y_full)
      sm <- hp_filter(y_imp, lambda)
      sm[is.na(y_full)] <- NA_real_
      tibble::tibble(period = df$period, pi_smoothed = sm,
                     pi_se = sd_fallback(y_full - sm))
    },
    locallinear = {
      y_full <- df$pi_hat
      sm <- local_linear_filter(impute_for_filter(y_full))
      tibble::tibble(period = df$period, pi_smoothed = sm$mean,
                     pi_se = sm$se)
    },
    tibble::tibble(period = df$period, pi_smoothed = df$pi_hat,
                   pi_se = sd_fallback(df$pi_hat))
  )
}

#' Simple linear interpolation for filter inputs. Returns the same
#' vector with internal NAs filled by the mean of the non-NA values,
#' edge NAs filled by the nearest non-NA.
#' @keywords internal
impute_for_filter <- function(y) {
  ok <- !is.na(y) & is.finite(y)
  if (!any(ok)) return(rep(0, length(y)))
  fill <- mean(y[ok])
  y[!ok] <- fill
  y
}

#' Standard Hodrick-Prescott two-sided filter.
#' @keywords internal
hp_filter <- function(y, lambda = 1600) {
  n <- length(y)
  if (n < 4) return(y)
  imat <- diag(n)
  d <- diff(diag(n), differences = 2)
  hp <- solve(imat + lambda * crossprod(d), y)
  as.numeric(hp)
}

#' Simple local-linear (random-walk plus drift) filter using KFAS.
#' @keywords internal
local_linear_filter <- function(y) {
  SSModel  <- KFAS::SSModel
  SSMtrend <- KFAS::SSMtrend
  ssm <- SSModel(y ~ SSMtrend(degree = 2, Q = list(NA, NA)), H = NA)
  fit <- tryCatch(KFAS::fitSSM(ssm, inits = c(0, 0, 0), method = "BFGS"),
                  error = function(e) NULL)
  if (is.null(fit)) return(list(mean = y, se = rep(stats::sd(y, na.rm = TRUE), length(y))))
  ks <- KFAS::KFS(fit$model, filtering = "state", smoothing = "state")
  mu <- as.numeric(ks$alphahat[, "level"])
  v  <- sqrt(pmax(as.numeric(ks$V[1, 1, ]), 0))
  list(mean = mu, se = v)
}
