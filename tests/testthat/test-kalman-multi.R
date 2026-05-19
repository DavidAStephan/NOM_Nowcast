test_that("shift_lag preserves length and NA-pads at the head", {
  expect_equal(shift_lag(c(1, 2, 3, 4), 0), c(1, 2, 3, 4))
  expect_equal(shift_lag(c(1, 2, 3, 4), 1), c(NA, 1, 2, 3))
  expect_equal(shift_lag(c(1, 2, 3, 4), 2), c(NA, NA, 1, 2))
  # When the lag exceeds the vector length, every element becomes NA
  # but the vector length is preserved (needed so it lines up with
  # the panel's period index).
  expect_equal(shift_lag(c(1, 2), 3), c(NA_real_, NA_real_))
})

test_that("gamma_lag_weights are normalised and well-behaved", {
  w <- gamma_lag_weights(2, 1, 6)
  expect_equal(length(w), 7)
  expect_true(all(w >= 0))
  expect_equal(sum(w), 1, tolerance = 1e-12)
})

test_that("gamma_lag_weights peak at the Gamma mode for alpha > 1", {
  # Gamma(2, 1) has mode at (alpha - 1)/beta = 1, so w[2] should be max
  w <- gamma_lag_weights(2, 1, 4)
  expect_equal(which.max(w), 2L)
})

test_that("build_multi_ssmodel augments state with K mu-lag copies when Gamma lag is enabled", {
  set.seed(7)
  n <- 40
  y_mat <- cbind(arr = log1p(60000 + rnorm(n, 0, 2000)),
                 vg  = log1p(50000 + rnorm(n, 0, 1500)))
  w <- gamma_lag_weights(2, 1, 4)
  ssm <- build_multi_ssmodel(y_mat, phi = 0.85, seasonal_period = 4L,
                             include_nom = FALSE,
                             visa_gamma_weights = w)
  # 2 + 3 + 4 = 9 state dims (trend + seasonal + mu-lag chain)
  expect_equal(dim(ssm$Z)[2], 9L)
  expect_equal(dim(ssm$T)[1], 9L)
  # Shift chain: mu_lag_1[t+1] = level[t]
  expect_equal(ssm$T[6, 1, 1], 1)
  # Visa row loads on level + 4 mu lags with Gamma weights summing to 1
  Z_visa <- c(ssm$Z[2, 1, 1], ssm$Z[2, 6, 1], ssm$Z[2, 7, 1],
              ssm$Z[2, 8, 1], ssm$Z[2, 9, 1])
  expect_equal(sum(Z_visa), 1, tolerance = 1e-12)
  # Loading on current level uses the smallest (last) Gamma weight w_K
  expect_equal(ssm$Z[2, 1, 1], w[length(w)])
})

test_that("build_multi_ssmodel produces a valid 2-equation SSModel", {
  set.seed(7)
  n <- 40
  y_mat <- cbind(arr = log1p(60000 + rnorm(n, 0, 2000)),
                 vg  = log1p(50000 + rnorm(n, 0, 1500)))
  ssm <- build_multi_ssmodel(y_mat, phi = 0.85, seasonal_period = 4L)
  expect_s3_class(ssm, "SSModel")
  expect_equal(dim(ssm$Z)[1], 2L)
  expect_equal(dim(ssm$H)[1], 2L)
  expect_equal(ssm$Z[1, 1, 1], 1)
  expect_equal(ssm$Z[2, 1, 1], 1)
  expect_equal(ssm$Z[1, 3, 1], 1)
  expect_equal(ssm$Z[2, 3, 1], 0)
})

test_that("build_multi_ssmodel can include a NOM observation row (v2)", {
  set.seed(11)
  n <- 40
  y_mat <- cbind(arr = log1p(60000 + rnorm(n, 0, 2000)),
                 vg  = log1p(50000 + rnorm(n, 0, 1500)),
                 nom = log1p(35000 + rnorm(n, 0, 1500)))
  ssm <- build_multi_ssmodel(y_mat, phi = 0.85, seasonal_period = 4L,
                             include_nom = TRUE)
  expect_s3_class(ssm, "SSModel")
  expect_equal(dim(ssm$Z)[1], 3L)         # trivariate observation
  expect_equal(dim(ssm$H)[1], 3L)
  # NOM (row 3) loads on level only, not seasonal
  expect_equal(ssm$Z[3, 1, 1], 1)
  expect_equal(ssm$Z[3, 3, 1], 0)
})

test_that("calibrate_offset computes the long-run mean log gap", {
  set.seed(13)
  ref <- runif(50, 11, 12.5)
  target <- ref + 0.5 + rnorm(50, 0, 0.05)
  off <- calibrate_offset(target, ref)
  expect_true(abs(off - 0.5) < 0.05)
  # Insufficient overlap -> zero offset (a cli warning is also
  # emitted as a side effect; we test the value only).
  out <- suppressMessages(calibrate_offset(c(1, 2), c(3, 4)))
  expect_equal(out, 0)
})

test_that("build_nowcast_headline_from_multi extracts NOM forecast cleanly", {
  multi_fc <- tibble::tibble(
    period = as.Date(c("2025-01-01", "2025-04-01")),
    mean_log = c(12.5, 12.4),
    se_log   = c(0.05, 0.10),
    forecast_horizon = c(0L, 1L),
    mean_level = c(268337, 242802),
    direction = "arrival", category = "total",
    nom_log    = c(11.4, 11.3),
    nom_level  = c(89321, 80821),
    nom_se_log = c(0.05, 0.10)
  )
  cfg <- list(reporting = list(headline_ci = 0.80, secondary_ci = 0.95))
  head <- build_nowcast_headline_from_multi(multi_fc, cfg)
  expect_equal(nrow(head), 2L)
  expect_equal(head$nom_mean, c(89321, 80821))
  expect_true(all(head$lower_80 <= head$nom_mean))
  expect_true(all(head$upper_80 >= head$nom_mean))
  expect_true(all(head$lower_95 <= head$lower_80))
  expect_true(all(head$upper_95 >= head$upper_80))
})

test_that("combine_kalman_forecasts no-ops when multi is disabled", {
  uni <- tibble::tibble(
    period = as.Date(c("2024-01-01", "2024-04-01")),
    mean_log = c(12.1, 12.2), se_log = c(0.05, 0.06),
    forecast_horizon = c(-1L, 0L),
    mean_level = c(180000, 200000),
    direction = "arrival", category = "total"
  )
  multi <- uni
  multi$mean_level <- c(160000, 170000)

  cfg_off <- list(models = list(kalman_multi = list(enabled = FALSE)))
  out_off <- combine_kalman_forecasts(uni, multi, cfg_off)
  expect_equal(out_off$mean_level, uni$mean_level)

  cfg_on <- list(models = list(kalman_multi = list(enabled = TRUE)))
  out_on <- combine_kalman_forecasts(uni, multi, cfg_on)
  expect_equal(sort(out_on$mean_level), sort(multi$mean_level))
})

test_that("fit_kalman_multi works on a small synthetic panel", {
  set.seed(11)
  periods <- seq.Date(as.Date("2015-01-01"), as.Date("2024-10-01"), by = "quarter")
  n_q <- length(periods)
  trend <- cumsum(rnorm(n_q, 200, 50)) + 60000

  panel <- tibble::tibble(
    period            = rep(periods, 2),
    category          = rep(c("total", "student"), each = n_q),
    oad_lt_arrivals   = c(trend, rep(20000, n_q) + cumsum(rnorm(n_q, 50, 30))),
    oad_lt_departures = c(0.5 * trend, rep(10000, n_q)),
    oad_lt_net        = NA_real_,
    visa_grants       = c(rep(NA_real_, n_q),
                          0.9 * (rep(20000, n_q) + cumsum(rnorm(n_q, 50, 30))) +
                            rnorm(n_q, 0, 500)),
    student_commencements = NA_real_,
    student_enrolments    = NA_real_,
    nom_preliminary       = NA_real_,
    nom_revised           = NA_real_,
    nom_final             = NA_real_
  )
  panel$oad_lt_net <- panel$oad_lt_arrivals - panel$oad_lt_departures

  cfg <- list(
    models = list(kalman_multi = list(enabled = TRUE,
                                      visa_lead_quarters = 1L,
                                      include_nom = FALSE),
                  kalman = list(damping_phi = 0.85, seasonal_period = 4L)),
    pi = list(completion_quarters = 6L)
  )
  fit <- fit_kalman_multi(panel, cfg)
  expect_s3_class(fit, "kalman_multi_fit")
  expect_true(fit$kind %in% c("kalman_multi_v1", "kalman_multi_v2"))
  expect_true(is.finite(fit$alpha_v))
  expect_equal(fit$lead_quarters, 1L)

  fc <- forecast_kalman_multi(fit, Sys.Date(), cfg)
  expect_gt(nrow(fc), 0L)
  expect_setequal(unique(fc$category),  "total")
  expect_setequal(unique(fc$direction), "arrival")
  # Forecast horizons should include 1..4 quarters ahead
  expect_true(any(fc$forecast_horizon >= 1L))
})
