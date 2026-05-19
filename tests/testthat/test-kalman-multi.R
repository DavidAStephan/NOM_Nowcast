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

test_that("build_multi_ssmodel produces a valid 2-equation SSModel", {
  set.seed(7)
  n <- 40
  y_mat <- cbind(arr = log1p(60000 + rnorm(n, 0, 2000)),
                 vg  = log1p(50000 + rnorm(n, 0, 1500)))
  ssm <- build_multi_ssmodel(y_mat, phi = 0.85, seasonal_period = 4L)
  expect_s3_class(ssm, "SSModel")
  expect_equal(dim(ssm$Z)[1], 2L)         # bivariate observation
  expect_equal(dim(ssm$H)[1], 2L)
  # Both rows should load on the level state (state index 1)
  expect_equal(ssm$Z[1, 1, 1], 1)
  expect_equal(ssm$Z[2, 1, 1], 1)
  # OAD row should additionally load on the first seasonal lag (index 3)
  expect_equal(ssm$Z[1, 3, 1], 1)
  expect_equal(ssm$Z[2, 3, 1], 0)
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
    models = list(kalman_multi = list(enabled = TRUE, visa_lead_quarters = 1L),
                  kalman = list(damping_phi = 0.85, seasonal_period = 4L)),
    pi = list(completion_quarters = 6L)
  )
  fit <- fit_kalman_multi(panel, cfg)
  expect_s3_class(fit, "kalman_multi_fit")
  expect_equal(fit$kind, "kalman_multi_v1")
  expect_true(is.finite(fit$alpha))
  expect_equal(fit$lead_quarters, 1L)

  fc <- forecast_kalman_multi(fit, Sys.Date(), cfg)
  expect_gt(nrow(fc), 0L)
  expect_setequal(unique(fc$category),  "total")
  expect_setequal(unique(fc$direction), "arrival")
  # Forecast horizons should include 1..4 quarters ahead
  expect_true(any(fc$forecast_horizon >= 1L))
})
