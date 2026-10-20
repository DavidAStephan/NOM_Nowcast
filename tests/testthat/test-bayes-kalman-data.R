make_bk_panel <- function(n_q = 40) {
  periods <- seq.Date(as.Date("2015-01-01"), by = "quarter", length.out = n_q)
  tibble::tibble(
    period = rep(periods, 2),
    category = rep(c("total", "student"), each = n_q),
    oad_lt_arrivals = c(rep(250000, n_q), rep(NA_real_, n_q)),
    oad_lt_departures = NA_real_, oad_lt_net = NA_real_, oad_months = 3L,
    visa_grants = c(rep(NA_real_, n_q), rep(90000, n_q)),
    nom_preliminary = NA_real_, nom_revised = NA_real_,
    nom_final = c(rep(80000, n_q - 2), NA, NA, rep(NA_real_, n_q))
  )
}

test_that("stan data for the plain model switches seasonality off", {
  prep <- bayes_kalman_stan_data(make_bk_panel(), list(gamma_lag = list(k_max = 4)))
  sd <- prep$stan_data
  expect_equal(sd$use_seasonal, 0L)
  expect_equal(sd$visa_seasonal, 0L)
  expect_equal(sd$T_obs, 40L)
  expect_equal(length(prep$all_periods), sd$T_obs + sd$H)
  expect_equal(sum(sd$has_nom), 38L)
  expect_equal(sum(sd$gamma_weights), 1, tolerance = 1e-12)
})

test_that("stan data for the seasonal model carries the seasonal flags and priors", {
  bcfg <- list(seasonal = TRUE, visa_loads_oad_seasonal = TRUE,
               gamma_lag = list(k_max = 4),
               priors = list(seas0_scale = 0.4, sigma_seas_scale = 0.03))
  sd <- bayes_kalman_stan_data(make_bk_panel(), bcfg)$stan_data
  expect_equal(sd$use_seasonal, 1L)
  expect_equal(sd$visa_seasonal, 1L)
  expect_equal(sd$seas0_scale, 0.4)
  expect_equal(sd$sigma_seas_scale, 0.03)
  bcfg$visa_loads_oad_seasonal <- FALSE
  expect_equal(bayes_kalman_stan_data(make_bk_panel(), bcfg)$stan_data$visa_seasonal, 0L)
})

test_that("stan data is NULL without a total row", {
  p <- make_bk_panel(); p <- p[p$category != "total", ]
  expect_null(bayes_kalman_stan_data(p, list()))
})
