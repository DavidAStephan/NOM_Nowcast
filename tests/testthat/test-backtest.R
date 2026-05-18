test_that("restrict_to_asof filters out unpublished observations", {
  df <- tibble::tibble(
    period      = seq.Date(as.Date("2023-01-01"), by = "month", length.out = 12),
    period_unit = "month",
    value       = 1:12
  )
  out <- restrict_to_asof(df, asof = "2023-06-15", lag_days = 35)
  # The Jan 2023 period needs to wait until 2023-02-01 + 35 days = 2023-03-08
  # so all rows whose first_observable <= 2023-06-15 are kept (Jan–Apr)
  expect_true(all(out$period <= as.Date("2023-04-01")))
  expect_true(all(out$value <= 4))
})

test_that("build_backtest_grid returns quarter starts", {
  cfg <- list(backtest = list(start = "2020-01-01",
                              end   = "2021-04-01"))
  out <- build_backtest_grid(cfg)
  expect_equal(out, as.Date(c("2020-01-01", "2020-04-01", "2020-07-01",
                              "2020-10-01", "2021-01-01", "2021-04-01")))
})

test_that("rmse and hit_rate behave as expected", {
  err <- c(1, -1, 2, -2, NA)
  expect_equal(rmse(err), sqrt(mean(c(1, 1, 4, 4))))
  expect_true(is.na(rmse(c(NA, NA))))

  hr <- hit_rate(
    pred   = c(1, 2, 3, 4),
    actual = c(1, 2, 3, 4),
    period = as.Date(c("2020-01-01", "2020-04-01",
                       "2020-07-01", "2020-10-01"))
  )
  expect_equal(hr, 1)
})

test_that("gamma_lag_weights are non-negative and sum to 1", {
  w <- gamma_lag_weights(2, 1, 6)
  expect_equal(length(w), 7)
  expect_true(all(w >= 0))
  expect_equal(sum(w), 1)
})
