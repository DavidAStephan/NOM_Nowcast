test_that("nn_quarter_start floors to quarter", {
  expect_equal(nn_quarter_start(as.Date("2024-05-18")), as.Date("2024-04-01"))
  expect_equal(nn_quarter_start(as.Date("2024-01-01")), as.Date("2024-01-01"))
  expect_equal(nn_quarter_start(as.Date("2024-12-31")), as.Date("2024-10-01"))
})

test_that("nn_last_completed_quarter is the previous quarter start", {
  expect_equal(nn_last_completed_quarter(as.Date("2024-05-18")), as.Date("2024-01-01"))
  expect_equal(nn_last_completed_quarter(as.Date("2024-01-01")), as.Date("2023-10-01"))
})

test_that("nn_quarter_seq enumerates inclusive quarter starts", {
  out <- nn_quarter_seq(as.Date("2023-01-15"), as.Date("2024-02-15"))
  expect_equal(out,
               as.Date(c("2023-01-01", "2023-04-01", "2023-07-01",
                         "2023-10-01", "2024-01-01")))
})

test_that("nn_first_observable accounts for period length and lag", {
  # Jan 2024 monthly: period end = 2024-01-31, + 35 days lag, + 1 day to be
  # first observable = 2024-03-07
  expect_equal(
    nn_first_observable(as.Date("2024-01-01"), "month", lag_days = 35),
    as.Date("2024-03-07")
  )
  # Jan 2024 quarterly: period end = 2024-03-31, + 183 days, + 1 day
  expect_equal(
    nn_first_observable(as.Date("2024-01-01"), "quarter", lag_days = 183),
    as.Date("2024-10-01")
  )
})

test_that("financial years are observable only after 30 June plus lag", {
  # FY2024-25 (keyed 2024-07-01) ends 2025-06-30; + 45 days + 1
  expect_equal(nn_first_observable(as.Date("2024-07-01"), "year", lag_days = 45),
               as.Date("2025-08-15"))
  # Calendar year keyed on 1 January still ends 31 December
  expect_equal(nn_first_observable(as.Date("2024-01-01"), "year", lag_days = 0),
               as.Date("2025-01-01"))
  # restrict_to_asof must not leak an unfinished FY into a backtest
  df <- tibble::tibble(period = as.Date(c("2023-07-01", "2024-07-01")),
                       period_unit = "year", value = c(1, 2))
  expect_equal(restrict_to_asof(df, "2025-04-01", lag_days = 45)$value, 1)
})
