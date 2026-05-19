test_that("spread_annual_to_quarters with no weights gives equal allocation", {
  df <- tibble::tibble(period = as.Date("2024-07-01"), value = 400)
  out <- spread_annual_to_quarters(df)
  expect_equal(nrow(out), 4L)
  expect_equal(out$period,
               as.Date(c("2024-07-01", "2024-10-01",
                         "2025-01-01", "2025-04-01")))
  expect_equal(out$value, rep(100, 4L))
})

test_that("spread_annual_to_quarters respects proportional weights", {
  df <- tibble::tibble(period = as.Date("2024-07-01"), value = 400)
  weights <- tibble::tibble(
    period = as.Date(c("2024-07-01", "2024-10-01",
                       "2025-01-01", "2025-04-01")),
    weight = c(40, 20, 30, 10)   # = 100; proportions 0.4 0.2 0.3 0.1
  )
  out <- spread_annual_to_quarters(df, weights)
  expect_equal(out$value, c(160, 80, 120, 40))
})

test_that("spread_annual_to_quarters falls back to equal when weights missing", {
  df <- tibble::tibble(period = as.Date("2024-07-01"), value = 400)
  weights <- tibble::tibble(
    period = as.Date(c("2024-07-01", "2024-10-01")),
    weight = c(NA_real_, NA_real_)
  )
  out <- spread_annual_to_quarters(df, weights)
  expect_equal(out$value, rep(100, 4L))
})

test_that("spread_annual_to_quarters preserves the annual total", {
  df <- tibble::tibble(period = as.Date(c("2024-07-01", "2025-07-01")),
                       value = c(400, 800))
  out <- spread_annual_to_quarters(df)
  expect_equal(sum(out$value), 1200)
})
