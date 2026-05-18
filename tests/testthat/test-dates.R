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
  expect_equal(
    nn_first_observable(as.Date("2024-01-01"), "month", lag_days = 35),
    as.Date("2024-03-06")
  )
  expect_equal(
    nn_first_observable(as.Date("2024-01-01"), "quarter", lag_days = 183),
    as.Date("2024-10-01")
  )
})
