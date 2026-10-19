make_monthly_oad <- function(last_month = "2026-03-01", partial_months = 0L) {
  months <- seq.Date(as.Date("2019-01-01"), as.Date(last_month), by = "month")
  # Seasonal pattern within each quarter: 2nd month is double the others.
  season <- ifelse(as.integer(format(months, "%m")) %% 3 == 2, 2, 1)
  base <- tibble::tibble(
    period = months, category = "total", direction = "arrival",
    value = 10000 * season, period_unit = "month"
  )
  if (partial_months > 0L) {
    next_q <- nn_quarter_start(as.Date(last_month)) + months(3L)
    extra <- tibble::tibble(
      period = seq.Date(next_q, by = "month", length.out = partial_months),
      category = "total", direction = "arrival",
      value = 10000 * ifelse(as.integer(format(seq.Date(next_q, by = "month",
                                                        length.out = partial_months), "%m")) %% 3 == 2, 2, 1),
      period_unit = "month"
    )
    base <- dplyr::bind_rows(base, extra)
  }
  base
}

test_that("complete quarters are plain sums and flagged with 3 months", {
  q <- aggregate_oad_quarters(make_monthly_oad(), strategy = "scale")
  expect_true(all(q$n_months == 3L))
  expect_true(all(q$value == 40000))
})

test_that("raw strategy reproduces the legacy under-count on a partial quarter", {
  q <- aggregate_oad_quarters(make_monthly_oad(partial_months = 1L), strategy = "raw")
  last <- q[which.max(q$quarter), ]
  expect_equal(last$n_months, 1L)
  expect_equal(last$value, 10000)        # 1 of 3 months, no correction
})

test_that("drop strategy NAs partial quarters", {
  q <- aggregate_oad_quarters(make_monthly_oad(partial_months = 2L), strategy = "drop")
  last <- q[which.max(q$quarter), ]
  expect_equal(last$n_months, 2L)
  expect_true(is.na(last$value))
  expect_false(any(is.na(q$value[q$n_months == 3L])))
})

test_that("scale strategy grosses a partial quarter up by the historical month shares", {
  # First month of the quarter has share 1/4 (pattern 1, 2, 1).
  q1 <- aggregate_oad_quarters(make_monthly_oad(partial_months = 1L), strategy = "scale")
  expect_equal(q1$value[which.max(q1$quarter)], 40000)
  # Two months observed: shares 1/4 + 1/2 = 3/4.
  q2 <- aggregate_oad_quarters(make_monthly_oad(partial_months = 2L), strategy = "scale")
  expect_equal(q2$value[which.max(q2$quarter)], 40000)
  expect_equal(q2$n_months[which.max(q2$quarter)], 2L)
})

test_that("scale falls back to n/3 when no share history exists", {
  df <- tibble::tibble(
    period = as.Date(c("2025-01-01", "2025-02-01")),
    category = "student", direction = "arrival",
    value = c(100, 100), period_unit = "month"
  )
  q <- aggregate_oad_quarters(df, strategy = "scale")
  expect_equal(q$value, 300)
})

test_that("build_quarterly_panel exposes oad_months and corrects the live edge", {
  oad <- make_monthly_oad(partial_months = 1L)
  nom <- empty_nom_clean()
  vg  <- empty_vg_clean()
  cfg <- list(categories = list(levels = c("student")),
              panel = list(oad_partial_quarter = "scale"))
  panel <- build_quarterly_panel(oad, nom, vg, cfg)
  expect_true("oad_months" %in% names(panel))
  tot <- panel |> dplyr::filter(category == "total") |> dplyr::arrange(period)
  expect_equal(tail(tot$oad_months, 1), 1L)
  expect_equal(tail(tot$oad_lt_arrivals, 1), 40000)
})
