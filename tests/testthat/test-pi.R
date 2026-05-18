make_panel <- function() {
  periods <- seq.Date(as.Date("2015-01-01"), as.Date("2022-10-01"), by = "quarter")
  tibble::tibble(
    period   = rep(periods, 2),
    category = rep(c("student", "skilled"), each = length(periods)),
    oad_lt_arrivals = c(rnorm(length(periods), 50000, 5000),
                        rnorm(length(periods), 20000, 2500)),
    oad_lt_departures = NA_real_,
    oad_lt_net = NA_real_,
    visa_grants = NA_real_,
    student_commencements = NA_real_,
    student_enrolments = NA_real_,
    nom_preliminary = NA_real_,
    nom_revised = NA_real_,
    nom_final = c(rnorm(length(periods), 35000, 4000),
                  rnorm(length(periods), 15000, 2000))
  )
}

test_that("estimate_pi_empirical respects completion_quarters cutoff", {
  set.seed(1)
  panel <- make_panel()
  cfg <- list(pi = list(completion_quarters = 6L,
                        floor = 0, ceiling = 1.5))
  pi_emp <- estimate_pi_empirical(panel, cfg)
  expected_cutoff <- seq.Date(max(panel$period), by = "-18 months",
                              length.out = 2L)[2L]
  expect_true(max(pi_emp$period) <= expected_cutoff)
  expect_true(all(pi_emp$pi_hat >= 0 & pi_emp$pi_hat <= 1.5))
})

test_that("smooth_pi(none) is the identity", {
  set.seed(1)
  panel <- make_panel()
  cfg <- list(pi = list(completion_quarters = 6L, smoother = "none",
                        regime_breaks = character(),
                        floor = 0, ceiling = 1.5))
  pi_emp <- estimate_pi_empirical(panel, cfg)
  pi_sm  <- smooth_pi(pi_emp, cfg)
  expect_equal(nrow(pi_sm), nrow(pi_emp))
})

test_that("smooth_pi(loess) returns same number of rows as input", {
  set.seed(1)
  panel <- make_panel()
  cfg <- list(pi = list(completion_quarters = 6L, smoother = "loess",
                        loess_span = 0.5,
                        regime_breaks = character(),
                        floor = 0, ceiling = 1.5))
  pi_emp <- estimate_pi_empirical(panel, cfg)
  pi_sm  <- smooth_pi(pi_emp, cfg)
  expect_equal(nrow(pi_sm), nrow(pi_emp))
})
