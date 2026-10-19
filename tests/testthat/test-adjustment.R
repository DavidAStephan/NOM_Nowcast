test_that("apply_nowcast_adjustment scales live horizons and leaves backcasts alone", {
  fc <- tibble::tibble(
    period   = as.Date(c("2025-07-01", "2025-10-01", "2026-01-01", "2026-04-01")),
    horizon  = c(-1L, 0L, 1L, 2L),
    nom_mean = c(80000, 50000, 50000, 50000),
    nom_se   = 20000,
    lower_80 = c(60000, 35000, 35000, 35000),
    upper_80 = c(100000, 70000, 70000, 70000),
    lower_95 = c(50000, 30000, 30000, 30000),
    upper_95 = c(110000, 80000, 80000, 80000)
  )
  adj <- tibble::tibble(horizon = c(0L, 1L), regime = "post_covid", n = 12L,
                        factor = c(2, 1.5), k80 = 1, k95 = 1)
  out <- apply_nowcast_adjustment(fc, adj)
  expect_equal(out$adjusted, c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(out$nom_mean[1], 80000)
  expect_equal(out$nom_mean[2], 100000)
  expect_equal(out$nom_mean[3], 75000)
  expect_equal(out$nom_mean[4], 75000)          # h beyond max reuses furthest horizon
  expect_true(all(out$lower_80 < out$nom_mean & out$upper_80 > out$nom_mean))
  expect_true(all(out$lower_80 > 0))
})

test_that("apply_nowcast_adjustment is a no-op without a calibration", {
  fc <- tibble::tibble(period = as.Date("2025-07-01"), horizon = 0L,
                       nom_mean = 1, lower_80 = 0.5, upper_80 = 2,
                       lower_95 = 0.4, upper_95 = 3)
  out <- apply_nowcast_adjustment(fc, NULL)
  expect_false(out$adjusted)
  expect_equal(out$nom_mean, 1)
})
