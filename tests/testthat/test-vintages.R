test_that("vintages_init creates a database with the canonical schema", {
  cfg <- list(paths = list(duckdb = tempfile(fileext = ".duckdb")))
  path <- vintages_init(cfg)
  on.exit(unlink(path), add = TRUE)

  con <- vintages_connect(path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info('vintages')")
  expect_true(all(c("source", "series_id", "period", "vintage_date", "value")
                  %in% cols$name))
})

test_that("vintages_write + read returns the asof-restricted view", {
  cfg <- list(paths = list(duckdb = tempfile(fileext = ".duckdb")))
  path <- vintages_init(cfg)
  on.exit(unlink(path), add = TRUE)

  df1 <- tibble::tibble(
    series_id = "x", period = as.Date("2023-01-01"),
    period_unit = "quarter", value = 100,
    unit = "p", metadata = "{}"
  )
  df2 <- df1; df2$value <- 110
  vintages_write(path, "nom", df1, asof = "2023-04-01")
  vintages_write(path, "nom", df2, asof = "2023-10-01")

  v0 <- vintages_read_asof(path, "nom", "2023-05-01")
  v1 <- vintages_read_asof(path, "nom", "2023-12-01")
  expect_equal(v0$value, 100)
  expect_equal(v1$value, 110)
})

test_that("vintages_history returns rows in chronological order", {
  cfg <- list(paths = list(duckdb = tempfile(fileext = ".duckdb")))
  path <- vintages_init(cfg)
  on.exit(unlink(path), add = TRUE)

  for (asof in c("2023-04-01", "2023-07-01", "2023-10-01")) {
    df <- tibble::tibble(
      series_id = "x", period = as.Date("2023-01-01"),
      period_unit = "quarter", value = runif(1) * 100,
      unit = "p", metadata = "{}"
    )
    vintages_write(path, "nom", df, asof = asof)
  }
  hist <- vintages_history(path, "nom", "x")
  expect_equal(nrow(hist), 3)
  expect_true(all(diff(as.Date(hist$vintage_date)) >= 0))
})
