## Synthetic DHA pivot in the shape readxl returns with col_names = FALSE:
## pivot filters on top, a header row with dimension labels + FY columns,
## merged outer-dimension cells (blank on inner rows), subtotal rows and
## a Grand Total row.
make_dha_raw <- function() {
  r <- function(...) as.character(c(...))
  tibble::tibble(
    ...1 = r("Visa Subclass", NA, "Sum of Total", "Applicant Type",
             "Primary", NA, "Primary Total", "Secondary", NA,
             "Secondary Total", "Grand Total", NA, NA, NA, NA),
    ...2 = r("(All)", NA, NA, "Sector",
             "Higher Education Sector", "VET Sector", NA,
             "Higher Education Sector", "VET Sector", NA, NA, NA, NA, NA, NA),
    ...3 = r(NA, NA, "Financial Year", "2022-23",
             100, 200, 300, 10, 20, 30, 330, NA, NA, NA, NA),
    ...4 = r(NA, NA, NA, "2023-24",
             110, 210, 320, 11, 21, 32, 352, NA, NA, NA, NA),
    ...5 = r(NA, NA, NA, "2024-25",
             50, 60, 110, 5, 6, 11, 121, NA, NA, NA, NA)
  )
}

test_that("pivot parser sums every leaf row, not just the first of each group", {
  out <- parse_dha_pivot_raw(make_dha_raw(), "Student visa program",
                             lock_date = as.Date("2025-09-30"))
  tot <- out |> dplyr::group_by(period) |> dplyr::summarise(v = sum(value), .groups = "drop") |>
    dplyr::arrange(period)
  expect_equal(tot$period, as.Date(c("2022-07-01", "2023-07-01", "2024-07-01")))
  expect_equal(tot$v, c(330, 352, 121))          # equals the Grand Total row
  expect_true(all(out$category == "student"))
  expect_true(all(out$period_unit == "year"))
  # Subtotal / grand-total rows must not leak into the series ids
  expect_false(any(grepl("(?i)total", out$series_id)))
})

test_that("pivot parser drops a year-to-date partial financial year", {
  out <- suppressMessages(parse_dha_pivot_raw(make_dha_raw(), "Student visa program",
                                              lock_date = as.Date("2024-12-31")))
  expect_equal(sort(unique(out$period)), as.Date(c("2022-07-01", "2023-07-01")))
})

test_that("dha_lock_date reads the cut-off from DHA file names", {
  expect_equal(dha_lock_date("bp0015l-student-visas-granted-report-locked-at-2025-08-31-v100.xlsx"),
               as.Date("2025-08-31"))
  expect_equal(dha_lock_date("https://x/y/bp0018l-visitor-visas-granted-quarterly-report-as-at-2026-06-30-v100.xlsx"),
               as.Date("2026-06-30"))
  expect_null(dha_lock_date("bp0019-no-date.xlsx"))
})

test_that("select_latest_dha_resources keeps one workbook per dataset", {
  res <- list(
    list(dataset_title = "Student visa program", url = "a-granted-locked-at-2024-08-31.xlsx"),
    list(dataset_title = "Student visa program", url = "a-granted-locked-at-2025-08-31.xlsx"),
    list(dataset_title = "Visitor visa program", url = "b-granted-as-at-2026-06-30.xlsx")
  )
  keep <- select_latest_dha_resources(res)
  expect_length(keep, 2L)
  expect_setequal(vapply(keep, `[[`, character(1), "url"),
                  c("a-granted-locked-at-2025-08-31.xlsx", "b-granted-as-at-2026-06-30.xlsx"))
})

test_that("visitor grants get their own category and are excluded from the panel", {
  expect_equal(classify_dha_dataset("Visitor visa program"), "visitor")
  expect_equal(classify_dha_dataset("Temporary Graduate visa program"), "other_temp")
  raw <- tibble::tibble(
    series_id = c("dha_grants:visitor:Tourist", "dha_grants:student:Primary"),
    period = as.Date("2024-07-01"), period_unit = "year",
    value = c(5e6, 4e5), category = c("visitor", "student"),
    onshore = NA, unit = "persons", metadata = "{}"
  )
  cleaned <- clean_visa_grants(raw, cfg = list())
  expect_equal(cleaned$category, "student")
  cleaned_keep <- clean_visa_grants(raw, cfg = list(panel = list(visa_grant_exclude = character())))
  expect_setequal(cleaned_keep$category, c("visitor", "student"))
})

test_that("financial-year labels from ABS and DHA agree on the FY start", {
  expect_equal(parse_fy_label("2023-24"), as.Date("2023-07-01"))
  expect_equal(parse_dha_period_label("2023-24"), as.Date("2023-07-01"))
})
