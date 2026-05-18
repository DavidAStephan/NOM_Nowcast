test_that("nn_category_map returns the documented top-level sources", {
  m <- nn_category_map()
  expect_setequal(names(m), c("oad", "nom", "visa_subclass"))
})

test_that("map_subclass_to_category classifies obvious subclasses", {
  expect_equal(map_subclass_to_category("500 — Student"), "student")
  expect_equal(map_subclass_to_category("482 Temporary Skill Shortage"), "skilled")
  expect_equal(map_subclass_to_category("417 Working Holiday"), "working_holiday")
  expect_equal(map_subclass_to_category("820 Partner"), "family")
  expect_equal(map_subclass_to_category("444 NZ"), "nz_citizen")
})

test_that("oad_classify_direction is case-insensitive", {
  expect_equal(oad_classify_direction("Long-term Arrivals"), "arrival")
  expect_equal(oad_classify_direction("LT departures"), "departure")
  expect_true(is.na(oad_classify_direction("Population")))
})
