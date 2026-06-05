test_that("load_data returns a non-empty data frame", {
  dem <- load_data()
  expect_s3_class(dem, "data.frame")
  expect_gt(nrow(dem), 0)
})

test_that("load_data contains the expected key columns", {
  dem <- load_data()
  expect_true(all(c("country_name", "country", "year", "is_democracy",
                    "is_presidential", "prop_in_par") %in% names(dem)))
})

test_that("is_democracy is logical and year is numeric", {
  dem <- load_data()
  expect_type(dem$is_democracy, "logical")
  expect_true(is.numeric(dem$year))
})
