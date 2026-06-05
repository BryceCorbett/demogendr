regime_ts_data <- tibble::tibble(
  year             = rep(1990:1995, each = 4),
  regime_category  = rep(
    c("Parliamentary democracy", "Presidential democracy",
      "Monarchy", "Autocracy"),
    times = 6
  )
)

test_that("plot_regime returns a ggplot", {
  plot <- plot_regime(regime_ts_data,
    regime_type = "Parliamentary democracy",
    min_year    = 1990,
    max_year    = 1995)
  expect_s3_class(plot, "ggplot")
})

test_that("different regime_type values produce a ggplot", {
  plot <- plot_regime(regime_ts_data,
    regime_type = "Monarchy",
    min_year    = 1990,
    max_year    = 1995)
  expect_s3_class(plot, "ggplot")
})

test_that("non-data-frame input throws an error", {
  expect_error(plot_regime("not a df"))
})

test_that("missing required columns throws an error", {
  bad <- tibble::tibble(year = 1990:1992, other_col = 1:3)
  expect_error(plot_regime(bad))
})

test_that("unknown regime_type throws an error", {
  expect_error(
    plot_regime(regime_ts_data, regime_type = "Oligarchy",
      min_year = 1990, max_year = 1995),
    regexp = "not found in data"
  )
})

test_that("out-of-range min_year throws an error", {
  expect_error(
    plot_regime(regime_ts_data, min_year = 1800, max_year = 1995),
    regexp = "outside the data range"
  )
})

test_that("out-of-range max_year throws an error", {
  expect_error(
    plot_regime(regime_ts_data, min_year = 1990, max_year = 2100),
    regexp = "outside the data range"
  )
})

test_that("min_year >= max_year throws an error", {
  expect_error(
    plot_regime(regime_ts_data, min_year = 1993, max_year = 1990),
    regexp = "min_year"
  )
})
