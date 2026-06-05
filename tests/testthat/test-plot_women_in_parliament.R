test_that("plot of women in parliament works", {
  dat <- load_data()

  plot_simple <- plot_women_in_parliament(dat)
  plot_year   <- plot_women_in_parliament(dat, "United States", min_year = 2000)
  plot_color  <- plot_women_in_parliament(dat, "United States", color_by = "is_presidential")

  expect_s3_class(plot_simple, "ggplot")
  expect_s3_class(plot_year,   "ggplot")
  expect_s3_class(plot_color,  "ggplot")
})

test_that("plot_women_in_parliament validates its inputs", {
  dat <- load_data()

  expect_error(plot_women_in_parliament("not a df"))

    expect_error(plot_women_in_parliament(dat, countries = character(0)))

      expect_error(
    plot_women_in_parliament(dat, color_by = "nonexistent_column")
  )
})
