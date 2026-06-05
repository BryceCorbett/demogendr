test_that("plot_accession_ages returns a ggplot", {
  dat  <- load_data()
  plot <- plot_accession_ages(dat)
  expect_s3_class(plot, "ggplot")
})

test_that("year range arguments are respected", {
  dat  <- load_data()
  plot <- plot_accession_ages(dat, min_year = 1990, max_year = 2010)
  expect_s3_class(plot, "ggplot")
})

test_that("plot_accession_ages validates its inputs", {
  dat <- load_data()

  # non-data-frame
  expect_error(plot_accession_ages("not a df"))

  # min_year >= max_year
  expect_error(plot_accession_ages(dat, min_year = 2010, max_year = 2000))

  # missing required column
  bad <- dat[, setdiff(names(dat), "is_presidential")]
  expect_error(plot_accession_ages(bad))
})
