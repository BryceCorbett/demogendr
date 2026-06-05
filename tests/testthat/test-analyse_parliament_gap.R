test_data <- tibble::tibble(
  is_democracy    = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  is_presidential = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
  prop_in_par     = c(20, 25, 18, 10, 12, 8)
)

test_that("returns a tibble with expected t-test columns", {
  result <- analyse_parliament_gap(test_data)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("estimate1", "estimate2", "statistic",
                     "p.value", "conf.low", "conf.high") %in% names(result)))
})

test_that("group_col tidy eval works with a different column", {
  result <- analyse_parliament_gap(test_data, group_col = is_presidential)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
})

test_that("estimate1 and estimate2 reflect group means", {
  result <- analyse_parliament_gap(test_data)
  group_means <- tapply(test_data$prop_in_par, test_data$is_democracy, mean)
  expect_equal(sort(c(result$estimate1, result$estimate2)),
               sort(as.numeric(group_means)))
})

test_that("non-data-frame input throws an error", {
  expect_error(analyse_parliament_gap("not a df"))
})

test_that("missing group column throws an error", {
  expect_error(analyse_parliament_gap(test_data, group_col = no_such_col))
})

test_that("missing prop_in_par column throws an error", {
  bad <- test_data[, c("is_democracy", "is_presidential")]
  expect_error(analyse_parliament_gap(bad))
})

test_that("column with more than two levels throws an error", {
  multi <- tibble::tibble(
    group       = c("a", "b", "c", "a"),
    prop_in_par = c(10, 20, 30, 15)
  )
  expect_error(analyse_parliament_gap(multi, group_col = group))
})
