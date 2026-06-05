regime_data <- tibble::tibble(
  regime_cat  = c("Democracy", "Democracy", "Democracy",
                  "Autocracy", "Autocracy", "Autocracy"),
  par_pct     = c(20, 25, 22, 10, 12, 8),
  with_na     = c(20, NA,  22, 10, 12, NA)
)

test_that("returns a one-row tidy tibble with expected columns", {
  result <- compare_regime_types2(
    regime_data,
    outcome_var = "par_pct",
    regime_var  = "regime_cat",
    regime1     = "Democracy",
    regime2     = "Autocracy"
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_true(all(c("estimate1", "estimate2", "statistic",
                     "p.value", "conf.low", "conf.high") %in% names(result)))
})

test_that("group means in the output match manual calculation", {
  result <- compare_regime_types2(
    regime_data,
    outcome_var = "par_pct",
    regime_var  = "regime_cat",
    regime1     = "Democracy",
    regime2     = "Autocracy"
  )
  expected_means <- sort(c(mean(c(20, 25, 22)), mean(c(10, 12, 8))))
  actual_means   <- sort(c(result$estimate1, result$estimate2))
  expect_equal(actual_means, expected_means)
})

test_that("NA values in outcome_var trigger a warning", {
  expect_warning(
    compare_regime_types2(
      regime_data,
      outcome_var = "with_na",
      regime_var  = "regime_cat",
      regime1     = "Democracy",
      regime2     = "Autocracy"
    ),
    regexp = "missing values"
  )
})

test_that("non-data-frame input throws an error", {
  expect_error(compare_regime_types2("not a df",
    "par_pct", "regime_cat", "Democracy", "Autocracy"))
})

test_that("missing outcome_var column throws an error", {
  expect_error(compare_regime_types2(regime_data,
    "no_such_col", "regime_cat", "Democracy", "Autocracy"))
})

test_that("missing regime_var column throws an error", {
  expect_error(compare_regime_types2(regime_data,
    "par_pct", "no_such_col", "Democracy", "Autocracy"))
})

test_that("regime values that leave one group empty after NA removal throw an error", {
  all_na <- tibble::tibble(
    regime_cat = c("Democracy", "Democracy", "Autocracy"),
    par_pct    = c(NA, NA, 10)
  )
  expect_error(
    suppressWarnings(
      compare_regime_types2(all_na, "par_pct", "regime_cat",
        "Democracy", "Autocracy")
    ),
    regexp = "no remaining observations"
  )
})
