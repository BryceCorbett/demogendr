transition_data <- tibble::tibble(
  country_name = c("Alpha", "Alpha", "Alpha", "Alpha",
                   "Beta",  "Beta",  "Beta",  "Beta",
                   "Gamma", "Gamma", "Gamma", "Gamma"),
  year         = rep(c(1990, 1991, 1992, 1993), 3),
  is_democracy = c(FALSE, TRUE,  FALSE, TRUE,    # Alpha: 3 transitions
                   FALSE, FALSE, TRUE,  TRUE,    # Beta:  1 transition
                   TRUE,  TRUE,  TRUE,  TRUE)    # Gamma: 0 transitions
)

test_that("returns a tibble with the right columns", {
  result <- top_transition_countries(transition_data)
  expect_named(result,
    c("country_name", "transitions_in", "transitions_out", "total_transitions"))
})

test_that("transition counts are correct", {
  result <- top_transition_countries(transition_data)

  alpha <- result[result$country_name == "Alpha", ]
  expect_equal(alpha$transitions_in,  2L)
  expect_equal(alpha$transitions_out, 1L)
  expect_equal(alpha$total_transitions, 3L)

  beta <- result[result$country_name == "Beta", ]
  expect_equal(beta$total_transitions, 1L)

  gamma <- result[result$country_name == "Gamma", ]
  expect_equal(gamma$total_transitions, 0L)
})

test_that("result is sorted descending by total_transitions", {
  result <- top_transition_countries(transition_data)
  expect_equal(result$country_name[1], "Alpha")
})

test_that("n limits the number of rows returned", {
  result <- top_transition_countries(transition_data, n = 2)
  expect_equal(nrow(result), 2)
})

test_that("year range filter works", {
  # exclude 1990 so Alpha has fewer transitions
  result <- top_transition_countries(transition_data, start_year = 1991)
  alpha <- result[result$country_name == "Alpha", ]
  expect_equal(alpha$total_transitions, 2L)
})

test_that("non-data-frame input throws an error", {
  expect_error(top_transition_countries("not a df"))
})

test_that("invalid year range throws an error", {
  expect_error(top_transition_countries(transition_data,
    start_year = 2000, end_year = 1990))
})
