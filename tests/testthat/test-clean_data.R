minimal_data <- function() {
  tibble::tibble(
    country                  = c("A", "A", "A", "B", "B"),
    year                     = c(1990, 1991, 1992, 1990, 1991),
    is_democracy             = c(FALSE, TRUE, TRUE, TRUE, FALSE),
    is_female_president      = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    is_female_monarch        = c(FALSE, FALSE, FALSE, TRUE,  TRUE),
    president_accesion_year  = c(1988, 1991, 1990, 1985, 1985),
    president_birthyear      = c(1950, 1955, 1945, 1940, 1940),
    monarch_accession_year   = c(1980, 1980, 1980, 1989, 1989),
    monarch_birthyear        = c(1955, 1955, 1955, 1960, 1960)
  )
}

test_that("clean_data returns expected new columns", {
  result <- clean_data(minimal_data())
  expect_true(all(c("president_age_at_accession",
                     "monarch_age_at_accession",
                     "has_female_leader",
                     "decade",
                     "to_democracy",
                     "from_democracy") %in% names(result)))
})

test_that("age at accession is computed correctly", {
  result <- clean_data(minimal_data())
  # A/1991: president_accesion_year 1991 - birthyear 1955 = 36
  expect_equal(result$president_age_at_accession[result$country == "A" & result$year == 1991], 36)
  # B/1990: monarch_accession_year 1989 - birthyear 1960 = 29
  expect_equal(result$monarch_age_at_accession[result$country == "B" & result$year == 1990], 29)
})

test_that("has_female_leader is 1 when either leader is female", {
  result <- clean_data(minimal_data())
  # A/1991: is_female_president TRUE → 1
  expect_equal(result$has_female_leader[result$country == "A" & result$year == 1991], 1L)
  # B/1990: is_female_monarch TRUE → 1
  expect_equal(result$has_female_leader[result$country == "B" & result$year == 1990], 1L)
  # A/1990: both FALSE → 0
  expect_equal(result$has_female_leader[result$country == "A" & result$year == 1990], 0L)
})

test_that("decade labels are correct", {
  result <- clean_data(minimal_data())
  expect_true(all(result$decade == "1990s"))
})

test_that("to_democracy and from_democracy transitions are detected", {
  result <- clean_data(minimal_data())
  # A: FALSE -> TRUE at 1991
  expect_equal(result$to_democracy[result$country == "A" & result$year == 1991], 1L)
  # B: TRUE -> FALSE at 1991
  expect_equal(result$from_democracy[result$country == "B" & result$year == 1991], 1L)
  # No transition in first rows
  expect_equal(result$to_democracy[result$country == "A" & result$year == 1990], 0L)
})

test_that("clean_data errors on non-data-frame input", {
  expect_error(clean_data("not a data frame"))
})

test_that("clean_data errors when required columns are missing", {
  expect_error(clean_data(tibble::tibble(x = 1)))
})
