#' Find Countries with the Most Democratic Transitions
#'
#' Filters the dataset to a user-specified year range and returns the top `n`
#' countries ranked by total number of democratic-regime transitions (both
#' entries into and exits from democracy).
#'
#' Transitions are computed from the `is_democracy` column using a within-country
#' lag ([data.table::shift()]), so no pre-processing with [clean_data()] is
#' required. The grouped lag and aggregation are performed with
#' [data.table][data.table::data.table-package], which keeps the function fast
#' on moderately large country-year panels.
#'
#' @param data A data frame containing democracy and leadership data, such as
#'   the output of [load_data()]. Must contain `country_name`, `year`, and
#'   `is_democracy`.
#' @param start_year Numeric. Earliest year to include. Defaults to `1950`.
#' @param end_year Numeric. Latest year to include; must be at least
#'   `start_year`. Defaults to `2020`.
#' @param n Integer. Number of top countries to return. Defaults to `10`.
#'
#' @return A tibble with one row per country (up to `n` rows) and columns:
#'   \describe{
#'     \item{`country_name`}{Country name.}
#'     \item{`transitions_in`}{Number of years in which the country became a
#'       democracy.}
#'     \item{`transitions_out`}{Number of years in which the country ceased to
#'       be a democracy.}
#'     \item{`total_transitions`}{Sum of `transitions_in` and
#'       `transitions_out`.}
#'   }
#'   Rows are sorted descending by `total_transitions`.
#'
#' @importFrom data.table as.data.table setorder shift
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' \dontrun{
#' dem <- load_data()
#' top_transition_countries(dem, start_year = 1970, end_year = 2010, n = 5)
#' }
top_transition_countries <- function(data,
                                     start_year = 1950,
                                     end_year   = 2020,
                                     n          = 10) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.numeric(start_year) || length(start_year) != 1) {
    stop("`start_year` must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(end_year) || length(end_year) != 1) {
    stop("`end_year` must be a single numeric value.", call. = FALSE)
  }
  if (start_year > end_year) {
    stop("`start_year` must be less than or equal to `end_year`.", call. = FALSE)
  }
  if (!is.numeric(n) || length(n) != 1 || n < 1) {
    stop("`n` must be a positive integer.", call. = FALSE)
  }

  required_cols <- c("country_name", "year", "is_democracy")
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "`data` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dt <- data.table::as.data.table(data)
  dt <- dt[dt$year >= start_year & dt$year <= end_year, ]
  data.table::setorder(dt, country_name, year)

  dt[, .prev := data.table::shift(is_democracy), by = country_name]
  dt[, to_democracy := as.integer(
    !is.na(is_democracy) & !is.na(.prev) & is_democracy & !.prev)]
  dt[, from_democracy := as.integer(
    !is.na(is_democracy) & !is.na(.prev) & !is_democracy & .prev)]

  res <- dt[, list(
    transitions_in  = sum(to_democracy,   na.rm = TRUE),
    transitions_out = sum(from_democracy, na.rm = TRUE)
  ), by = country_name]

  res[, total_transitions := transitions_in + transitions_out]
  data.table::setorder(res, -total_transitions)

  res <- res[seq_len(min(as.integer(n), nrow(res))), ]
  tibble::as_tibble(res)
}
