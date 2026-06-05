#' Plot Women's Representation in Parliament Over Time
#'
#' Creates a line plot showing the proportion of parliamentary seats held by
#' women over time for selected countries.
#'
#' @param data A data frame containing democracy and leadership data, such as
#'   the output of [load_data()].
#' @param countries A character vector of country names to include in the plot.
#'   Defaults to `c("United States", "Sweden", "Honduras")`.
#' @param min_year A numeric value giving the earliest year to include.
#'   Defaults to `1990`.
#' @param color_by A character string giving the variable used to color the
#'   lines. Defaults to `"is_democracy"`.
#'
#' @return A `ggplot` object showing `prop_in_par` over time, with one line per
#'   country. Line color indicates the value of the variable specified by
#'   `color_by`, such as democratic status.
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dem <- load_data()
#' plot_women_in_parliament(dem, countries = c("Sweden", "United States"))
#' }
plot_women_in_parliament <- function(data,
                                     countries = c("United States", "Sweden", "Honduras"),
                                     min_year = 1990,
                                     color_by = "is_democracy") {

  # Input validation
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.character(countries) || length(countries) < 1) {
    stop("`countries` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.numeric(min_year) || length(min_year) != 1) {
    stop("`min_year` must be a single numeric value.", call. = FALSE)
  }
  if (!is.character(color_by) || length(color_by) != 1) {
    stop("`color_by` must be a single character string.", call. = FALSE)
  }
  required_cols <- c("country_name", "year", "prop_in_par", color_by)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "`data` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  data |>
    dplyr::filter(
      .data$country_name %in% countries,
      .data$year >= min_year
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$year,
        y = .data$prop_in_par,
        group = .data$country_name,
        color = .data[[color_by]]
      )
    ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = "Women's Representation in Parliament Over Time",
      subtitle = paste("Countries:", paste(countries, collapse = ", ")),
      x = "Year",
      y = "Proportion of Parliamentary Seats Held by Women",
      color = color_by
    ) +
    ggplot2::theme_minimal()
}
