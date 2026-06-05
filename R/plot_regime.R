#' Plot Proportion of Countries in a Given Political Regime Over Time
#'
#' Creates a filled area + line chart showing what proportion of countries in
#' the dataset were classified under a specified regime category in each year.
#'
#' @param data A data frame containing at minimum the columns `year` and
#'   `regime_category`.
#' @param regime_type A character string specifying which value of
#'   `regime_category` to plot. Defaults to `"Parliamentary democracy"`.
#' @param min_year A numeric value giving the earliest year to include.
#'   Must fall within the range of years present in `data`. Defaults to `1950`.
#' @param max_year A numeric value giving the latest year to include.
#'   Must fall within the range of years present in `data`. Defaults to `2020`.
#'
#' @return A `ggplot` object showing the percentage of countries classified
#'   under `regime_type` for each year from `min_year` to `max_year`.
#'
#' @importFrom dplyr filter group_by summarise
#' @importFrom ggplot2 ggplot aes geom_area geom_line scale_y_continuous labs theme element_blank element_text
#' @importFrom scales label_percent
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' dem <- load_data()
#' plot_regime(dem, regime_type = "Parliamentary democracy",
#'             min_year = 1960, max_year = 2020)
#' }
plot_regime <- function(data,
                        regime_type = "Parliamentary democracy",
                        min_year    = 1950,
                        max_year    = 2020) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!all(c("year", "regime_category") %in% names(data))) {
    stop("`data` must contain `year` and `regime_category` columns.",
      call. = FALSE)
  }

  available_regimes <- unique(data$regime_category)
  year_range        <- range(data$year, na.rm = TRUE)

  if (!regime_type %in% available_regimes) {
    stop(
      paste0("regime_type '", regime_type, "' not found in data. ",
        "Available values: ",
        paste(sort(available_regimes), collapse = ", "), "."),
      call. = FALSE
    )
  }
  if (min_year < year_range[1] || min_year > year_range[2]) {
    stop(
      paste0("min_year (", min_year, ") is outside the data range [",
        year_range[1], ", ", year_range[2], "]."),
      call. = FALSE
    )
  }
  if (max_year < year_range[1] || max_year > year_range[2]) {
    stop(
      paste0("max_year (", max_year, ") is outside the data range [",
        year_range[1], ", ", year_range[2], "]."),
      call. = FALSE
    )
  }
  if (min_year >= max_year) {
    stop("`min_year` must be less than `max_year`.", call. = FALSE)
  }

  summary_data <- data |>
    dplyr::filter(.data$year >= min_year, .data$year <= max_year) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      prop_regime = mean(.data$regime_category == regime_type, na.rm = TRUE),
      .groups = "drop"
    )

  ggplot2::ggplot(summary_data,
    ggplot2::aes(x = .data$year, y = .data$prop_regime)) +
    ggplot2::geom_area(fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_line(color = "navy", linewidth = 1.2) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(),
      limits = c(0, 1)
    ) +
    ggplot2::labs(
      x     = NULL,
      y     = paste0("% of Countries That Are ",
                     gsub("_", " ", regime_type)),
      title = paste0("Global Trends in ", gsub("_", " ", regime_type),
                     ", ", min_year, "–", max_year)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.title  = ggplot2::element_text(color = "steelblue"),
      axis.text   = ggplot2::element_text(color = "steelblue"),
      plot.title  = ggplot2::element_text(color = "steelblue", size = 12)
    )
}
