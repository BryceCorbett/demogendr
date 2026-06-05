#' Plot Average Leader Accession Ages Over Time
#'
#' Creates a ribbon-and-line chart showing the average age at which presidents
#' and monarchs took office, broken down by gender, over a chosen year range.
#' The shaded ribbon between the female and male lines illustrates the
#' gender gap for each leader type.
#'
#' No female presidents were recorded before 1980 in the bundled dataset, so
#' the default `min_year` starts there to avoid empty series.
#'
#' @param data A data frame containing democracy and leadership data, such as
#'   the output of [load_data()]. If the columns
#'   `president_age_at_accession` and `monarch_age_at_accession` are not
#'   present, they are derived from the raw accession-year and birth-year
#'   columns.
#' @param min_year Numeric. Earliest year to include. Defaults to `1980`.
#' @param max_year Numeric. Latest year to include. Defaults to `2020`.
#'
#' @return A `ggplot` object with four lines (female/male × president/monarch)
#'   and two shaded ribbons showing the gender gap for each leader type.
#'
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line scale_color_manual scale_x_continuous labs theme_minimal theme element_text element_rect
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' dem <- load_data()
#' plot_accession_ages(dem)
#' plot_accession_ages(dem, min_year = 1990, max_year = 2015)
#' }
plot_accession_ages <- function(data, min_year = 1980, max_year = 2020) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.numeric(min_year) || length(min_year) != 1) {
    stop("`min_year` must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(max_year) || length(max_year) != 1) {
    stop("`max_year` must be a single numeric value.", call. = FALSE)
  }
  if (min_year >= max_year) {
    stop("`min_year` must be less than `max_year`.", call. = FALSE)
  }

  # Derive age-at-accession columns if not already present
  if (!"president_age_at_accession" %in% names(data)) {
    required <- c("president_accesion_year", "president_birthyear",
                  "monarch_accession_year",  "monarch_birthyear")
    missing  <- setdiff(required, names(data))
    if (length(missing) > 0) {
      stop("`data` is missing required columns: ",
        paste(missing, collapse = ", "), call. = FALSE)
    }
    data <- data |>
      dplyr::mutate(
        president_age_at_accession = .data$president_accesion_year - .data$president_birthyear,
        monarch_age_at_accession   = .data$monarch_accession_year  - .data$monarch_birthyear
      )
  }

  required_cols <- c("year", "is_presidential", "is_female_president",
                     "is_monarchy", "is_female_monarch",
                     "president_age_at_accession", "monarch_age_at_accession")
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing required columns: ",
      paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  yearly <- data |>
    dplyr::filter(.data$year >= min_year, .data$year <= max_year) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      avg_female_pres = mean(
        .data$president_age_at_accession[.data$is_presidential & .data$is_female_president],
        na.rm = TRUE),
      avg_male_pres   = mean(
        .data$president_age_at_accession[.data$is_presidential & !.data$is_female_president],
        na.rm = TRUE),
      avg_female_mon  = mean(
        .data$monarch_age_at_accession[.data$is_monarchy & .data$is_female_monarch],
        na.rm = TRUE),
      avg_male_mon    = mean(
        .data$monarch_age_at_accession[.data$is_monarchy & !.data$is_female_monarch],
        na.rm = TRUE),
      .groups = "drop"
    )

  ggplot2::ggplot(yearly, ggplot2::aes(x = .data$year)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$avg_female_pres, ymax = .data$avg_male_pres),
      fill = "#D3D3D3", alpha = 0.8) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$avg_female_mon, ymax = .data$avg_male_mon),
      fill = "#D3D3D3", alpha = 0.8) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$avg_female_pres, color = "Female President"),
      linewidth = 1.2) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$avg_male_pres,   color = "Male President"),
      linewidth = 1.2) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$avg_female_mon,  color = "Female Monarch"),
      linewidth = 1.2) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$avg_male_mon,    color = "Male Monarch"),
      linewidth = 1.2) +
    ggplot2::scale_color_manual(
      name   = "Leader Type",
      values = c(
        "Female President" = "#e8a0b0",
        "Male President"   = "#5b8db8",
        "Female Monarch"   = "#e8a0b0",
        "Male Monarch"     = "#89afd4"
      ),
      breaks = c("Female President", "Male President",
                 "Female Monarch",   "Male Monarch")
    ) +
    ggplot2::scale_x_continuous(limits = c(min_year, max_year)) +
    ggplot2::labs(
      title    = "Average Accession Ages of Leaders Over Time",
      subtitle = "Shaded ribbon shows the gender gap; pink = female, blue = male",
      x        = "Year",
      y        = "Average Age at Accession",
      color    = "Leader Type"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.direction = "horizontal"
    )
}
