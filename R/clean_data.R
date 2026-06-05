#' Clean the Democracy Dataset
#'
#' Adds derived columns to the raw dataset returned by [load_data()]:
#' age-at-accession for presidents and monarchs, a binary `has_female_leader`
#' indicator, a `decade` label, and year-over-year democratic-transition flags
#' (`to_democracy` / `from_democracy`).
#'
#' The transition flags are computed per country using [dplyr::lag()]: a
#' `to_democracy` event is recorded in the year a country's `is_democracy`
#' status flips from `FALSE` to `TRUE`, and vice-versa for `from_democracy`.
#'
#' @param data A data frame such as the output of [load_data()]. Must contain
#'   the columns `president_accesion_year`, `president_birthyear`,
#'   `monarch_accession_year`, `monarch_birthyear`, `is_female_president`,
#'   `is_female_monarch`, `year`, `country`, and `is_democracy`.
#'
#' @return A tibble with all original columns plus:
#'   \describe{
#'     \item{`president_age_at_accession`}{Age (years) when the president took
#'       office.}
#'     \item{`monarch_age_at_accession`}{Age (years) when the monarch took the
#'       throne.}
#'     \item{`has_female_leader`}{`1` if the country had a female president
#'       *or* monarch that year, `0` otherwise.}
#'     \item{`decade`}{Character label such as `"1990s"` derived from `year`.}
#'     \item{`to_democracy`}{`1` in years when a country transitioned *into*
#'       democracy, `0` otherwise.}
#'     \item{`from_democracy`}{`1` in years when a country transitioned *out
#'       of* democracy, `0` otherwise.}
#'   }
#'
#' @importFrom dplyr arrange group_by ungroup mutate select lag case_when
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' raw  <- load_data()
#' clean <- clean_data(raw)
#' head(clean[, c("country_name", "year", "decade",
#'                "to_democracy", "from_democracy",
#'                "president_age_at_accession")])
#' }
clean_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  required_cols <- c(
    "president_accesion_year", "president_birthyear",
    "monarch_accession_year",  "monarch_birthyear",
    "is_female_president", "is_female_monarch",
    "year", "country", "is_democracy"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "`data` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  data |>
    dplyr::mutate(
      president_age_at_accession = .data$president_accesion_year - .data$president_birthyear,
      monarch_age_at_accession   = .data$monarch_accession_year  - .data$monarch_birthyear,
      has_female_leader = as.integer(.data$is_female_president | .data$is_female_monarch),
      decade = dplyr::case_when(
        .data$year < 1960 ~ "1950s",
        .data$year < 1970 ~ "1960s",
        .data$year < 1980 ~ "1970s",
        .data$year < 1990 ~ "1980s",
        .data$year < 2000 ~ "1990s",
        .data$year < 2010 ~ "2000s",
        TRUE              ~ "2010s"
      )
    ) |>
    dplyr::arrange(.data$country, .data$year) |>
    dplyr::group_by(.data$country) |>
    dplyr::mutate(
      .prev_dem      = dplyr::lag(.data$is_democracy),
      to_democracy   = as.integer(
        !is.na(.data$is_democracy) & !is.na(.data$.prev_dem) &
          .data$is_democracy & !.data$.prev_dem
      ),
      from_democracy = as.integer(
        !is.na(.data$is_democracy) & !is.na(.data$.prev_dem) &
          !.data$is_democracy & .data$.prev_dem
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of(".prev_dem"))
}
