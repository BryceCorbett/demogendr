#' Summarize Gender Gaps by Group
#'
#' Internal helper function that calculates gender representation and
#' accession-age summaries within groups. For each level of the grouping
#' variable, the function computes the proportion of female and male leaders
#' and the mean accession age for each gender.
#'
#' This helper is used internally by [summarise_leader_gender_gap()]. It is
#' column-agnostic: the caller specifies which columns to use for the group,
#' gender, and age inputs.
#'
#' @param data A data frame containing leadership data.
#' @param group_var A character string giving the variable used for grouping.
#' @param gender_var A character string giving the name of the gender column.
#'   Expects values like `"female"` and `"male"`.
#' @param age_var A character string giving the name of the accession-age
#'   column.
#'
#' @return A tibble with one row per group and columns for the proportion of
#'   female leaders, proportion of male leaders, and the mean accession ages by
#'   gender.
#'
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#'
#' @keywords internal
summarise_gender_gap <- function(data, group_var, gender_var, age_var) {
  data |>
    dplyr::group_by(.data[[group_var]]) |>
    dplyr::summarise(
      prop_female_leaders = mean(.data[[gender_var]] == "female", na.rm = TRUE),
      prop_male_leaders   = mean(.data[[gender_var]] == "male",   na.rm = TRUE),
      mean_female_accession_age = mean(
        .data[[age_var]][.data[[gender_var]] == "female"],
        na.rm = TRUE
      ),
      mean_male_accession_age = mean(
        .data[[age_var]][.data[[gender_var]] == "male"],
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}
