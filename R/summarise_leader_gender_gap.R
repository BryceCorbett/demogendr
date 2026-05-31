#' Summarize Gender Gaps in Political Leadership
#'
#' Creates a tidy summary table comparing female and male leaders by a selected
#' grouping variable. The summary includes the proportion of leaders of each
#' gender and the mean accession age for each gender.
#'
#' If the data does not already contain `<leader>_gender` and
#' `<leader>_accession_age` columns, the function will derive them from the raw
#' columns shipped in [load_data()] (`is_female_<leader>`, accession year, and
#' birth year). If `group_var = "decade"` and no `decade` column is present, it
#' will be derived from `year`.
#'
#' @param data A data frame containing democracy and leadership data, such as
#'   the output of [load_data()], or a pre-processed tibble already containing
#'   `<leader>_gender` and `<leader>_accession_age` columns.
#' @param group_var A character string giving the variable to group by, such as
#'   `"decade"`, `"year"`, `"is_democracy"`, or `"region"`. Defaults to
#'   `"decade"`.
#' @param leader A character string indicating which type of leader to
#'   summarize. Use `"president"` for presidents or `"monarch"` for monarchs.
#'   Defaults to `"president"`.
#'
#' @return A tidy tibble grouped by `group_var`, with summary columns for the
#'   proportion of female leaders, the proportion of male leaders, the mean
#'   accession age for female leaders, and the mean accession age for male
#'   leaders.
#'
#' @export
summarise_leader_gender_gap <- function(data,
                                        group_var = "decade",
                                        leader = "president") {

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.character(group_var) || length(group_var) != 1) {
    stop("`group_var` must be a single character string.", call. = FALSE)
  }
  if (!leader %in% c("president", "monarch")) {
    stop('`leader` must be either "president" or "monarch".', call. = FALSE)
  }

  gender_var <- paste0(leader, "_gender")
  age_var <- paste0(leader, "_accession_age")

  # Derive the gender column from is_female_<leader> if not already there
  if (!gender_var %in% names(data)) {
    raw_gender <- paste0("is_female_", leader)
    if (raw_gender %in% names(data)) {
      data[[gender_var]] <- ifelse(data[[raw_gender]], "female", "male")
    }
  }

  # Derive the accession-age column from accession_year - birthyear if not
  # already there. Note the typo "accesion" preserved from the source dataset
  # for presidents.
  if (!age_var %in% names(data)) {
    acc_col <- if (leader == "president") {
      "president_accesion_year"
    } else {
      "monarch_accession_year"
    }
    birth_col <- paste0(leader, "_birthyear")
    if (all(c(acc_col, birth_col) %in% names(data))) {
      data[[age_var]] <- data[[acc_col]] - data[[birth_col]]
    }
  }

  # Derive decade from year if grouping by decade and decade is missing
  if (group_var == "decade" && !"decade" %in% names(data) && "year" %in% names(data)) {
    data$decade <- floor(data$year / 10) * 10
  }

  # Final column check
  if (!all(c(group_var, gender_var, age_var) %in% names(data))) {
    stop(
      "`data` must contain (or be able to derive) `", group_var,
      "`, `", gender_var, "`, and `", age_var, "` columns.",
      call. = FALSE
    )
  }

  # Delegate to the internal helper
  summarise_gender_gap(
    data = data,
    group_var = group_var,
    gender_var = gender_var,
    age_var = age_var
  )
}
