#' Test for a Gender-Representation Gap in Parliament
#'
#' Runs a Welch two-sample t-test comparing the proportion of parliamentary
#' seats held by women (`prop_in_par`) across two groups defined by a
#' user-supplied grouping column. The column is passed as an **unquoted name**
#' using tidy evaluation, so no quotes are needed at the call site.
#'
#' @param data A data frame containing democracy and leadership data, such as
#'   the output of [load_data()]. Must contain `prop_in_par` and the column
#'   named by `group_col`.
#' @param group_col \<[`tidy-select`][dplyr::dplyr_tidy_select]\> An unquoted
#'   column name whose values define the two groups. Defaults to
#'   `is_democracy`. The column must have exactly two distinct non-`NA` levels
#'   for the t-test to be well-defined.
#'
#' @return A one-row [tibble][tibble::tibble-package] produced by
#'   [broom::tidy()], with columns including `estimate1`, `estimate2`,
#'   `statistic`, `p.value`, `conf.low`, `conf.high`, `parameter`, and
#'   `method`.
#'
#' @importFrom rlang ensym as_name
#' @importFrom broom tidy
#' @importFrom stats t.test reformulate
#' @export
#'
#' @examples
#' \dontrun{
#' dem <- load_data()
#'
#' # Compare women in parliament between democracies and non-democracies
#' analyse_parliament_gap(dem)
#'
#' # Use a different grouping column (must be binary / two-level)
#' analyse_parliament_gap(dem, group_col = is_presidential)
#' }
analyse_parliament_gap <- function(data, group_col = is_democracy) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  col_name <- rlang::as_name(rlang::ensym(group_col))

  if (!col_name %in% names(data)) {
    stop("`data` does not contain a column named `", col_name, "`.",
      call. = FALSE)
  }
  if (!"prop_in_par" %in% names(data)) {
    stop("`data` must contain a `prop_in_par` column.", call. = FALSE)
  }

  n_levels <- length(unique(stats::na.omit(data[[col_name]])))
  if (n_levels != 2L) {
    stop(
      "`", col_name, "` must have exactly two non-NA levels for a t-test, ",
      "but found ", n_levels, ".",
      call. = FALSE
    )
  }

  fmla   <- stats::reformulate(col_name, "prop_in_par")
  result <- stats::t.test(fmla, data = data)
  broom::tidy(result)
}
