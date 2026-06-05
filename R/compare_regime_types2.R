#' Compare a Numeric Variable Between Two Political Regime Types
#'
#' Performs a Welch two-sample t-test comparing the mean of a numeric variable
#' between two groups defined by a categorical regime column. Rows with missing
#' values in `outcome_var` are dropped with a warning before the test is run.
#'
#' @param data A data frame containing the variables named by `outcome_var` and
#'   `regime_var`.
#' @param outcome_var A character string naming the numeric column to compare
#'   (e.g. `"prop_in_par"`).
#' @param regime_var A character string naming the categorical column used to
#'   define the two groups (e.g. `"regime_category"`).
#' @param regime1 A character string for the first regime value
#'   (e.g. `"Democracy"`).
#' @param regime2 A character string for the second regime value
#'   (e.g. `"Non-Democracy"`).
#'
#' @return A one-row [tibble][tibble::tibble-package] produced by
#'   [broom::tidy()], with columns including `estimate1`, `estimate2`,
#'   `statistic`, `p.value`, `conf.low`, `conf.high`, `parameter`, and
#'   `method`.
#'
#' @importFrom dplyr filter
#' @importFrom broom tidy
#' @importFrom stats t.test as.formula
#' @export
#'
#' @examples
#' \dontrun{
#' dem <- load_data()
#'
#' # Compare women in parliament between TRUE/FALSE democracy status
#' compare_regime_types2(dem,
#'   outcome_var = "prop_in_par",
#'   regime_var  = "is_democracy",
#'   regime1     = TRUE,
#'   regime2     = FALSE)
#' }
compare_regime_types2 <- function(data, outcome_var, regime_var, regime1, regime2) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!outcome_var %in% names(data)) {
    stop("`data` does not contain a column named `", outcome_var, "`.",
      call. = FALSE)
  }
  if (!regime_var %in% names(data)) {
    stop("`data` does not contain a column named `", regime_var, "`.",
      call. = FALSE)
  }

  filtered_data <- data |>
    dplyr::filter(.data[[regime_var]] %in% c(regime1, regime2))

  na_count <- sum(is.na(filtered_data[[outcome_var]]))
  if (na_count > 0) {
    warning(
      paste(
        na_count,
        "observations with missing values in",
        outcome_var,
        "were removed before performing the t-test."
      ),
      call. = FALSE
    )
  }

  filtered_data <- filtered_data |>
    dplyr::filter(!is.na(.data[[outcome_var]]))

  if (dplyr::n_distinct(filtered_data[[regime_var]]) != 2) {
    stop(
      "After removing missing values, at least one regime type has no remaining observations.",
      call. = FALSE
    )
  }

  test_result <- stats::t.test(
    stats::as.formula(paste(outcome_var, "~", regime_var)),
    data = filtered_data
  )

  broom::tidy(test_result)
}
