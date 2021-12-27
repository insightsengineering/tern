#' Control Function for Logistic Regression Model Fitting
#'
#' This is an auxiliary function for controlling arguments for logistic regression models.
#' `conf_level` refers to the confidence level used for the Odds Ratio CIs.
#'
#' @param response_definition (`string`)\cr the definition of what an event is in terms of `response`.
#'   This will be used when fitting the logistic regression model on the left hand side of the formula.
#'   Note that the evaluated expression should result in either a logical vector or a factor with 2
#'   levels. By default this is just `"response"` such that the original response variable is used
#'   and not modified further.
#' @inheritParams argument_convention
#' @return A list of components with the same names as the arguments.
#' @export
#' @examples
#' # Standard options.
#' control_logistic()
#'
#' # Modify confidence level.
#' control_logistic(conf_level = 0.9)
#'
#' # Use a different response definition.
#' control_logistic(response_definition = "I(response %in% c('CR', 'PR'))")
control_logistic <- function(response_definition = "response",
                             conf_level = 0.95) {
  assertthat::assert_that(
    assertthat::is.string(response_definition),
    grepl("response", response_definition),
    is_proportion(conf_level)
  )
  list(
    response_definition = response_definition,
    conf_level = conf_level
  )
}
