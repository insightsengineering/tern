#' Odds Ratio Estimation
#'
#' Compares bivariate responses between two groups in terms of odds ratios
#' along with a confidence interval.
#'
#' @details This function uses either logistic regression for unstratified
#'   analyses, or conditional logistic regression for stratified analyses.
#'   The Wald confidence interval with the specified confidence level is
#'   calculated. Note that, for stratified analyses, there is currently no
#'   implementation for conditional likelihood confidence intervals,
#'   therefore the likelihood confidence interval as an option is not yet
#'   available. Besides, when `rsp` contains only responders or non-responders,
#'   then the result values will be `NA`, because no odds ratio estimation is
#'   possible.
#'
#' @template formatting_arguments
#'
#' @name odds_ratio
#' @md
#'
NULL


#' @describeIn odds_ratio estimates the odds ratio based on [stats::glm()].
#'
#' @inheritParams argument_convention
#' @param data (`data frame`)\cr
#'   expected with three variables `rsp`, `grp`.
#' @importFrom stats binomial as.formula glm coef confint setNames
#' @export
#' @examples
#'
#' data <- data.frame(
#'   rsp = as.logical(c(1, 1, 0, 1, 0, 0)),
#'   grp = letters[c(1, 1, 1, 2, 2, 2)]
#' )
#'
#' or_glm(data, conf_level = 0.95)
#'
or_glm <- function(data, conf_level) {

  assert_that(
    is_df_with_variables(data, list(rsp = "rsp", grp = "grp")),
    is.logical(data$rsp),
    is_character_or_factor(data$grp),
    is_proportion(conf_level)
  )

  data$grp <- as_factor_keep_attributes(data$grp)
  formula <- stats::as.formula("rsp ~ grp")
  model_fit <- stats::glm(
    formula = formula, data = data,
    family = stats::binomial(link = "logit")
  )

  # Note that here we need to discard the intercept.
  or <- exp(stats::coef(model_fit)[-1])
  or_ci <- exp(
    stats::confint.default(model_fit, level = conf_level)[-1, , drop = FALSE]
  )

  values <- stats::setNames(c(or, or_ci), c("or", "or_lcl", "or_ucl"))

  list(or_ci = values)

}
