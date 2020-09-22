#' ANCOVA Model Helper
#'
#' Helper function to infer the ANCOVA model response and arm variables.
#'
#' @param formula The \code{formula} passed to `s_ancova`.
#' @param cl The original call to the `s_ancova` function.
#' @param data The \code{data.frame} passed to `s_ancova`.
#' @param env The \code{environment} in which to evaluate the model frame call.
#'
#' @return Named list with the following elements:
#' \describe{
#'   \item{rsp}{The vector with the response variable values.}
#'   \item{rsp_name}{The string of the response variable name.}
#'   \item{arm}{The vector with the arm variable values.}
#'   \item{arm_name}{The string of the arm variable name.}
#'   \item{formula}{The input formula.}
#'   \item{model_frame}{The \code{model.frame} object which was created internally.}
#' }
#'
#' @note For missing values, this function inherits the behavior from \code{\link[stats]{model.frame}}:
#'   Rows with missing values are by default removed. However, if the user sets the `na.action` attribute
#'   of `data`, then this is respected. For example, if it is set to `na.fail`, then the `s_ancova_items`
#'   call will fail.
#'
#' @importFrom stats terms model.frame
#'
#' @noRd
s_ancova_items <- function(formula, # nousage # nolint
                           cl,
                           data,
                           env) {

  model_terms <- stats::terms(
    formula,
    specials = c("arm"),
    data = data
  )
  if (!all(all.vars(attr(model_terms, "variables")) %in% names(data))) {
    stop("All formula variables must appear in 'data'.")
  }

  col_rsp <- attr(model_terms, "response")
  col_arm <- attr(model_terms, "specials")$arm
  if (is.null(col_rsp) || is.null(col_arm)) {
    stop("Formula must include both response and arm.")
  }

  # Evaluate the model frame call, and in this force "na.omit" action
  # on missing values.
  args_position <- match(c("formula", "data"), names(cl), 0L)
  args_call <- cl[c(1L, args_position)]
  args_call[[1L]] <- quote(stats::model.frame)
  model_frame <- eval(args_call, env)

  # Obtain names of response and arm variable, and adjust `model_frame` arm column name.
  # This makes downstream use in `s_ancova` easier.
  rsp_name <- all.vars(formula)[col_rsp]
  arm_name <- all.vars(formula)[col_arm]
  colnames(model_frame)[col_arm] <- arm_name

  result <- list(
    rsp = model_frame[, col_rsp],
    rsp_name = rsp_name,
    arm = model_frame[, col_arm],
    arm_name = arm_name,
    formula = formula,
    model_frame = model_frame
  )
  return(result)
}

#' Analysis of Covariance
#'
#' Summary for analysis of covariance (ANCOVA).
#'
#' @param formula A \code{formula} corresponding to the investigated \code{\link[stats:lm]{linear model}}.
#'   The left-hand side must include the dependent variable. The right-hand side must include the group variable
#'   name wrapped in the special `arm()`.
#' @param data A \code{data.frame} which includes all the variables that are called in \code{formula}.
#' @param conf_level The confidence level of the resulting confidence intervals (default 0.95).
#'
#' @md
#'
#' @return Named list with analysis results as \code{summary_emm} objects, which are \code{data.frame} objects
#'   with a special print method:
#' \describe{
#'   \item{sum_fit}{Summary of the estimated marginal means in the groups.}
#'   \item{sum_contrasts}{Summary of the estimated contrasts of the marginal means. Here the first level of the arm
#'     variable is taken as the reference or control group, and all other groups are compared with this control group.
#'     Specifically, the differences of the marginal means are estimated.
#'   }
#' }
#'
#' @export
#'
#' @importFrom stats lm
#' @importFrom emmeans emmeans contrast
#'
#' @examples
#'
#' # Estimate the adjusted means of sepal length in the different iris species,
#' # adjusting for covariates:
#' result <- s_ancova(
#'   formula = Sepal.Length ~ arm(Species) + Sepal.Width + Petal.Length + Petal.Width,
#'   data = iris
#' )
#' result
#' # We can see that the adjusted mean sepal length in the "setosa" species is significantly
#' # higher than in the "versicolor" and "virginica" species (p-values 0.003 in both differences).
s_ancova <- function(formula,
                     data,
                     conf_level = 0.95) {
  # Extract and check the ANCOVA variables.
  ancova_items <- s_ancova_items(
    formula = formula,
    cl = match.call(),
    data = data,
    env = parent.frame()
  )
  y <- ancova_items$rsp
  arm <- ancova_items$arm
  arm_name <- ancova_items$arm_name
  stopifnot(
    is.numeric(y),
    !any(is.na(y)),
    is.factor(arm),
    !any(is.na(arm))
  )

  # Fit the linear model and derive estimated marginal means (EMM).
  lm_fit <- stats::lm(
    formula = ancova_items$formula,
    data = ancova_items$model_frame
  )
  emmeans_fit <- emmeans::emmeans(
    lm_fit,
    # We specify here the group variable over which EMM are desired.
    specs = arm_name,
    # We pass the data again so that the factor levels of the arm variable can be inferred.
    data = data
  )
  sum_fit <- summary(
    emmeans_fit,
    level = conf_level
  )

  # Derive total and complete sample sizes for the groups and add to summary.
  n_total <- table(data[[arm_name]])
  n_complete <- table(arm)
  sum_fit_group_rows <- as.character(sum_fit[[arm_name]])
  sum_fit$n_total <- as.numeric(n_total[sum_fit_group_rows])
  sum_fit$n_complete <- as.numeric(n_complete[sum_fit_group_rows])

  # Estimate the differences between the marginal means.
  emmeans_contrasts <- emmeans::contrast(
    emmeans_fit,
    # Compare all arms versus the control arm.
    method = "trt.vs.ctrl",
    # Take the first level of the arm factor as the control arm.
    ref = 1
  )
  sum_contrasts <- summary(
    emmeans_contrasts,
    # Derive confidence intervals, t-tests and p-values.
    infer = TRUE,
    # Don't adjust the p-values for multiplicity.
    adjust = "none"
  )

  result <- list(
    sum_fit = sum_fit,
    sum_contrasts = sum_contrasts
  )
  return(result)
}
