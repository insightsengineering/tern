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
#' @importFrom stats terms model.frame
#'
#' @noRd
s_ancova_items <- function(formula,
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

  args_position <- match(c("formula", "data"), names(cl), 0L)
  args_call <- cl[c(1L, args_position)]
  args_call[[1L]] <- quote(stats::model.frame)
  model_frame <- eval(args_call, env)

  result <- list(
    rsp = model_frame[, col_rsp],
    rsp_name = all.vars(formula)[col_rsp],
    arm = model_frame[, col_arm],
    arm_name = all.vars(formula)[col_arm],
    formula = formula,
    model_frame = model_frame
  )
  return(result)
}
