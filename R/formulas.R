has_special_strata <- function(formula) {
  !is.null(attr(terms(formula, specials = "strata"), "specials")$strata)
}
has_special_arm <- function(formula) { # nousage # nolint
  !is.null(attr(terms(formula, specials = "arm"), "arm")$arm)
}

#' Indicate Arm Variable in Formula
#'
#' We use `arm` to indicate the study arm variable in `tern` formulas.
#'
#' @param x arm information
#'
#' @export
#'
arm <- function(x) {
  structure(x, varname = deparse(substitute(x)))
}
