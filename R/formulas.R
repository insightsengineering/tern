#' Drop Special from Formula
#'
#' For example, this can be used to get an unstratified version of a formula.
#'
#' @param terms an object return by \code{\link[stats]{terms}}
#' @param special a character string of the special
#'
#' @return a formula object witouth the special
#'
#' @noRd
#'
#' @importFrom stats update.formula as.formula
#'
#' @examples
#' formula <- X ~ Y + strata(B, C, D)
#' trm <- terms(formula, specials = "strata")
#'
#' tern:::drop_special(trm, "strata")
#'
drop_special <- function(terms, special) {
  specials <- attr(terms, "specials")
  if (!is.null(specials[[special]])) {
    keep <- which(attr(terms, "factors")[specials[[special]], ] == 0)
    update.formula(
      old = as.formula(terms),
      new = as.formula(
        paste(". ~", paste(attr(terms, "term.labels")[keep], collapse = "+")))
      )
  } else {
    as.formula(terms)
  }
}

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
