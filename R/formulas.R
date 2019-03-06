#' Drop a special from a formula
#'
#' For example get an unstratified version of a forumla
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
#' @author boruvkaa
#'
#' @examples
#' formula <- X ~ Y + strata(B, C, D)
#' trm <- terms(formula, specials = "strata")
#'
#' tern:::drop_special(trm, "strata")
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


#' Indentity function to indicate specials in formulas
#'
#' We use \code{arm} to in tern formulas
#'
#' @param x arm information
#'
#' @export
#'
#' @author boruvkaa
arm <- function(x) {
  structure(x, varname = deparse(substitute(x)))
}
