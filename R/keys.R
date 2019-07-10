#' Get object keys
#'
#'
#' @param x any R object
#' @return keys attribute from given object
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' ASL <- cadsl
#'
#' keys(ASL) <- c("USUBJID", "STUDYID")
#' keys(ASL)

keys <- function(x) {
  attr(x, "keys")
}

#' Set object keys
#'
#'
#' @param x any R object
#' @param value (\code{character}) with keys
#'
#' @export
#' @examples
#' library(random.cdisc.data)
#' ASL <- cadsl
#'
#' keys(ASL) <- c("USUBJID", "STUDYID")
#' keys(ASL)
`keys<-` <- function(x, value) {
  stopifnot(is.data.frame(x),
            all(value %in% names(x)))

  if (any(duplicated(x[, value]))) {
    stop("keys don't uniquely distinguish the rows,  i.e. some rows share the same keys")
  }

  attr(x, "keys") <- value
  x
}
