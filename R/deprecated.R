


# January 2019

#' Replace NA values by NA string level
#'
#' @param x factor, possibly with NAs
#' @param na_level name of new level for NAs
#'
#' @return factor with additional NA level
#'
#' @export
#'
#' @examples
#' na_as_level(factor(c(1, 1, 2)))
#' na_as_level(factor(c(1, 1, NA)), "na")
na_as_level <- function(x, na_level = "NA") {
  warning("na_as_level deprecated use explicit_na instead")

  stopifnot(is.factor(x))

  if (any(is.na(x))) {
    if (na_level %in% levels(x)) {
      stop(na_level, " can not be a level of x")
    }
    levels(x) <- c(levels(x), na_level)
    x[is.na(x)] <- na_level
  }
  x
}
