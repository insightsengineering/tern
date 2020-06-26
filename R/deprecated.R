# March 2020
t_summary_true <- function(x, # nousage # nolint
                           col_by,
                           col_N = NULL, # nolint
                           total = NULL,
                           row_name = deparse(substitute(x)),
                           denominator = c("N", "n", "omit")) {
  warning("t_summary_true is deprecated, use t_count_true instead")
}


# January 2019

#' Replace NA values by NA string level
#'
#' @param x factor, possibly with NAs
#' @param na_level name of new level for NAs
#'
#' @return factor with additional NA level
#'
#' @export
#' @examples
#' explicit_na(factor(c(1, 1, 2)))
#' explicit_na(factor(c(1, 1, NA)), "na")
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
