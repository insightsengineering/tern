#' Additional Formatting Functions
#'
#' This summarizes the additional Formatting Functions to work with rtables.
#'
#' @family formatting functions
#' @name formatting_functions
NULL

#' Formatting Fraction and Percentage
#'
#' Formats a fraction together with ratio in percent.
#'
#' @param x (`integer`)\cr with elements `num` and `denom`.
#' @param ... required for rtables interface.
#' @return a string in the format `num / denom (ratio %)`
#'
#' @family formatting functions
#' @export
#'
#' @examples
#' format_fraction(x = c(num = 2L, denom = 3L))
#'
format_fraction <- function(x, ...) {
  assert_that(
    is.vector(x),
    is_nonnegative_count(x["num"]),
    is_nonnegative_count(x["denom"])
  )
  result <- paste0(
    x["num"], "/", x["denom"],
    " (", round(x["num"] / x["denom"] * 100, 1), "%)"
  )
  return(result)
}
