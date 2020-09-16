# Additional format functions to work with rtables.

#' Format: fraction and percentage
#'
#' Format a fraction together with ratio in percent.
#'
#' @param x a vector with integer elements `num` and `denom`
#' @param ... required for rtables interface
#'
#' @return a string in the format `num / denom (ratio %)`
#' @export
#'
#' @examples
#' format_fraction(x = c(num = 2L, denom = 3L))
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
