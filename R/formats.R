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
#' @return a string in the format `num / denom (ratio %)`. If `num` is 0 the format is `num / denom`.
#'
#' @family formatting functions
#' @export
#'
#' @examples
#' format_fraction(x = c(num = 2L, denom = 3L))
#' format_fraction(x = c(num = 0L, denom = 3L))
#'
format_fraction <- function(x, ...) {

  attr(x, "label") <- NULL

  assert_that(
    is.vector(x),
    is_nonnegative_count(x["num"]),
    is_nonnegative_count(x["denom"])
  )

  result <- if (x["num"] == 0) {

    paste0(x["num"], "/", x["denom"])

  } else {

    paste0(
      x["num"], "/", x["denom"],
      " (", round(x["num"] / x["denom"] * 100, 1), "%)"
    )

  }

  return(result)
}

#' Formatting Count and Fraction
#'
#' Formats a count together with fraction with special consideration when count is `0`.
#'
#' @param x (`integer`)\cr vector of length 2, count and fraction.
#' @param ... required for rtables interface.
#' @importFrom rlang is_integerish
#' @return a string in the format `count (fraction %)`. If `count` is 0 the format is `0`.
#'
#' @family formatting functions
#' @export
#'
#' @examples
#' format_count_fraction(x = c(2, 0.6667))
#' format_count_fraction(x = c(0, 0))
#'
format_count_fraction <- function(x, ...) {

  attr(x, "label") <- NULL

  assert_that(
    is.vector(x),
    rlang::is_integerish(x[1]),
    is_proportion(x[2], include_boundaries = TRUE)
  )

  result <- if (x[1] == 0) {
    "0"
  } else {
    paste0(x[1], " (", round(x[2] * 100, 1), "%)")
  }

  return(result)
}

#' Formatting: XX as Formatting Function
#'
#' Translate a string where x and dots are interpreted as number place
#' holders, and others as formatting elements.
#'
#' @param str (`string`)\cr template.
#'
#' @export
#' @return A `rtables` formatting function.
#' @examples
#' test <- list(c(1.658, 0.5761), c(1e1, 785.6))
#'
#' z <- format_xx("xx (xx.x)")
#' sapply(test, z)
#'
#' z <- format_xx("xx.x - xx.x")
#' sapply(test, z)
#'
#' z <- format_xx("xx.x, incl. xx.x% NE")
#' sapply(test, z)
#'
format_xx <- function(str) {

  # Find position in the string.
  positions <- gregexpr(pattern = "x+\\.x+|x+", text = str, perl = TRUE)
  x_positions <- regmatches(x = str, m = positions)[[1]]

  # Roundings depends on the number of x behind [.].
  roundings <- lapply(
    X = x_positions,
    function(x) {
      y <- strsplit(split = "\\.", x = x)[[1]]
      rounding <- function(x)
        round(x, digits = ifelse(length(y) > 1, nchar(y[2]), 0))
      return(rounding)
    }
  )

  rtable_format <- function(x, output) {
    values <- Map(y = x, fun = roundings, function(y, fun) fun(y))
    regmatches(x = str, m = positions)[[1]] <- values
    return(str)
  }

  return(rtable_format)
}
