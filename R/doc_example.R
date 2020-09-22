#' Documentation Example for Helper, Statistics and Analyze Functions
#'
#' This illustrates the conventions on how to document helper, statistics and analyze functions
#' together.
#'
#' @template formatting_arguments
#'
#' @name dummy_summation
#'
NULL

#' @describeIn dummy_summation Helper Function which does the actual summation.
#'
#' @export
#'
#' @examples
#' mysum(1:10)
#'
mysum <- function(x) {
  sum(x, na.rm = TRUE)
}

#' @describeIn dummy_summation Statistics Function which sums the numbers, after removing `NA` values.
#' @inheritParams argument_convention
#' @return The statistics are:
#'   * `sum`: the sum of all numbers, after removing `NA` values.
#' @export
#'
#' @examples
#' s_dummy_sum(c(1, 2))
#'
s_dummy_sum <- function(x, .var = "x") {
  list(
    sum = with_label(mysum(x), paste("Sum of", .var))
  )
}

#' @describeIn dummy_summation Analyze Function which adds the sum analysis to
#'   the input layout. Note that additional formatting arguments can be used
#'   here.
#' @inheritParams argument_convention
#'
#' @export
#'
#' @examples
#' basic_table() %>%
#'   split_cols_by("Species") %>%
#'   dummy_sum("Sepal.Length") %>%
#'   build_table(iris)
#'
#' basic_table() %>%
#'   split_cols_by("Species") %>%
#'   dummy_sum(c("Sepal.Length", "Petal.Length")) %>%
#'   build_table(iris)
#'
dummy_sum <- function(lyt, vars) {
  afun <- format_wrap_x(
    s_dummy_sum,
    formats = c(sum = "xx.xx"),
    indent_mods = c(sum = 0L)
  )
  analyze(
    lyt,
    vars,
    afun = afun,
    show_labels = "hidden"
  )
}
