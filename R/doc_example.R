#' Documentation Example for Helper, Statistics, Formatted Analysis and Analyze Functions
#'
#' This illustrates the conventions on how to document helper, statistics, formatted analysis
#' and analyze functions together.
#'
#' @name dummy_summation
#'
NULL

#' @describeIn dummy_summation Helper Function which does the actual summation.
#' @inheritParams argument_convention
#'
#' @export
#'
#' @examples
#' h_dummy_sum(1:10)
#'
h_dummy_sum <- function(x) {
  sum(x, na.rm = TRUE)
}

#' @describeIn dummy_summation Statistics Function which sums the numbers, after removing `NA` values.
#' @inheritParams argument_convention
#' @return The statistics are:
#'   * `sum`: the sum of all numbers, after removing `NA` values.
#'
#' @export
#'
#' @examples
#' s_dummy_sum(c(1, 2))
#'
s_dummy_sum <- function(x, .var = "x") {
  list(
    sum = with_label(h_dummy_sum(x), paste("Sum of", .var))
  )
}

#' @describeIn dummy_summation Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' a_dummy_sum(c(1, 2))
#'
a_dummy_sum <- make_afun(
  s_dummy_sum,
  .formats = c(sum = "xx.xx")
)

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
dummy_sum <- function(lyt,
                      vars,
                      ...,
                      show_labels = "hidden",
                      .stats = NULL,
                      .formats = NULL,
                      .labels = NULL,
                      .indent_mods = NULL) {
  afun <- make_afun(
    a_dummy_sum,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )
  analyze(
    lyt,
    vars,
    afun = afun,
    extra_args = list(...),
    show_labels = show_labels
  )
}
