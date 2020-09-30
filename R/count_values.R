#' Counting Specific Values
#'
#' We can count the occurrence of specific values in a variable of interest.
#'
#' @template formatting_arguments
#'
#' @name count_values_funs
#' @order 1
#'
NULL

#' @describeIn count_values_funs Statistics Function which is a generic function to count values.
#' @inheritParams argument_convention
#' @inheritParams s_summary.logical
#' @param values (`character`)\cr specific values that should be counted.
#' @return See [s_summary.logical()] for the returned statistics, as this is used inside.
#'
#' @export
#' @order 2
#'
s_count_values <- function(x,
                           values,
                           na.rm = TRUE, #nolint
                           .N_col, #nolint
                           .N_row, #nolint
                           denom = c("n", "N_row", "N_col")) {
  UseMethod("s_count_values", x)
}

#' @describeIn count_values_funs Method for `character` vectors `x`.
#' @method s_count_values character
#'
#' @export
#' @order 3
#'
#' @examples
#'
#' # `s_count_values.character`
#' s_count_values(x = c("a", "b", "a"), values = "a")
#' s_count_values(x = c("a", "b", "a", NA, NA), values = "b", na.rm = FALSE)
#'
s_count_values.character <- function(x,
                                     values = "Y",
                                     na.rm = TRUE, #nolint
                                     ...) {
  assert_that(
    is.character(values)
  )
  if (na.rm) x <- x[!is.na(x)]
  is_in_values <- x %in% values
  assert_that(
    noNA(is_in_values)
  )
  s_summary(is_in_values, ...)
}

#' @describeIn count_values_funs method for `factor` vectors `x`. This checks whether `values` are all
#'   included in the levels of `x` and fails otherwise. It then proceeds by converting to `character`
#'   and calling `s_count_values.character`.
#' @method s_count_values factor
#'
#' @export
#' @order 4
#'
#' @examples
#'
#' # `s_count_values.factor`
#' s_count_values(x = factor(c("a", "b", "a")), values = "a")
#'
s_count_values.factor <- function(x,
                                  values = "Y",
                                  ...) {
  assert_that(
    is.character(values),
    all(values %in% levels(x))
  )
  s_count_values(as.character(x), values = values, ...)
}

#' @describeIn count_values_funs Analyze Function which adds the counting analysis to
#'   the input layout. Note that additional formatting arguments can be used
#'   here.
#' @inheritParams argument_convention
#'
#' @note Variable labels are shown when there is more than one element in `vars`, otherwise they
#' are hidden.
#'
#' @export
#' @order 5
#'
#' @examples
#'
#' # `count_values`
#' basic_table() %>%
#'   count_values("Species", values = "setosa") %>%
#'   build_table(iris)
#'
count_values <- function(lyt,
                         vars,
                         values,
                         ...,
                         .stats = "count_fraction",
                         .labels = c(count_fraction = paste(values, collapse = ", "))) {
  afun <- format_wrap_x(
    s_count_values,
    indent_mods = c(count_fraction = 0L),
    formats = c(count_fraction = "xx (xx.xx%)")
  )
  analyze(
    lyt,
    vars,
    afun = afun,
    extra_args = c(
      list(
        values = values,
        .stats = .stats,
        .labels = .labels
      ),
      list(...)
    ),
    show_labels = ifelse(length(vars) > 1, "visible", "hidden")
  )
}
