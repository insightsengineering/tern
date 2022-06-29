#' Counting Specific Values
#'
#' @description `r lifecycle::badge("stable")`
#'
#' We can count the occurrence of specific values in a variable of interest.
#'
#' @order 1
#'
#' @name count_values_funs
NULL

#' @describeIn count_values_funs Statistics Function which is a generic function to count values.
#'
#' @inheritParams argument_convention
#' @inheritParams s_summary.logical
#' @param values (`character`)\cr specific values that should be counted.
#' @return See [s_summary.logical()] for the returned statistics, as this is used inside.
#' @order 2
#'
#' @export
s_count_values <- function(x,
                           values,
                           na.rm = TRUE, # nolint
                           .N_col, # nolint
                           .N_row, # nolint
                           denom = c("n", "N_row", "N_col")) {
  UseMethod("s_count_values", x)
}

#' @describeIn count_values_funs Method for `character` vectors `x`.
#'
#' @method s_count_values character
#' @order 3
#'
#' @examples
#' # `s_count_values.character`
#' s_count_values(x = c("a", "b", "a"), values = "a")
#' s_count_values(x = c("a", "b", "a", NA, NA), values = "b", na.rm = FALSE)
#'
#' @export
s_count_values.character <- function(x,
                                     values = "Y",
                                     na.rm = TRUE, # nolint
                                     ...) {
  checkmate::assert_character(values)
  if (na.rm) x <- x[!is.na(x)]
  is_in_values <- x %in% values

  checkmate::assert_false(anyNA(is_in_values))

  s_summary(is_in_values, ...)
}

#' @describeIn count_values_funs method for `factor` vectors `x`. This checks whether `values` are all
#'   included in the levels of `x` and fails otherwise. It then proceeds by converting to `character`
#'   and calling `s_count_values.character`.
#'
#' @method s_count_values factor
#' @order 4
#'
#' @examples
#' # `s_count_values.factor`
#' s_count_values(x = factor(c("a", "b", "a")), values = "a")
#'
#' @export
s_count_values.factor <- function(x,
                                  values = "Y",
                                  ...) {
  s_count_values(as.character(x), values = as.character(values), ...)
}

#' @describeIn count_values_funs method for `logical` vectors `x`.
#'
#' @method s_count_values logical
#' @order 5
#'
#' @examples
#' # `s_count_values.logical`
#' s_count_values(x = c(TRUE, FALSE, TRUE))
#'
#' @export
s_count_values.logical <- function(x, values = TRUE, ...) {
  checkmate::assert_logical(values)
  s_count_values(as.character(x), values = as.character(values), ...)
}


#' @describeIn count_values_funs Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#'
#' @order 6
#'
#' @examples
#' # `a_count_values`
#' a_count_values(x = factor(c("a", "b", "a")), values = "a", .N_col = 10, .N_row = 10)
#'
#' @export
a_count_values <- make_afun(
  s_count_values,
  .formats = c(count_fraction = "xx (xx.xx%)", count = "xx")
)

#' @describeIn count_values_funs Analyze Function which adds the counting analysis to
#'   the input layout. Note that additional formatting arguments can be used
#'   here.
#'
#' @inheritParams argument_convention
#' @note Variable labels are shown when there is more than one element in `vars`, otherwise they
#' are hidden.
#' @order 7
#'
#' @examples
#' # `count_values`
#' basic_table() %>%
#'   count_values("Species", values = "setosa") %>%
#'   build_table(iris)
#'
#' @export
count_values <- function(lyt,
                         vars,
                         values,
                         ...,
                         table_names = vars,
                         .stats = "count_fraction",
                         .formats = NULL,
                         .labels = c(count_fraction = paste(values, collapse = ", ")),
                         .indent_mods = NULL) {
  afun <- make_afun(
    a_count_values,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )
  analyze(
    lyt,
    vars,
    afun = afun,
    extra_args = c(list(values = values), list(...)),
    show_labels = ifelse(length(vars) > 1, "visible", "hidden"),
    table_names = table_names
  )
}
