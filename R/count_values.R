#' Counting Specific Values
#'
#' @description `r lifecycle::badge("stable")`
#'
#' We can count the occurrence of specific values in a variable of interest.
#'
#' @inheritParams argument_convention
#' @param values (`character`)\cr specific values that should be counted.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("count_values")`
#'   to see available statistics for this function.
#'
#' @note
#' * For `factor` variables, `s_count_values` checks whether `values` are all included in the levels of `x`
#'   and fails otherwise.
#' * For `count_values()`, variable labels are shown when there is more than one element in `vars`,
#'   otherwise they are hidden.
#'
#' @name count_values_funs
#' @order 1
NULL

#' @describeIn count_values_funs S3 generic function to count values.
#'
#' @inheritParams s_summary.logical
#'
#' @return
#' * `s_count_values()` returns output of [s_summary()] for specified values of a non-numeric variable.
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

#' @describeIn count_values_funs Method for `character` class.
#'
#' @method s_count_values character
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

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  is_in_values <- x %in% values

  s_summary(is_in_values, ...)
}

#' @describeIn count_values_funs Method for `factor` class. This makes an automatic
#'   conversion to `character` and then forwards to the method for characters.
#'
#' @method s_count_values factor
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

#' @describeIn count_values_funs Method for `logical` class.
#'
#' @method s_count_values logical
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

#' @describeIn count_values_funs Formatted analysis function which is used as `afun`
#'   in `count_values()`.
#'
#' @return
#' * `a_count_values()` returns the corresponding list with formatted [rtables::CellValue()].
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

#' @describeIn count_values_funs Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_values()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_values()` to the table layout.
#'
#' @examples
#' # `count_values`
#' basic_table() %>%
#'   count_values("Species", values = "setosa") %>%
#'   build_table(iris)
#'
#' @export
#' @order 2
count_values <- function(lyt,
                         vars,
                         values,
                         na_str = default_na_str(),
                         nested = TRUE,
                         ...,
                         table_names = vars,
                         .stats = "count_fraction",
                         .formats = NULL,
                         .labels = c(count_fraction = paste(values, collapse = ", ")),
                         .indent_mods = NULL) {
  extra_args <- list(values = values, ...)

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
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    show_labels = ifelse(length(vars) > 1, "visible", "hidden"),
    table_names = table_names
  )
}
