#' Compare variables between groups
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [compare_vars()] creates a layout element to summarize and compare one or more variables, using
#' the S3 generic function [s_summary()] to calculate a list of summary statistics. A list of all available statistics
#' for numeric variables can be viewed by running `get_stats("analyze_vars_numeric", add_pval = TRUE)` and for
#' non-numeric variables by running `get_stats("analyze_vars_counts", add_pval = TRUE)`. Use the `.stats` parameter to
#' specify the statistics to include in your output summary table.
#'
#' Prior to using this function in your table layout you must use [rtables::split_cols_by()] to create a column
#' split on the variable to be used in comparisons, and specify a reference group via the `ref_group` parameter.
#' Comparisons can be performed for each group (column) against the specified reference group by including the p-value
#' statistic.
#'
#' @inheritParams argument_convention
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options for numeric variables are: ``r shQuote(get_stats("analyze_vars_numeric", add_pval = TRUE))``
#'
#'   Options for non-numeric variables are: ``r shQuote(get_stats("analyze_vars_counts", add_pval = TRUE))``
#'
#' @note
#' * For factor variables, `denom` for factor proportions can only be `n` since the purpose is to compare proportions
#'   between columns, therefore a row-based proportion would not make sense. Proportion based on `N_col` would
#'   be difficult since we use counts for the chi-squared test statistic, therefore missing values should be accounted
#'   for as explicit factor levels.
#' * If factor variables contain `NA`, these `NA` values are excluded by default. To include `NA` values
#'   set `na.rm = FALSE` and missing values will be displayed as an `NA` level. Alternatively, an explicit
#'   factor level can be defined for `NA` values during pre-processing via [df_explicit_na()] - the
#'   default `na_level` (`"<Missing>"`) will also be excluded when `na.rm` is set to `TRUE`.
#' * For character variables, automatic conversion to factor does not guarantee that the table
#'   will be generated correctly. In particular for sparse tables this very likely can fail.
#'   Therefore it is always better to manually convert character variables to factors during pre-processing.
#' * For `compare_vars()`, the column split must define a reference group via `ref_group` so that the comparison
#'   is well defined.
#'
#' @seealso [s_summary()] which is used internally to compute a summary within `s_compare()`, and [a_summary()]
#'   which is used (with `compare = TRUE`) as the analysis function for `compare_vars()`.
#'
#' @name compare_variables
#' @include analyze_variables.R
#' @order 1
NULL

#' @describeIn compare_variables S3 generic function to produce a comparison summary.
#'
#' @return
#' * `s_compare()` returns output of [s_summary()] and comparisons versus the reference group in the form of p-values.
#'
#' @export
s_compare <- function(x,
                      ...) {
  UseMethod("s_compare", x)
}

#' @describeIn compare_variables Method for `numeric` class. This uses the standard t-test
#'   to calculate the p-value.
#'
#' @method s_compare numeric
#'
#' @examples
#' # `s_compare.numeric`
#'
#' ## Usual case where both this and the reference group vector have more than 1 value.
#' s_compare(rnorm(10, 5, 1), .ref_group = rnorm(5, -5, 1), .in_ref_col = FALSE)
#'
#' ## If one group has not more than 1 value, then p-value is not calculated.
#' s_compare(rnorm(10, 5, 1), .ref_group = 1, .in_ref_col = FALSE)
#'
#' ## Empty numeric does not fail, it returns NA-filled items and no p-value.
#' s_compare(numeric(), .ref_group = numeric(), .in_ref_col = FALSE)
#'
#' @export
s_compare.numeric <- function(x, ...) {
  s_summary.numeric(x = x, compare_with_ref_group = TRUE, ...)
}

#' @describeIn compare_variables Method for `factor` class. This uses the chi-squared test
#'   to calculate the p-value.
#'
#' @method s_compare factor
#'
#' @examples
#' # `s_compare.factor`
#'
#' ## Basic usage:
#' x <- factor(c("a", "a", "b", "c", "a"))
#' y <- factor(c("a", "b", "c"))
#' s_compare(x = x, .ref_group = y, .in_ref_col = FALSE)
#'
#' ## Management of NA values.
#' x <- explicit_na(factor(c("a", "a", "b", "c", "a", NA, NA)))
#' y <- explicit_na(factor(c("a", "b", "c", NA)))
#' s_compare(x = x, .ref_group = y, .in_ref_col = FALSE, na_rm = TRUE)
#' s_compare(x = x, .ref_group = y, .in_ref_col = FALSE, na_rm = FALSE)
#'
#' @export
s_compare.factor <- function(x, ...) {
  s_summary.factor(
    x = x,
    compare_with_ref_group = TRUE,
    ...
  )
}

#' @describeIn compare_variables Method for `character` class. This makes an automatic
#'   conversion to `factor` (with a warning) and then forwards to the method for factors.
#'
#' @method s_compare character
#'
#' @examples
#' # `s_compare.character`
#'
#' ## Basic usage:
#' x <- c("a", "a", "b", "c", "a")
#' y <- c("a", "b", "c")
#' s_compare(x, .ref_group = y, .in_ref_col = FALSE, .var = "x", verbose = FALSE)
#'
#' ## Note that missing values handling can make a large difference:
#' x <- c("a", "a", "b", "c", "a", NA)
#' y <- c("a", "b", "c", rep(NA, 20))
#' s_compare(x,
#'   .ref_group = y, .in_ref_col = FALSE,
#'   .var = "x", verbose = FALSE
#' )
#' s_compare(x,
#'   .ref_group = y, .in_ref_col = FALSE, .var = "x",
#'   na.rm = FALSE, verbose = FALSE
#' )
#'
#' @export
s_compare.character <- function(x, ...) {
  s_summary.character(
    x,
    compare_with_ref_group = TRUE,
    ...
  )
}

#' @describeIn compare_variables Method for `logical` class. A chi-squared test
#'   is used. If missing values are not removed, then they are counted as `FALSE`.
#'
#' @method s_compare logical
#'
#' @examples
#' # `s_compare.logical`
#'
#' ## Basic usage:
#' x <- c(TRUE, FALSE, TRUE, TRUE)
#' y <- c(FALSE, FALSE, TRUE)
#' s_compare(x, .ref_group = y, .in_ref_col = FALSE)
#'
#' ## Management of NA values.
#' x <- c(NA, TRUE, FALSE)
#' y <- c(NA, NA, NA, NA, FALSE)
#' s_compare(x, .ref_group = y, .in_ref_col = FALSE, na_rm = TRUE)
#' s_compare(x, .ref_group = y, .in_ref_col = FALSE, na_rm = FALSE)
#'
#' @export
s_compare.logical <- function(x, ...) {
  s_summary.logical(
    x = x,
    compare_with_ref_group = TRUE,
    ...
  )
}

#' @describeIn compare_variables Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @param ... additional arguments passed to `s_compare()`, including:
#'   * `denom`: (`string`) choice of denominator. Options are `c("n", "N_col", "N_row")`. For factor variables, can
#'     only be `"n"` (number of values in this row and column intersection).
#'   * `.N_row`: (`numeric(1)`) Row-wise N (row group count) for the group of observations being analyzed (i.e. with no
#'     column-based subsetting).
#'   * `.N_col`: (`numeric(1)`) Column-wise N (column count) for the full column being tabulated within.
#'   * `verbose`: (`flag`) Whether additional warnings and messages should be printed. Mainly used to print out
#'     information about factor casting. Defaults to `TRUE`. Used for `character`/`factor` variables only.
#' @param .indent_mods (named `integer`)\cr indent modifiers for the labels. Each element of the vector
#'   should be a name-value pair with name corresponding to a statistic specified in `.stats` and value the indentation
#'   for that statistic's row label.
#'
#' @return
#' * `compare_vars()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_compare()` to the table layout.
#'
#' @examples
#' # `compare_vars()` in `rtables` pipelines
#'
#' ## Default output within a `rtables` pipeline.
#' lyt <- basic_table() %>%
#'   split_cols_by("ARMCD", ref_group = "ARM B") %>%
#'   compare_vars(c("AGE", "SEX"))
#' build_table(lyt, tern_ex_adsl)
#'
#' ## Select and format statistics output.
#' lyt <- basic_table() %>%
#'   split_cols_by("ARMCD", ref_group = "ARM C") %>%
#'   compare_vars(
#'     vars = "AGE",
#'     .stats = c("mean_sd", "pval"),
#'     .formats = c(mean_sd = "xx.x, xx.x"),
#'     .labels = c(mean_sd = "Mean, SD")
#'   )
#' build_table(lyt, df = tern_ex_adsl)
#'
#' @export
#' @order 2
compare_vars <- function(lyt,
                         vars,
                         var_labels = vars,
                         na_str = default_na_str(),
                         nested = TRUE,
                         ...,
                         na_rm = TRUE,
                         show_labels = "default",
                         table_names = vars,
                         section_div = NA_character_,
                         .stats = c("n", "mean_sd", "count_fraction", "pval"),
                         .stat_names_in = NULL,
                         .formats = NULL,
                         .labels = NULL,
                         .indent_mods = NULL) {
  analyze_vars(
    lyt = lyt,
    compare_with_ref_group = TRUE,
    vars = vars,
    var_labels = var_labels,
    na_str = na_str,
    nested = nested,
    na_rm = na_rm,
    show_labels = show_labels,
    table_names = table_names,
    section_div = section_div,
    .stats = .stats,
    .stat_names_in = .stat_names_in,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    ...
  )
}
