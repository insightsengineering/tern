#' Compare Variables Between Groups
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Comparison with a reference group for different `x` objects.
#'
#' @inheritParams argument_convention
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("analyze_vars_numeric")` to see
#'   statistics available for numeric variables, and `get_stats("analyze_vars_counts")` for statistics available
#'   for non-numeric variables.
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
#' @seealso Relevant constructor function [create_afun_compare()], [s_summary()] which is used internally
#'   to compute a summary within `s_compare()`, and [a_compare()] which is used (with `compare = TRUE`) as the analysis
#'   function for `compare_vars()`.
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
                      .ref_group,
                      .in_ref_col,
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
s_compare.numeric <- function(x,
                              .ref_group,
                              .in_ref_col,
                              ...) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(.ref_group)
  checkmate::assert_flag(.in_ref_col)

  y <- s_summary.numeric(x = x, ...)

  y$pval <- if (!.in_ref_col && n_available(x) > 1 && n_available(.ref_group) > 1) {
    stats::t.test(x, .ref_group)$p.value
  } else {
    character()
  }

  y
}

#' @describeIn compare_variables Method for `factor` class. This uses the chi-squared test
#'   to calculate the p-value.
#'
#' @param denom (`string`)\cr choice of denominator for factor proportions,
#'   can only be `n` (number of values in this row and column intersection).
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
#' s_compare(x = x, .ref_group = y, .in_ref_col = FALSE, na.rm = TRUE)
#' s_compare(x = x, .ref_group = y, .in_ref_col = FALSE, na.rm = FALSE)
#'
#' @export
s_compare.factor <- function(x,
                             .ref_group,
                             .in_ref_col,
                             denom = "n",
                             na.rm = TRUE, # nolint
                             ...) {
  checkmate::assert_flag(.in_ref_col)
  assert_valid_factor(x)
  assert_valid_factor(.ref_group)
  denom <- match.arg(denom)

  y <- s_summary.factor(
    x = x,
    denom = denom,
    na.rm = na.rm,
    ...
  )

  if (na.rm) {
    x <- x[!is.na(x)] %>% fct_discard("<Missing>")
    .ref_group <- .ref_group[!is.na(.ref_group)] %>% fct_discard("<Missing>")
  } else {
    x <- x %>% explicit_na(label = "NA")
    .ref_group <- .ref_group %>% explicit_na(label = "NA")
  }

  if ("NA" %in% levels(x)) levels(.ref_group) <- c(levels(.ref_group), "NA")
  checkmate::assert_factor(x, levels = levels(.ref_group), min.levels = 2)

  y$pval_counts <- if (!.in_ref_col && length(x) > 0 && length(.ref_group) > 0) {
    tab <- rbind(table(x), table(.ref_group))
    res <- suppressWarnings(stats::chisq.test(tab))
    res$p.value
  } else {
    character()
  }

  y
}

#' @describeIn compare_variables Method for `character` class. This makes an automatic
#'   conversion to `factor` (with a warning) and then forwards to the method for factors.
#'
#' @param verbose (`logical`)\cr Whether warnings and messages should be printed. Mainly used
#'   to print out information about factor casting. Defaults to `TRUE`.
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
s_compare.character <- function(x,
                                .ref_group,
                                .in_ref_col,
                                denom = "n",
                                na.rm = TRUE, # nolint
                                .var,
                                verbose = TRUE,
                                ...) {
  x <- as_factor_keep_attributes(x, verbose = verbose)
  .ref_group <- as_factor_keep_attributes(.ref_group, verbose = verbose)
  s_compare(
    x = x,
    .ref_group = .ref_group,
    .in_ref_col = .in_ref_col,
    denom = denom,
    na.rm = na.rm,
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
#' s_compare(x, .ref_group = y, .in_ref_col = FALSE, na.rm = TRUE)
#' s_compare(x, .ref_group = y, .in_ref_col = FALSE, na.rm = FALSE)
#'
#' @export
s_compare.logical <- function(x,
                              .ref_group,
                              .in_ref_col,
                              na.rm = TRUE, # nolint
                              denom = "n",
                              ...) {
  denom <- match.arg(denom)

  y <- s_summary.logical(
    x = x,
    na.rm = na.rm,
    denom = denom,
    ...
  )

  if (na.rm) {
    x <- stats::na.omit(x)
    .ref_group <- stats::na.omit(.ref_group)
  } else {
    x[is.na(x)] <- FALSE
    .ref_group[is.na(.ref_group)] <- FALSE
  }

  y$pval_counts <- if (!.in_ref_col && length(x) > 0 && length(.ref_group) > 0) {
    x <- factor(x, levels = c(TRUE, FALSE))
    .ref_group <- factor(.ref_group, levels = c(TRUE, FALSE))
    tbl <- rbind(table(x), table(.ref_group))
    suppressWarnings(prop_chisq(tbl))
  } else {
    character()
  }

  y
}

#' @describeIn compare_variables Formatted analysis function which is used as `afun`
#'   in `compare_vars()`.
#'
#' @return
#' * `a_compare()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @note `a_compare()` has been deprecated in favor of `a_summary()` with argument `compare` set to `TRUE`.
#'
#' @examples
#' # `a_compare` deprecated - use `a_summary()` instead
#' a_compare(rnorm(10, 5, 1), .ref_group = rnorm(20, -5, 1), .stats = c("n", "pval"))
#'
#' @export
a_compare <- function(x,
                      .N_col, # nolint
                      .N_row, # nolint
                      .var = NULL,
                      .df_row = NULL,
                      .ref_group = NULL,
                      .in_ref_col = FALSE,
                      ...) {
  lifecycle::deprecate_warn(
    "0.8.3",
    "a_compare()",
    details = "Please use a_summary() with argument `compare` set to TRUE instead."
  )
  a_summary(
    x = x,
    .N_col = .N_col,
    .N_row = .N_row,
    .var = .var,
    .df_row = .df_row,
    .ref_group = .ref_group,
    .in_ref_col = .in_ref_col,
    compare = TRUE,
    ...
  )
}

#' Constructor Function for [compare_vars()]
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Constructor function which creates a combined formatted analysis function.
#'
#' @inheritParams argument_convention
#' @param .indent_mods (named `vector` of `integer`)\cr indent modifiers for the labels. Each element of the vector
#'   should be a name-value pair with name corresponding to a statistic specified in `.stats` and value the indentation
#'   for that statistic's row label.
#'
#' @return Combined formatted analysis function for use in [compare_vars()].
#'
#' @note This function has been deprecated in favor of direct implementation of `a_summary()` with argument `compare`
#'   set to `TRUE`.
#'
#' @seealso [compare_vars()]
#'
#' @export
create_afun_compare <- function(.stats = NULL,
                                .formats = NULL,
                                .labels = NULL,
                                .indent_mods = NULL) {
  lifecycle::deprecate_warn(
    "0.8.5.9010",
    "create_afun_compare()",
    details = "Please use a_summary(compare = TRUE) directly instead."
  )
  function(x,
           .ref_group,
           .in_ref_col,
           ...,
           .var) {
    a_summary(x,
      compare = TRUE,
      .stats = .stats,
      .formats = .formats,
      .labels = .labels,
      .indent_mods = .indent_mods,
      .ref_group = .ref_group,
      .in_ref_col = .in_ref_col,
      .var = .var, ...
    )
  }
}

#' @describeIn compare_variables Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @param ... arguments passed to `s_compare()`.
#' @param .indent_mods (named `vector` of `integer`)\cr indent modifiers for the labels. Each element of the vector
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
                         na_level = lifecycle::deprecated(),
                         na_str = default_na_str(),
                         nested = TRUE,
                         ...,
                         na.rm = TRUE, # nolint
                         show_labels = "default",
                         table_names = vars,
                         section_div = NA_character_,
                         .stats = c("n", "mean_sd", "count_fraction", "pval"),
                         .formats = NULL,
                         .labels = NULL,
                         .indent_mods = NULL) {
  if (lifecycle::is_present(na_level)) {
    lifecycle::deprecate_warn("0.9.1", "compare_vars(na_level)", "compare_vars(na_str)")
    na_str <- na_level
  }

  extra_args <- list(.stats = .stats, na.rm = na.rm, na_str = na_str, compare = TRUE, ...)
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  analyze(
    lyt = lyt,
    vars = vars,
    var_labels = var_labels,
    afun = a_summary,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    inclNAs = TRUE,
    show_labels = show_labels,
    table_names = table_names,
    section_div = section_div
  )
}
