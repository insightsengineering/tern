#' Compare Variables Between Groups
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Comparison with a reference group for different `x` objects.
#'
#' @inheritParams argument_convention
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
#' @seealso Relevant constructor function [create_afun_compare()], and [s_summary()] which is used internally
#'   to compute a summary within `s_compare()`.
#'
#' @name compare_variables
#' @include summarize_variables.R
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

  checkmate::assert_factor(x, levels = levels(.ref_group), min.levels = 2)

  y$pval <- if (!.in_ref_col && length(x) > 0 && length(.ref_group) > 0) {
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
  x <- as_factor_keep_attributes(x, x_name = .var, verbose = verbose)
  .ref_group <- as_factor_keep_attributes(.ref_group, x_name = .var, verbose = verbose)
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

  y$pval <- if (!.in_ref_col && length(x) > 0 && length(.ref_group) > 0) {
    x <- factor(x, levels = c(TRUE, FALSE))
    .ref_group <- factor(.ref_group, levels = c(TRUE, FALSE))
    tbl <- rbind(table(x), table(.ref_group))
    suppressWarnings(prop_chisq(tbl))
  } else {
    character()
  }

  y
}

.a_compare_numeric_formats <- c(.a_summary_numeric_formats, pval = "x.xxxx | (<0.0001)")
.a_compare_numeric_labels <- c(.a_summary_numeric_labels, pval = "p-value (t-test)")
.a_compare_numeric_indent_mods <- c(.a_summary_numeric_indent_mods, pval = 0L)
.a_compare_counts_formats <- c(.a_summary_counts_formats, pval = "x.xxxx | (<0.0001)")
.a_compare_counts_labels <- c(.a_summary_counts_labels, pval = "p-value (chi-squared test)")
.a_compare_counts_indent_mods <- c(.a_summary_counts_indent_mods, pval = 0L)

#' @describeIn compare_variables Formatted analysis function which is used as `afun`
#'   in `compare_vars()`.
#'
#' @return
#' * `a_compare()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' a_compare(rnorm(10, 5, 1), .ref_group = rnorm(20, -5, 1), .in_ref_col = FALSE, .var = "bla")
#' a_compare(factor(c("a", "a", "b", "c", "a")), .ref_group = factor(c("a", "a", "b", "c")), .in_ref_col = FALSE)
#' a_compare(c("A", "B", "A", "C"), .ref_group = c("B", "A", "C"), .in_ref_col = FALSE, .var = "x", verbose = FALSE)
#' a_compare(c(TRUE, FALSE, FALSE, TRUE, TRUE), .ref_group = c(TRUE, FALSE), .in_ref_col = FALSE)
#'
#' @export
a_compare <- function(x,
                      .N_col,
                      .N_row,
                      .var,
                      .df_row,
                      .ref_group,
                      .in_ref_col,
                      .stats = NULL,
                      .formats = NULL,
                      .labels = NULL,
                      .indent_mods = NULL,
                      na.rm = TRUE,
                      na_level = NA_character_,
                      ...) {
  if (is.null(.stats)) .stats <- names(if (is.numeric(x)) .a_compare_numeric_labels else .a_compare_counts_labels)
  if (is.null(.formats)) .formats <- if (is.numeric(x)) .a_compare_numeric_formats else .a_compare_counts_formats
  if (is.null(.labels)) .labels <- if (is.numeric(x)) .a_compare_numeric_labels else .a_compare_counts_labels
  if (is.null(.indent_mods)) {
    .indent_mods <- if (is.numeric(x)) .a_compare_numeric_indent_mods else .a_compare_counts_indent_mods
  }
  if (length(.indent_mods) == 1 & is.null(names(.indent_mods))) {
    .indent_mods <- rep(.indent_mods, length(.stats)) %>% `names<-`(.stats)
  }
  if (any(is.na(.df_row[[.var]])) && !any(is.na(x)) && !na.rm) levels(x) <- c(levels(x), "na-level")
  x_stats <- s_compare(x = x, .N_col = .N_col, .N_row = .N_row, na.rm = na.rm, .ref_group = .ref_group, .in_ref_col = .in_ref_col, ...)
  if (is.numeric(x)) {
    .labels[c("mean_ci", "mean_pval", "median_ci", "quantiles")] <- sapply(
      c("mean_ci", "mean_pval", "median_ci", "quantiles"),
      function(x) attr(x_stats[[x]], "label")
    )
  }
  .stats <- intersect(.stats, names(x_stats))
  x_stats <- x_stats[.stats]
  if (!is.numeric(x) && !is.logical(x)) {
    x_ungrp <- ungroup_stats(x_stats, .stats, .formats, .labels, .indent_mods, .in_ref_col)
    x_stats <- x_ungrp[["x"]]
    .stats <- x_ungrp[[".stats"]]
    .formats <- x_ungrp[[".formats"]]
    .labels <- x_ungrp[[".labels"]]
    .indent_mods <- x_ungrp[[".indent_mods"]]
  }
  .formats_x <- extract_by_name(
    .formats, .stats, if (is.numeric(x)) .a_compare_numeric_formats else .a_compare_counts_formats
  )
  .labels_x <- extract_by_name(.labels, .stats, if (is.numeric(x)) .a_compare_numeric_labels else .a_compare_counts_labels)
  .indent_mods_x <- extract_by_name(
    .indent_mods, .stats, if (is.numeric(x)) .a_compare_numeric_indent_mods else .a_compare_counts_indent_mods
  )

  in_rows(
    .list = x_stats,
    .formats = .formats_x,
    .names = .labels_x,
    .labels = .labels_x,
    .indent_mods = .indent_mods_x,
    .format_na_strs = na_level
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
#' @note This function has been deprecated in favor of direct implementation of `a_compare()`.
#'
#' @seealso [compare_vars()]
#'
#' @export
create_afun_compare <- function(.stats = NULL,
                                .formats = NULL,
                                .labels = NULL,
                                .indent_mods = NULL) {
  lifecycle::deprecate_stop(
    "0.8.2",
    "create_afun_compare()",
    "a_compare()"
  )
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
compare_vars <- function(lyt,
                         vars,
                         var_labels = vars,
                         nested = TRUE,
                         ...,
                         na.rm = TRUE,
                         na_level = NA_character_,
                         show_labels = "default",
                         table_names = vars,
                         section_div = NA_character_,
                         .stats = c("n", "mean_sd", "count_fraction", "pval"),
                         .formats = NULL,
                         .labels = NULL,
                         .indent_mods = NULL) {
  analyze(
    lyt = lyt,
    vars = vars,
    var_labels = var_labels,
    afun = a_compare,
    nested = nested,
    extra_args = list(
      .stats = .stats, .formats = .formats, .labels = .labels, .indent_mods = .indent_mods, na.rm = na.rm, na_level = na_level, ...
    ),
    inclNAs = TRUE,
    show_labels = show_labels,
    table_names = table_names,
    section_div = section_div
  )
}
