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
#' * For character variables, automatic conversion to factor does not guarantee that the table
#'   will be generated correctly. In particular for sparse tables this very likely can fail.
#'   Therefore it is always better to manually convert character variables to factors during pre-processing.
#' * For `compare_vars()`, the column split must define a reference group via `ref_group` so that the comparison
#'   is well defined.
#' * When factor variables contains `NA`, it is expected that `NA` values have been conveyed to `na_level`
#'   appropriately beforehand via [df_explicit_na()].
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
#'
s_compare <- function(x,
                      .ref_group,
                      .in_ref_col,
                      ...) {
  UseMethod("s_compare", x)
}

#' @describeIn compare_variables Method for `numeric` class. This uses the standard t-test
#'   to calculate the p-value.
#' @method s_compare numeric
#'
#' @export
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
#' @param denom (`string`)\cr choice of denominator for factor proportions,
#'   can only be `n` (number of values in this row and column intersection).
#' @method s_compare factor
#'
#' @export
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
s_compare.factor <- function(x,
                             .ref_group,
                             .in_ref_col,
                             denom = "n",
                             na.rm = TRUE, # nolint
                             na_level = "<Missing>",
                             ...) {
  checkmate::assert_flag(.in_ref_col)
  assert_valid_factor(x, any.missing = FALSE)
  assert_valid_factor(.ref_group, any.missing = FALSE)
  denom <- match.arg(denom)

  y <- s_summary.factor(
    x = x,
    denom = denom,
    na.rm = na.rm,
    na_level = na_level,
    ...
  )

  if (na.rm) {
    x <- fct_discard(x, na_level)
    .ref_group <- fct_discard(.ref_group, na_level)
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
#' @param verbose (`logical`)\cr Whether warnings and messages should be printed. Mainly used
#'   to print out information about factor casting. Defaults to `TRUE`.
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
                                na_level = "<Missing>",
                                .var,
                                verbose = TRUE,
                                ...) {
  x <- as_factor_keep_attributes(x, x_name = .var, na_level = na_level, verbose = verbose)
  .ref_group <- as_factor_keep_attributes(.ref_group, x_name = .var, na_level = na_level, verbose = verbose)
  s_compare(
    x = x,
    .ref_group = .ref_group,
    .in_ref_col = .in_ref_col,
    denom = denom,
    na.rm = na.rm,
    na_level = na_level,
    ...
  )
}

#' @describeIn compare_variables Method for `logical` class. A chi-squared test
#'   is used. If missing values are not removed, then they are counted as `FALSE`.
#' @method s_compare logical
#'
#' @export
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

#' @describeIn compare_variables Formatted analysis function which is used as `afun`
#'   in `compare_vars()`.
#'
#' @return
#' * `a_compare()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @export
#'
a_compare <- function(x,
                      .ref_group,
                      .in_ref_col,
                      ...,
                      .var) {
  UseMethod("a_compare", x)
}

#' @describeIn compare_variables Formatted analysis function method for `numeric` class.
#' @export
#'
#' @examples
#' # `a_compare.numeric`
#' a_compare(
#'   rnorm(10, 5, 1),
#'   .ref_group = rnorm(20, -5, 1),
#'   .in_ref_col = FALSE,
#'   .var = "bla"
#' )
a_compare.numeric <- make_afun(
  s_compare.numeric,
  .formats = c(
    .a_summary_numeric_formats,
    pval = "x.xxxx | (<0.0001)"
  ),
  .labels = c(
    .a_summary_numeric_labels,
    pval = "p-value (t-test)"
  ),
  .null_ref_cells = FALSE
)

.a_compare_counts_formats <- c(
  .a_summary_counts_formats,
  pval = "x.xxxx | (<0.0001)"
)

.a_compare_counts_labels <- c(
  pval = "p-value (chi-squared test)"
)

#' @describeIn compare_variables Formatted analysis function method for `factor` class.
#' @export
#'
#' @examples
#' # `a_compare.factor`
#' # We need to ungroup `count` and `count_fraction` first so that the `rtables` formatting
#' # functions can be applied correctly.
#' afun <- make_afun(
#'   getS3method("a_compare", "factor"),
#'   .ungroup_stats = c("count", "count_fraction")
#' )
#' x <- factor(c("a", "a", "b", "c", "a"))
#' y <- factor(c("a", "a", "b", "c"))
#' afun(x, .ref_group = y, .in_ref_col = FALSE)
a_compare.factor <- make_afun(
  s_compare.factor,
  .formats = .a_compare_counts_formats,
  .labels = .a_compare_counts_labels,
  .null_ref_cells = FALSE
)

#' @describeIn compare_variables Formatted analysis function method for `character` class.
#' @export
#'
#' @examples
#' # `a_compare.character`
#' afun <- make_afun(
#'   getS3method("a_compare", "character"),
#'   .ungroup_stats = c("count", "count_fraction")
#' )
#' x <- c("A", "B", "A", "C")
#' y <- c("B", "A", "C")
#' afun(x, .ref_group = y, .in_ref_col = FALSE, .var = "x", verbose = FALSE)
a_compare.character <- make_afun(
  s_compare.character,
  .formats = .a_compare_counts_formats,
  .labels = .a_compare_counts_labels,
  .null_ref_cells = FALSE
)

#' @describeIn compare_variables Formatted analysis function method for `logical` class.
#' @export
#'
#' @examples
#' # `a_compare.logical`
#' afun <- make_afun(
#'   getS3method("a_compare", "logical")
#' )
#' x <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
#' y <- c(TRUE, FALSE)
#' afun(x, .ref_group = y, .in_ref_col = FALSE)
a_compare.logical <- make_afun(
  s_compare.logical,
  .formats = .a_compare_counts_formats,
  .labels = .a_compare_counts_labels,
  .null_ref_cells = FALSE
)

#' Constructor Function for [compare_vars()]
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Constructor function which creates a combined formatted analysis function.
#'
#' @inheritParams argument_convention
#'
#' @return Combined formatted analysis function for use in [compare_vars()].
#'
#' @note Since [a_compare()] is generic and we want customization of the formatting arguments
#'   via [rtables::make_afun()], we need to create another temporary generic function, with
#'   corresponding customized methods. Then in order for the methods to be found,
#'   we need to wrap them in a combined `afun`. Since this is required by two layout creating
#'   functions (and possibly others in the future), we provide a constructor that does this:
#'   [create_afun_compare()].
#'
#' @export
#' @seealso [compare_vars()]
#'
#' @examples
#' # `create_afun_compare()` to create combined `afun`
#'
#' afun <- create_afun_compare(
#'   .stats = c("n", "count_fraction", "mean_sd", "pval"),
#'   .indent_mods = c(pval = 1L)
#' )
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARMCD", ref_group = "ARM A") %>%
#'   analyze(
#'     "AGE",
#'     afun = afun,
#'     show_labels = "visible"
#'   )
#' build_table(lyt, df = tern_ex_adsl)
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARMCD", ref_group = "ARM A") %>%
#'   analyze(
#'     "SEX",
#'     afun = afun,
#'     show_labels = "visible"
#'   )
#' build_table(lyt, df = tern_ex_adsl)
create_afun_compare <- function(.stats = NULL,
                                .formats = NULL,
                                .labels = NULL,
                                .indent_mods = NULL) {
  function(x,
           .ref_group,
           .in_ref_col,
           ...,
           .var) {
    afun <- function(x, ...) {
      UseMethod("afun", x)
    }

    numeric_stats <- afun_selected_stats(
      .stats,
      all_stats = c(names(.a_summary_numeric_formats), "pval")
    )
    afun.numeric <- make_afun( # nolint
      a_compare.numeric,
      .stats = numeric_stats,
      .formats = extract_by_name(.formats, numeric_stats),
      .labels = extract_by_name(.labels, numeric_stats),
      .indent_mods = extract_by_name(.indent_mods, numeric_stats),
      .null_ref_cells = FALSE
    )

    factor_stats <- afun_selected_stats(
      .stats,
      all_stats = names(.a_compare_counts_formats)
    )
    ungroup_stats <- afun_selected_stats(.stats, c("count", "count_fraction"))
    afun.factor <- make_afun( # nolint
      a_compare.factor,
      .stats = factor_stats,
      .formats = extract_by_name(.formats, factor_stats),
      .labels = extract_by_name(.labels, factor_stats),
      .indent_mods = extract_by_name(.indent_mods, factor_stats),
      .ungroup_stats = ungroup_stats,
      .null_ref_cells = FALSE
    )

    afun.character <- make_afun( # nolint
      a_compare.character,
      .stats = factor_stats,
      .formats = extract_by_name(.formats, factor_stats),
      .labels = extract_by_name(.labels, factor_stats),
      .indent_mods = extract_by_name(.indent_mods, factor_stats),
      .ungroup_stats = ungroup_stats,
      .null_ref_cells = FALSE
    )

    afun.logical <- make_afun( # nolint
      a_compare.logical,
      .stats = factor_stats,
      .formats = extract_by_name(.formats, factor_stats),
      .labels = extract_by_name(.labels, factor_stats),
      .indent_mods = extract_by_name(.indent_mods, factor_stats),
      .null_ref_cells = FALSE
    )

    afun(
      x = x,
      .ref_group = .ref_group,
      .in_ref_col = .in_ref_col,
      ...,
      .var = .var
    )
  }
}

#' @describeIn compare_variables Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @param ... arguments passed to `s_compare()`.
#'
#' @return
#' * `compare_vars()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_compare()` to the table layout.
#'
#' @export
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
compare_vars <- function(lyt,
                         vars,
                         var_labels = vars,
                         nested = TRUE,
                         ...,
                         show_labels = "default",
                         table_names = vars,
                         .stats = c("n", "mean_sd", "count_fraction", "pval"),
                         .formats = NULL,
                         .labels = NULL,
                         .indent_mods = NULL) {
  afun <- create_afun_compare(.stats, .formats, .labels, .indent_mods)

  analyze(
    lyt = lyt,
    vars = vars,
    var_labels = var_labels,
    afun = afun,
    nested = nested,
    extra_args = list(...),
    inclNAs = TRUE,
    show_labels = show_labels,
    table_names = table_names
  )
}
