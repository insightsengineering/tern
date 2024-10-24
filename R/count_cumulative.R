#' Cumulative counts of numeric variable by thresholds
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [count_cumulative()] creates a layout element to calculate cumulative counts of values in a
#' numeric variable that are less than, less or equal to, greater than, or greater or equal to user-specified
#' threshold values.
#'
#' This function analyzes numeric variable `vars` against the threshold values supplied to the `thresholds`
#' argument as a numeric vector. Whether counts should include the threshold values, and whether to count
#' values lower or higher than the threshold values can be set via the `include_eq` and `lower_tail`
#' parameters, respectively.
#'
#' @inheritParams h_count_cumulative
#' @inheritParams argument_convention
#' @param thresholds (`numeric`)\cr vector of cutoff values for the counts.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("count_cumulative"))``
#'
#' @seealso Relevant helper function [h_count_cumulative()], and descriptive function [d_count_cumulative()].
#'
#' @name count_cumulative
#' @order 1
NULL

#' Helper function for `s_count_cumulative()`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper function to calculate count and fraction of `x` values in the lower or upper tail given a threshold.
#'
#' @inheritParams argument_convention
#' @param threshold (`numeric(1)`)\cr a cutoff value as threshold to count values of `x`.
#' @param lower_tail (`flag`)\cr whether to count lower tail, default is `TRUE`.
#' @param include_eq (`flag`)\cr whether to include value equal to the `threshold` in
#'   count, default is `TRUE`.
#'
#' @return A named vector with items:
#'   * `count`: the count of values less than, less or equal to, greater than, or greater or equal to a threshold
#'     of user specification.
#'   * `fraction`: the fraction of the count.
#'
#' @seealso [count_cumulative]
#'
#' @examples
#' set.seed(1, kind = "Mersenne-Twister")
#' x <- c(sample(1:10, 10), NA)
#' .N_col <- length(x)
#'
#' h_count_cumulative(x, 5, .N_col = .N_col)
#' h_count_cumulative(x, 5, lower_tail = FALSE, include_eq = FALSE, na.rm = FALSE, .N_col = .N_col)
#' h_count_cumulative(x, 0, lower_tail = FALSE, .N_col = .N_col)
#' h_count_cumulative(x, 100, lower_tail = FALSE, .N_col = .N_col)
#'
#' @export
h_count_cumulative <- function(x,
                               threshold,
                               lower_tail = TRUE,
                               include_eq = TRUE,
                               na.rm = TRUE, # nolint
                               .N_col) { # nolint
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(threshold)
  checkmate::assert_numeric(.N_col)
  checkmate::assert_flag(lower_tail)
  checkmate::assert_flag(include_eq)
  checkmate::assert_flag(na.rm)

  is_keep <- if (na.rm) !is.na(x) else rep(TRUE, length(x))
  count <- if (lower_tail && include_eq) {
    length(x[is_keep & x <= threshold])
  } else if (lower_tail && !include_eq) {
    length(x[is_keep & x < threshold])
  } else if (!lower_tail && include_eq) {
    length(x[is_keep & x >= threshold])
  } else if (!lower_tail && !include_eq) {
    length(x[is_keep & x > threshold])
  }

  result <- c(
    count = count,
    fraction = if (count == 0 && .N_col == 0) 0 else count / .N_col
  )
  result
}

#' Description of cumulative count
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a helper function that describes the analysis in [s_count_cumulative()].
#'
#' @inheritParams h_count_cumulative
#'
#' @return Labels for [s_count_cumulative()].
#'
#' @export
d_count_cumulative <- function(threshold, lower_tail = TRUE, include_eq = TRUE) {
  checkmate::assert_numeric(threshold)
  lg <- if (lower_tail) "<" else ">"
  eq <- if (include_eq) "=" else ""
  paste0(lg, eq, " ", threshold)
}

#' @describeIn count_cumulative Statistics function that produces a named list given a numeric vector of thresholds.
#'
#' @return
#' * `s_count_cumulative()` returns a named list of `count_fraction`s: a list with each `thresholds` value as a
#'   component, each component containing a vector for the count and fraction.
#'
#' @keywords internal
s_count_cumulative <- function(x,
                               thresholds,
                               lower_tail = TRUE,
                               include_eq = TRUE,
                               .N_col, # nolint
                               .N_row, # nolint
                               denom = c("N_col", "n", "N_row"),
                               ...) {
  checkmate::assert_numeric(thresholds, min.len = 1, any.missing = FALSE)

  denom <- match.arg(denom) %>%
    switch(
      n = length(x),
      N_row = .N_row,
      N_col = .N_col
    )

  count_fraction_list <- Map(function(thres) {
    result <- h_count_cumulative(x, thres, lower_tail, include_eq, .N_col = denom, ...)
    label <- d_count_cumulative(thres, lower_tail, include_eq)
    formatters::with_label(result, label)
  }, thresholds)

  names(count_fraction_list) <- thresholds
  list(count_fraction = count_fraction_list)
}

#' @describeIn count_cumulative Formatted analysis function which is used as `afun`
#'   in `count_cumulative()`.
#'
#' @return
#' * `a_count_cumulative()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_count_cumulative <- make_afun(
  s_count_cumulative,
  .formats = c(count_fraction = format_count_fraction)
)

#' @describeIn count_cumulative Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_cumulative()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_cumulative()` to the table layout.
#'
#' @examples
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_cumulative(
#'     vars = "AGE",
#'     thresholds = c(40, 60)
#'   ) %>%
#'   build_table(tern_ex_adsl)
#'
#' @export
#' @order 2
count_cumulative <- function(lyt,
                             vars,
                             thresholds,
                             lower_tail = TRUE,
                             include_eq = TRUE,
                             var_labels = vars,
                             show_labels = "visible",
                             na_str = default_na_str(),
                             nested = TRUE,
                             ...,
                             table_names = vars,
                             .stats = NULL,
                             .formats = NULL,
                             .labels = NULL,
                             .indent_mods = NULL) {
  extra_args <- list(thresholds = thresholds, lower_tail = lower_tail, include_eq = include_eq, ...)

  afun <- make_afun(
    a_count_cumulative,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .ungroup_stats = "count_fraction"
  )
  analyze(
    lyt,
    vars,
    afun = afun,
    na_str = na_str,
    table_names = table_names,
    var_labels = var_labels,
    show_labels = show_labels,
    nested = nested,
    extra_args = extra_args
  )
}
