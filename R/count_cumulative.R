#' Cumulative Counts with Thresholds
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Summarize cumulative counts of a (`numeric`) vector that is less than, less or equal to,
#' greater than, or greater or equal to user-specific thresholds.
#'
#' @seealso Relevant helper function [h_count_cumulative()], and descriptive function [d_count_cumulative()]
#'
#' @name count_cumulative
NULL

#' Helper Function for [s_count_cumulative()]
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper function to calculate count and fraction of `x` values in the lower or upper tail given a threshold.
#'
#' @inheritParams argument_convention
#' @param threshold (`number`)\cr a cutoff value as threshold to count values of `x`.
#' @param lower_tail (`logical`)\cr whether to count lower tail, default is `TRUE`.
#' @param include_eq (`logical`)\cr whether to include value equal to the `threshold` in
#' count, default is `TRUE`.
#' @param .N_col (`count`)\cr denominator for fraction calculation.
#'
#' @return A named vector with items:
#'   * `count`: the count of values less than, less or equal to, greater than, or greater or equal to a threshold
#'     of user specification.
#'   * `fraction`: the fraction of the count.
#'
#' @seealso [count_cumulative]
#'
#' @export
#'
#' @examples
#' set.seed(1, kind = "Mersenne-Twister")
#' x <- c(sample(1:10, 10), NA)
#' .N_col <- length(x)
#' h_count_cumulative(x, 5, .N_col = .N_col)
#' h_count_cumulative(x, 5, lower_tail = FALSE, include_eq = FALSE, na.rm = FALSE, .N_col = .N_col)
#' h_count_cumulative(x, 0, lower_tail = FALSE, .N_col = .N_col)
#' h_count_cumulative(x, 100, lower_tail = FALSE, .N_col = .N_col)
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

  result <- c(count = count, fraction = count / .N_col)
  result
}

#' Description of Cumulative Count
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
d_count_cumulative <- function(threshold, lower_tail, include_eq) {
  checkmate::assert_numeric(threshold)
  lg <- if (lower_tail) "<" else ">"
  eq <- if (include_eq) "=" else ""
  paste0(lg, eq, " ", threshold)
}

#' @describeIn count_cumulative Statistics function that produces a named list given a numeric vector of thresholds.
#'
#' @inheritParams h_count_cumulative
#' @param thresholds (`numeric`)\cr vector of cutoff value for the counts.
#'
#' @return
#' * `s_count_cumulative()` returns a named list of `count_fraction`s: a list with each `thresholds` value as a
#'   component, each component containing a vector for the count and fraction.
#'
#' @examples
#' # Internal function - s_count_cumulative
#' \dontrun{
#' set.seed(1, kind = "Mersenne-Twister")
#' x <- c(sample(1:10, 10), NA)
#' .N_col <- length(x)
#' s_count_cumulative(x, thresholds = c(0, 5, 11), .N_col = .N_col)
#' s_count_cumulative(x, thresholds = c(0, 5, 11), include_eq = FALSE, na.rm = FALSE, .N_col = .N_col)
#' }
#'
#' @keywords internal
s_count_cumulative <- function(x,
                               thresholds,
                               lower_tail = TRUE,
                               include_eq = TRUE,
                               .N_col, # nolint
                               ...) {
  checkmate::assert_numeric(thresholds, min.len = 1, any.missing = FALSE)

  count_fraction_list <- Map(function(thres) {
    result <- h_count_cumulative(x, thres, lower_tail, include_eq, .N_col = .N_col, ...)
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
#' @examples
#' # Internal function - a_count_cumulative
#' \dontrun{
#' # Use the Formatted Analysis function for `analyze()`. We need to ungroup `count_fraction` first
#' # so that the `rtables` formatting function `format_count_fraction()` can be applied correctly.
#' afun <- make_afun(a_count_cumulative, .ungroup_stats = "count_fraction")
#' afun(x, thresholds = c(0, 5, 11), .N_col = .N_col)
#' }
#'
#' @keywords internal
a_count_cumulative <- make_afun(
  s_count_cumulative,
  .formats = c(count_fraction = format_count_fraction)
)

#' @describeIn count_cumulative Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @inheritParams argument_convention
#'
#' @return
#' * `count_cumulative()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_cumulative()` to the table layout.
#'
#' @export
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
count_cumulative <- function(lyt,
                             vars,
                             var_labels = vars,
                             show_labels = "visible",
                             ...,
                             table_names = vars,
                             .stats = NULL,
                             .formats = NULL,
                             .labels = NULL,
                             .indent_mods = NULL) {
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
    table_names = table_names,
    var_labels = var_labels,
    show_labels = show_labels,
    extra_args = list(...)
  )
}
