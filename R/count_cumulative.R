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
#'   Options are: ``r shQuote(get_stats("count_cumulative"), type = "sh")``
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
#' h_count_cumulative(x, 5, denom = .N_col)
#' h_count_cumulative(x, 5, lower_tail = FALSE, include_eq = FALSE, na_rm = FALSE, denom = .N_col)
#' h_count_cumulative(x, 0, lower_tail = FALSE, denom = .N_col)
#' h_count_cumulative(x, 100, lower_tail = FALSE, denom = .N_col)
#'
#' @export
h_count_cumulative <- function(x,
                               threshold,
                               lower_tail = TRUE,
                               include_eq = TRUE,
                               na_rm = TRUE,
                               denom) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(threshold)
  checkmate::assert_numeric(denom)
  checkmate::assert_flag(lower_tail)
  checkmate::assert_flag(include_eq)
  checkmate::assert_flag(na_rm)

  is_keep <- if (na_rm) !is.na(x) else rep(TRUE, length(x))
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
    fraction = if (count == 0 && denom == 0) 0 else count / denom
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
                               denom = c("N_col", "n", "N_row"),
                               .N_col, # nolint
                               .N_row, # nolint
                               na_rm = TRUE,
                               ...) {
  checkmate::assert_numeric(thresholds, min.len = 1, any.missing = FALSE)

  denom <- match.arg(denom) %>%
    switch(
      n = length(x),
      N_row = .N_row,
      N_col = .N_col
    )

  count_fraction_list <- Map(function(thres) {
    result <- h_count_cumulative(x, thres, lower_tail, include_eq, na_rm = na_rm, denom = denom)
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
a_count_cumulative <- function(x,
                               ...,
                               .stats = NULL,
                               .stat_names = NULL,
                               .formats = NULL,
                               .labels = NULL,
                               .indent_mods = NULL) {
  dots_extra_args <- list(...)

  # Check if there are user-defined functions
  default_and_custom_stats_list <- .split_std_from_custom_stats(.stats)
  .stats <- default_and_custom_stats_list$all_stats
  custom_stat_functions <- default_and_custom_stats_list$custom_stats

  # Adding automatically extra parameters to the statistic function (see ?rtables::additional_fun_params)
  extra_afun_params <- retrieve_extra_afun_params(
    names(dots_extra_args$.additional_fun_parameters)
  )
  dots_extra_args$.additional_fun_parameters <- NULL # After extraction we do not need them anymore

  # Main statistical functions application
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_count_cumulative,
    custom_stat_fnc_list = custom_stat_functions,
    args_list = c(
      x = list(x),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in with stats defaults if needed
  .stats <- get_stats("count_cumulative",
    stats_in = .stats,
    custom_stats_in = names(custom_stat_functions)
  )

  x_stats <- x_stats[.stats]
  levels_per_stats <- lapply(x_stats, names)

  # Fill in formats/indents/labels with custom input and defaults
  .formats <- get_formats_from_stats(.stats, .formats, levels_per_stats)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods, levels_per_stats)
  .labels <- get_labels_from_stats(
    .stats, .labels, levels_per_stats,
    label_attr_from_stats = sapply(.unlist_keep_nulls(x_stats), attr, "label")
  )

  # Unlist stats
  x_stats <- x_stats %>%
    .unlist_keep_nulls() %>%
    setNames(names(.formats))

  # Auto format handling
  .formats <- apply_auto_formatting(
    .formats,
    x_stats,
    extra_afun_params$.df_row,
    extra_afun_params$.var
  )

  # Get and check statistical names from defaults
  .stat_names <- get_stat_names(x_stats, .stat_names) # note is x_stats

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = names(.labels),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

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
                             table_names = vars,
                             ...,
                             na_rm = TRUE,
                             .stats = c("count_fraction"),
                             .stat_names = NULL,
                             .formats = NULL,
                             .labels = NULL,
                             .indent_mods = NULL) {
  # Depending on main functions
  extra_args <- list(
    "na_rm" = na_rm,
    "thresholds" = thresholds,
    "lower_tail" = lower_tail,
    "include_eq" = include_eq,
    ...
  )

  # Needed defaults
  if (!is.null(.stats)) extra_args[[".stats"]] <- .stats
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Adding all additional information from layout to analysis functions (see ?rtables::additional_fun_params)
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_count_cumulative) <- c(
    formals(a_count_cumulative),
    extra_args[[".additional_fun_parameters"]]
  )

  # Main {rtables} structural call
  analyze(
    lyt,
    vars,
    afun = a_count_cumulative,
    na_str = na_str,
    inclNAs = !na_rm,
    table_names = table_names,
    var_labels = var_labels,
    show_labels = show_labels,
    nested = nested,
    extra_args = extra_args
  )
}
