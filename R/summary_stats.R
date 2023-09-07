#' Summary Statistic Settings Functions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Functions to retrieve default settings for summary statistics and customize these settings.
#'
#' @param type (`character`)\cr choice of summary data type. Only `counts` and `numeric` types are currently supported.
#' @param include_pval (`logical`)\cr whether p-value should be included as a default statistic.
#'
#' @name summary_stats
NULL

#' @describeIn summary_stats Function to retrieve default formats for summary statistics. Returns format patterns for
#'   descriptive statistics which are understood by `rtables`.
#'
#' @return
#' * `summary_formats` returns a named `vector` of default statistic formats for the given data type.
#'
#' @examples
#' summary_formats()
#' summary_formats(type = "counts", include_pval = TRUE)
#'
#' @export
summary_formats <- function(type = "numeric", include_pval = FALSE) {
  get_format_from_stats(get_stats("analyze_vars", type, add_pval = include_pval))
}

#' @describeIn summary_stats Function to retrieve default labels for summary statistics. Returns labels of descriptive
#'   statistics which are understood by `rtables`.
#'
#' @return
#' * `summary_labels` returns a named `vector` of default statistic labels for the given data type.
#'
#' @examples
#' summary_labels()
#' summary_labels(type = "counts", include_pval = TRUE)
#'
#' @export
summary_labels <- function(type = "numeric", include_pval = FALSE) {
  get_label_from_stats(get_stats("analyze_vars", type, add_pval = include_pval))
}

#' @describeIn summary_stats Function to configure settings for default or custom summary statistics for a given data
#'   type. In addition to selecting a custom subset of statistics, the user can also set custom formats, labels, and
#'   indent modifiers for any of these statistics.
#'
#' @param stats_custom (`named vector` of `character`)\cr vector of statistics to include if not the defaults. This
#'   argument overrides `include_pval` and other custom value arguments such that only settings for these statistics
#'   will be returned.
#' @param formats_custom (`named vector` of `character`)\cr vector of custom statistics formats to use in place of the
#'   defaults defined in [`summary_formats()`]. Names should be a subset of the statistics defined in `stats_custom` (or
#'   default statistics if this is `NULL`).
#' @param labels_custom (`named vector` of `character`)\cr vector of custom statistics labels to use in place of the
#'   defaults defined in [`summary_labels()`]. Names should be a subset of the statistics defined in `stats_custom` (or
#'   default statistics if this is `NULL`).
#' @param indent_mods_custom (`integer` or `named vector` of `integer`)\cr vector of custom indentation modifiers for
#'   statistics to use instead of the default of `0L` for all statistics. Names should be a subset of the statistics
#'   defined in `stats_custom` (or default statistics if this is `NULL`). Alternatively, the same indentation modifier
#'   can be applied to all statistics by setting `indent_mods_custom` to a single integer value.
#'
#' @return
#' * `summary_custom` returns a `list` of 4 named elements: `stats`, `formats`, `labels`, and `indent_mods`.
#'
#' @examples
#' summary_custom()
#' summary_custom(type = "counts", include_pval = TRUE)
#' summary_custom(
#'   include_pval = TRUE, stats_custom = c("n", "mean", "sd", "pval"),
#'   labels_custom = c(sd = "Std. Dev."), indent_mods_custom = 3L
#' )
#'
#' @export
summary_custom <- function(type = "numeric",
                           include_pval = FALSE,
                           stats_custom = NULL,
                           formats_custom = NULL,
                           labels_custom = NULL,
                           indent_mods_custom = NULL) {
  lifecycle::deprecate_warn(
    "0.9.0.9001",
    "summary_custom()",
    details = "Please use `get_stats`, `get_format_from_stats`, and `get_label_from_stats` directly instead."
  )

  .stats <- get_stats("analyze_vars", type, stats_custom, add_pval = include_pval)
  .formats <- get_format_from_stats(.stats, formats_custom)
  .labels <- get_label_from_stats(.stats, labels_custom)
  .indent_mods <- stats::setNames(rep(0L, length(.stats)), .stats)

  if (!is.null(indent_mods_custom)) {
    if (is.null(names(indent_mods_custom)) && length(indent_mods_custom) == 1) {
      .indent_mods[names(.indent_mods)] <- indent_mods_custom
    } else {
      .indent_mods[names(indent_mods_custom)] <- indent_mods_custom
    }
  }

  list(
    stats = .stats,
    formats = .formats,
    labels = .labels,
    indent_mods = .indent_mods[.stats]
  )
}
