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
#' summary_formats(type = "count", include_pval = TRUE)
#'
#' @export
summary_formats <- function(type = "numeric", include_pval = FALSE) {
  fmts <- if (type == "counts") {
    c(
      n = "xx.",
      count = "xx.",
      count_fraction = format_count_fraction,
      n_blq = "xx.",
      pval = "x.xxxx | (<0.0001)"
    )
  } else {
    c(
      n = "xx.",
      sum = "xx.x",
      mean = "xx.x",
      sd = "xx.x",
      se = "xx.x",
      mean_sd = "xx.x (xx.x)",
      mean_se = "xx.x (xx.x)",
      mean_ci = "(xx.xx, xx.xx)",
      mean_sei = "(xx.xx, xx.xx)",
      mean_sdi = "(xx.xx, xx.xx)",
      mean_pval = "xx.xx",
      median = "xx.x",
      mad = "xx.x",
      median_ci = "(xx.xx, xx.xx)",
      quantiles = "xx.x - xx.x",
      iqr = "xx.x",
      range = "xx.x - xx.x",
      min = "xx.x",
      max = "xx.x",
      median_range = "xx.x (xx.x - xx.x)",
      cv = "xx.x",
      geom_mean = "xx.x",
      geom_mean_ci = "(xx.xx, xx.xx)",
      geom_cv = "xx.x",
      pval = "x.xxxx | (<0.0001)"
    )
  }
  if (!include_pval) fmts <- head(fmts, -1)
  fmts
}

#' @describeIn summary_stats Function to retrieve default labels for summary statistics. Returns labels of descriptive
#'   statistics which are understood by `rtables`.
#'
#' @return
#' * `summary_labels` returns a named `vector` of default statistic labels for the given data type.
#'
#' @examples
#' summary_labels()
#' summary_labels(type = "count", include_pval = TRUE)
#'
#' @export
summary_labels <- function(type = "numeric", include_pval = FALSE) {
  lbls <- if (type == "counts") {
    c(
      n = "n",
      count = "count",
      count_fraction = "count_fraction",
      n_blq = "n_blq",
      pval = "p-value (chi-squared test)"
    )
  } else {
    c(
      n = "n",
      sum = "Sum",
      mean = "Mean",
      sd = "SD",
      se = "SE",
      mean_sd = "Mean (SD)",
      mean_se = "Mean (SE)",
      mean_ci = "Mean 95% CI",
      mean_sei = "Mean -/+ 1xSE",
      mean_sdi = "Mean -/+ 1xSD",
      mean_pval = "Mean p-value (H0: mean = 0)",
      median = "Median",
      mad = "Median Absolute Deviation",
      median_ci = "Median 95% CI",
      quantiles = "25% and 75%-ile",
      iqr = "IQR",
      range = "Min - Max",
      min = "Minimum",
      max = "Maximum",
      median_range = "Median (Min - Max)",
      cv = "CV (%)",
      geom_mean = "Geometric Mean",
      geom_mean_ci = "Geometric Mean 95% CI",
      geom_cv = "CV % Geometric Mean",
      pval = "p-value (t-test)"
    )
  }
  if (!include_pval) lbls <- head(lbls, -1)
  lbls
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
  if ("pval" %in% stats_custom) include_pval <- TRUE

  .formats <- summary_formats(type = type, include_pval = include_pval)
  .stats <- if (is.null(stats_custom)) names(.formats) else intersect(stats_custom, names(.formats))
  .labels <- summary_labels(type = type, include_pval = include_pval)
  .indent_mods <- stats::setNames(rep(0L, length(.stats)), .stats)

  if (!is.null(formats_custom)) .formats[names(formats_custom)] <- formats_custom
  if (!is.null(labels_custom)) .labels[names(labels_custom)] <- labels_custom
  if (!is.null(indent_mods_custom)) {
    if (is.null(names(indent_mods_custom)) && length(indent_mods_custom) == 1) {
      .indent_mods[names(.indent_mods)] <- indent_mods_custom
    } else {
      .indent_mods[names(indent_mods_custom)] <- indent_mods_custom
    }
  }

  list(
    stats = .stats,
    formats = .formats[.stats],
    labels = .labels[.stats],
    indent_mods = .indent_mods[.stats]
  )
}

#' Control Function for Descriptive Statistics
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Sets a list of parameters for summaries of descriptive statistics. Typically used internally to specify
#' details for [s_summary()]. This function family is mainly used by [analyze_vars()].
#'
#' @inheritParams argument_convention
#' @param quantiles (`numeric`)\cr of length two to specify the quantiles to calculate.
#' @param quantile_type (`numeric`)\cr between 1 and 9 selecting quantile algorithms to be used.
#'   Default is set to 2 as this matches the default quantile algorithm in SAS `proc univariate` set by `QNTLDEF=5`.
#'   This differs from R's default. See more about `type` in [stats::quantile()].
#' @param test_mean (`numeric`)\cr to test against the mean under the null hypothesis when calculating p-value.
#'
#' @note Deprecation cycle started for `control_summarize_vars` as it is going to renamed into
#'   `control_analyze_vars`. Intention is to reflect better the core underlying `rtables`
#'   functions; in this case [analyze_vars()] wraps [rtables::analyze()].
#'
#' @return A list of components with the same names as the arguments.
#'
#' @export control_analyze_vars control_summarize_vars
#' @aliases control_summarize_vars
control_analyze_vars <- function(conf_level = 0.95,
                                 quantiles = c(0.25, 0.75),
                                 quantile_type = 2,
                                 test_mean = 0) {
  checkmate::assert_vector(quantiles, len = 2)
  checkmate::assert_int(quantile_type, lower = 1, upper = 9)
  checkmate::assert_numeric(test_mean)
  lapply(quantiles, assert_proportion_value)
  assert_proportion_value(conf_level)
  list(conf_level = conf_level, quantiles = quantiles, quantile_type = quantile_type, test_mean = test_mean)
}

control_summarize_vars <- control_analyze_vars
