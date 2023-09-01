#' Defaults for stats methods names and their relative formats/labels
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Utility functions to get valid statistic methods for different method groups
#' (`.stats`) and their relative formats (`.formats`) and labels (`.labels`). This utility
#' is used across `tern`, but some of its working principles can be seen in [analyze_vars()].
#'
#' @param method_group (`string`) \cr Indicates the group of statistical methods that
#'   we need the defaults from.
#' @param stats (`character`) \cr statistical methods to get defaults formats for.
#'
#' @name default_stats_and_formats
NULL

#' @describeIn default_stats_and_formats Get defaults statistical methods for different
#'   groups of methods.
#'
#' @return
#' * `get_stats()` returns a character vector with all default statistical methods.
#'
#' @export
get_stats <- function(method_group) {
  checkmate::assert_string(method_group)
  switch (method_group,
          "count_occurrences" = c("count", "count_fraction", "fraction"),
          "summarize_num_patients" = c("unique", "nonunique", "unique_count"),
          stop(method_group, " is a method_group that has no default statistical method.")
  )
}

#' @describeIn default_stats_and_formats Get formats from vector of statistical methods. If not
#'   present `NULL` is returned.
#'
#' @return
#' * `get_format_from_stats()` returns a named list of formats, they being a value from
#'   [formatters::list_valid_format_labels()] or a custom function (e.g. [formatting_functions]).
#'
#' @seealso [formatting_functions]
#'
#' @export
get_format_from_stats <- function(stats) {
  checkmate::assert_character(stats, min.len = 1)
  default_formats <- tern_default_formats()
  which_fmt <- match(stats, names(default_formats))

  ret <- vector("list", length = length(stats))
  ret[!is.na(which_fmt)] <- default_formats[which_fmt[!is.na(which_fmt)]]

  setNames(ret, stats)
}

#' @describeIn default_stats_and_formats Get labels from vector of statistical methods.
#'
#' @return
#' * `get_label_from_stats()` returns a named list of default labels (if present
#'   otherwise `NULL`).
#'
#' @export
get_label_from_stats <- function(stats) {
  checkmate::assert_character(stats, min.len = 1)
  default_lbl <- tern_default_labels()
  which_lbl <- match(stats, names(default_lbl))

  ret <- vector("list", length = length(stats))
  ret[!is.na(which_lbl)] <- default_lbl[which_lbl[!is.na(which_lbl)]]

  setNames(ret, stats)
}
#' @describeIn default_stats_and_formats Function that produce the complete named list
#'   of default formats for `tern`.
#'
#' @return
#' * `tern_default_formats()` returns a complete named list of default formats for `tern`.
#'
#' @export
tern_default_formats <- function() {
  c(
    list(
      # "count" = "xx", # Conflict coming from count_occurences
      # "count_fraction" = format_count_fraction_fixed_dp, # Conflict coming from count_occurences
      fraction = format_fraction_fixed_dp
    ),
    list(
      unique = format_count_fraction_fixed_dp,
      nonunique = "xx",
      unique_count = "xx"
    ),
  # coming from tern:::summary_formats() but it should be exported and more general
    list(
      # n = "xx.", # Duplication in summary_formats
      count = "xx.",
      count_fraction = format_count_fraction,
      n_blq = "xx."
      # pval = "x.xxxx | (<0.0001)" # Duplication in summary_formats
    ),
    list(
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
  )
}
#' @describeIn default_stats_and_formats Function that produce the complete named list
#'   of default labels for `tern`.
#'
#' @return
#' * `tern_default_labels()` returns a complete named list of default labels for `tern`.
#'
#' @export
tern_default_labels <- function() {
  c(
    list(
      "unique" = "Number of patients with at least one event",
      "nonunique" = "Number of events"
    )
  )
}
