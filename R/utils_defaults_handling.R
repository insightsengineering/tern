# Helper function so to modify only in one place for checks
#' @keywords internal
assert_allowed_types <- function(type) {
  checkmate::assert_string(type, null.ok = TRUE)
  checkmate::assert_choice(type, c("counts", "numeric", "count_fraction_fixed_dp"))
}

#' Defaults for stats methods names and their relative formats/labels
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Utility functions to get valid statistic methods for different method groups
#' (`.stats`) and their relative formats (`.formats`) and labels (`.labels`). This utility
#' is used across `tern`, but some of its working principles can be seen in [analyze_vars()].
#'
#' @param stats (`character`) \cr statistical methods to get defaults formats for.
#' @param type (`string`) \cr type of data and result desired. Some stats defaults (and their formats and labels),
#'   differ if you need to analyze different type of values. See Details for available options.
#'
#' @details
#' Current choices for `type` are `counts` and `numeric` for [analyze_vars()] and affect `get_stats()` and
#' `get_label_from_stats()`. Another option, `count_fraction_fixed_dp` affects only formats for count fractions.
#'
#' @name default_stats_and_formats
NULL

#' @describeIn default_stats_and_formats Get defaults statistical methods for different
#'   groups of methods.
#'
#' @param method_group (`string`) \cr indicates the group of statistical methods that
#'   we need the defaults from.
#' @param stats_in (`character`) \cr desired stats to be picked out from the selected method group.
#'
#' @return
#' * `get_stats()` returns a character vector with all default statistical methods.
#'
#' @export
get_stats <- function(method_group, type = NULL, stats_in = NULL, add_pval = FALSE) {
  checkmate::assert_string(method_group)
  assert_allowed_types(type)
  checkmate::assert_character(stats_in, null.ok = TRUE)
  checkmate::assert_flag(add_pval)

  if (!is.null(type)) {
    # For the moment the type changes the defaults only in "analyze_vars"
    if (method_group %in% c("analyze_vars")) {
      method_group <- paste0(method_group, "_", type)
    }
    # No error here because of "count_fraction_fixed_dp", exception only in formats
  }

  out <- switch (method_group,
          "count_occurrences" = c("count", "count_fraction", "fraction"),
          "summarize_num_patients" = c("unique", "nonunique", "unique_count"),
          "analyze_vars_counts" = c("n", "count", "count_fraction", "n_blq"),
          "analyze_vars_numeric" = c(
            "n", "sum", "mean", "sd", "se", "mean_sd", "mean_se", "mean_ci", "mean_sei",
            "mean_sdi", "mean_pval", "median", "mad", "median_ci", "quantiles", "iqr",
            "range", "min", "max", "median_range", "cv", "geom_mean", "geom_mean_ci",
            "geom_cv"
          ),
          stop(method_group, " is a method_group that has no default statistical method.")
  )

  # Filtering for stats_in (character vector)
  if (!is.null(stats_in)) {
    out <- intersect(stats_in, out)
  }

  # Mainly used in "analyze_vars" but it could be necessary elsewhere
  if (isTRUE(add_pval)) {
    out <- unique(c(out, "pval"))
  }

  out
}

#' @describeIn default_stats_and_formats Get formats from vector of statistical methods. If not
#'   present `NULL` is returned.
#'
#' @param formats_in (named `vector`) \cr inserted formats to replace defaults. It can be a
#'   character vector from [formatters::list_valid_format_labels()] or a custom format function.
#'
#' @return
#' * `get_format_from_stats()` returns a named list of formats, they being a value from
#'   [formatters::list_valid_format_labels()] or a custom function (e.g. [formatting_functions]).
#'
#' @note Formats in `tern` and `rtables` can be functions that take in the table cell value and
#'   return a string. This is well documented in `vignette("custom_appearance", package = "rtables")`.
#'
#' @seealso [formatting_functions]
#'
#' @export
get_format_from_stats <- function(stats, type = NULL, formats_in = NULL) {
  checkmate::assert_character(stats, min.len = 1)
  assert_allowed_types(type)
  checkmate::assert_character(formats_in, null.ok = TRUE)

  default_formats <- tern_default_formats(type)
  which_fmt <- match(stats, names(default_formats))

  ret <- vector("list", length = length(stats))
  ret[!is.na(which_fmt)] <- default_formats[which_fmt[!is.na(which_fmt)]]

  out <- setNames(ret, stats)

  if (!is.null(formats_in)) {
    out[names(formats_in)] <- formats_in
  }

  out
}

#' @describeIn default_stats_and_formats Get labels from vector of statistical methods.
#'
#' @param formats_in (named `vector`) \cr inserted formats to replace defaults.
#'
#' @return
#' * `get_label_from_stats()` returns a named list of default labels (if present
#'   otherwise `NULL`).
#'
#' @export
get_label_from_stats <- function(stats, type = NULL, labels_in = NULL) {
  checkmate::assert_character(stats, min.len = 1)
  assert_allowed_types(type)
  checkmate::assert_character(labels_in, null.ok = TRUE)

  default_lbl <- tern_default_labels(type)
  which_lbl <- match(stats, names(default_lbl))

  ret <- vector("list", length = length(stats))
  ret[!is.na(which_lbl)] <- default_lbl[which_lbl[!is.na(which_lbl)]]

  out <- setNames(ret, stats)

  if (!is.null(labels_in)) {
    out[names(labels_in)] <- labels_in
  }

  out
}

#' @describeIn default_stats_and_formats Function that produce the complete named list
#'   of default formats for `tern`.
#'
#' @return
#' * `tern_default_formats()` returns a complete named list of default formats for `tern`.
#'
#' @export
tern_default_formats <- function(type = NULL) {
  assert_allowed_types(type)
  out <- list(
    fraction = format_fraction_fixed_dp,
    unique = format_count_fraction_fixed_dp,
    nonunique = "xx",
    unique_count = "xx",
    n = "xx.",
    count = "xx.",
    count_fraction = format_count_fraction,
    n_blq = "xx.",
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
  if (!is.null(type) && type == "count_fraction_fixed_dp") {
    out[["count_fraction"]] <- format_count_fraction_fixed_dp
  }

  out
}

#' @describeIn default_stats_and_formats Function that produce the complete named list
#'   of default labels for `tern`.
#'
#' @return
#' * `tern_default_labels()` returns a complete named list of default labels for `tern`.
#'
#' @export
tern_default_labels <- function(type) {
  assert_allowed_types(type)

  # list of labels -> sorted? xxx it should be not relevant due to match
  out <- list(
    unique = "Number of patients with at least one event",
    nonunique = "Number of events",
    n = "n",
    count = "count",
    count_fraction = "count_fraction",
    n_blq = "n_blq",
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
    pval = "p-value (t-test)" # Default for numeric
  )

  # type exceptions to be handled
  if (!is.null(type) && type == "counts") {
    out[["pval"]] <- "p-value (chi-squared test)"
  }

  out
}
