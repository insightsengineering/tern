# Helper function so to modify only in one place for checks
#' @keywords internal
assert_allowed_types <- function(type) {
  default_type <- c("counts", "numeric")
  checkmate::assert_character(type, null.ok = TRUE)
  checkmate::assert_subset(type,
    choices = default_type,
    empty.ok = TRUE
  )
  invisible(default_type)
}

#' Defaults for stats methods names and their relative formats/labels
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Utility functions to get valid statistic methods for different method groups
#' (`.stats`) and their relative formats (`.formats`) and labels (`.labels`). This utility
#' is used across `tern`, but some of its working principles can be seen in [analyze_vars()].
#' See notes to understand why this is experimental.
#'
#' @param stats (`character`) \cr statistical methods to get defaults formats for.
#' @param type (`character`) \cr type of data and result desired. Some stats defaults (and their formats and labels),
#'   differ if you need to analyze different type of values. See Details for available options.
#'
#' @details
#' Current choices for `type` are `counts` and `numeric` for [analyze_vars()] and affect `get_stats()`.
#'
#' @note
#' These defaults are experimental because we use the names of functions to retrieve the default statistics. This
#' should be generalized in groups of methods according to more reasonable groupings.
#'
#' @name default_stats_and_formats
NULL

#' @describeIn default_stats_and_formats Get defaults statistical methods for different
#'   groups of methods.
#'
#' @param method_groups (`character`) \cr indicates the group of statistical methods that
#'   we need the defaults from. A character vector can be used to collect more than one group of statistical
#'   methods.
#' @param stats_in (`character`) \cr desired stats to be picked out from the selected method group.
#'
#' @return
#' * `get_stats()` returns a character vector with all default statistical methods.
#'
#' @examples
#' # Default is numeric
#' num_stats <- get_stats("analyze_vars")
#'
#' # Other type
#' cnt_stats <- get_stats("analyze_vars", type = "counts")
#'
#' # Weirdly taking the pval from count_occurrences
#' only_pval <- get_stats("count_occurrences", add_pval = TRUE, stats_in = "pval")
#'
#' # All count_occurrences
#' all_cnt_occ <- get_stats("count_occurrences")
#'
#' # Multiple
#' get_stats(c("count_occurrences", "analyze_vars"))
#' get_stats(c("count_occurrences", "analyze_vars"), type = c("numeric", "counts"))
#'
#' @export
get_stats <- function(method_groups, type = NULL, stats_in = NULL, add_pval = FALSE) {
  checkmate::assert_character(method_groups)
  assert_allowed_types(type)
  checkmate::assert_character(stats_in, null.ok = TRUE)
  checkmate::assert_flag(add_pval)

  # Defaults for loop
  out <- NULL
  type_loop <- if (is.null(type)) {
    "numeric"
  } else {
    type
  }

  # Loop for multiple method groups
  for (mgi in method_groups) {
    # Loop if you have more than one type
    for (typ in type_loop) {
      add_typ_to_err <- FALSE
      mgi_fin <- mgi
      if (mgi %in% c("analyze_vars")) {
        mgi_fin <- paste0(mgi, "_", typ)
        add_typ_to_err <- TRUE
      }
      out_tmp <- switch(mgi_fin,
        "count_occurrences" = c("count", "count_fraction_fixed_dp", "fraction"),
        "summarize_num_patients" = c("unique", "nonunique", "unique_count"),
        "analyze_vars_counts" = c("n", "count", "count_fraction", "n_blq"),
        "analyze_vars_numeric" = c(
          "n", "sum", "mean", "sd", "se", "mean_sd", "mean_se", "mean_ci", "mean_sei",
          "mean_sdi", "mean_pval", "median", "mad", "median_ci", "quantiles", "iqr",
          "range", "min", "max", "median_range", "cv", "geom_mean", "geom_mean_ci",
          "geom_cv"
        ),
        stop(
          "The inserted method_group (", mgi, ")",
          ifelse(add_typ_to_err, "", paste0(" and type (", paste0(typ, collapse = " "), ")")),
          " has no default statistical method."
        )
      )

      out <- unique(c(out, out_tmp))

      # Mainly used in "analyze_vars" but it could be necessary elsewhere
      if (isTRUE(add_pval)) {
        pval_spec <- if (typ == "counts") {
          "pval_counts"
        } else {
          "pval"
        }
        out <- unique(c(out, pval_spec))
      }
    }
  }

  # Filtering for stats_in (character vector)
  if (!is.null(stats_in)) {
    out <- intersect(stats_in, out)
  }

  # If intersect did not find matches (and no pval?) -> error
  if (length(out) == 0) {
    stop(
      "The selected method_groups (", paste0(method_groups, collapse = " "), ")",
      ifelse(is.null(type), "", paste0(" and types (",
                                       paste0(type, collapse = " "), ")")),
      " does not have the required default statistical methods:\n",
      paste0(stats_in, collapse = " ")
    )
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
#' @examples
#' # Defaults formats
#' get_format_from_stats(num_stats)
#' get_format_from_stats(cnt_stats)
#' get_format_from_stats(only_pval)
#' get_format_from_stats(all_cnt_occ)
#'
#' # Addition of customs
#' get_format_from_stats(all_cnt_occ, formats_in = c("fraction" = c("xx")))
#' get_format_from_stats(all_cnt_occ, formats_in = list("fraction" = c("xx.xx", "xx")))
#'
#' @seealso [formatting_functions]
#'
#' @export
get_format_from_stats <- function(stats, formats_in = NULL) {
  checkmate::assert_character(stats, min.len = 1)
  # It may be a list if there is a function in the formats
  if (checkmate::test_list(formats_in, null.ok = TRUE)) {
    checkmate::assert_list(formats_in, null.ok = TRUE)
    # Or it may be a vector of characters
  } else {
    checkmate::assert_character(formats_in, null.ok = TRUE)
  }

  # Extract global defaults
  which_fmt <- match(stats, names(tern_default_formats))

  # Select only needed formats from stats
  ret <- vector("list", length = length(stats)) # Returning a list is simpler
  ret[!is.na(which_fmt)] <- tern_default_formats[which_fmt[!is.na(which_fmt)]]

  out <- setNames(ret, stats)

  # Modify some with custom formats
  if (!is.null(formats_in)) {
    # Stats is the main
    common_names <- intersect(names(out), names(formats_in))
    out[common_names] <- formats_in[common_names]
  }

  out
}

#' @describeIn default_stats_and_formats Get labels from vector of statistical methods.
#'
#' @param formats_in (named `vector`) \cr inserted formats to replace defaults.
#'
#' @return
#' * `get_label_from_stats()` returns a named character vector of default labels (if present
#'   otherwise `NULL`).
#'
#' @examples
#' # Defaults labels
#' get_label_from_stats(num_stats)
#' get_label_from_stats(cnt_stats)
#' get_label_from_stats(only_pval)
#' get_label_from_stats(all_cnt_occ)
#'
#' # Addition of customs
#' get_label_from_stats(all_cnt_occ, labels_in = c("fraction" = "Fraction"))
#' get_label_from_stats(all_cnt_occ, labels_in = list("fraction" = c("Some more fractions")))
#'
#' @export
get_label_from_stats <- function(stats, labels_in = NULL) {
  checkmate::assert_character(stats, min.len = 1)
  # It may be a list
  if (checkmate::test_list(labels_in, null.ok = TRUE)) {
    checkmate::assert_list(labels_in, null.ok = TRUE)
    # Or it may be a vector of characters
  } else {
    checkmate::assert_character(labels_in, null.ok = TRUE)
  }

  which_lbl <- match(stats, names(tern_default_labels))

  ret <- vector("character", length = length(stats)) # it needs to be a character vector
  ret[!is.na(which_lbl)] <- tern_default_labels[which_lbl[!is.na(which_lbl)]]

  out <- setNames(ret, stats)

  # Modify some with custom labels
  if (!is.null(labels_in)) {
    # Stats is the main
    common_names <- intersect(names(out), names(labels_in))
    out[common_names] <- labels_in[common_names]
  }

  out
}

#' @describeIn default_stats_and_formats Named list of default formats for `tern`.
#'
#' @export
tern_default_formats <- list(
  fraction = format_fraction_fixed_dp,
  unique = format_count_fraction_fixed_dp,
  nonunique = "xx",
  unique_count = "xx",
  n = "xx.",
  count = "xx.",
  count_fraction = format_count_fraction,
  count_fraction_fixed_dp = format_count_fraction_fixed_dp,
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
  pval = "x.xxxx | (<0.0001)",
  pval_counts = "x.xxxx | (<0.0001)"
)

#' @describeIn default_stats_and_formats `character` vector that contains default labels
#'   for `tern`.
#'
#' @export
tern_default_labels <- c(
  # list of labels -> sorted? xxx it should be not relevant due to match
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
  pval = "p-value (t-test)", # Default for numeric
  pval_counts = "p-value (chi-squared test)" # Default for counts
)
