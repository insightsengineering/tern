#' Get default statistical methods and their associated formats, labels, and indent modifiers
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Utility functions to get valid statistic methods for different method groups
#' (`.stats`) and their associated formats (`.formats`), labels (`.labels`), and indent modifiers
#' (`.indent_mods`). This utility is used across `tern`, but some of its working principles can be
#' seen in [analyze_vars()]. See notes to understand why this is experimental.
#'
#' @param stats (`character`)\cr statistical methods to get defaults for.
#'
#' @details
#' Current choices for `type` are `counts` and `numeric` for [analyze_vars()] and affect `get_stats()`.
#'
#' @note
#' These defaults are experimental because we use the names of functions to retrieve the default
#' statistics. This should be generalized in groups of methods according to more reasonable groupings.
#'
#' @name default_stats_formats_labels
NULL

#' @describeIn default_stats_formats_labels Get statistics available for a given method
#'   group (analyze function).
#'
#' @param method_groups (`character`)\cr indicates the statistical method group (`tern` analyze function)
#'   to retrieve default statistics for. A character vector can be used to specify more than one statistical
#'   method group.
#' @param stats_in (`character`)\cr statistics to retrieve for the selected method group.
#' @param add_pval (`flag`)\cr should `"pval"` (or `"pval_counts"` if `method_groups` contains
#'   `"analyze_vars_counts"`) be added to the statistical methods?
#'
#' @return
#' * `get_stats()` returns a `character` vector of statistical methods.
#'
#' @examples
#' # analyze_vars is numeric
#' num_stats <- get_stats("analyze_vars_numeric") # also the default
#'
#' # Other type
#' cnt_stats <- get_stats("analyze_vars_counts")
#'
#' # Weirdly taking the pval from count_occurrences
#' only_pval <- get_stats("count_occurrences", add_pval = TRUE, stats_in = "pval")
#'
#' # All count_occurrences
#' all_cnt_occ <- get_stats("count_occurrences")
#'
#' # Multiple
#' get_stats(c("count_occurrences", "analyze_vars_counts"))
#'
#' @export
get_stats <- function(method_groups = "analyze_vars_numeric", stats_in = NULL, add_pval = FALSE) {
  checkmate::assert_character(method_groups)
  checkmate::assert_character(stats_in, null.ok = TRUE)
  checkmate::assert_flag(add_pval)

  # Default is still numeric
  if (any(method_groups == "analyze_vars")) {
    method_groups[method_groups == "analyze_vars"] <- "analyze_vars_numeric"
  }

  type_tmp <- ifelse(any(grepl("counts", method_groups)), "counts", "numeric") # for pval checks

  # Defaults for loop
  out <- NULL

  # Loop for multiple method groups
  for (mgi in method_groups) {
    out_tmp <- if (mgi %in% names(tern_default_stats)) {
      tern_default_stats[[mgi]]
    } else {
      stop("The selected method group (", mgi, ") has no default statistical method.")
    }
    out <- unique(c(out, out_tmp))
  }

  # If you added pval to the stats_in you certainly want it
  if (!is.null(stats_in) && any(grepl("^pval", stats_in))) {
    stats_in_pval_value <- stats_in[grepl("^pval", stats_in)]

    # Must be only one value between choices
    checkmate::assert_choice(stats_in_pval_value, c("pval", "pval_counts"))

    # Mismatch with counts and numeric
    if (any(grepl("counts", method_groups)) && stats_in_pval_value != "pval_counts" ||
      any(grepl("numeric", method_groups)) && stats_in_pval_value != "pval") { # nolint
      stop(
        "Inserted p-value (", stats_in_pval_value, ") is not valid for type ",
        type_tmp, ". Use ", paste(ifelse(stats_in_pval_value == "pval", "pval_counts", "pval")),
        " instead."
      )
    }

    # Lets add it even if present (thanks to unique)
    add_pval <- TRUE
  }

  # Mainly used in "analyze_vars" but it could be necessary elsewhere
  if (isTRUE(add_pval)) {
    if (any(grepl("counts", method_groups))) {
      out <- unique(c(out, "pval_counts"))
    } else {
      out <- unique(c(out, "pval"))
    }
  }

  # Filtering for stats_in (character vector)
  if (!is.null(stats_in)) {
    out <- intersect(stats_in, out) # It orders them too
  }

  # If intersect did not find matches (and no pval?) -> error
  if (length(out) == 0) {
    stop(
      "The selected method group(s) (", paste0(method_groups, collapse = ", "), ")",
      " do not have the required default statistical methods:\n",
      paste0(stats_in, collapse = " ")
    )
  }

  out
}

#' @describeIn default_stats_formats_labels Get formats corresponding to a list of statistics.
#'
#' @param formats_in (named `vector`) \cr inserted formats to replace defaults. It can be a
#'   character vector from [formatters::list_valid_format_labels()] or a custom format function.
#'
#' @return
#' * `get_formats_from_stats()` returns a named vector of formats (if present in either
#'   `tern_default_formats` or `formats_in`, otherwise `NULL`). Values can be taken from
#'   [formatters::list_valid_format_labels()] or a custom function (e.g. [formatting_functions]).
#'
#' @note Formats in `tern` and `rtables` can be functions that take in the table cell value and
#'   return a string. This is well documented in `vignette("custom_appearance", package = "rtables")`.
#'
#' @examples
#' # Defaults formats
#' get_formats_from_stats(num_stats)
#' get_formats_from_stats(cnt_stats)
#' get_formats_from_stats(only_pval)
#' get_formats_from_stats(all_cnt_occ)
#'
#' # Addition of customs
#' get_formats_from_stats(all_cnt_occ, formats_in = c("fraction" = c("xx")))
#' get_formats_from_stats(all_cnt_occ, formats_in = list("fraction" = c("xx.xx", "xx")))
#'
#' @seealso [formatting_functions]
#'
#' @export
get_formats_from_stats <- function(stats, formats_in = NULL) {
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

#' @describeIn default_stats_formats_labels Get labels corresponding to a list of statistics.
#'
#' @param labels_in (named `vector` of `character`)\cr inserted labels to replace defaults.
#' @param row_nms (`character`)\cr row names. Levels of a `factor` or `character` variable, each
#'   of which the statistics in `.stats` will be calculated for. If this parameter is set, these
#'   variable levels will be used as the defaults, and the names of the given custom values should
#'   correspond to levels (or have format `statistic.level`) instead of statistics. Can also be
#'   variable names if rows correspond to different variables instead of levels. Defaults to `NULL`.
#'
#' @return
#' * `get_labels_from_stats()` returns a named `character` vector of labels (if present in either
#'   `tern_default_labels` or `labels_in`, otherwise `NULL`).
#'
#' @examples
#' # Defaults labels
#' get_labels_from_stats(num_stats)
#' get_labels_from_stats(cnt_stats)
#' get_labels_from_stats(only_pval)
#' get_labels_from_stats(all_cnt_occ)
#'
#' # Addition of customs
#' get_labels_from_stats(all_cnt_occ, labels_in = c("fraction" = "Fraction"))
#' get_labels_from_stats(all_cnt_occ, labels_in = list("fraction" = c("Some more fractions")))
#'
#' @export
get_labels_from_stats <- function(stats, labels_in = NULL, row_nms = NULL) {
  checkmate::assert_character(stats, min.len = 1)
  checkmate::assert_character(row_nms, null.ok = TRUE)
  # It may be a list
  if (checkmate::test_list(labels_in, null.ok = TRUE)) {
    checkmate::assert_list(labels_in, null.ok = TRUE)
    # Or it may be a vector of characters
  } else {
    checkmate::assert_character(labels_in, null.ok = TRUE)
  }

  if (!is.null(row_nms)) {
    ret <- rep(row_nms, length(stats))
    out <- setNames(ret, paste(rep(stats, each = length(row_nms)), ret, sep = "."))

    if (!is.null(labels_in)) {
      lvl_lbls <- intersect(names(labels_in), row_nms)
      for (i in lvl_lbls) out[paste(stats, i, sep = ".")] <- labels_in[[i]]
    }
  } else {
    which_lbl <- match(stats, names(tern_default_labels))

    ret <- vector("character", length = length(stats)) # it needs to be a character vector
    ret[!is.na(which_lbl)] <- tern_default_labels[which_lbl[!is.na(which_lbl)]]

    out <- setNames(ret, stats)
  }

  # Modify some with custom labels
  if (!is.null(labels_in)) {
    # Stats is the main
    common_names <- intersect(names(out), names(labels_in))
    out[common_names] <- labels_in[common_names]
  }

  out
}

#' @describeIn default_stats_formats_labels Format indent modifiers for a given vector/list of statistics.
#'
#' @param indents_in (named `vector`)\cr inserted indent modifiers to replace defaults (default is `0L`).
#'
#' @return
#' * `get_indents_from_stats()` returns a single indent modifier value to apply to all rows
#'   or a named numeric vector of indent modifiers (if present, otherwise `NULL`).
#'
#' @examples
#' get_indents_from_stats(all_cnt_occ, indents_in = 3L)
#' get_indents_from_stats(all_cnt_occ, indents_in = list(count = 2L, count_fraction = 5L))
#' get_indents_from_stats(
#'   all_cnt_occ,
#'   indents_in = list(a = 2L, count.a = 1L, count.b = 5L), row_nms = c("a", "b")
#' )
#'
#' @export
get_indents_from_stats <- function(stats, indents_in = NULL, row_nms = NULL) {
  checkmate::assert_character(stats, min.len = 1)
  checkmate::assert_character(row_nms, null.ok = TRUE)
  # It may be a list
  if (checkmate::test_list(indents_in, null.ok = TRUE)) {
    checkmate::assert_list(indents_in, null.ok = TRUE)
    # Or it may be a vector of integers
  } else {
    checkmate::assert_integerish(indents_in, null.ok = TRUE)
  }

  if (is.null(names(indents_in)) && length(indents_in) == 1) {
    out <- rep(indents_in, length(stats) * if (!is.null(row_nms)) length(row_nms) else 1)
    return(out)
  }

  if (!is.null(row_nms)) {
    ret <- rep(0L, length(stats) * length(row_nms))
    out <- setNames(ret, paste(rep(stats, each = length(row_nms)), rep(row_nms, length(stats)), sep = "."))

    if (!is.null(indents_in)) {
      lvl_lbls <- intersect(names(indents_in), row_nms)
      for (i in lvl_lbls) out[paste(stats, i, sep = ".")] <- indents_in[[i]]
    }
  } else {
    ret <- rep(0L, length(stats))
    out <- setNames(ret, stats)
  }

  # Modify some with custom labels
  if (!is.null(indents_in)) {
    # Stats is the main
    common_names <- intersect(names(out), names(indents_in))
    out[common_names] <- indents_in[common_names]
  }

  out
}

#' Update Labels According to Control Specifications
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Given a list of statistic labels and and a list of control parameters, updates labels with a relevant
#' control specification. For example, if control has element `conf_level` set to `0.9`, the default
#' label for statistic `mean_ci` will be updated to `"Mean 90% CI"`. Any labels that are supplied
#' via `labels_custom` will not be updated regardless of `control`.
#'
#' @param labels_default (named `vector` of `character`)\cr a named vector of statistic labels to modify
#'   according to the control specifications. Labels that are explicitly defined in `labels_custom` will
#'   not be affected.
#' @param labels_custom (named `vector` of `character`)\cr named vector of labels that are customized by
#'   the user and should not be affected by `control`.
#' @param control (named `list`)\cr list of control parameters to apply to adjust default labels.
#'
#' @return A named character vector of labels with control specifications applied to relevant labels.
#'
#' @examples
#' control <- list(conf_level = 0.80, quantiles = c(0.1, 0.83), test_mean = 0.57)
#' get_labels_from_stats(c("mean_ci", "quantiles", "mean_pval")) %>%
#'   labels_use_control(control = control)
#'
#' @export
labels_use_control <- function(labels_default, control, labels_custom = NULL) {
  if ("conf_level" %in% names(control)) {
    labels_default <- sapply(
      names(labels_default),
      function(x) {
        if (!x %in% names(labels_custom)) {
          gsub(labels_default[[x]], pattern = "[0-9]+% CI", replacement = f_conf_level(control[["conf_level"]]))
        } else {
          labels_default[[x]]
        }
      }
    )
  }
  if ("quantiles" %in% names(control) && "quantiles" %in% names(labels_default) &&
    !"quantiles" %in% names(labels_custom)) { # nolint
    labels_default["quantiles"] <- gsub(
      "[0-9]+% and [0-9]+", paste0(control[["quantiles"]][1] * 100, "% and ", control[["quantiles"]][2] * 100, ""),
      labels_default["quantiles"]
    )
  }
  if ("test_mean" %in% names(control) && "mean_pval" %in% names(labels_default) &&
    !"mean_pval" %in% names(labels_custom)) { # nolint
    labels_default["mean_pval"] <- gsub(
      "p-value \\(H0: mean = [0-9\\.]+\\)", f_pval(control[["test_mean"]]), labels_default["mean_pval"]
    )
  }

  labels_default
}

#' @describeIn default_stats_formats_labels Named list of available statistics by method group for `tern`.
#'
#' @format
#' * `tern_default_stats` is a named list of available statistics, with each element
#'   named for their corresponding statistical method group.
#'
#' @export
tern_default_stats <- list(
  abnormal = c("fraction"),
  abnormal_by_baseline = c("fraction"),
  abnormal_by_marked = c("count_fraction", "count_fraction_fixed_dp"),
  abnormal_by_worst_grade = c("count_fraction", "count_fraction_fixed_dp"),
  abnormal_by_worst_grade_worsen = c("fraction"),
  analyze_patients_exposure_in_cols = c("n_patients", "sum_exposure"),
  analyze_vars_counts = c("n", "count", "count_fraction", "count_fraction_fixed_dp", "n_blq"),
  analyze_vars_numeric = c(
    "n", "sum", "mean", "sd", "se", "mean_sd", "mean_se", "mean_ci", "mean_sei", "mean_sdi", "mean_pval",
    "median", "mad", "median_ci", "quantiles", "iqr", "range", "min", "max", "median_range", "cv",
    "geom_mean", "geom_mean_ci", "geom_cv"
  ),
  count_cumulative = c("count_fraction", "count_fraction_fixed_dp"),
  count_missed_doses = c("n", "count_fraction", "count_fraction_fixed_dp"),
  count_occurrences = c("count", "count_fraction", "count_fraction_fixed_dp", "fraction"),
  count_occurrences_by_grade = c("count_fraction", "count_fraction_fixed_dp"),
  count_patients_with_event = c("n", "count", "count_fraction", "count_fraction_fixed_dp", "n_blq"),
  count_patients_with_flags = c("n", "count", "count_fraction", "count_fraction_fixed_dp", "n_blq"),
  count_values = c("n", "count", "count_fraction", "count_fraction_fixed_dp", "n_blq"),
  coxph_pairwise = c("pvalue", "hr", "hr_ci", "n_tot", "n_tot_events"),
  estimate_incidence_rate = c("person_years", "n_events", "rate", "rate_ci"),
  estimate_multinomial_response = c("n_prop", "prop_ci"),
  estimate_odds_ratio = c("or_ci", "n_tot"),
  estimate_proportion = c("n_prop", "prop_ci"),
  estimate_proportion_diff = c("diff", "diff_ci"),
  summarize_ancova = c("n", "lsmean", "lsmean_diff", "lsmean_diff_ci", "pval"),
  summarize_coxreg = c("n", "hr", "ci", "pval", "pval_inter"),
  summarize_glm_count = c("n", "rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
  summarize_num_patients = c("unique", "nonunique", "unique_count"),
  summarize_patients_events_in_cols = c("unique", "all"),
  surv_time = c("median", "median_ci", "quantiles", "range_censor", "range_event", "range"),
  surv_timepoint = c("pt_at_risk", "event_free_rate", "rate_se", "rate_ci", "rate_diff", "rate_diff_ci", "ztest_pval"),
  tabulate_rsp_biomarkers = c("n_tot", "n_rsp", "prop", "or", "ci", "pval"),
  tabulate_rsp_subgroups = c("n", "n_rsp", "prop", "n_tot", "or", "ci", "pval"),
  tabulate_survival_biomarkers = c("n_tot", "n_tot_events", "median", "hr", "ci", "pval"),
  tabulate_survival_subgroups = c("n_tot_events", "n_events", "n_tot", "n", "median", "hr", "ci", "pval"),
  test_proportion_diff = c("pval")
)

#' @describeIn default_stats_formats_labels Named vector of default formats for `tern`.
#'
#' @format
#' * `tern_default_formats` is a named vector of available default formats, with each element
#'   named for their corresponding statistic.
#'
#' @export
tern_default_formats <- c(
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
  pval_counts = "x.xxxx | (<0.0001)",
  range_censor = "xx.x to xx.x",
  range_event = "xx.x to xx.x"
)

#' @describeIn default_stats_formats_labels Named `character` vector of default labels for `tern`.
#'
#' @format
#' * `tern_default_labels` is a named `character` vector of available default labels, with each element
#'   named for their corresponding statistic.
#'
#' @export
tern_default_labels <- c(
  fraction = "fraction",
  unique = "Number of patients with at least one event",
  nonunique = "Number of events",
  n = "n",
  count = "count",
  count_fraction = "count_fraction",
  count_fraction_fixed_dp = "count_fraction",
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

# To deprecate ---------

#' @describeIn default_stats_formats_labels Quick function to retrieve default formats for summary statistics:
#'   [analyze_vars()] and [analyze_vars_in_cols()] principally.
#'
#' @param type (`flag`)\cr is it going to be `"numeric"` or `"counts"`?
#'
#' @return
#' * `summary_formats()` returns a named `vector` of default statistic formats for the given data type.
#'
#' @examples
#' summary_formats()
#' summary_formats(type = "counts", include_pval = TRUE)
#'
#' @export
summary_formats <- function(type = "numeric", include_pval = FALSE) {
  met_grp <- paste0(c("analyze_vars", type), collapse = "_")
  get_formats_from_stats(get_stats(met_grp, add_pval = include_pval))
}

#' @describeIn default_stats_formats_labels Quick function to retrieve default labels for summary statistics.
#'   Returns labels of descriptive statistics which are understood by `rtables`. Similar to `summary_formats`
#'
#' @param include_pval (`flag`)\cr deprecated parameter. Same as `add_pval`.
#' @return
#' * `summary_labels` returns a named `vector` of default statistic labels for the given data type.
#'
#' @examples
#' summary_labels()
#' summary_labels(type = "counts", include_pval = TRUE)
#'
#' @export
summary_labels <- function(type = "numeric", include_pval = FALSE) {
  met_grp <- paste0(c("analyze_vars", type), collapse = "_")
  get_labels_from_stats(get_stats(met_grp, add_pval = include_pval))
}

#' @describeIn default_stats_formats_labels `r lifecycle::badge("deprecated")` Function to
#'   configure settings for default or custom summary statistics for a given data type. In
#'   addition to selecting a custom subset of statistics, the user can also set custom
#'   formats, labels, and indent modifiers for any of these statistics.
#'
#' @param stats_custom (`named vector` of `character`)\cr vector of statistics to include if
#'   not the defaults. This argument overrides `include_pval` and other custom value arguments
#'   such that only settings for these statistics will be returned.
#' @param formats_custom (`named vector` of `character`)\cr vector of custom statistics formats
#'   to use in place of the defaults defined in [`summary_formats()`]. Names should be a subset
#'   of the statistics defined in `stats_custom` (or default statistics if this is `NULL`).
#' @param labels_custom (`named vector` of `character`)\cr vector of custom statistics labels
#'   to use in place of the defaults defined in [`summary_labels()`]. Names should be a subset
#'   of the statistics defined in `stats_custom` (or default statistics if this is `NULL`).
#' @param indent_mods_custom (`integer` or `named vector` of `integer`)\cr vector of custom
#'   indentation modifiers for statistics to use instead of the default of `0L` for all statistics.
#'   Names should be a subset of the statistics defined in `stats_custom` (or default statistics
#'   if this is `NULL`). Alternatively, the same indentation modifier can be applied to all
#'   statistics by setting `indent_mods_custom` to a single integer value.
#'
#' @return
#' * `summary_custom` returns a `list` of 4 named elements: `stats`, `formats`, `labels`,
#'   and `indent_mods`.
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
    details = "Please use `get_stats`, `get_formats_from_stats`, and `get_labels_from_stats` directly instead."
  )
  met_grp <- paste0(c("analyze_vars", type), collapse = "_")
  .stats <- get_stats(met_grp, stats_custom, add_pval = include_pval)
  .formats <- get_formats_from_stats(.stats, formats_custom)
  .labels <- get_labels_from_stats(.stats, labels_custom)
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
