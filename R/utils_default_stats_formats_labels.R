#' Get default statistical methods and their associated formats, labels, and indent modifiers
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Utility functions to get valid statistic methods for different method groups
#' (`.stats`) and their associated formats (`.formats`), labels (`.labels`), and indent modifiers
#' (`.indent_mods`). This utility is used across `tern`, but some of its working principles can be
#' seen in [analyze_vars()]. See notes to understand why this is experimental.
#'
#' @param stats (`character`)\cr statistical methods to return defaults for.
#' @param levels_per_stats (named `list` of `character` or `NULL`)\cr named list where the name of each element is a
#'   statistic from `stats` and each element is the levels of a `factor` or `character` variable (or variable name),
#'   each corresponding to a single row, for which the named statistic should be calculated for. If a statistic is only
#'   calculated once (one row), the element can be either `NULL` or the name of the statistic. Each list element will be
#'   flattened such that the names of the list elements returned by the function have the format `statistic.level` (or
#'   just `statistic` for statistics calculated for a single row). Defaults to `NULL`.
#' @param tern_defaults (`list` or `vector`)\cr defaults to use to fill in missing values if no user input is given.
#'   Must be of the same type as the values that are being filled in (e.g. indentation must be integers).
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
#'   group (analyze function). To check available defaults see `tern::tern_default_stats` list.
#'
#' @param method_groups (`character`)\cr indicates the statistical method group (`tern` analyze function)
#'   to retrieve default statistics for. A character vector can be used to specify more than one statistical
#'   method group.
#' @param stats_in (`character`)\cr statistics to retrieve for the selected method group. If custom statistical
#'   functions are used, `stats_in` needs to have them in too.
#' @param custom_stats_in (`character`)\cr custom statistics to add to the default statistics.
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
get_stats <- function(method_groups = "analyze_vars_numeric",
                      stats_in = NULL, custom_stats_in = NULL, add_pval = FALSE) {
  checkmate::assert_character(method_groups)
  checkmate::assert_character(stats_in, null.ok = TRUE)
  checkmate::assert_character(custom_stats_in, null.ok = TRUE)
  checkmate::assert_flag(add_pval)

  # Default is still numeric
  if (any(method_groups == "analyze_vars")) {
    method_groups[method_groups == "analyze_vars"] <- "analyze_vars_numeric"
  }

  type_tmp <- ifelse(any(grepl("counts$", method_groups)), "counts", "numeric") # for pval checks

  # Defaults for loop
  out <- NULL

  # Loop for multiple method groups
  for (mgi in method_groups) {
    if (mgi %in% names(tern_default_stats)) {
      out_tmp <- tern_default_stats[[mgi]]
    } else {
      stop("The selected method group (", mgi, ") has no default statistical method.")
    }
    out <- unique(c(out, out_tmp))
  }

  # Add custom stats
  out <- c(out, custom_stats_in)

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

#' @describeIn default_stats_formats_labels Get statistical *names* available for a given method
#'   group (analyze function). Please use the `s_*` functions to get the statistical names.
#' @param stat_results (`list`)\cr list of statistical results. It should be used close to the end of
#'   a statistical function. See examples for a structure with two statistical results and two groups.
#' @param stat_names_in (`character`)\cr custom modification of statistical values.
#'
#' @return
#' * `get_stat_names()` returns a named list of`character` vectors, indicating the names of
#'    statistical outputs.
#'
#' @examples
#' stat_results <- list(
#'   "n" = list("M" = 1, "F" = 2),
#'   "count_fraction" = list("M" = c(1, 0.2), "F" = c(2, 0.1))
#' )
#' get_stat_names(stat_results)
#' get_stat_names(stat_results, list("n" = "argh"))
#'
#' @export
get_stat_names <- function(stat_results, stat_names_in = NULL) {
  checkmate::assert_character(names(stat_results), min.len = 1)
  checkmate::assert_list(stat_names_in, null.ok = TRUE)

  stat_nms_from_stats <- lapply(stat_results, function(si) {
    nm <- names(si)
    if (is.null(nm)) {
      nm <- rep(NA_character_, length(si)) # no statistical names
    }
    nm
  })

  # Modify some with custom stat names
  if (!is.null(stat_names_in)) {
    # Stats is the main
    common_names <- intersect(names(stat_nms_from_stats), names(stat_names_in))
    stat_nms_from_stats[common_names] <- stat_names_in[common_names]
  }

  stat_nms_from_stats
}

# Utility function used to separate custom stats (user-defined functions) from defaults
.split_std_from_custom_stats <- function(stats_in) {
  out <- list(default_stats = NULL, custom_stats = NULL, all_stats = NULL)
  if (is.list(stats_in)) {
    is_custom_fnc <- sapply(stats_in, is.function)
    checkmate::assert_list(stats_in[is_custom_fnc], types = "function", names = "named")
    out[["custom_stats"]] <- stats_in[is_custom_fnc]
    out[["default_stats"]] <- unlist(stats_in[!is_custom_fnc])
    all_stats <- names(stats_in) # to keep the order
    all_stats[!is_custom_fnc] <- out[["default_stats"]]
    out[["all_stats"]] <- all_stats
  } else {
    out[["default_stats"]] <- out[["all_stats"]] <- stats_in
  }
  out
}

# Utility function to apply statistical functions
.apply_stat_functions <- function(default_stat_fnc, custom_stat_fnc_list, args_list) {
  # Default checks
  checkmate::assert_function(default_stat_fnc)
  checkmate::assert_list(custom_stat_fnc_list, types = "function", null.ok = TRUE, names = "named")
  checkmate::assert_list(args_list)

  # Checking custom stats have same formals
  if (!is.null(custom_stat_fnc_list)) {
    fundamental_call_to_data <- names(formals(default_stat_fnc))[[1]]
    for (fnc in custom_stat_fnc_list) {
      if (!identical(names(formals(fnc))[[1]], fundamental_call_to_data)) {
        stop(
          "The first parameter of a custom statistical function needs to be the same (it can be `df` or `x`) ",
          "as the default statistical function. In this case your custom function has ", names(formals(fnc))[[1]],
          " as first parameter, while the default function has ", fundamental_call_to_data, "."
        )
      }
      if (!any(names(formals(fnc)) == "...")) {
        stop(
          "The custom statistical function needs to have `...` as a parameter to accept additional arguments. ",
          "In this case your custom function does not have `...`."
        )
      }
    }
  }

  # Merging
  stat_fnc_list <- c(default_stat_fnc, custom_stat_fnc_list)

  # Applying
  out <- unlist(lapply(stat_fnc_list, function(fnc) do.call(fnc, args = args_list)), recursive = FALSE)

  out
}

#' @describeIn default_stats_formats_labels Get formats corresponding to a list of statistics.
#'   To check available defaults see list `tern::tern_default_formats`.
#'
#' @param formats_in (named `vector`)\cr custom formats to use instead of defaults. Can be a character vector with
#'   values from [formatters::list_valid_format_labels()] or custom format functions. Defaults to `NULL` for any rows
#'   with no value is provided.
#'
#' @return
#' * `get_formats_from_stats()` returns a named list of formats as strings or functions.
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
get_formats_from_stats <- function(stats,
                                   formats_in = NULL,
                                   levels_per_stats = NULL,
                                   tern_defaults = tern_default_formats) {
  checkmate::assert_character(stats, min.len = 1)
  # It may be a list if there is a function in the formats
  if (checkmate::test_list(formats_in, null.ok = TRUE)) {
    checkmate::assert_list(formats_in, null.ok = TRUE)
    # Or it may be a vector of characters
  } else {
    checkmate::assert_character(formats_in, null.ok = TRUE)
  }
  checkmate::assert_list(levels_per_stats, null.ok = TRUE)

  # If levels_per_stats not given, assume one row per statistic
  if (is.null(levels_per_stats)) levels_per_stats <- as.list(stats) %>% setNames(stats)

  # Apply custom formats
  out <- .fill_in_vals_by_stats(levels_per_stats, formats_in, tern_defaults)

  # Default to NULL if no format
  out[names(out) == out] <- list(NULL)

  out
}

#' @describeIn default_stats_formats_labels Get labels corresponding to a list of statistics.
#'   To check for available defaults see list `tern::tern_default_labels`.
#'
#' @param labels_in (named `character`)\cr custom labels to use instead of defaults. If no value is provided, the
#'   variable level (if rows correspond to levels of a variable) or statistic name will be used as label.
#'
#' @return
#' * `get_labels_from_stats()` returns a named list of labels as strings.
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
get_labels_from_stats <- function(stats,
                                  labels_in = NULL,
                                  levels_per_stats = NULL,
                                  tern_defaults = tern_default_labels) {
  checkmate::assert_character(stats, min.len = 1)
  # It may be a list
  if (checkmate::test_list(labels_in, null.ok = TRUE)) {
    checkmate::assert_list(labels_in, null.ok = TRUE)
    # Or it may be a vector of characters
  } else {
    checkmate::assert_character(labels_in, null.ok = TRUE)
  }
  checkmate::assert_list(levels_per_stats, null.ok = TRUE)

  # If levels_per_stats not given, assume one row per statistic
  if (is.null(levels_per_stats)) levels_per_stats <- as.list(stats) %>% setNames(stats)

  # Apply custom labels
  out <- .fill_in_vals_by_stats(levels_per_stats, labels_in, tern_defaults)
  out
}

#' @describeIn default_stats_formats_labels Get row indent modifiers corresponding to a list of statistics/rows.
#'
#' @param indents_in (named `integer`)\cr custom row indent modifiers to use instead of defaults. Defaults to `0L` for
#'   all values.
#' @param row_nms `r lifecycle::badge("deprecated")` Deprecation cycle started. See the `levels_per_stats` parameter
#'   for details.
#'
#' @return
#' * `get_indents_from_stats()` returns a named list of indentation modifiers as integers.
#'
#' @examples
#' get_indents_from_stats(all_cnt_occ, indents_in = 3L)
#' get_indents_from_stats(all_cnt_occ, indents_in = list(count = 2L, count_fraction = 5L))
#' get_indents_from_stats(
#'   all_cnt_occ,
#'   indents_in = list(a = 2L, count.a = 1L, count.b = 5L)
#' )
#'
#' @export
get_indents_from_stats <- function(stats,
                                   indents_in = NULL,
                                   levels_per_stats = NULL,
                                   tern_defaults = as.list(rep(0L, length(stats))) %>% setNames(stats),
                                   row_nms = lifecycle::deprecated()) {
  checkmate::assert_character(stats, min.len = 1)
  # It may be a list
  if (checkmate::test_list(indents_in, null.ok = TRUE)) {
    checkmate::assert_list(indents_in, null.ok = TRUE)
    # Or it may be a vector of integers
  } else {
    checkmate::assert_integerish(indents_in, null.ok = TRUE)
  }
  checkmate::assert_list(levels_per_stats, null.ok = TRUE)

  # If levels_per_stats not given, assume one row per statistic
  if (is.null(levels_per_stats)) levels_per_stats <- as.list(stats) %>% setNames(stats)

  # Single indentation level for all rows
  if (is.null(names(indents_in)) && length(indents_in) == 1) {
    out <- rep(indents_in, length(levels_per_stats %>% unlist()))
    return(out)
  }

  # Apply custom indentation
  out <- .fill_in_vals_by_stats(levels_per_stats, indents_in, tern_defaults)
  out
}

# Function to loop over each stat and levels to set correct values
.fill_in_vals_by_stats <- function(levels_per_stats, user_in, tern_defaults) {
  out <- list()

  for (stat_i in names(levels_per_stats)) {
    # Get all levels of the statistic
    all_lvls <- levels_per_stats[[stat_i]]

    if ((length(all_lvls) == 1 && all_lvls == stat_i) || is.null(all_lvls)) { # One row per statistic
      out[[stat_i]] <- if (stat_i %in% names(user_in)) { # 1. Check for stat_i in user input
        user_in[[stat_i]]
      } else if (stat_i %in% names(tern_defaults)) { # 2. Check for stat_i in tern defaults
        tern_defaults[[stat_i]]
      } else { # 3. Otherwise stat_i
        stat_i
      }
    } else { # One row per combination of variable level and statistic
      # Loop over levels for each statistic
      for (lev_i in all_lvls) {
        # Construct row name (stat_i.lev_i)
        row_nm <- paste(stat_i, lev_i, sep = ".")

        out[[row_nm]] <- if (row_nm %in% names(user_in)) { # 1. Check for stat_i.lev_i in user input
          user_in[[row_nm]]
        } else if (lev_i %in% names(user_in)) { # 2. Check for lev_i in user input
          user_in[[lev_i]]
        } else if (stat_i %in% names(user_in)) { # 3. Check for stat_i in user input
          user_in[[stat_i]]
        } else if (lev_i %in% names(tern_defaults)) { # 4. Check for lev_i in tern defaults (only used for labels)
          tern_defaults[[lev_i]]
        } else if (stat_i %in% names(tern_defaults)) { # 5. Check for stat_i in tern defaults
          tern_defaults[[stat_i]]
        } else { # 6. Otherwise lev_i
          lev_i
        }
      }
    }
  }

  out
}

# Custom unlist function to retain NULL as "NULL" or NA
.unlist_keep_nulls <- function(lst, null_placeholder = "NULL", recursive = FALSE) {
  lapply(lst, function(x) if (is.null(x)) null_placeholder else x) %>%
    unlist(recursive = recursive)
}

#' Update labels according to control specifications
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Given a list of statistic labels and and a list of control parameters, updates labels with a relevant
#' control specification. For example, if control has element `conf_level` set to `0.9`, the default
#' label for statistic `mean_ci` will be updated to `"Mean 90% CI"`. Any labels that are supplied
#' via `labels_custom` will not be updated regardless of `control`.
#'
#' @param labels_default (named `character`)\cr a named vector of statistic labels to modify
#'   according to the control specifications. Labels that are explicitly defined in `labels_custom` will
#'   not be affected.
#' @param labels_custom (named `character`)\cr named vector of labels that are customized by
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
  if ("quantiles" %in% names(control) && "quantiles_lower" %in% names(labels_default) &&
    !"quantiles_lower" %in% names(labels_custom)) { # nolint
    labels_default["quantiles_lower"] <- gsub(
      "[0-9]+%-ile", paste0(control[["quantiles"]][1] * 100, "%-ile", ""),
      labels_default["quantiles_lower"]
    )
  }
  if ("quantiles" %in% names(control) && "quantiles_upper" %in% names(labels_default) &&
    !"quantiles_upper" %in% names(labels_custom)) { # nolint
    labels_default["quantiles_upper"] <- gsub(
      "[0-9]+%-ile", paste0(control[["quantiles"]][2] * 100, "%-ile", ""),
      labels_default["quantiles_upper"]
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

# tern_default_stats -----------------------------------------------------------
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
  abnormal_lab_worsen_by_baseline = c("fraction"),
  analyze_patients_exposure_in_cols = c("n_patients", "sum_exposure"),
  analyze_vars_counts = c("n", "count", "count_fraction", "count_fraction_fixed_dp", "fraction", "n_blq"),
  analyze_vars_numeric = c(
    "n", "sum", "mean", "sd", "se", "mean_sd", "mean_se", "mean_ci", "mean_sei", "mean_sdi", "mean_pval",
    "median", "mad", "median_ci", "quantiles", "iqr", "range", "min", "max", "median_range", "cv",
    "geom_mean", "geom_sd", "geom_mean_sd", "geom_mean_ci", "geom_cv",
    "median_ci_3d",
    "mean_ci_3d", "geom_mean_ci_3d"
  ),
  count_cumulative = c("count_fraction", "count_fraction_fixed_dp"),
  count_missed_doses = c("n", "count_fraction", "count_fraction_fixed_dp"),
  count_occurrences = c("count", "count_fraction", "count_fraction_fixed_dp", "fraction"),
  count_occurrences_by_grade = c("count_fraction", "count_fraction_fixed_dp"),
  count_patients_with_event = c("n", "count", "count_fraction", "count_fraction_fixed_dp", "n_blq"),
  count_patients_with_flags = c("n", "count", "count_fraction", "count_fraction_fixed_dp", "n_blq"),
  count_values = c("n", "count", "count_fraction", "count_fraction_fixed_dp", "n_blq"),
  coxph_pairwise = c("pvalue", "hr", "hr_ci", "n_tot", "n_tot_events"),
  estimate_incidence_rate = c("person_years", "n_events", "rate", "rate_ci", "n_unique", "n_rate"),
  estimate_multinomial_response = c("n_prop", "prop_ci"),
  estimate_odds_ratio = c("or_ci", "n_tot"),
  estimate_proportion = c("n_prop", "prop_ci"),
  estimate_proportion_diff = c("diff", "diff_ci"),
  summarize_ancova = c("n", "lsmean", "lsmean_diff", "lsmean_diff_ci", "pval"),
  summarize_coxreg = c("n", "hr", "ci", "pval", "pval_inter"),
  summarize_glm_count = c("n", "rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
  summarize_num_patients = c("unique", "nonunique", "unique_count"),
  summarize_patients_events_in_cols = c("unique", "all"),
  surv_time = c(
    "median", "median_ci", "median_ci_3d", "quantiles",
    "quantiles_lower", "quantiles_upper", "range_censor", "range_event", "range"
  ),
  surv_timepoint = c(
    "pt_at_risk", "event_free_rate", "rate_se", "rate_ci", "rate_diff", "rate_diff_ci", "ztest_pval",
    "event_free_rate_3d"
  ),
  tabulate_rsp_biomarkers = c("n_tot", "n_rsp", "prop", "or", "ci", "pval"),
  tabulate_rsp_subgroups = c("n", "n_rsp", "prop", "n_tot", "or", "ci", "pval", "riskdiff"),
  tabulate_survival_biomarkers = c("n_tot", "n_tot_events", "median", "hr", "ci", "pval"),
  tabulate_survival_subgroups = c("n_tot_events", "n_events", "n_tot", "n", "median", "hr", "ci", "pval"),
  test_proportion_diff = c("pval")
)

# tern_default_formats ---------------------------------------------------------
#' @describeIn default_stats_formats_labels Named vector of default formats for `tern`.
#'
#' @format
#' * `tern_default_formats` is a named vector of available default formats, with each element
#'   named for their corresponding statistic.
#'
#' @export
tern_default_formats <- c(
  ci = list(format_extreme_values_ci(2L)),
  count = "xx.",
  count_fraction = format_count_fraction,
  count_fraction_fixed_dp = format_count_fraction_fixed_dp,
  cv = "xx.x",
  fraction = format_fraction_fixed_dp,
  geom_cv = "xx.x",
  geom_mean = "xx.x",
  geom_mean_ci = "(xx.xx, xx.xx)",
  geom_mean_ci_3d = "xx.xx (xx.xx - xx.xx)",
  geom_mean_sd = "xx.x (xx.x)",
  geom_sd = "xx.x",
  iqr = "xx.x",
  lsmean = "xx.xx",
  lsmean_diff = "xx.xx",
  lsmean_diff_ci = "(xx.xx, xx.xx)",
  mad = "xx.x",
  max = "xx.x",
  mean = "xx.x",
  mean_ci = "(xx.xx, xx.xx)",
  mean_ci_3d = "xx.xx (xx.xx - xx.xx)",
  mean_pval = "x.xxxx | (<0.0001)",
  mean_sd = "xx.x (xx.x)",
  mean_sdi = "(xx.xx, xx.xx)",
  mean_se = "xx.x (xx.x)",
  mean_sei = "(xx.xx, xx.xx)",
  median = "xx.x",
  median_ci = "(xx.xx, xx.xx)",
  median_ci_3d = "xx.xx (xx.xx - xx.xx)",
  median_range = "xx.x (xx.x - xx.x)",
  min = "xx.x",
  n = "xx.",
  n_blq = "xx.",
  n_events = "xx",
  n_patients = "xx (xx.x%)",
  n_rate = "xx (xx.x)",
  n_rsp = "xx",
  n_tot = "xx",
  n_unique = "xx",
  nonunique = "xx",
  or = list(format_extreme_values(2L)),
  person_years = "xx.x",
  prop = "xx.x%",
  pval = "x.xxxx | (<0.0001)",
  pval_counts = "x.xxxx | (<0.0001)",
  quantiles = "xx.x - xx.x",
  quantiles_lower = "xx.xx (xx.xx - xx.xx)",
  quantiles_upper = "xx.xx (xx.xx - xx.xx)",
  range = "xx.x - xx.x",
  range_censor = "xx.x to xx.x",
  range_event = "xx.x to xx.x",
  rate = "xx.xxxx",
  rate_ci = "(xx.xxxx, xx.xxxx)",
  rate_ratio = "xx.xxxx",
  rate_ratio_ci = "(xx.xxxx, xx.xxxx)",
  riskdiff = "xx.x (xx.x - xx.x)",
  sd = "xx.x",
  se = "xx.x",
  sum = "xx.x",
  sum_exposure = "xx",
  unique = format_count_fraction_fixed_dp,
  unique_count = "xx"
)

# tern_default_labels ----------------------------------------------------------
#' @describeIn default_stats_formats_labels Named `character` vector of default labels for `tern`.
#'
#' @format
#' * `tern_default_labels` is a named `character` vector of available default labels, with each element
#'   named for their corresponding statistic.
#'
#' @export
tern_default_labels <- c(
  cv = "CV (%)",
  iqr = "IQR",
  geom_cv = "CV % Geometric Mean",
  geom_mean = "Geometric Mean",
  geom_mean_sd = "Geometric Mean (SD)",
  geom_mean_ci = "Geometric Mean 95% CI",
  geom_mean_ci_3d = "Geometric Mean (95% CI)",
  geom_sd = "Geometric SD",
  mad = "Median Absolute Deviation",
  max = "Maximum",
  mean = "Mean",
  mean_ci = "Mean 95% CI",
  mean_ci_3d = "Mean (95% CI)",
  mean_pval = "Mean p-value (H0: mean = 0)",
  mean_sd = "Mean (SD)",
  mean_sdi = "Mean -/+ 1xSD",
  mean_se = "Mean (SE)",
  mean_sei = "Mean -/+ 1xSE",
  median = "Median",
  median_ci = "Median 95% CI",
  median_ci_3d = "Median (95% CI)",
  median_range = "Median (Min - Max)",
  min = "Minimum",
  n = "n",
  n_blq = "n_blq",
  nonunique = "Number of events",
  pval = "p-value (t-test)", # Default for numeric
  pval_counts = "p-value (chi-squared test)", # Default for counts
  quantiles = "25% and 75%-ile",
  quantiles_lower = "25%-ile (95% CI)",
  quantiles_upper = "75%-ile (95% CI)",
  range = "Min - Max",
  rate = "Adjusted Rate",
  rate_ratio = "Adjusted Rate Ratio",
  sd = "SD",
  se = "SE",
  sum = "Sum",
  unique = "Number of patients with at least one event"
)

#' @describeIn default_stats_formats_labels Quick function to retrieve default formats for summary statistics:
#'   [analyze_vars()] and [analyze_vars_in_cols()] principally.
#'
#' @param type (`string`)\cr `"numeric"` or `"counts"`.
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
#'   Returns labels of descriptive statistics which are understood by `rtables`. Similar to `summary_formats`.
#'
#' @param include_pval (`flag`)\cr same as the `add_pval` argument in [get_stats()].
#'
#' @details
#' `summary_*` quick get functions for labels or formats uses `get_stats` and `get_labels_from_stats` or
#' `get_formats_from_stats` respectively to retrieve relevant information.
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
  met_grp <- paste0(c("analyze_vars", type), collapse = "_")
  get_labels_from_stats(get_stats(met_grp, add_pval = include_pval))
}
