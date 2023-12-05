#' Analyze Functions
#'
#' @description
#'
#' These functions are wrappers of [rtables::analyze()] which apply corresponding `tern` statistics functions
#' to add an analysis to a given table layout:
#'
#' * [analyze_num_patients()]
#' * [compare_vars()]
#' * [count_abnormal()]
#' * [count_abnormal_by_baseline()]
#' * [count_abnormal_by_marked()]
#' * [count_abnormal_by_worst_grade()]
#' * [count_cumulative()]
#' * [count_missed_doses()]
#' * [count_occurrences()]
#' * [count_occurrences_by_grade()]
#' * [count_patients_events_in_cols()]
#' * [count_patients_with_event()]
#' * [count_patients_with_flags()]
#' * [count_values()]
#' * [coxph_pairwise()]
#' * [estimate_incidence_rate()]
#' * [estimate_multinomial_rsp()]
#' * [estimate_odds_ratio()]
#' * [estimate_proportion()]
#' * [estimate_proportion_diff()]
#' * [summarize_ancova()]
#' * [summarize_colvars()]: even if this function uses [rtables::analyze_colvars()],
#'   it applies the analysis methods as different rows for one or more
#'   variables that are split into different columns. In comparison, [analyze_colvars_functions]
#'   leverage `analyze_colvars` to have the context split in rows and the analysis
#'   methods in columns.
#' * [summarize_change()]
#' * [analyze_vars()]: formerly known as `summarize_vars()`, it was renamed to reflect
#'   core function [rtables::analyze()].
#' * [surv_time()]
#' * [surv_timepoint()]
#' * [test_proportion_diff()]
#'
#' @seealso
#'   * [summarize_functions] for functions which are wrappers for [rtables::summarize_row_groups()].
#'   * [analyze_colvars_functions] for functions that are wrappers for [rtables::analyze_colvars()].
#'
#' @name analyze_functions
NULL
