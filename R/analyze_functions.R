#' Analyze Functions
#'
#' @description
#' These functions are wrappers of [rtables::analyze()], applying custom `tern` statistics functions
#' to return the corresponding analysis within a given table layout.
#'
#' * [analyze_num_patients()]
#' * [analyze_vars_in_cols()]
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
#' * [cox_pairwise()]
#' * [estimate_incidence_rate()]
#' * [estimate_multinomial_rsp()]
#' * [estimate_odds_ratio()]
#' * [estimate_proportion()]
#' * [estimate_proportion_diff()]
#' * [summarize_ancova()]
#' * [summarize_change()]
#' * [summarize_vars()]
#' * [surv_time()]
#' * [surv_timepoint()]
#' * [test_proportion_diff()]
#'
#' @seealso [summarize_functions] for functions which are wrappers for [rtables::summarize_row_groups()], and\cr
#' [analyze_vars_in_cols()] which is a wrapper for [rtables::analyze_colvars()].
#'
#' @name analyze_functions
NULL
