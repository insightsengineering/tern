#' Summarize Functions
#'
#' @description
#'
#' These functions are wrappers for [`rtables::summarize_row_groups()`], applying corresponding `tern` content functions
#' to add summary rows to a given table layout:
#'
#' * [add_rowcounts()]
#' * [estimate_multinomial_response()] (with [rtables::analyze()])
#' * [h_tab_one_biomarker()] (probably to deprecate)
#' * [logistic_summary_by_flag()]
#' * [summarize_num_patients()]
#' * [summarize_occurrences()]
#' * [summarize_occurrences_by_grade()]
#' * [summarize_patients_events_in_cols()]
#' * [summarize_patients_exposure_in_cols()]
#' * [tabulate_rsp_subgroups()]
#'
#' Additionally, the [summarize_coxreg()] function utilizes [`rtables::summarize_row_groups()`]
#' (in combination with several other `rtables` functions like [rtables::analyze_colvars()]) to
#' output a Cox regression summary table.
#'
#' @seealso
#'   * [analyze_functions] for functions which are wrappers for [rtables::analyze()].
#'   * [analyze_colvars_functions] for functions that are wrappers for [rtables::analyze_colvars()].
#'
#' @name summarize_functions
NULL
