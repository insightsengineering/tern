#' Analyze Functions on Columns
#'
#' @description
#'
#' These functions are wrappers of [rtables::analyze_colvars()] which apply corresponding `tern` statistics functions
#' to add an analysis to a given table layout:
#'
#' * [analyze_vars_in_cols()] (extended wrapper, never used in other `tern` functions)
#' * [summarize_colvars()] (low level wrapper, never used in other `tern` functions)
#' * [summarize_coxreg()] (if the analysis is multivariate)
#' * [tabulate_rsp_subgroups()]
#' * [tabulate_survival_subgroups()]
#'
#' @seealso
#'   * [summarize_functions] for functions which are wrappers for [rtables::summarize_row_groups()].
#'   * [analyze_functions] for functions which are wrappers for [rtables::analyze()].
#'
#' @name analyze_colvars_functions
NULL
