#' Analyze Functions on Columns
#'
#' @description
#' These functions are wrappers of [rtables::analyze_colvars()] which apply corresponding `tern`
#' statistics functions to add an analysis to a given table layout. In particular, these functions
#' where designed to have the analysis methods split into different columns.
#'
#' * [analyze_vars_in_cols()]: fundamental tabulation of analysis methods onto columns.
#'   In other words, the analysis methods are defined in the column space, i.e. they become
#'   column labels. By changing the variable vector, the list of functions can be applied on
#'   different variables, with the caveat of having the same number of statistical functions.
#'
#' * [tabulate_rsp_subgroups()]: similarly to `analyze_vars_in_cols`, this
#'   function combines `analyze_colvars` and `summarize_row_groups` in a compact way
#'   to produce standard tables that show analysis methods as columns.
#' * [tabulate_survival_subgroups()]: this function is very similar to the above, but
#'   it is used for other tables.
#'
#' * [analyze_patients_exposure_in_cols()]: based only on `analyze_colvars`. It needs
#'   [summarize_patients_exposure_in_cols()] to leverage nesting of label rows analysis
#'   with [rtables::summarize_row_groups()].
#' * [summarize_coxreg()]: generally based on [rtables::summarize_row_groups()], it behaves
#'   similarly to `tabulate_*` functions described above as it is designed to provide
#'   specific standard tables that may contain nested structure with a combination of
#'   `summarize_row_groups()` and [rtables::analyze_colvars()].
#'
#' @seealso
#'   * [summarize_functions] for functions which are wrappers for [rtables::summarize_row_groups()].
#'   * [analyze_functions] for functions which are wrappers for [rtables::analyze()].
#'
#' @name analyze_colvars_functions
NULL
