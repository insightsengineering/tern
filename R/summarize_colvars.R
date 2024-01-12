#' Summarize Variables in Columns
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This analyze function uses the S3 generic function [s_summary()] to summarize different variables
#' that are arranged in columns. Additional standard formatting arguments are available. It is a
#' minimal wrapper for [rtables::analyze_colvars()]. The latter function is meant to add different
#' analysis methods for each column variables as different rows. To have the analysis methods as
#' column labels, please refer to [analyze_vars_in_cols()].
#'
#' @inheritParams argument_convention
#' @param ... arguments passed to `s_summary()`.
#' @param .indent_mods (named `vector` of `integer`)\cr indent modifiers for the labels. Each element of the vector
#'   should be a name-value pair with name corresponding to a statistic specified in `.stats` and value the indentation
#'   for that statistic's row label.
#'
#' @return
#' A layout object suitable for passing to further layouting functions, or to [rtables::build_table()].
#' Adding this function to an `rtable` layout will summarize the given variables, arrange the output
#' in columns, and add it to the table layout.
#'
#' @seealso [rtables::split_cols_by_multivar()] and [`analyze_colvars_functions`].
#'
#' @examples
#' dta_test <- data.frame(
#'   USUBJID = rep(1:6, each = 3),
#'   PARAMCD = rep("lab", 6 * 3),
#'   AVISIT = rep(paste0("V", 1:3), 6),
#'   ARM = rep(LETTERS[1:3], rep(6, 3)),
#'   AVAL = c(9:1, rep(NA, 9)),
#'   CHG = c(1:9, rep(NA, 9))
#' )
#'
#' ## Default output within a `rtables` pipeline.
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("AVISIT") %>%
#'   split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
#'   summarize_colvars() %>%
#'   build_table(dta_test)
#'
#' ## Selection of statistics, formats and labels also work.
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("AVISIT") %>%
#'   split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
#'   summarize_colvars(
#'     .stats = c("n", "mean_sd"),
#'     .formats = c("mean_sd" = "xx.x, xx.x"),
#'     .labels = c(n = "n", mean_sd = "Mean, SD")
#'   ) %>%
#'   build_table(dta_test)
#'
#' ## Use arguments interpreted by `s_summary`.
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("AVISIT") %>%
#'   split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
#'   summarize_colvars(na.rm = FALSE) %>%
#'   build_table(dta_test)
#'
#' @export
summarize_colvars <- function(lyt,
                              ...,
                              na_level = lifecycle::deprecated(),
                              na_str = default_na_str(),
                              .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
                              .formats = NULL,
                              .labels = NULL,
                              .indent_mods = NULL) {
  if (lifecycle::is_present(na_level)) {
    lifecycle::deprecate_warn("0.9.1", "summarize_colvars(na_level)", "summarize_colvars(na_str)")
    na_str <- na_level
  }

  extra_args <- list(.stats = .stats, na_str = na_str, ...)
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  analyze_colvars(
    lyt,
    afun = a_summary,
    na_str = na_str,
    extra_args = extra_args
  )
}
