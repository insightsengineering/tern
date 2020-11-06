#' Summarize Variables in Columns
#'
#' This Analyze Function uses the new S3 generic function [s_summary()] to summarize
#' different variables that are arranged in columns.
#' Additional standard formatting arguments are available.
#'
#' @inheritParams argument_convention
#' @param ... arguments passed to `s_summary()`.
#'
#' @seealso [rtables::split_cols_by_multivar()]
#'
#' @template formatting_arguments
#'
#' @export
#' @examples
#'
#' dta_test <- data.frame(
#'   USUBJID = rep(1:6, each = 3),
#'   PARAMCD = rep("lab", 6*3),
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
summarize_colvars <- function(lyt,
                              ...,
                              .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
                              .formats = NULL,
                              .labels = NULL,
                              .indent_mods = NULL) {

  afun <- create_afun_summary(.stats, .formats, .labels, .indent_mods)

  analyze_colvars(
    lyt,
    afun = afun,
    extra_args = list(...)
  )
}
