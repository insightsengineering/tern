#' Summarize Variables in Columns
#'
#' This Analyze Function uses the new S3 generic function [s_summary()] to summarize
#' different variables that are arranged in columns.
#' Additional standard formatting arguments are available.
#'
#' @inheritParams argument_convention
#' @param ... arguments passed to `s_summary()`.
#'
#' @note Currently a warning is thrown when building the table. This will be fixed in an upcoming
#' new rtables version. Similarly, it is not yet possible to select statistics with `.stats`, which
#' leads to an error.
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
#' l <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("AVISIT") %>%
#'   split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
#'   summarize_colvars()
#' tab <- testthat::expect_warning(build_table(l, dta_test))
#' tab
#'
#' ## Selection of statistics does not work yet, but formats and labels do.
#' l <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("AVISIT") %>%
#'   split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
#'   summarize_colvars(
#'     # .stats = c("n", "mean_sd")  # not possible yet
#'     .formats = c("mean_sd" = "xx.x, xx.x"),
#'     .labels = c(n = "n", mean_sd = "Mean, SD")
#'   )
#' tab <- testthat::expect_warning(build_table(l, dta_test))
#' tab
#'
#' ## Use arguments interpreted by `s_summary`.
#' l <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("AVISIT") %>%
#'   split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
#'   summarize_colvars(na.rm = FALSE)
#' tab <- testthat::expect_warning(build_table(l, dta_test))
#' tab
#'
summarize_colvars <- function(lyt,
                              .stats = c("n", "mean_sd", "median", "range", "count_fraction"),
                              ...) {

  afun <- format_wrap_x(
    s_summary,
    indent_mods = c(
      n = 0L,
      mean_sd = 0L,
      median = 0L,
      range = 0L,
      count = 0L,
      count_fraction = 0L
    ),
    formats = c(
      n = "xx",
      mean_sd = "xx.x (xx.x)",
      median = "xx.x",
      range = "xx.x - xx.x",
      count = "xx",
      count_fraction = "xx (xx.x%)"
    )
  )

  analyze_colvars(
    lyt,
    afun = afun,
    extra_args = c(list(.stats = .stats), list(...))
  )
}
