#' Summarize the Change from Baseline or Absolute Baseline Values
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The primary analysis variable `.var` indicates the numerical change from baseline results,
#' and additional required secondary analysis variables are `value` and `baseline_flag`.
#' Depending on the baseline flag, either the absolute baseline values (at baseline)
#' or the change from baseline values (post-baseline) are then summarized.
#'
#' @inheritParams argument_convention
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("analyze_vars_numeric)`
#'   to see available statistics for this function.
#'
#' @name summarize_change
#' @order 1
NULL

#' @describeIn summarize_change Statistics function that summarizes baseline or post-baseline visits.
#'
#' @return
#' * `s_change_from_baseline()` returns the same values returned by [s_summary.numeric()].
#'
#' @note The data in `df` must be either all be from baseline or post-baseline visits. Otherwise
#'   an error will be thrown.
#'
#' @keywords internal
s_change_from_baseline <- function(df,
                                   .var,
                                   variables,
                                   na.rm = TRUE, # nolint
                                   ...) {
  checkmate::assert_numeric(df[[variables$value]])
  checkmate::assert_numeric(df[[.var]])
  checkmate::assert_logical(df[[variables$baseline_flag]])
  checkmate::assert_vector(unique(df[[variables$baseline_flag]]), max.len = 1)
  assert_df_with_variables(df, c(variables, list(chg = .var)))

  combined <- ifelse(
    df[[variables$baseline_flag]],
    df[[variables$value]],
    df[[.var]]
  )
  if (is.logical(combined) && identical(length(combined), 0L)) {
    combined <- numeric(0)
  }
  s_summary(combined, na.rm = na.rm, ...)
}

#' @describeIn summarize_change Formatted analysis function which is used as `afun` in `summarize_change()`.
#'
#' @return
#' * `a_change_from_baseline()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_change_from_baseline <- make_afun(
  s_change_from_baseline,
  .formats = c(
    n = "xx",
    mean_sd = "xx.xx (xx.xx)",
    mean_se = "xx.xx (xx.xx)",
    median = "xx.xx",
    range = "xx.xx - xx.xx",
    mean_ci = "(xx.xx, xx.xx)",
    median_ci = "(xx.xx, xx.xx)",
    mean_pval = "xx.xx"
  ),
  .labels = c(
    mean_sd = "Mean (SD)",
    mean_se = "Mean (SE)",
    median = "Median",
    range = "Min - Max"
  )
)

#' @describeIn summarize_change Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `summarize_change()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_change_from_baseline()` to the table layout.
#'
#' @note To be used after a split on visits in the layout, such that each data subset only contains
#'   either baseline or post-baseline data.
#'
#' @examples
#' library(dplyr)
#'
#' ## Fabricate dataset
#' dta_test <- data.frame(
#'   USUBJID = rep(1:6, each = 3),
#'   AVISIT = rep(paste0("V", 1:3), 6),
#'   ARM = rep(LETTERS[1:3], rep(6, 3)),
#'   AVAL = c(9:1, rep(NA, 9))
#' ) %>%
#'   mutate(ABLFLL = AVISIT == "V1") %>%
#'   group_by(USUBJID) %>%
#'   mutate(
#'     BLVAL = AVAL[ABLFLL],
#'     CHG = AVAL - BLVAL
#'   ) %>%
#'   ungroup()
#'
#' results <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("AVISIT") %>%
#'   summarize_change("CHG", variables = list(value = "AVAL", baseline_flag = "ABLFLL")) %>%
#'   build_table(dta_test)
#'
#' results
#'
#' @export
#' @order 2
summarize_change <- function(lyt,
                             vars,
                             variables,
                             na_str = default_na_str(),
                             nested = TRUE,
                             ...,
                             table_names = vars,
                             .stats = c("n", "mean_sd", "median", "range"),
                             .formats = NULL,
                             .labels = NULL,
                             .indent_mods = NULL) {
  extra_args <- list(variables = variables, ...)

  afun <- make_afun(
    a_change_from_baseline,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )

  analyze(
    lyt,
    vars,
    afun = afun,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    table_names = table_names
  )
}
