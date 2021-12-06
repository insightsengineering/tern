#' Summarize the Change from Baseline or Absolute Baseline Values
#'
#' The primary analysis variable `.var` indicates the numerical change from baseline results,
#' and additional required secondary analysis variables are `value` and `baseline_flag`.
#' Depending on the baseline flag, either the absolute baseline values (at baseline)
#' or the change from baseline values (post-baseline) are then summarized.
#'
#' @name summarize_change
#'
NULL

#' @inheritParams argument_convention
#' @describeIn summarize_change Statistics Function that summarizes baseline or post-baseline visits.
#' @return See [s_summary.numeric()] for the return values.
#' @note The data in `df` must be either all be from baseline or post-baseline visits. Otherwise
#'   an error will be thrown.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   chg = c(1, 2, 3),
#'   is_bl = c(TRUE, TRUE, TRUE),
#'   val = c(4, 5, 6)
#' )
#' s_change_from_baseline(
#'   df,
#'   .var = "chg",
#'   variables = list(value = "val", baseline_flag = "is_bl")
#' )
#'
s_change_from_baseline <- function(df,
                                   .var,
                                   variables,
                                   na.rm = TRUE,  #nolint
                                   ...) {
  assertthat::assert_that(
    is_df_with_variables(df, c(variables, list(chg = .var))),
    is.numeric(df[[variables$value]]),
    is.numeric(df[[.var]]),
    is.logical(df[[variables$baseline_flag]]),
    length(unique(df[[variables$baseline_flag]])) < 2
  )

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

#' @describeIn summarize_change Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' a_change_from_baseline(
#'   df,
#'   .var = "chg",
#'   variables = list(value = "val", baseline_flag = "is_bl")
#' )
#'
a_change_from_baseline <- make_afun(
  s_change_from_baseline,
  .formats = c(
    n = "xx",
    mean_sd = "xx.xx (xx.xx)",
    median = "xx.xx",
    range = "xx.xx - xx.xx",
    mean_ci = "(xx.xx, xx.xx)",
    median_ci = "(xx.xx, xx.xx)"
  ),
  .labels = c(
    mean_sd = "Mean (SD)",
    median = "Median",
    range = "Min - Max"
  )
)

#' @describeIn summarize_change Analyze Function for change from baseline analysis.
#'   To be used after a split on visits in the layout, such that each data
#'   subset only contains either baseline or post-baseline data. Allows additional
#'   formatting options.
#' @inheritParams argument_convention
#'
#' @export
#' @examples
#'
#' # `summarize_change()`
#'
#' ## Fabricated dataset.
#' library(dplyr)
#'
#' dta_test <- data.frame(
#'   USUBJID = rep(1:6, each = 3),
#'   AVISIT = rep(paste0("V", 1:3), 6),
#'   ARM = rep(LETTERS[1:3], rep(6, 3)),
#'   AVAL = c(9:1, rep(NA, 9))
#'   ) %>%
#'   dplyr::mutate(ABLFLL = AVISIT == "V1") %>%
#'   dplyr::group_by(USUBJID) %>%
#'   dplyr::mutate(
#'     BLVAL = AVAL[ABLFLL],
#'     CHG = AVAL - BLVAL
#'   ) %>%
#'   dplyr::ungroup()
#'
#' results <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("AVISIT") %>%
#'   summarize_change("CHG", variables = list(value = "AVAL", baseline_flag = "ABLFLL")) %>%
#'   build_table(dta_test)
#'
#' \dontrun{
#' Viewer(results)
#' }
#'
summarize_change <- function(lyt,
                             vars,
                             ...,
                             table_names = vars,
                             .stats = c("n", "mean_sd", "median", "range"),
                             .formats = NULL,
                             .labels = NULL,
                             .indent_mods = NULL) {
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
    extra_args = list(...),
    table_names = table_names
  )
}
