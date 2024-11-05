#' Summarize change from baseline values or absolute baseline values
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [summarize_change()] creates a layout element to summarize the change from baseline or absolute
#' baseline values. The primary analysis variable `vars` indicates the numerical change from baseline results.
#'
#' Required secondary analysis variables `value` and `baseline_flag` can be supplied to the function via
#' the `variables` argument. The `value` element should be the name of the analysis value variable, and the
#' `baseline_flag` element should be the name of the flag variable that indicates whether or not records contain
#' baseline values. Depending on the baseline flag given, either the absolute baseline values (at baseline)
#' or the change from baseline values (post-baseline) are then summarized.
#'
#' @inheritParams argument_convention
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("analyze_vars_numeric"))``
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
s_change_from_baseline <- function(df, ...) {
  # s_summary should get na.rm
  args_list <- list(...)
  .var <- args_list[[".var"]]
  variables <- args_list[["variables"]]

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
  s_summary(combined, ...)
}

#' @describeIn summarize_change Formatted analysis function which is used as `afun` in `summarize_change()`.
#'
#' @return
#' * `a_change_from_baseline()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_change_from_baseline <- function(df,
                                   ...,
                                   .stats = NULL,
                                   .formats = NULL,
                                   .labels = NULL,
                                   .indent_mods = NULL) {

  # Check if there are user-defined functions
  default_and_custom_stats_list <- .split_default_from_custom_stats(.stats)
  .stats <- default_and_custom_stats_list$default_stats
  custom_stat_functions <- default_and_custom_stats_list$custom_stats

  # Adding automatically extra parameters to the statistic function (see ?rtables::additional_fun_params)
  extra_afun_params <- names(get_additional_analysis_fun_parameters(add_alt_df = FALSE))
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_change_from_baseline,
    custom_stat_fnc_list = custom_stat_functions,
    args_list = c(
      df = list(df),
      retrieve_extra_afun_params(extra_afun_params),
      list(...)
    )
  )

  # Fill in with formatting defaults if needed
  .stats <- get_stats("analyze_vars_numeric", stats_in = .stats)
  .formats <- get_formats_from_stats(.stats, .formats)
  .labels <- get_labels_from_stats(.stats, .labels)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods)

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, .df_row, .var)

  in_rows(
    .list = x_stats[.stats],
    .formats = .formats,
    .names = names(.labels),
    .labels = .labels,
    .indent_mods = .indent_mods
    # .format_na_strs = na_str # set above level
  )
}

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
#' # Fabricate dataset
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
                             var_labels = vars,
                             na_str = default_na_str(),
                             na_rm = TRUE,
                             nested = TRUE,
                             show_labels = "default",
                             table_names = vars,
                             section_div = NA_character_,
                             ...,
                             .stats = c("n", "mean_sd", "median", "range"),
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
                             ),
                             .indent_mods = NULL) {
  # Extra args must contain .stats, .formats, .labels, .indent_mods - sent to the analysis level
  extra_args <- list(".stats" = .stats)
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Adding additional arguments to the analysis function (depends on the specific call)
  extra_args <- c(extra_args, "variables" = list(variables), ...)

  # Adding all additional information from layout to analysis functions (see ?rtables::additional_fun_params)
  formals(a_change_from_baseline) <- c(
    formals(a_change_from_baseline),
    get_additional_analysis_fun_parameters()
  )

  # Main analysis call - Nothing with .* -> these should be dedicated to the analysis function
  analyze(
    lyt = lyt,
    vars = vars,
    var_labels = var_labels,
    afun = a_change_from_baseline,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    inclNAs = na_rm, # adds na.rm = TRUE to the analysis function
    show_labels = show_labels,
    table_names = table_names,
    section_div = section_div
  )
}
