#' Count the number of patients with a particular event
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [count_patients_with_event()] creates a layout element to calculate patient counts for a
#' user-specified set of events.
#'
#' This function analyzes primary analysis variable `vars` which indicates unique subject identifiers. Events
#' are defined by the user as a named vector via the `filters` argument, where each name corresponds to a
#' variable and each value is the value(s) that that variable takes for the event.
#'
#' If there are multiple records with the same event recorded for a patient, only one occurrence is counted.
#'
#' @inheritParams argument_convention
#' @param filters (`character`)\cr a character vector specifying the column names and flag variables
#'   to be used for counting the number of unique identifiers satisfying such conditions.
#'   Multiple column names and flags are accepted in this format
#'   `c("column_name1" = "flag1", "column_name2" = "flag2")`.
#'   Note that only equality is being accepted as condition.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("count_patients_with_event"), type = "sh")``
#'
#' @seealso [count_patients_with_flags()]
#'
#' @name count_patients_with_event
#' @order 1
NULL

#' @describeIn count_patients_with_event Statistics function which counts the number of patients for which
#'   the defined event has occurred.
#'
#' @inheritParams analyze_variables
#' @param .var (`string`)\cr name of the column that contains the unique identifier.
#'
#' @return
#' * `s_count_patients_with_event()` returns the count and fraction of unique identifiers with the defined event.
#'
#' @examples
#' s_count_patients_with_event(
#'   tern_ex_adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y"),
#' )
#'
#' s_count_patients_with_event(
#'   tern_ex_adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL")
#' )
#'
#' s_count_patients_with_event(
#'   tern_ex_adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
#'   denom = "N_col",
#'   .N_col = 456
#' )
#'
#' @export
s_count_patients_with_event <- function(df,
                                        .var,
                                        .N_col = ncol(df), # nolint
                                        .N_row = nrow(df), # nolint
                                        ...,
                                        filters,
                                        denom = c("n", "N_col", "N_row")) {
  col_names <- names(filters)
  filter_values <- filters

  checkmate::assert_subset(col_names, colnames(df))

  temp <- Map(
    function(x, y) which(df[[x]] == y),
    col_names,
    filter_values
  )
  position_satisfy_filters <- Reduce(intersect, temp)
  id_satisfy_filters <- as.character(unique(df[position_satisfy_filters, ][[.var]]))
  result <- s_count_values(
    as.character(unique(df[[.var]])),
    id_satisfy_filters,
    denom = denom,
    .N_col = .N_col,
    .N_row = .N_row
  )
  result
}

#' @describeIn count_patients_with_event Formatted analysis function which is used as `afun`
#'   in `count_patients_with_event()`.
#'
#' @return
#' * `a_count_patients_with_event()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' a_count_patients_with_event(
#'   tern_ex_adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y"),
#'   .N_col = 100,
#'   .N_row = 100
#' )
#'
#' @export
a_count_patients_with_event <- function(df,
                                        labelstr = "",
                                        ...,
                                        .stats = NULL,
                                        .stat_names = NULL,
                                        .formats = NULL,
                                        .labels = NULL,
                                        .indent_mods = NULL) {
  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)
  extra_afun_params <- retrieve_extra_afun_params(names(dots_extra_args$.additional_fun_parameters))
  dots_extra_args$.additional_fun_parameters <- NULL

  # Check for user-defined functions
  default_and_custom_stats_list <- .split_std_from_custom_stats(.stats)
  .stats <- default_and_custom_stats_list$all_stats
  custom_stat_functions <- default_and_custom_stats_list$custom_stats

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_count_patients_with_event,
    custom_stat_fnc_list = custom_stat_functions,
    args_list = c(
      df = list(df),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in formatting defaults
  .stats <- get_stats("count_patients_with_event", stats_in = .stats, custom_stats_in = names(custom_stat_functions))
  .formats <- get_formats_from_stats(.stats, .formats)
  .labels <- get_labels_from_stats(.stats, .labels)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods)

  x_stats <- x_stats[.stats]

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, extra_afun_params$.df_row, extra_afun_params$.var)

  # Get and check statistical names
  .stat_names <- get_stat_names(x_stats, .stat_names)

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = names(.labels),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

#' @describeIn count_patients_with_event Layout-creating function which can take statistics function
#'   arguments and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_patients_with_event()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_patients_with_event()` to the table layout.
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_values(
#'     "STUDYID",
#'     values = "AB12345",
#'     .stats = "count",
#'     .labels = c(count = "Total AEs")
#'   ) %>%
#'   count_patients_with_event(
#'     "SUBJID",
#'     filters = c("TRTEMFL" = "Y"),
#'     .labels = c(count_fraction = "Total number of patients with at least one adverse event"),
#'     table_names = "tbl_all"
#'   ) %>%
#'   count_patients_with_event(
#'     "SUBJID",
#'     filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
#'     .labels = c(count_fraction = "Total number of patients with fatal AEs"),
#'     table_names = "tbl_fatal"
#'   ) %>%
#'   count_patients_with_event(
#'     "SUBJID",
#'     filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL", "AEREL" = "Y"),
#'     .labels = c(count_fraction = "Total number of patients with related fatal AEs"),
#'     .indent_mods = c(count_fraction = 2L),
#'     table_names = "tbl_rel_fatal"
#'   )
#'
#' build_table(lyt, tern_ex_adae, alt_counts_df = tern_ex_adsl)
#'
#' @export
#' @order 2
count_patients_with_event <- function(lyt,
                                      vars,
                                      filters,
                                      riskdiff = FALSE,
                                      na_str = default_na_str(),
                                      nested = TRUE,
                                      show_labels = ifelse(length(vars) > 1, "visible", "hidden"),
                                      ...,
                                      table_names = vars,
                                      .stats = "count_fraction",
                                      .stat_names = NULL,
                                      .formats = list(count_fraction = format_count_fraction_fixed_dp),
                                      .labels = NULL,
                                      .indent_mods = NULL) {
  checkmate::assert_flag(riskdiff)
  afun <- if (isFALSE(riskdiff)) a_count_patients_with_event else afun_riskdiff

  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    filters = list(filters),
    if (!isFALSE(riskdiff)) list(afun = list("s_count_patients_with_event" = a_count_patients_with_event)),
    ...
  )

  # Append additional info from layout to the analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(afun) <- c(formals(afun), extra_args[[".additional_fun_parameters"]])

  analyze(
    lyt = lyt,
    vars = vars,
    afun = afun,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    show_labels = show_labels,
    table_names = table_names
  )
}
