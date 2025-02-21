#' Count occurrences
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [count_occurrences()] creates a layout element to calculate occurrence counts for patients.
#'
#' This function analyzes the variable(s) supplied to `vars` and returns a table of occurrence counts for
#' each unique value (or level) of the variable(s). This variable (or variables) must be
#' non-numeric. The `id` variable is used to indicate unique subject identifiers (defaults to `USUBJID`).
#'
#' If there are multiple occurrences of the same value recorded for a patient, the value is only counted once.
#'
#' The summarize function [summarize_occurrences()] performs the same function as [count_occurrences()] except it
#' creates content rows, not data rows, to summarize the current table row/column context and operates on the level of
#' the latest row split or the root of the table if no row splits have occurred.
#'
#' @inheritParams argument_convention
#' @param drop (`flag`)\cr whether non-appearing occurrence levels should be dropped from the resulting table.
#'   Note that in that case the remaining occurrence levels in the table are sorted alphabetically.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("count_occurrences"), type = "sh")``
#'
#' @note By default, occurrences which don't appear in a given row split are dropped from the table and
#'   the occurrences in the table are sorted alphabetically per row split. Therefore, the corresponding layout
#'   needs to use `split_fun = drop_split_levels` in the `split_rows_by` calls. Use `drop = FALSE` if you would
#'   like to show all occurrences.
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(
#'   USUBJID = as.character(c(
#'     1, 1, 2, 4, 4, 4,
#'     6, 6, 6, 7, 7, 8
#'   )),
#'   MHDECOD = c(
#'     "MH1", "MH2", "MH1", "MH1", "MH1", "MH3",
#'     "MH2", "MH2", "MH3", "MH1", "MH2", "MH4"
#'   ),
#'   ARM = rep(c("A", "B"), each = 6),
#'   SEX = c("F", "F", "M", "M", "M", "M", "F", "F", "F", "M", "M", "F")
#' )
#' df_adsl <- df %>%
#'   select(USUBJID, ARM) %>%
#'   unique()
#'
#' @name count_occurrences
#' @order 1
NULL

#' @describeIn count_occurrences Statistics function which counts number of patients that report an
#' occurrence.
#'
#' @param denom (`string`)\cr choice of denominator for proportion. Options are:
#'   * `N_col`: total number of patients in this column across rows.
#'   * `n`: number of patients with any occurrences.
#'   * `N_row`: total number of patients in this row across columns.
#'
#' @return
#' * `s_count_occurrences()` returns a list with:
#'   * `count`: list of counts with one element per occurrence.
#'   * `count_fraction`: list of counts and fractions with one element per occurrence.
#'   * `fraction`: list of numerators and denominators with one element per occurrence.
#'
#' @examples
#' # Count unique occurrences per subject.
#' s_count_occurrences(
#'   df,
#'   .N_col = 4L,
#'   .N_row = 4L,
#'   .df_row = df,
#'   .var = "MHDECOD",
#'   id = "USUBJID"
#' )
#'
#' @export
s_count_occurrences <- function(df,
                                .var = "MHDECOD",
                                .N_col, # nolint
                                .N_row, # nolint
                                .df_row,
                                ...,
                                drop = TRUE,
                                id = "USUBJID",
                                denom = c("N_col", "n", "N_row")) {
  checkmate::assert_flag(drop)
  assert_df_with_variables(df, list(range = .var, id = id))
  checkmate::assert_count(.N_col)
  checkmate::assert_multi_class(df[[.var]], classes = c("factor", "character"))
  checkmate::assert_multi_class(df[[id]], classes = c("factor", "character"))

  occurrences <- if (drop) {
    # Note that we don't try to preserve original level order here since a) that would required
    # more time to look up in large original levels and b) that would fail for character input variable.
    occurrence_levels <- sort(unique(.df_row[[.var]]))
    if (length(occurrence_levels) == 0) {
      stop(
        "no empty `.df_row` input allowed when `drop = TRUE`,",
        " please use `split_fun = drop_split_levels` in the `rtables` `split_rows_by` calls"
      )
    }
    factor(df[[.var]], levels = occurrence_levels)
  } else {
    df[[.var]]
  }
  ids <- factor(df[[id]])
  denom <- match.arg(denom) %>%
    switch(
      n = nlevels(ids),
      N_row = .N_row,
      N_col = .N_col
    )
  has_occurrence_per_id <- table(occurrences, ids) > 0
  n_ids_per_occurrence <- as.list(rowSums(has_occurrence_per_id))
  cur_count_fraction <- lapply(
    n_ids_per_occurrence,
    function(i, denom) {
      if (i == 0 && denom == 0) {
        c(0, 0)
      } else {
        c(i, i / denom)
      }
    },
    denom = denom
  )

  list(
    count = n_ids_per_occurrence,
    count_fraction = cur_count_fraction,
    count_fraction_fixed_dp = cur_count_fraction,
    fraction = lapply(
      n_ids_per_occurrence,
      function(i, denom) c("num" = i, "denom" = denom),
      denom = denom
    )
  )
}

#' @describeIn count_occurrences Formatted analysis function which is used as `afun`
#'   in `count_occurrences()`.
#'
#' @return
#' * `a_count_occurrences()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' a_count_occurrences(
#'   df,
#'   .N_col = 4L,
#'   .df_row = df,
#'   .var = "MHDECOD",
#'   id = "USUBJID"
#' )
#'
#' @export
a_count_occurrences <- function(df,
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

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_count_occurrences,
    custom_stat_fnc_list = NULL,
    args_list = c(
      df = list(df),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in formatting defaults
  .stats <- get_stats("count_occurrences", stats_in = .stats)
  x_stats <- x_stats[.stats]
  levels_per_stats <- lapply(x_stats, names)
  .formats <- get_formats_from_stats(.stats, .formats, levels_per_stats)
  .labels <- get_labels_from_stats(.stats, .labels, levels_per_stats)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods, levels_per_stats)

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, .df_row, .var)

  in_rows(
    .list = x_stats %>% .unlist_keep_nulls(),
    .formats = .formats,
    .names = .labels %>% .unlist_keep_nulls(),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

#' @describeIn count_occurrences Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_occurrences()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_occurrences()` to the table layout.
#'
#' @examples
#' # Create table layout
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_occurrences(vars = "MHDECOD", .stats = c("count_fraction"))
#'
#' # Apply table layout to data and produce `rtable` object
#' tbl <- lyt %>%
#'   build_table(df, alt_counts_df = df_adsl) %>%
#'   prune_table()
#'
#' tbl
#'
#' @export
#' @order 2
count_occurrences <- function(lyt,
                              vars,
                              id = "USUBJID",
                              drop = TRUE,
                              var_labels = vars,
                              show_labels = "hidden",
                              riskdiff = FALSE,
                              na_str = default_na_str(),
                              nested = TRUE,
                              ...,
                              table_names = vars,
                              .stats = "count_fraction_fixed_dp",
                              .stat_names = NULL,
                              .formats = NULL,
                              .labels = NULL,
                              .indent_mods = NULL) {
  checkmate::assert_flag(riskdiff)
  afun <- if (isFALSE(riskdiff)) a_count_occurrences else afun_riskdiff

  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    id = id, drop = drop,
    if (!isFALSE(riskdiff)) list(afun = list("s_count_occurrences" = a_count_occurrences)),
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
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names
  )
}

#' @describeIn count_occurrences Layout-creating function which can take content function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::summarize_row_groups()].
#'
#' @return
#' * `summarize_occurrences()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted content rows
#'   containing the statistics from `s_count_occurrences()` to the table layout.
#'
#' @examples
#' # Layout creating function with custom format.
#' basic_table() %>%
#'   add_colcounts() %>%
#'   split_rows_by("SEX", child_labels = "visible") %>%
#'   summarize_occurrences(
#'     var = "MHDECOD",
#'     .formats = c("count_fraction" = "xx.xx (xx.xx%)")
#'   ) %>%
#'   build_table(df, alt_counts_df = df_adsl)
#'
#' @export
#' @order 3
summarize_occurrences <- function(lyt,
                                  var,
                                  id = "USUBJID",
                                  drop = TRUE,
                                  riskdiff = FALSE,
                                  na_str = default_na_str(),
                                  ...,
                                  .stats = "count_fraction_fixed_dp",
                                  .stat_names = NULL,
                                  .formats = NULL,
                                  .indent_mods = NULL,
                                  .labels = NULL) {
  checkmate::assert_flag(riskdiff)
  afun <- if (isFALSE(riskdiff)) a_count_occurrences else afun_riskdiff

  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    id = id, drop = drop,
    if (!isFALSE(riskdiff)) list(afun = list("s_count_occurrences" = a_count_occurrences)),
    ...
  )

  # Append additional info from layout to the analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(afun) <- c(formals(afun), extra_args[[".additional_fun_parameters"]])

  summarize_row_groups(
    lyt = lyt,
    var = var,
    cfun = afun,
    na_str = na_str,
    extra_args = extra_args
  )
}
