#' Count number of patients with missed doses by thresholds
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function creates a layout element to calculate cumulative counts of patients with number of missed
#' doses at least equal to user-specified threshold values.
#'
#' This function analyzes numeric variable `vars`, a variable with numbers of missed doses,
#' against the threshold values supplied to the `thresholds` argument as a numeric vector. This function
#' assumes that every row of the given data frame corresponds to a unique patient.
#'
#' @inheritParams s_count_cumulative
#' @inheritParams argument_convention
#' @param thresholds (`numeric`)\cr minimum number of missed doses the patients had.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("count_missed_doses"), type = "sh")``
#'
#' @seealso
#' * Relevant description function [d_count_missed_doses()] which generates labels for [count_missed_doses()].
#' * Similar analyze function [count_cumulative()] which more generally counts cumulative values and has more
#'   options for threshold handling, but uses different labels.
#'
#' @name count_missed_doses
#' @order 1
NULL

#' Description function that calculates labels for `s_count_missed_doses()`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams s_count_missed_doses
#'
#' @return [d_count_missed_doses()] returns a named `character` vector with the labels.
#'
#' @seealso [s_count_missed_doses()]
#'
#' @export
d_count_missed_doses <- function(thresholds) {
  paste0("At least ", thresholds, " missed dose", ifelse(thresholds > 1, "s", ""))
}

#' @describeIn count_missed_doses Statistics function to count patients with missed doses.
#'
#' @return
#' * `s_count_missed_doses()` returns the statistics `n` and `count_fraction` with one element for each threshold.
#'
#' @keywords internal
s_count_missed_doses <- function(x,
                                 thresholds,
                                 .N_col, # nolint
                                 .N_row, # nolint
                                 denom = c("N_col", "n", "N_row"),
                                 ...) {
  stat <- s_count_cumulative(
    x = x,
    thresholds = thresholds,
    lower_tail = FALSE,
    include_eq = TRUE,
    .N_col = .N_col,
    .N_row = .N_row,
    denom = denom,
    ...
  )
  labels <- d_count_missed_doses(thresholds)
  for (i in seq_along(stat$count_fraction)) {
    stat$count_fraction[[i]] <- formatters::with_label(stat$count_fraction[[i]], label = labels[i])
  }

  c(list(n = n_available(x)), stat)
}

#' @describeIn count_missed_doses Formatted analysis function which is used as `afun`
#'   in `count_missed_doses()`.
#'
#' @return
#' * `a_count_missed_doses()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_count_missed_doses <- function(x,
                                 ...,
                                 .stats = NULL,
                                 .stat_names = NULL,
                                 .formats = NULL,
                                 .labels = NULL,
                                 .indent_mods = NULL) {
  dots_extra_args <- list(...)

  # Check if there are user-defined functions
  default_and_custom_stats_list <- .split_std_from_custom_stats(.stats)
  .stats <- default_and_custom_stats_list$all_stats
  custom_stat_functions <- default_and_custom_stats_list$custom_stats

  # Adding automatically extra parameters to the statistic function (see ?rtables::additional_fun_params)
  extra_afun_params <- retrieve_extra_afun_params(
    names(dots_extra_args$.additional_fun_parameters)
  )
  dots_extra_args$.additional_fun_parameters <- NULL # After extraction we do not need them anymore

  # Main statistical functions application
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_count_missed_doses,
    custom_stat_fnc_list = custom_stat_functions,
    args_list = c(
      x = list(x),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in with stats defaults if needed
  .stats <- get_stats("count_missed_doses",
    stats_in = .stats,
    custom_stats_in = names(custom_stat_functions)
  )

  x_stats <- x_stats[.stats]
  levels_per_stats <- lapply(x_stats, names)

  # Fill in formats/indents/labels with custom input and defaults
  .formats <- get_formats_from_stats(.stats, .formats, levels_per_stats)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods, levels_per_stats)
  .labels <- get_labels_from_stats(
    .stats, .labels, levels_per_stats,
    label_attr_from_stats = sapply(.unlist_keep_nulls(x_stats), attr, "label")
  )

  # Unlist stats
  x_stats <- x_stats %>%
    .unlist_keep_nulls() %>%
    setNames(names(.formats))

  # Auto format handling
  .formats <- apply_auto_formatting(
    .formats,
    x_stats,
    extra_afun_params$.df_row,
    extra_afun_params$.var
  )

  # Get and check statistical names from defaults
  .stat_names <- get_stat_names(x_stats, .stat_names) # note is x_stats

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = names(.labels),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

#' @describeIn count_missed_doses Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_missed_doses()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_missed_doses()` to the table layout.
#'
#' @examples
#' library(dplyr)
#'
#' anl <- tern_ex_adsl %>%
#'   distinct(STUDYID, USUBJID, ARM) %>%
#'   mutate(
#'     PARAMCD = "TNDOSMIS",
#'     PARAM = "Total number of missed doses during study",
#'     AVAL = sample(0:20, size = nrow(tern_ex_adsl), replace = TRUE),
#'     AVALC = ""
#'   )
#'
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_missed_doses("AVAL", thresholds = c(1, 5, 10, 15), var_labels = "Missed Doses") %>%
#'   build_table(anl, alt_counts_df = tern_ex_adsl)
#'
#' @export
#' @order 2
count_missed_doses <- function(lyt,
                               vars,
                               thresholds,
                               var_labels = vars,
                               show_labels = "visible",
                               na_str = default_na_str(),
                               nested = TRUE,
                               table_names = vars,
                               ...,
                               na_rm = TRUE,
                               .stats = c("n", "count_fraction"),
                               .stat_names = NULL,
                               .formats = NULL,
                               .labels = NULL,
                               .indent_mods = NULL) {
  # Depending on main functions
  extra_args <- list(
    "na_rm" = na_rm,
    "thresholds" = thresholds,
    ...
  )

  # Needed defaults
  if (!is.null(.stats)) extra_args[[".stats"]] <- .stats
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Adding all additional information from layout to analysis functions (see ?rtables::additional_fun_params)
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_count_missed_doses) <- c(
    formals(a_count_missed_doses),
    extra_args[[".additional_fun_parameters"]]
  )

  # Main {rtables} structural call
  analyze(
    lyt,
    vars,
    afun = a_count_missed_doses,
    na_str = na_str,
    inclNAs = !na_rm,
    table_names = table_names,
    var_labels = var_labels,
    show_labels = show_labels,
    nested = nested,
    extra_args = extra_args
  )
}
