#' Counting Missed Doses
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These are specific functions to count patients with missed doses. The difference to [count_cumulative()] is
#' mainly the special labels.
#'
#' @inheritParams s_count_cumulative
#' @inheritParams argument_convention
#' @param thresholds (vector of `count`)\cr number of missed doses the patients at least had.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("count_missed_doses")`
#'   to see available statistics for this function.
#'
#' @seealso Relevant description function [d_count_missed_doses()].
#'
#' @name count_missed_doses
#' @order 1
NULL

#' @describeIn count_missed_doses Statistics function to count non-missing values.
#'
#' @return
#' * `s_count_nonmissing()` returns the statistic `n` which is the count of non-missing values in `x`.
#'
#' @keywords internal
s_count_nonmissing <- function(x) {
  list(n = n_available(x))
}

#' Description Function that Calculates Labels for [s_count_missed_doses()].
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
                                 .N_col) { # nolint
  stat <- s_count_cumulative(
    x = x,
    thresholds = thresholds,
    lower_tail = FALSE,
    include_eq = TRUE,
    .N_col = .N_col
  )
  labels <- d_count_missed_doses(thresholds)
  for (i in seq_along(stat$count_fraction)) {
    stat$count_fraction[[i]] <- formatters::with_label(stat$count_fraction[[i]], label = labels[i])
  }
  n_stat <- s_count_nonmissing(x)
  c(n_stat, stat)
}

#' @describeIn count_missed_doses Formatted analysis function which is used as `afun`
#'   in `count_missed_doses()`.
#'
#' @return
#' * `a_count_missed_doses()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_count_missed_doses <- make_afun(
  s_count_missed_doses,
  .formats = c(n = "xx", count_fraction = format_count_fraction)
)

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
                               ...,
                               table_names = vars,
                               .stats = NULL,
                               .formats = NULL,
                               .labels = NULL,
                               .indent_mods = NULL) {
  extra_args <- list(thresholds = thresholds, ...)

  afun <- make_afun(
    a_count_missed_doses,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .ungroup_stats = "count_fraction"
  )
  analyze(
    lyt = lyt,
    vars = vars,
    afun = afun,
    var_labels = var_labels,
    table_names = table_names,
    show_labels = show_labels,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args
  )
}
