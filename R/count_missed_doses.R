#' Counting Missed Doses
#'
#' These are specific functions to count patients with missed doses. The difference to [count_cumulative()] is
#' mainly the special labels.
#'
#' @name count_missed_doses
#'
NULL

#' @describeIn count_missed_doses Statistics function to count non-missing values.
#' @return [s_count_nonmissing()] returns the statistic `n` which is the count of non-missing values in `x`.
#' @export
#' @examples
#' set.seed(1)
#' x <- c(sample(1:10, 10), NA)
#' s_count_nonmissing(x)
s_count_nonmissing <- function(x) {
  list(n = n_available(x))
}

#' @describeIn count_missed_doses Description function that calculates labels for  [s_count_missed_doses()].
#' @inheritParams s_count_missed_doses
#' @return [d_count_missed_doses()] returns a named `character` vector with the labels.
#'
d_count_missed_doses <- function(thresholds) {
  paste0("At least ", thresholds, " missed dose", ifelse(thresholds > 1, "s", ""))
}

#' @describeIn count_missed_doses Statistics function to count patients with missed doses when `x`
#'   is the vector of number of missed doses with one value for each patient.
#' @inheritParams argument_convention
#' @param thresholds (vector of `count`)\cr number of missed doses the patients at least had.
#' @return [s_count_missed_doses()] returns the statistics `n` and
#'  `count_fraction` with one element for each threshold.
#' @export
#' @examples
#' s_count_missed_doses(x = c(0, 1, 0, 2, 3, 4, 0, 2), thresholds = c(2, 5), .N_col = 10)
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
    stat$count_fraction[[i]] <- with_label(stat$count_fraction[[i]], label = labels[i])
  }
  n_stat <- s_count_nonmissing(x)
  c(n_stat, stat)
}

#' @describeIn count_missed_doses Formatted Analysis function to count non-missing values.
#' @export
#' @examples
#' #  We need to ungroup `count_fraction` first so that the rtables formatting
#' # function `format_count_fraction()` can be applied correctly.
#' afun <- make_afun(a_count_missed_doses, .ungroup_stats = "count_fraction")
#' afun(x = c(0, 1, 0, 2, 3, 4, 0, 2), thresholds = c(2, 5), .N_col = 10)
a_count_missed_doses <- make_afun(
  s_count_missed_doses,
  .formats = c(n = "xx", count_fraction = format_count_fraction)
)

#' @describeIn count_missed_doses Layout creating function which can be be used for creating
#'   summary tables for summarizing missed doses given user-specified `thresholds`. This is
#'   an additional layer on top of `count_cumulative` specifically for missed doses.
#' @inheritParams argument_convention
#' @inheritParams s_count_cumulative
#' @export
#' @examples
#' library(dplyr)
#' library(scda)
#' adex <- synthetic_cdisc_data("latest")$adex
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' anl <- adex %>%
#'   distinct(STUDYID, USUBJID, ARM) %>%
#'   mutate(
#'     PARAMCD = "TNDOSMIS",
#'     PARAM = "Total number of missed doses during study",
#'     AVAL = sample(0:20, size = nrow(adsl), replace = TRUE),
#'     AVALC = ""
#'   )
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_missed_doses("AVAL", thresholds = c(1, 5, 10, 15), var_labels = "Missed Doses") %>%
#'   build_table(anl, alt_counts_df = adsl)
count_missed_doses <- function(lyt,
                               vars,
                               var_labels = vars,
                               show_labels = "visible",
                               ...,
                               table_names = vars,
                               .stats = NULL,
                               .formats = NULL,
                               .labels = NULL,
                               .indent_mods = NULL) {
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
    extra_args = list(...)
  )
}
