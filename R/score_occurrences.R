#' Occurrence Table Sorting
#'
#' Functions to sort occurrence tables.
#'
#' @template formatting_arguments
#'
#' @name score_occurrences
#'
NULL

#' @describeIn score_occurrences Sorting function that works on occurrence data tabulated in rtables.
#'
#' @param rtable_object (`rtable`) \cr rtable object name.
#'
#' @returns A sorted rtable object.
#'
#' @export
#'
#' @examples
#'
#' library(assertthat)
#' library(random.cdisc.data)
#' library(rtables)
#' ADSL <- radsl(cached = TRUE)
#' ADAE <- radae(cached = TRUE)
#'
#' N_per_arm <- table(ADSL$ARM)
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   summarize_num_patients(var = "USUBJID",
#'                          .stats = c("unique"),
#'                          .labels = c("Total number of patients with at least one event")) %>%
#'   split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE)  %>%
#'   summarize_num_patients(var = "USUBJID",
#'                          .stats = c("unique", "nonunique"),
#'                          .labels = c("Total number of patients with at least one event",
#'                                      "Total number of events")) %>%
#'  count_occurrences(vars="AEDECOD")
#'
#' rtable_object <- build_table(lyt, ADAE, col_counts = N_per_arm) %>%
#'  prune_table()
#'
#' rtable_object_sorted <- rtable_object %>%
#' sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)
#'
#' rtable_object_sorted
#'
score_occurrences <- function(rtable_object) {
  vals <- row_values(rtable_object)
  vals1 <- vapply(vals, '[[', i=1, numeric(1)) # nolint
  sum(vals1)
}
