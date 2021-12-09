#' Occurrence Table Sorting
#'
#' Functions to score occurrence table subtables and rows which can be used in the
#' sorting of occurrence tables.
#'
#' @name score_occurrences
#'
NULL

#' @describeIn score_occurrences Scoring function which sums the counts across all columns.
#' @inheritParams rtables_access
#' @return [score_occurrences()] returns the sum of counts across all columns of a table row.
#'
#' @export
#'
#' @examples
#'
#' library(assertthat)
#' library(scda)
#' library(rtables)
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adae <- synthetic_cdisc_data("latest")$adae
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   summarize_num_patients(
#'     var = "USUBJID",
#'     .stats = c("unique"),
#'     .labels = c("Total number of patients with at least one event")
#'   ) %>%
#'   split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE)  %>%
#'   summarize_num_patients(
#'     var = "USUBJID",
#'     .stats = c("unique", "nonunique"),
#'     .labels = c("Total number of patients with at least one event",
#'                 "Total number of events")
#'   ) %>%
#'   count_occurrences(vars = "AEDECOD")
#'
#' rtable_object <- build_table(lyt, adae, alt_counts_df = adsl) %>%
#'   prune_table()
#'
#' rtable_object_sorted <- rtable_object %>%
#'   sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)
#'
#' rtable_object_sorted
#'
score_occurrences <- function(table_row) {
  row_counts <- h_row_counts(table_row, col_indices = seq_len(ncol(table_row)))
  sum(row_counts)
}

#' @describeIn score_occurrences Scoring functions can be produced by this constructor to only include
#'   specific columns in the scoring.
#' @inheritParams has_count_in_cols
#' @return [score_occurrences_cols()] returns a function that sums counts across all specified columns
#'   of a table row.
#'
#' @export
#'
#' @examples
#' score_cols_a_and_b <- score_occurrences_cols(col_names = c("A: Drug X", "B: Placebo"))
#'
#' # Note that this here just sorts the AEDECOD inside the AEBODSYS. The AEBODSYS are not sorted.
#' # That would require a second pass of `sort_at_path`.
#' rtable_object_sorted <- rtable_object %>%
#'   sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_cols_a_and_b)
#'
#' rtable_object_sorted
#'
score_occurrences_cols <- function(...) {
  function(table_row) {
    row_counts <- h_row_counts(table_row, ...)
    sum(row_counts)
  }
}

#' @describeIn score_occurrences Scoring functions produced by this constructor can be used on
#'   subtables: They sum up all specified column counts in the subtable. This is useful when
#'   there is no available content row summing up these counts.
#' @return [score_occurrences_subtable()] returns a function that sums counts in each subtable
#'   across all specified columns.
#'
#' @export
#'
#' @examples
#' score_subtable_all <- score_occurrences_subtable(col_names = names(rtable_object))
#'
#' # Note that this code just sorts the AEBODSYS, not the AEDECOD within AEBODSYS. That
#' # would require a second pass of `sort_at_path`.
#' rtable_object_sorted <- rtable_object %>%
#'   sort_at_path(path = c("AEBODSYS"), scorefun = score_subtable_all, decreasing = FALSE)
#'
#' rtable_object_sorted
#'
score_occurrences_subtable <- function(...) {
  score_table_row <- score_occurrences_cols(...)
  function(table_tree) {
    table_rows <- collect_leaves(table_tree)
    counts <- vapply(table_rows, score_table_row, numeric(1))
    sum(counts)
  }
}

#' @describeIn score_occurrences Produce score function for sorting table by summing the first content row in
#'   specified columns. Note that this is extending [rtables::cont_n_onecol()] and [rtables::cont_n_allcols()].
#' @export
#'
score_occurrences_cont_cols <- function(...) {
  score_table_row <- score_occurrences_cols(...)
  function(table_tree) {
    if (inherits(table_tree, "ContentRow")) {
      return(NA)
    }
    content_row <- h_content_first_row(table_tree)
    score_table_row(content_row)
  }
}
