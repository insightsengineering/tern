#' Occurrence Table Sorting
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Functions to score occurrence table subtables and rows which can be used in the
#' sorting of occurrence tables.
#'
#' @name score_occurrences
NULL

#' @describeIn score_occurrences Scoring function which sums the counts across all
#'   columns. It will fail if anything else but counts are used.
#'
#' @inheritParams rtables_access
#'
#' @return
#' * `score_occurrences()` returns the sum of counts across all columns of a table row.
#'
#' @seealso [h_row_first_values()]
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   analyze_num_patients(
#'     vars = "USUBJID",
#'     .stats = c("unique"),
#'     .labels = c("Total number of patients with at least one event")
#'   ) %>%
#'   split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE) %>%
#'   summarize_num_patients(
#'     var = "USUBJID",
#'     .stats = c("unique", "nonunique"),
#'     .labels = c(
#'       "Total number of patients with at least one event",
#'       "Total number of events"
#'     )
#'   ) %>%
#'   count_occurrences(vars = "AEDECOD")
#'
#' tbl <- build_table(lyt, tern_ex_adae, alt_counts_df = tern_ex_adsl) %>%
#'   prune_table()
#'
#' tbl_sorted <- tbl %>%
#'   sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)
#'
#' tbl_sorted
#'
#' @export
score_occurrences <- function(table_row) {
  row_counts <- h_row_counts(table_row)
  sum(row_counts)
}

#' @describeIn score_occurrences Scoring functions can be produced by this constructor to only include
#'   specific columns in the scoring. See [h_row_counts()] for further information.
#'
#' @inheritParams has_count_in_cols
#'
#' @return
#' * `score_occurrences_cols()` returns a function that sums counts across all specified columns
#'   of a table row.
#'
#' @seealso [h_row_counts()]
#'
#' @examples
#' score_cols_a_and_b <- score_occurrences_cols(col_names = c("A: Drug X", "B: Placebo"))
#'
#' # Note that this here just sorts the AEDECOD inside the AEBODSYS. The AEBODSYS are not sorted.
#' # That would require a second pass of `sort_at_path`.
#' tbl_sorted <- tbl %>%
#'   sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_cols_a_and_b)
#'
#' tbl_sorted
#'
#' @export
score_occurrences_cols <- function(...) {
  function(table_row) {
    row_counts <- h_row_counts(table_row, ...)
    sum(row_counts)
  }
}

#' @describeIn score_occurrences Scoring functions produced by this constructor can be used on
#'   subtables: They sum up all specified column counts in the subtable. This is useful when
#'   there is no available content row summing up these counts.
#'
#' @return
#' * `score_occurrences_subtable()` returns a function that sums counts in each subtable
#'   across all specified columns.
#'
#' @examples
#' score_subtable_all <- score_occurrences_subtable(col_names = names(tbl))
#'
#' # Note that this code just sorts the AEBODSYS, not the AEDECOD within AEBODSYS. That
#' # would require a second pass of `sort_at_path`.
#' tbl_sorted <- tbl %>%
#'   sort_at_path(path = c("AEBODSYS"), scorefun = score_subtable_all, decreasing = FALSE)
#'
#' tbl_sorted
#'
#' @export
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
#'
#' @return
#' * `score_occurrences_cont_cols()` returns a function that sums counts in the first content row in
#'   specified columns.
#'
#' @export
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
