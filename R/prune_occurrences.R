#' Occurrence Table Pruning
#'
#' Family of constructor and condition functions to flexibly prune occurrence tables.
#' The condition functions always return whether the row result is higher than the threshold.
#' Since they are of class [CombinationFunction()] they can be logically combined with other condition
#' functions.
#'
#' @note Since most table specifications are worded positively, we name our constructor and condition
#'   functions positively, too. However, note that the result of [keep_rows()] says what
#'   should be pruned though, to conform with the [rtables::prune_table()] interface.
#'
#' @name prune_occurrences
#' @examples
#' \dontrun{
#' tab <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("RACE") %>%
#'   split_rows_by("STRATA1") %>%
#'   summarize_row_groups() %>%
#'   summarize_vars("COUNTRY", .stats = "count_fraction") %>%
#'   build_table(DM)
#' }
#'
NULL

#' @describeIn prune_occurrences constructor for creating pruning functions based on
#'   a row condition function. This removes all analysis rows (`TableRow`) that should be
#'   pruned, i.e., don't fulfill the row condition. It removes the sub tree if there are no
#'   children left.
#' @param row_condition (`CombinationFunction`)\cr condition function which works on individual
#'   analysis rows and flags whether these should be kept in the pruned table.
#' @return [keep_rows()] returns a pruning function that can be used with [rtables::prune_table()]
#'   on `[rtables::TableTree()]` objects.
#' @export
#' @examples
#' \dontrun{
#' # `keep_rows`
#' is_non_empty <- !CombinationFunction(all_zero_or_na)
#' prune_table(tab, keep_rows(is_non_empty))
#' }
keep_rows <- function(row_condition) {
  assertthat::assert_that(is.function(row_condition))
  function(table_tree) {
    if (inherits(table_tree, "TableRow")) {
      return(!row_condition(table_tree))
    }
    children <- tree_children(table_tree)
    identical(length(children), 0L)
  }
}

#' @describeIn prune_occurrences constructor for creating pruning functions based on
#'   a condition for the (first) content row in leaf tables. This removes all leaf tables where
#'   the first content row does not fulfill the condition. It does not check individual rows.
#'   It then proceeds recursively by removing the sub tree if there are no children left.
#' @param content_row_condition (`CombinationFunction`)\cr condition function which works on individual
#'   first content rows of leaf tables and flags whether these leaf tables should be kept in the pruned table.
#' @return [keep_content_rows()] also returns a pruning function, the difference is that it
#'   checks the condition on the first content row of leaf tables in the table.
#' @export
#' @examples
#' \dontrun{
#' # `keep_content_rows`
#' more_than_twenty <- has_count_in_cols(atleast = 20L, col_names = names(tab))
#' prune_table(tab, keep_content_rows(more_than_twenty))
#' }
keep_content_rows <- function(content_row_condition) {
  assertthat::assert_that(is.function(content_row_condition))
  function(table_tree) {
    if (is_leaf_table(table_tree)) {
      content_row <- h_content_first_row(table_tree)
      return(!content_row_condition(content_row))
    }
    if (inherits(table_tree, "DataRow")) {
      return(FALSE)
    }
    children <- tree_children(table_tree)
    identical(length(children), 0L)
  }
}

#' @describeIn prune_occurrences constructor for creating condition functions on total counts in
#'  the specified columns.
#' @param atleast (`count` or `proportion`)\cr threshold which should be met in order to
#'   keep the row.
#' @param ... arguments for row or column access, see [rtables_access]: either
#'   `col_names` (`character`) including the names of the columns which should be used,
#'   or alternatively `col_indices` (`integer`) giving the indices directly instead.
#' @return [has_count_in_cols()] returns a condition function that sums the counts in the specified
#'   column.
#' @export
#' @examples
#' \dontrun{
#' # `has_count_in_cols`
#' more_than_one <- has_count_in_cols(atleast = 1L, col_names = names(tab))
#' prune_table(tab, keep_rows(more_than_one))
#' }
has_count_in_cols <- function(atleast, ...) {
  assertthat::assert_that(
    assertthat::is.count(atleast)
  )
  CombinationFunction(function(table_row) {
    row_counts <- h_row_counts(table_row, ...)
    total_count <- sum(row_counts)
    total_count >= atleast
  })
}

#' @describeIn prune_occurrences constructor for creating condition functions on any of the counts in
#'  the specified columns satisfying a threshold.
#' @param atleast (`count` or `proportion`)\cr threshold which should be met in order to
#'   keep the row.
#' @return [has_count_in_any_col()] returns a condition function that compares the counts in the
#'   specified columns with the threshold.
#' @export
#' @examples
#' \dontrun{
#' # `has_count_in_any_col`
#' any_more_than_one <- has_count_in_any_col(atleast = 1L, col_names = names(tab))
#' prune_table(tab, keep_rows(any_more_than_one))
#' }
has_count_in_any_col <- function(atleast, ...) {
  assertthat::assert_that(
    assertthat::is.count(atleast)
  )
  CombinationFunction(function(table_row) {
    row_counts <- h_row_counts(table_row, ...)
    any(row_counts >= atleast)
  })
}

#' @describeIn prune_occurrences constructor for creating condition functions on total fraction in
#'  the specified columns.
#' @return [has_fraction_in_cols()] returns a condition function that sums the counts in the
#'   specified column, and computes the fraction by dividing by the total column counts.
#' @export
#' @examples
#' \dontrun{
#' # `has_fraction_in_cols`
#' more_than_five_percent <- has_fraction_in_cols(atleast = 0.05, col_names = names(tab))
#' prune_table(tab, keep_rows(more_than_five_percent))
#' }
has_fraction_in_cols <- function(atleast, ...) {
  assertthat::assert_that(
    is_proportion(atleast, include_boundaries = TRUE)
  )
  CombinationFunction(function(table_row) {
    row_counts <- h_row_counts(table_row, ...)
    total_count <- sum(row_counts)
    col_counts <- h_col_counts(table_row, ...)
    total_n <- sum(col_counts)
    total_percent <- total_count / total_n
    total_percent >= atleast
  })
}

#' @describeIn prune_occurrences constructor for creating condition functions on any fraction in
#'  the specified columns.
#' @return [has_fraction_in_cols()] returns a condition function that looks at the fractions
#'  in the specified columns and checks whether any of them fulfill the threshold.
#' @export
#' @examples
#' \dontrun{
#' # `has_fraction_in_any_col`
#' any_atleast_five_percent <- has_fraction_in_any_col(atleast = 0.05, col_names = names(tab))
#' prune_table(tab, keep_rows(more_than_five_percent))
#' }
has_fraction_in_any_col <- function(atleast, ...) {
  assertthat::assert_that(
    is_proportion(atleast, include_boundaries = TRUE)
  )
  CombinationFunction(function(table_row) {
    row_fractions <- h_row_fractions(table_row, ...)
    any(row_fractions >= atleast)
  })
}

#' @describeIn prune_occurrences constructor for creating condition function that checks the difference
#'   between the fractions reported in each specified column.
#' @return [has_fractions_difference()] returns a condition function that extracts the fractions of each
#'   specified column, and computes the difference of the minimum and maximum.
#' @export
#' @examples
#' \dontrun{
#' # `has_fractions_difference`
#' more_than_five_percent_diff <- has_fractions_difference(atleast = 0.05, col_names = names(tab))
#' prune_table(tab, keep_rows(more_than_five_percent_diff))
#' }
has_fractions_difference <- function(atleast, ...) {
  assertthat::assert_that(
    is_proportion(atleast, include_boundaries = TRUE)
  )
  CombinationFunction(function(table_row) {
    fractions <- h_row_fractions(table_row, ...)
    difference <- diff(range(fractions))
    difference >= atleast
  })
}

#' @describeIn prune_occurrences constructor for creating condition function that checks the difference
#'   between the counts reported in each specified column.
#' @return [has_counts_difference()] returns a condition function that extracts the counts of each
#'   specified column, and computes the difference of the minimum and maximum.
#' @export
#' @examples
#' \dontrun{
#' # `has_counts_difference`
#' more_than_one_diff <- has_counts_difference(atleast = 1L, col_names = names(tab))
#' prune_table(tab, keep_rows(more_than_one_diff))
#' }
has_counts_difference <- function(atleast, ...) {
  assertthat::assert_that(
    assertthat::is.count(atleast)
  )
  CombinationFunction(function(table_row) {
    counts <- h_row_counts(table_row, ...)
    difference <- diff(range(counts))
    difference >= atleast
  })
}
