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
#' tab <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("RACE") %>%
#'   split_rows_by("STRATA1") %>%
#'   summarize_vars("COUNTRY", .stats = "count_fraction") %>%
#'   build_table(DM)
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
#' # `keep_rows`
#' is_non_empty <- !CombinationFunction(all_zero_or_na)
#' prune_table(tab, keep_rows(is_non_empty))
#'
keep_rows <- function(row_condition) {
  assert_that(is.function(row_condition))
  function(table_tree) {
    if (is(table_tree, "TableRow")) {
      return(! row_condition(table_tree))
    }
    children <- tree_children(table_tree)
    identical(length(children), 0L)
  }
}

#' @describeIn prune_occurrences constructor for creating condition functions on total counts in
#'  the specified columns.
#' @param above (`count` or `proportion`)\cr threshold which should be met in order to
#'   keep the row.
#' @param col_names (`character`)\cr names of the columns which should be used in the condition
#'   calculation.
#' @return [has_count_in_cols()] returns a condition function that sums the counts in the specified
#'   column.
#' @export
#' @examples
#' # `has_count_in_cols`
#' more_than_one <- has_count_in_cols(above = 1L, col_names = names(tab))
#' prune_table(tab, keep_rows(more_than_one))
#'
has_count_in_cols <- function(above, col_names) {
  assert_that(
    is_nonnegative_count(above),
    is.character(col_names)
  )
  CombinationFunction(function(table_row) {
    row_counts <- h_row_counts(table_row, col_names)
    total_count <- sum(row_counts)
    total_count > above
  })
}

#' @describeIn prune_occurrences constructor for creating condition functions on total fraction in
#'  the specified columns.
#' @return [has_fraction_in_cols()] returns a condition function that sums the counts in the
#'   specified column, and computes the fraction by dividing by the total column counts.
#' @export
#' @examples
#' # `has_fraction_in_cols`
#' more_than_five_percent <- has_fraction_in_cols(above = 0.05, col_names = names(tab))
#' prune_table(tab, keep_rows(more_than_five_percent))
#'
has_fraction_in_cols <- function(above, col_names) {
  assert_that(
    is_proportion(above, include_boundaries = TRUE),
    is.character(col_names)
  )
  CombinationFunction(function(table_row) {
    row_counts <- h_row_counts(table_row, col_names)
    total_count <- sum(row_counts)
    col_counts <- h_col_counts(table_row, col_names)
    total_n <- sum(col_counts)
    total_percent <- total_count / total_n
    total_percent > above
  })
}

#' @describeIn prune_occurrences constructor for creating condition function that checks the difference
#'   between the fractions reported in each specified column.
#' @return [has_fractions_difference()] returns a condition function that extracts the fractions of each
#'   specified column, and computes the difference of the minimum and maximum.
#' @export
#' @examples
#' # `has_fractions_difference`
#' more_than_five_percent_diff <- has_fractions_difference(above = 0.05, col_names = names(tab))
#' prune_table(tab, keep_rows(more_than_five_percent_diff))
#'
has_fractions_difference <- function(above, col_names) {
  assert_that(
    is_proportion(above, include_boundaries = TRUE),
    is.character(col_names),
    length(col_names) > 1
  )
  CombinationFunction(function(table_row) {
    fractions <- h_row_fractions(table_row, col_names)
    difference <- diff(range(fractions))
    difference > above
  })
}

#' @describeIn prune_occurrences constructor for creating condition function that checks the difference
#'   between the counts reported in each specified column.
#' @return [has_counts_difference()] returns a condition function that extracts the counts of each
#'   specified column, and computes the difference of the minimum and maximum.
#' @export
#' @examples
#' # `has_counts_difference`
#' more_than_one_diff <- has_counts_difference(above = 1L, col_names = names(tab))
#' prune_table(tab, keep_rows(more_than_one_diff))
#'
has_counts_difference <- function(above, col_names) {
  assert_that(
    is_nonnegative_count(above),
    is.character(col_names),
    length(col_names) > 1
  )
  CombinationFunction(function(table_row) {
    counts <- h_row_counts(table_row, col_names)
    difference <- diff(range(counts))
    difference > above
  })
}
