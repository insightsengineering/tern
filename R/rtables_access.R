#' rtables Access Helper Functions
#'
#' These are a couple of functions that help with accessing the data in rtables objects.
#' Currently these work for occurrence tables, which are defined
#' as having a count as the first element and a fraction as the second element in each cell.
#'
#' @seealso `[prune_occurrences]` for usage of these functions.
#' @name rtables_access
#'
NULL

#' @describeIn rtables_access helper function to extract counts from specified columns
#'   in a `TableRow`.
#' @param table_row (`TableRow`)\cr an analysis row in a occurrence table.
#' @param col_names (`character`)\cr the names of the columns to extract from.
#' @export
#' @importFrom rlang is_integerish
#'
h_row_counts <- function(table_row, col_names) {
  col_indices <- col_indices(table_row, col_names)
  row_vals <- row_values(table_row)[col_indices]
  counts <- sapply(row_vals, "[", 1L)
  assert_that(rlang::is_integerish(counts))
  counts
}

#' @describeIn rtables_access helper function to extract fractions from specified columns
#'   in a `TableRow`.
#' @export
#'
h_row_fractions <- function(table_row, col_names) {
  col_indices <- col_indices(table_row, col_names)
  row_vals <- row_values(table_row)[col_indices]
  fractions <- sapply(row_vals, "[", 2L)
  assert_that(is_proportion_vector(fractions, include_boundaries = TRUE))
  fractions
}

#' @describeIn rtables_access Helper function to extract column counts from specified columns
#'   in a table.
#' @param table (`VTableNodeInfo`)\cr an occurrence table or row.
#' @export
#'
h_col_counts <- function(table, col_names) {
  col_indices <- col_indices(table, col_names)
  counts <- col_counts(table)[col_indices]
  setNames(counts, col_names)
}

#' @describeIn rtables_access Helper function which says whether current table is a leaf in the tree.
#' @export
#'
is_leaf_table <- function(table) {
  children <- tree_children(table)
  child_classes <- unique(sapply(children, class))
  identical(child_classes, "ElementaryTable")
}

#' @describeIn rtables_access Helper function to get first row of content table of current table.
#' @export
#'
h_content_first_row <- function(table) {
  ct <- content_table(table)
  tree_children(ct)[[1]]
}
