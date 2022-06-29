#' `rtables` Access Helper Functions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These are a couple of functions that help with accessing the data in `rtables` objects.
#' Currently these work for occurrence tables, which are defined
#' as having a count as the first element and a fraction as the second element in each cell.
#'
#' @seealso [prune_occurrences()] for usage of these functions.
#'
#' @name rtables_access
NULL

#' @describeIn rtables_access helper function to extract counts from specified columns
#'   in a `TableRow`.
#'
#' @param table_row (`TableRow`)\cr an analysis row in a occurrence table.
#' @param col_names (`character`)\cr the names of the columns to extract from.
#' @param col_indices (`integer`)\cr the indices of the columns to extract from. If `col_names` are provided,
#'   then these are inferred from the names of `table_row`. (Note that this currently only works well with a single
#'   column split.)
#'
#' @export
h_row_counts <- function(table_row,
                         col_names = NULL,
                         col_indices = h_col_indices(table_row, col_names)) {
  row_vals <- row_values(table_row)[col_indices]
  counts <- vapply(row_vals, function(rv) {
    if (is.null(rv)) {
      NA_real_
    } else {
      rv[1L]
    }
  }, FUN.VALUE = numeric(1))
  checkmate::assert_integerish(counts)
  counts
}

#' @describeIn rtables_access helper function to extract fractions from specified columns
#'   in a `TableRow`.
#'
#' @export
h_row_fractions <- function(table_row,
                            col_names = NULL,
                            col_indices = h_col_indices(table_row, col_names)) {
  row_vals <- row_values(table_row)[col_indices]
  fractions <- sapply(row_vals, "[", 2L)
  checkmate::assert_numeric(fractions, lower = 0, upper = 1)
  fractions
}

#' @describeIn rtables_access Helper function to extract column counts from specified columns
#'   in a table.
#'
#' @param table (`VTableNodeInfo`)\cr an occurrence table or row.
#'
#' @export
h_col_counts <- function(table,
                         col_names = NULL,
                         col_indices = h_col_indices(table, col_names)) {
  counts <- col_counts(table)[col_indices]
  stats::setNames(counts, col_names)
}

#' @describeIn rtables_access Helper function which says whether current table is a leaf in the tree.
#'
#' @keywords internal
is_leaf_table <- function(table) {
  children <- tree_children(table)
  child_classes <- unique(sapply(children, class))
  identical(child_classes, "ElementaryTable")
}

#' @describeIn rtables_access Helper function to get first row of content table of current table.
#'
#' @export
h_content_first_row <- function(table) {
  ct <- content_table(table)
  tree_children(ct)[[1]]
}
