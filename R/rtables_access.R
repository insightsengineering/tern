#' `rtables` Access Helper Functions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' These are a couple of functions that help with accessing the data in `rtables` objects.
#' Currently these work for occurrence tables, which are defined as having a count as the first
#' element and a fraction as the second element in each cell.
#'
#' @seealso [prune_occurrences] for usage of these functions.
#'
#' @name rtables_access
NULL

#' @describeIn rtables_access Helper function to extract the first values from each content
#'   cell and from specified columns in a `TableRow`. Defaults to all columns.
#'
#' @param table_row (`TableRow`)\cr an analysis row in a occurrence table.
#' @param col_names (`character`)\cr the names of the columns to extract from.
#' @param col_indices (`integer`)\cr the indices of the columns to extract from. If `col_names` are provided,
#'   then these are inferred from the names of `table_row`. Note that this currently only works well with a single
#'   column split.
#'
#' @return
#' * `h_row_first_values()` returns a `vector` of numeric values.
#'
#' @examples
#' tbl <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("RACE") %>%
#'   analyze("AGE", function(x) {
#'     list(
#'       "mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.x (xx.x)"),
#'       "n" = length(x),
#'       "frac" = rcell(c(0.1, 0.1), format = "xx (xx)")
#'     )
#'   }) %>%
#'   build_table(tern_ex_adsl) %>%
#'   prune_table()
#' tree_row_elem <- collect_leaves(tbl[2, ])[[1]]
#' result <- max(h_row_first_values(tree_row_elem))
#' result
#'
#' @export
h_row_first_values <- function(table_row,
                               col_names = NULL,
                               col_indices = NULL) {
  col_indices <- check_names_indices(table_row, col_names, col_indices)
  checkmate::assert_integerish(col_indices)
  checkmate::assert_subset(col_indices, seq_len(ncol(table_row)))

  # Main values are extracted
  row_vals <- row_values(table_row)[col_indices]

  # Main return
  vapply(row_vals, function(rv) {
    if (is.null(rv)) {
      NA_real_
    } else {
      rv[1L]
    }
  }, FUN.VALUE = numeric(1))
}

#' @describeIn rtables_access Helper function that extracts row values and checks if they are
#'   convertible to integers (`integerish` values).
#'
#' @return
#' * `h_row_counts()` returns a `vector` of numeric values.
#'
#' @examples
#' # Row counts (integer values)
#' # h_row_counts(tree_row_elem) # Fails because there are no integers
#' # Using values with integers
#' tree_row_elem <- collect_leaves(tbl[3, ])[[1]]
#' result <- h_row_counts(tree_row_elem)
#' # result
#'
#' @export
h_row_counts <- function(table_row,
                         col_names = NULL,
                         col_indices = NULL) {
  counts <- h_row_first_values(table_row, col_names, col_indices)
  checkmate::assert_integerish(counts)
  counts
}

#' @describeIn rtables_access helper function to extract fractions from specified columns in a `TableRow`.
#'   More specifically it extracts the second values from each content cell and checks it is a fraction.
#'
#' @return
#' * `h_row_fractions()` returns a `vector` of proportions.
#'
#' @examples
#' # Row fractions
#' tree_row_elem <- collect_leaves(tbl[4, ])[[1]]
#' h_row_fractions(tree_row_elem)
#'
#' @export
h_row_fractions <- function(table_row,
                            col_names = NULL,
                            col_indices = NULL) {
  col_indices <- check_names_indices(table_row, col_names, col_indices)
  row_vals <- row_values(table_row)[col_indices]
  fractions <- sapply(row_vals, "[", 2L)
  checkmate::assert_numeric(fractions, lower = 0, upper = 1)
  fractions
}

#' @describeIn rtables_access Helper function to extract column counts from specified columns in a table.
#'
#' @param table (`VTableNodeInfo`)\cr an occurrence table or row.
#'
#' @return
#' * `h_col_counts()` returns a `vector` of column counts.
#'
#' @export
h_col_counts <- function(table,
                         col_names = NULL,
                         col_indices = NULL) {
  col_indices <- check_names_indices(table, col_names, col_indices)
  counts <- col_counts(table)[col_indices]
  stats::setNames(counts, col_names)
}

#' @describeIn rtables_access Helper function to get first row of content table of current table.
#'
#' @return
#' * `h_content_first_row()` returns a row from an `rtables` table.
#'
#' @export
h_content_first_row <- function(table) {
  ct <- content_table(table)
  tree_children(ct)[[1]]
}

#' @describeIn rtables_access Helper function which says whether current table is a leaf in the tree.
#'
#' @return
#' * `is_leaf_table()` returns a `logical` value indicating whether current table is a leaf.
#'
#' @keywords internal
is_leaf_table <- function(table) {
  children <- tree_children(table)
  child_classes <- unique(sapply(children, class))
  identical(child_classes, "ElementaryTable")
}

#' @describeIn rtables_access Internal helper function that tests standard inputs for column indices.
#'
#' @return
#' * `check_names_indices` returns column indices.
#'
#' @keywords internal
check_names_indices <- function(table_row,
                                col_names = NULL,
                                col_indices = NULL) {
  if (!is.null(col_names)) {
    if (!is.null(col_indices)) {
      stop(
        "Inserted both col_names and col_indices when selecting row values. ",
        "Please choose one."
      )
    }
    col_indices <- h_col_indices(table_row, col_names)
  }
  if (is.null(col_indices)) {
    ll <- ifelse(is.null(ncol(table_row)), length(table_row), ncol(table_row))
    col_indices <- seq_len(ll)
  }

  return(col_indices)
}
