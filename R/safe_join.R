#' Safer join for tern
#'
#' @param x (\code{data.frame}) left hand side of the join data frame
#' @param y (\code{data.frame}) right hand side of the join data frame
#'
#' @param by (\code{character}) Key columns to use for joining
#' @param method (\code{function}) \code{dplyr::join} function to join
#'   the two data sets.
#'
#' @param ... Additional parameters handed over to the function called in \code{method}
#'
#'
#' @importFrom dplyr select full_join
#' @importFrom magrittr %<>%
#' @export
safe_join <- function(x, y, by, method = full_join, ...) {

  check_intersect_cols_identical(x = x, y = y, exclude_columns = by, keys = by)

  # we pass in by because key columns are not prefixed

  new_y_cols <- union(by, setdiff(names(y), names(x)))
  y %<>% select(new_y_cols)
  method(x, y, by = by, ...)
}

#' @importFrom dplyr arrange
#' @importFrom rlang parse_expr
check_intersect_cols_identical <- function(x, y, exclude_columns, keys) {
  stopifnot(all(keys %in% names(x)))
  stopifnot(all(keys %in% names(y)))

  # checks that intersecting columns are identical
  # for this, it first orders the columns by the rowid of the dataset y (therefore y needs a dataname)
  # if it also appears in x, it means that x already contains part of the data of y and we must check it
  # otherwise, there should be no intersection between the columns
  y_dataname <- attr(y, "dataname")
  if (!is.null(y_dataname)) {
    warning("merging without dataname.")
  } # x dataset may not have name because it is already merged

  common_cols <- setdiff(intersect(names(x), names(y)), c(exclude_columns, "rowid"))

  if (length(common_cols) > 0) {

    y_row_id_col_name_expr <- parse_expr("rowid") # parse from string into an expression

    if (!identical(
      x %>%
        arrange(!!y_row_id_col_name_expr) %>%
        select(common_cols),
      y %>%
        arrange(!!y_row_id_col_name_expr) %>%
        select(common_cols)
    )) {
      stop(glue(
        "Datasets cannot be merged because the columns '{paste(common_cols, collapse=', ')}'
        do not agree. This happens because the same dataset has been filtered twice with two different filters.
        Please ensure that the filter per dataset is identical."
      ))
    }
  } else {
    stopifnot(length(common_cols) <= 1)
  }
}
