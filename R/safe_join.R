#' Safer join for tern
#'
#' This is a wrapper function around \code{dplyr::\link[dplyr]{join}}. It gives an error
#' in case two \code{data.frames} cannot be joined due to wrong keys or dimension mismatches
#' of equally named columns inside the \code{data.frames}. Additionally columns that are
#' equal inside the \code{data.frames} do not get merged twice. The join is applied without
#' duplicates.
#'
#' @param x (\code{data.frame} or \code{\link[dplyr]{tbl}}) left hand side of the join
#' @param y (\code{data.frame} or \code{\link[dplyr]{tbl}}) right hand side of the join
#'
#' @param by (\code{character}) Key columns to use for joining. For a safe join these
#'  are obligatory to guarantee the ability to merge the datasets.
#' @param method (\code{function}) \code{dplyr::\link[dplyr]{join}} function to join
#'   the two data sets.
#'
#' @param ... Additional parameters handed over to the function called in \code{method}
#'
#' @references \link[dplyr]{join}
#'
#' @importFrom dplyr select full_join
#' @importFrom magrittr %<>% %>%
#' @export
safe_join <- function(x, y, by = NULL, method = full_join, ...) {
  stopifnot(is.function(method))
  if (is.null(by)) {
    stop("A safe_join cannot be executed without the 'by' input.")
  }
  stopifnot(is.character(by))

  check_intersect_cols_identical(x = x, y = y, exclude_columns = by, keys = by)

  # we pass in by because key columns are not prefixed

  new_y_cols <- union(by, setdiff(names(y), names(x)))
  y %<>% select(new_y_cols)
  method(x, y, by = by, ...)
}

#' @importFrom dplyr arrange select
#' @importFrom magrittr %>%
check_intersect_cols_identical <- function(x, y, exclude_columns, keys) {
  if (!all(keys %in% names(x))) {
    stop("One key variable is not selected anymore for merging.")
  }
  if (!all(keys %in% names(y))) {
    stop("One key variable is not selected anymore for merging.")
  }

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

    rowid <- NULL # just for package check

    if (is.character(all.equal(
      x %>%
        arrange(rowid) %>%
        select(common_cols),
      y %>%
        arrange(rowid) %>%
        select(common_cols)
    ))) {
      stop(paste0(
        "Datasets cannot be merged because the columns '", paste(common_cols, collapse=", ") , ",'
        do not agree. This happens because the same dataset has been filtered twice with two different filters.
        Please ensure that the filter per dataset is identical."
      ))
    }
  } else {
    stopifnot(length(common_cols) <= 1)
  }
}
