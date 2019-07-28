#todo: maybe move to rtables

#' Split objects according to by object
#'
#' @param x object to split
#' @param by split by, either a factor or a col_by matrix
#'
#' @return list, one item for each cateogory in by
#'
#' @export
#'
#' @examples
#' by <- factor(c("M", "M", "F", "F", "F"))
#' esplit(1:5, by)
#' esplit(data.frame(x = 1:5, y = 6:10), by)
#' esplit(list(x = 1:5, y = 6:10), by)
#' esplit(list(), by)
#' esplit(non_rsplit(as.list(1:5)), by) # not working: esplit(as.list(1:5), by)
#' esplit(
#'   list(data.frame(x = 1:5, y = 6:10), data.frame(z1 = 11:15, z2 = 16:20)),
#'   by
#' )
#' esplit(
#'   list(
#'     list(data.frame(x = 1:2, y = 3:4), data.frame(x = 5:6, y = 7:8)),
#'     list(data.frame(x = 11:12, y = 13:14), data.frame(x = 15:16, y = 17:18))
#'   ),
#'   by = factor(c("M", "F"))
#' )
#' esplit(
#'   non_rsplit(list(
#'     list(data.frame(x = 1:2, y = 3:4), data.frame(x = 5:6, y = 7:8)),
#'     list(data.frame(x = 11:12, y = 13:14), data.frame(x = 15:16, y = 17:18))
#'   )),
#'   by = factor(c("M", "F"))
#' )
#'
#' by <- data.frame("Old" = c(TRUE, TRUE, FALSE), "Tall" = c(FALSE, TRUE, TRUE))
#' esplit(1:3, by)
#' esplit(list(x = 1:3, y = 4:6), by)
#' esplit(
#'   list(
#'     x1 = factor(c(1,2,3,4,5,6)),
#'     x2 = data.frame(x = 7:12, y = 13:18)
#'   ),
#'   factor(c("a", "a", "b", "b", "b", "b"))
#' )
esplit <- function (x, by, ...) {
  UseMethod("esplit", x)
}

esplit.default <- function(x, by) {
  stopifnot(is.atomic(x))
  by <- col_by_to_matrix(by) #todo: rename to by_to_matrix
  lapply(by, function(rows) x[rows])
}

esplit.data.frame <- function(df, by) {
  by <- col_by_to_matrix(by)
  lapply(by, function(rows) df[rows,])
}

esplit.list <- function(lst, by) {
  # splits each list item
  # applies recursively to each list element
  by <- col_by_to_matrix(by)
  if (length(lst) == 0) {
    # must be careful because list can be empty and we always want to return ncol(by) elements
    lapply(by, function(discard) NULL) # use lapply to keep names
  } else {
    # apply by to each list item, then transpose list
    purrr::transpose(lapply(lst, function(elem) esplit(elem, by)))
  }
}

# add this class to avoid recursion
esplit.non_rsplit <- function(lst, by) {
  # same as default method
  by <- col_by_to_matrix(by)
  lapply(by, function(rows) lst[rows])
}

#' Split list in the classic way instead of applying split to each element
#'
#' See \code{\link{esplit.list}} for the alternative behavior.
#'
#' @param x list to protect from splitting
#'
#' @export
non_rsplit <- function(x) {
  stopifnot(is.list(x))
  structure(x, class = "non_rsplit")
}

#' Split recursively
#'
#' This is an extension of \code{\link{esplit}} to the case when by is a list to split by recursively
#'
#' @param x object to split
#' @param by_lst list of by objects to recursively split by
#'
#' @return nested list, first level corresponding to split by by_lst[[1]], second to recursive split by by_lst[[2]] etc.
#'
#' @examples
#' by_lst <- list(factor(c("M", "M", "F", "F", "F")), factor(c("O", "Y", "Y", "Y", "Y")))
#' rsplit(
#' list(data.frame(x = 1:5, y = 6:10), data.frame(z1 = 11:15, z2 = 16:20)),
#' list(by_lst, by_lst)
#' )
rsplit <- function(x, by_lst) {
  # this is the extension of esplit to the recursive case when by is a list
  if (is.null(by_lst) || length(by_lst) == 0) {
    return(x)
  }
  stopifnot(is.list(by_lst))
  lapply(esplit(list(x = x, by_lst = by_lst[-1]), by_lst[[1]]), function(elem) rsplit(x = elem$x, by_lst = elem$by_lst))
}
