
#' Recursively split over variables in a data.frame
#'
#' @param x vector or data frame containing values to be divided into groups.
#' @param by factor or a data.frame with factors
#'
#' @return nested list
#'
#' @export
#' @seealso  \code{\link{split}}
#'
#' @examples
#' df_by <- expand.grid(u = c("ii", "iii"), x = LETTERS[1:4], y = letters[1:3], z = letters[21:22])
#' x <- data.frame(num = 1:nrow(df_by))
#' rsplit(x, df_by[[1]])
#' rsplit(x, df_by[1])
#' rsplit(x, df_by[1:2])
#' rsplit(x, df_by[1:3])
#' rsplit(x, df_by[1:4])
rsplit <- function(x, by) {

  if (is.data.frame(by) && ncol(by) == 1) {
    by <- by[[1]]
  }

  if (is.atomic(by)) {
    split(x, by)
  } else {
    xs <- split(x, by[[1]])
    bys <- split(by[, -1], by[[1]])
    Map(rsplit, xs, bys)
  }
}


# TODO
rsplit_overall <- function(x, by, label = "- Overall -", set_attr = FALSE) {
  if (is.data.frame(by) && ncol(by) == 1) {
    by <- by[[1]]
  }

  if (is.atomic(by)) {
    split(x, by)
  } else {
    xs <- split(x, by[[1]])
    bys <- split(by[, -1], by[[1]])
    Map(rsplit, xs, bys)
  }
}


#' Add overall on leaf depth
#'
#' @param x a data_tree: nested list of data.frames
#' @param label overall label
#' @param set_attr set \code{overall} attribute
#'
#' @export
#'
#' @examples
#'
#' df_by <- expand.grid(u = c("ii", "iii"), x = LETTERS[1:4], y = letters[1:3], z = letters[21:22])
#' x <- data.frame(num = 1:nrow(df_by))
#' dfs <- rsplit(x, df_by[1:3])
#' data_tree_add_overall(dfs)
#'
data_tree_add_overall <- function(x, label = "- Overall -", set_attr = FALSE) {

  stopifnot(
    is.list(x) && is.list(x[[1]]),
    is.logical(set_attr)
  )


  are_dfs <- vapply(x, is.data.frame, logical(1))
  are_lists <- vapply(x, is.list, logical(1))

  if (all(are_dfs)) {
    loverall <- setNames(list(do.call(rbind, x)), label)
    if (set_attr) {
      attr(loverall[[1]], "overall") <- TRUE
    }
    c(loverall, x)
  } else if (all(are_lists)) {
    lapply(x, data_tree_add_overall, label = label)
  } else {
    stop("no valid data_tree")
  }
}

#' @export
rapply_data_tree <- function(x, f, apply_to_NULL = FALSE, ...) { # nolint
    if (is(x, "data.frame") || is.null(x)) {
      if (is.null(x) && !apply_to_NULL) NULL else f(x, ...)
    } else {
      lapply(x, FUN = rapply_data_tree, f = f, ...)
    }
}

