#' Create a tree object with \code{data.frame} objects as leaf
#'
#' Converts a list of \code{data.frame}s or a nested list of rtables into a \code{data_tree} object. Note that
#' \code{NULL} objects are used interchangably with rtable objects.
#'
#' @param x a list or nested list of rtables, if \code{x} is an rtable then the rtable is returned
#'
#'
#' @details \code{data_tree} does not change the underlying data structure of the argument \code{x} other than adding
#'   the \code{"data_tree"} class attribute. The \code{data_tree} function also checks if the leafs are either
#'   \code{NULL} objects or objects of class \code{data.frame}
#'
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:2, b = 3:4)
#'
#' nl1 <- list(df1 = df, df2 = df, df3 = NULL)
#' tt1 <- data_tree(nl1)
#' tt1
#' summary(tt1)
#'
#' nl2 <- list(A = nl1, B = nl1)
#' tt2 <- data_tree(nl2)
#' tt2
#' summary(tt2)
#'
#' nl3 <- list(X = nl2, Y = nl2, Z = nl2)
#' tt3 <- data_tree(nl3)
#' summary(tt3)
#'
data_tree <- function(x) {
  tree(x, "data.frame", "data_tree")
}

#' Summarize a data_tree object
#'
#' @inheritParams summary_tree
#' @param ... arguments which are not used
#'
#' @return an \code{rtable} object with the columns \code{class} and \code{dimension}
#'
#' @export
#'
summary.data_tree <- function(object,  name = "", indent = 0, first = TRUE, ...) {
  summary_tree(object, class_name = "data.frame", name = name, indent = indent, first = first)
}

#' Recursively split over variables in a data.frame
#'
#' @param x vector or data frame containing values to be divided into groups.
#' @param by factor or a data.frame with factors
#' @param total label with name of \code{x} added to the split, if \code{NULL} only splitted data is given
#'
#'
#' @return nested list
#'
#' @export
#' @seealso  \code{\link{split}}
#'
#' @examples
#' library(magrittr)
#' df_by <- expand.grid(u = c("ii", "iii"), x = LETTERS[1:4], y = letters[1:3], z = letters[21:22])
#' x <- data.frame(num = 1:nrow(df_by))
#'
#' rsplit(x, df_by[[1]]) %>%
#'    data_tree() %>%
#'    summary()
#'
#' rsplit(x, df_by[1]) %>% data_tree() %>% summary()
#' rsplit(x, df_by[1:2]) %>% data_tree() %>% summary()
#' rsplit(x, df_by[1:3]) %>% data_tree() %>% summary()
#' rsplit(x, df_by[1:4]) %>% data_tree() %>% summary()
#'
#'
#' y <- rsplit(x, df_by[1], total = "-Overall-")
#'
#' y %>% data_tree() %>% summary()
#'
#' identical(y[[1]], x)
#'
#' y <- rsplit(x, df_by[1:4], total = "-total-")
#' y %>% data_tree() %>% summary()
#' identical( y$`-total-`$`-total-`$`-total-`$`-total-`, x)
#'
rsplit <- function(x, by, total = NULL) {

  stopifnot(
    is.atomic(by) || is.data.frame(by),
    is.character(total) || is.null(total)
  )

  if (is.data.frame(by) && ncol(by) == 1) {
    by <- by[[1]]
  }

  x_split <- if (is.atomic(by)) {
    split(x, by)
  } else {
    split(x, by[[1]])
  }

  if (!is.null(total)) {
    x_split <- c(setNames(list(x), total), x_split)
  }

  if (is.atomic(by)) {
    x_split
  } else {
    next_by <- by[, -1]
    next_by_split <- split(next_by, by[[1]])
    if (!is.null(total)) {
      next_by_split <- c(list(next_by), next_by_split)
    }
    Map(function(xi, byi) rsplit(xi, byi, total = total), x_split, next_by_split)
  }
}




# Add overall on leaf depth
#
# @param x a data_tree: nested list of data.frames
# @param label overall label
# @param set_attr set \code{overall} attribute
#
# @examples
# df_by <- expand.grid(u = c("ii", "iii"), x = LETTERS[1:4], y = letters[1:3], z = letters[21:22])
# x <- data.frame(num = 1:nrow(df_by))
# dfs <- rsplit(x, df_by[1:3])
# data_tree_add_overall(dfs)
#
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

#' Recursive Apply A Function to leafs of a Data Tree
#'
#' The return value of \code{rapply_data_tree} has the same tree structure as the input \code{data_tree} object.
#'
#' @inheritParams rapply_table_tree
#' @param x a list or nested list of \code{data.frame}, if \code{x} is a \code{data.frame}
#'
#' @export
#'
rapply_data_tree <- function(x, f, apply_to_NULL = FALSE, ...) { # nolint
    if (is(x, "data.frame") || is.null(x)) {
      if (is.null(x) && !apply_to_NULL) NULL else f(x, ...)
    } else {
      lapply(x, FUN = rapply_data_tree, f = f, ...)
    }
}
