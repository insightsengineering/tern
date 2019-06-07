


#' Create a tree object with rtable objects as leaf
#'
#' Converts a list of rtables or a nested list of rtables into a \code{table_tree} object. Note that \code{NULL} objects
#' are used interchangably with rtable objects.
#'
#' @param x a list or nested list of rtables, if \code{x} is an rtable then the rtable is returned
#'
#'
#' @details \code{table_tree} does not change the underlying data structure of the argument \code{x} other than adding
#'   the \code{"table_tree"} class attribute. The \code{table_tree} function also checks if the leafs are either
#'   \code{NULL} objects or objects of class \code{rtables}
#'
#' @export
#'
#' @examples
#'
#' tbl <- rtable(header = 1:3, rrowl("row", letters[1:3]))
#'
#' nl1 <- list(tbl1 = tbl, tbl2 = tbl, tbl3 = NULL)
#' tt1 <- table_tree(nl1)
#' tt1
#' summary(tt1)
#'
#' nl2 <- list(A = nl1, B = nl1)
#' tt2 <- table_tree(nl2)
#' tt2
#' summary(tt2)
#'
#' nl3 <- list(X = nl2, Y = nl2, Z = nl2)
#' tt3 <- table_tree(nl3)
#' summary(tt3)
#' \dontrun{
#' nl4 <- c(nl1, list(iris))
#' table_tree(nl4)
#' }
#'
table_tree <- function(x) {
   tree(x, "rtable", "table_tree")
}

tree <- function(x, class_name, tree_name) {

  if (!(is(x, class_name) || is.null(x) || is(x, tree_name) && is.list(x) ||
        identical(class(x), "list"))) {
    stop(paste0("object is not a nested list of ", class_name, "s"), call. = FALSE)
  }

  cl <- class(x)
  if (cl == "list") {
    structure(
      lapply(x, tree, class_name = class_name, tree_name = tree_name),
      class = tree_name
    )
  } else if (cl %in% c(tree_name, class_name, "NULL")) {
    x
  } else {
    stop(paste("x is not a list, ", tree_name, class_name, "or NULL"))
  }

}



#' @export
print.table_tree <- function(x, ...) {
  print(unclass(x), ...)
}


#' Summarize a table_tree object
#'
#' @inheritParams summary_tree
#' @param ... arguments which are not used
#'
#' @return an \code{rtable} object with the columns \code{class} and \code{dimension}
#'
#' @export
#'
#' @examples
#'
#' tbl <- rtable(header = 1:3, rrowl("row", letters[1:3]))
#'
#' nl1 <- list(tbl1 = tbl, tbl2 = tbl, tbl3 = NULL)
#' summary(table_tree(nl1))
#'
#' nl2 <- list(A = nl1, B = nl1)
#' summary(table_tree(nl2))
#'
#' nl3 <- list(X = nl2, Y = nl2, Z = nl2)
#' summary(table_tree(nl3))
#'
summary.table_tree <- function(object, name = "", indent = 0, first = TRUE, ...) {
  summary_tree(object, class_name = "rtable", name = name, indent = indent, first = first)
}


#' Summarize a table_tree object
#'
#' @param object a \code{\link{table_tree}} object
#' @param name list element name of table
#' @param first logical, whether the nested table should be indented
#' @param indent indentation of table
#'
summary_tree <-  function(object, class_name, name = "", indent = 0, first = TRUE) {

  default_header <- c("class", "dimension")

  if (is(object, class_name)) {
    rtable(default_header, rrow(name, class_name, paste(dim(object), collapse = " x "), indent = indent))
  } else if (is.null(object)) {
    rtable(default_header, rrow(name, "NULL", 0, indent = indent))
  } else if (is.list(object)) {
    object <- ensure_children_named(object)
    tbls <- c(
      if (first) NULL else list(rtable(default_header, rrow(name, indent = indent))),
      Map(function(xi, namei) {
        summary_tree(xi, class_name = class_name, name = namei, indent = indent + if (first) 0 else 1, first = FALSE)
      }, object, names(object))
    )
    rbindl_rtables(tbls)
  } else {
    stop(paste("class", class(object), "not valid"))
  }

}





#' Recursive Bind rtable Objects while Retaining Tree Hierarchy
#'
#' \code{table_tree} obejcts can be named, when recursivly stacking it is sometimes desired to add these names as rrows
#' and indent the child elements accordingly to preserve the ancestry information.
#'
#'
#' @param x a \code{\link{table_tree}} object
#' @param gap argument passed to rtables.
#'
#' @export
#'
#' @examples
#' tbl <- rtable(header = 1:3, rrowl("row", letters[1:3]))
#'
#' nl1 <- list(tbl1 = tbl, tbl2 = tbl, tbl3 = NULL)
#' rbind_table_tree(table_tree(nl1))
#'
#' nl2 <- list(A = nl1, B = nl1)
#' rbind_table_tree(table_tree(nl2))
#'
#' nl3 <- list(X = nl2, Y = nl2, Z = nl2)
#' rbind_table_tree(nl3)
#'
#'
rbind_table_tree  <- function(x, gap = 1) {

  if (is(x, "rtable") || is.null(x)) {
    x
  } else {

    x <- ensure_children_named(x)

    tbls <- Filter(Negate(is.null), lapply(x, rbind_table_tree))

    ltbls <- Map(function(tbli, section_name) {
      rbind(
        rtable(header = header(tbli), rrow(section_name)),
        indent(tbli, 1)
      )
    }, tbls, names(tbls))

    rbindl_rtables(ltbls, gap = gap)
  }

}



ensure_children_named <- function(x) {
  if (is.null(names(x))) {
    names(x) <- rep("<no-name>", length(x))
  }
  x
}

#' Recursive Apply A Function to leafs of a Table Tree
#'
#' The return value of \code{rapply_table_tree} has the same tree structure as the input \code{table_tree} object.
#'
#' @inheritParams rbind_table_tree
#' @param f a function to be applied to leafs
#' @param apply_to_NULL boolean, should \code{f} also be applied to a \code{NULL} leaf or should \code{NULL} leafs
#'   remain \code{NULL} leafs.
#' @param ... arguments passed to \code{f}
#'
#' @return \code{table_tree} object
#'
#' @export
#'
#' @examples
#'
#' tbl <- rtable(header = 1:3, rrowl("row", letters[1:3]))
#'
#' rename_rows <- function(x) {
#'      row.names(x) <- rep("new name", nrow(x))
#'      x
#' }
#'
#' nl1 <- list(tbl1 = tbl, tbl2 = tbl, tbl3 = NULL)
#' rapply_table_tree(table_tree(nl1), rename_rows)
#'
#' nl2 <- list(A = nl1, B = nl1)
#' rapply_table_tree(table_tree(nl2), rename_rows)
#'
#' nl3 <- list(X = nl2, Y = nl2, Z = nl2)
#' rapply_table_tree(table_tree(nl3), rename_rows)
#'
rapply_table_tree <- function(x, f, apply_to_NULL = FALSE, ...) { # nolint
  if (is(x, "rtable") || is.null(x)) {
    if (is.null(x) && !apply_to_NULL) NULL else f(x, ...)
  } else {
    table_tree(lapply(x, FUN = rapply_table_tree, f = f, ...))
  }
}
