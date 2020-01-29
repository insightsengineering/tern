#' An S4 Class to represent nodes in a tree
#'
#' From Wikipedia: A tree data structure can be defined recursively as a collection of nodes (starting at a root node),
#' where each node is a data structure consisting of a value, together with a list of references to nodes (the
#' "children"), with the constraints that no reference is duplicated, and none points to the root.
#'
#' Use the function \code{\link{node}} to create objects of this class.
#'
#' @slot name name of node
#' @slot content of node (e.g. \code{data.frame} or \code{rtable} objects)
#' @slot children a list of \code{node} objects
#' @slot format_data formatting instructions for conversion with \code{\link{to_rtable}}
#'
#' @exportClass node
#' @name node-class
#' @rdname node-class
setClass(
  "node",
  slots = c(name = "ANY", content = "ANY",
            children = "list",
            format_data = "ANY")
)
# only called when method new is called

#' Children must not be null
#'
#' @name node-validity
#' @rdname node-class
setValidity("node", function(object) {
  format_data <- object@format_data
  # returning FALSE also works, but error message is not informative
  format_integer_or_null <- function(entry) {
    is.null(format_data[[entry]]) || is_numeric_single(format_data[[entry]])
  }
  stopifnot(
    is.null(format_data) || (
      format_integer_or_null("gap_to_children") &&
        format_integer_or_null("children_gap") &&
        format_integer_or_null("children_indent") &&
        format_integer_or_null("content_indent") &&
        (is.null(format_data[["left_header"]]) || is(format_data[["left_header"]], "rheader"))
    ),
    # if it has > 0 children, no child is null
    is_character_single(object@name) ||
      (is(object@name, "invisible_node_name") && is_character_single(unclass(object@name))),
    !any(vapply(object@children, is.null, logical(1))),
    all(vapply(object@children, is, logical(1), "node")),
    # check all names are unique
    length(union(names(object@children), c())) == length(names(object@children)),
    # check names of children agree with list
    all(names(object@children) == vapply(object@children, function(child) child@name, character(1)))
  )
})

#' Create an object of class node
#'
#' A node is also a tree. This function makes sure that the children are properly named.
#' They can then be properly accessed through \code{node_children[['childName']]}.
#' See the class node for more info about parameters.
#'
#' Note: changing the names of the children after calling this function will cause the children list names
#' to be no longer in-sync because assignment via the \code{@@} operator does not check whether the new object is valid,
#' e.g. names of children nodes agree with names in children list.
#' Ideally, we should define functions like setChildren and getChildren, but this seems to clutter the code.
#' Temporarily, we always reassign children names when accessing via \code{`[[`}.
#'
#' @name node
#' @rdname node-class
#'
#' @param name name of node
#' @param content content of node
#' @param children children of node
#' @param format_data format data for conversion with \code{\link{to_rtable}},
#'   \code{list(gap_to_children, children_gap, children_indent, content_indent, left_header)},
#'   default values if not all given, see that function
#'
#' @return object of class node
#'
#' @importFrom methods new
#' @export
#'
#' @examples
#'
#' n11 <- node(name = "A", content = array(c(1:6), dim = c(2,3)), children = list())
#' n12 <- node(name = "B", content = array(c(1:6), dim = c(2,3)), children = list())
#' n13 <- node(name = "C", content = array(c(1:6), dim = c(2,3)), children = list())
#' n2 <- node(name = "D", content = c(1:3), children = list(n11, n12, n13))
#'
#' summary(n11)
#' summary(n2)
#'
#' \dontrun{
#' # incorrect example (children with same name):
#' node(name = "A", content = c(1:3), children = list("A"))
#' }
#'
node <- function(name, content, children = list(), format_data = list()) {
  # keep names?, even if not agreeing with child names? or alternatively treat node@name as display name
  # and list name as invisible name
  # names(children) <- vapply(children, function(child) child@name, character(1)) #nolintr
  children <- children %||% list()
  new("node", name = name, content = content, children = unname(children), format_data = format_data)
}

#' Creates an invisible node with NULL content and invisible name
#'
#' The name of the created node is \code{invisible_node_name(name)}.
#'
#' @param children children of node
#' @param name name of invisible node (invisible name in rtable prints)
#' @param content content
#' @param format_data format_data
#'
#' @return node
#'
#' @export
#'
#'
#' @examples
#' n11 <- node(name = "A", content = array(c(1:6), dim = c(2,3)), children = list())
#' n12 <- node(name = "B", content = array(c(1:6), dim = c(2,3)), children = list())
#' n13 <- node(name = "C", content = array(c(1:6), dim = c(2,3)), children = list())
#' summary(invisible_node(list(n11, n12, n13)))
invisible_node <- function(children, name = "root", content = NULL, format_data = NULL) {
  stopifnot(is_character_single(name))
  node(
    name = invisible_node_name(name),
    content = NULL,
    children = children,
    format_data = format_data
  )
}

#' Create a node with only this object
#'
#' To add children, use the \code{\link{node}} function.
#'
#' @param x object to add as node
#' @param node_name name of the node
#'
#' @return node
#'
#' @export
#'
#' @examples
#' object_to_node(1:2)
#' my_obj <- 1:2
#' object_to_node(my_obj)
#' object_to_node(my_obj, node_name = "hello")
object_to_node <- function(x, node_name = invisible_node_name(deparse(substitute(x)))) {
  node(name = node_name, content = x)
}

#' Overwrite s3 method because s3 and s4 with same names cannot coexist
#'
#' @param object node object
#' @param ... ignored currently
#' @return a summary of the tree, see \code{\link{basic_node_info}}
#'
#' @export
summary.node <- function(object, ...) {
  basic_node_info(object)
}

# basic_node_info ----

#' Summarizes an object
#'
#' @param x node object
#' @param ... other args
#'
#' @export basic_node_info
setGeneric(
  "basic_node_info",
  function(x, ...) standardGeneric("basic_node_info"),
  signature = "x"
)

#' Summarizes a tree of a tree using depth-first.
#' It displays the dimension and class of each node in the tree recursively.
#'
#' @param index path to node (excluding node name itself)
#'
#' @rdname basic_node_info
#'
#' @importFrom rtables rtable rcell rrow rbindl_rtables
#'
#' @examples
#' n11 <- node(name = "A", content = array(c(1:6), dim = c(2,3)), children = list())
#' n2 <- node(name = "D", content = c(1:3), children = list(
#'   node(name = "A", content = array(c(1:6), dim = c(2,3)), children = list()),
#'   node(name = "B", content = array(c(1:6), dim = c(2,3)), children = list()),
#'   node(name = "C", content = array(c(1:6), dim = c(2,3)), children = list())
#' ))
#' summary(n11)
#' summary(n2)
#'
setMethod("basic_node_info", signature = "node", definition = function(x, index = numeric(0)) {
  format_dim_fcn <- function(x, output) {
    paste(x, collapse = " x ")
  }
  rbindl_rtables(c(
    list(rtable(
      header = c("index", "content", "dim", "hide label", "format"),
      rrow(
        x@name,
        index,
        class(x@content),
        rcell(dim(x@content), format = format_dim_fcn),
        is(x@name, "invisible_node_name"),
        x@format_data, #to_string_with_names(x@format_data),
        indent = length(index)
      )
    )),
    Map(function(x, i) {
      basic_node_info(x, index = c(index, i))
    },
    x@children, seq_along(x@children))
  ))
})

# rapply_tree ----

#' Applies the function f to each node in the tree x
#'
#' @param x tree object
#' @param f function to apply to each node in the tree recursively,
#'   f(name, content, path, ...) -> list(name = new_name, content = new_content)
#'   path includes node itself
#'
#' @export rapply_tree
setGeneric(
  "rapply_tree",
  function(x, f, ...) standardGeneric("rapply_tree"),
  signature = "x"
)


# rapply_tree.node ----

#' f is applied to the content of each node and a new node of class 'target_obj_class' is created
#' with children applied recursively
#' depth-first fashion
#'
#' @param target_obj_class target obj class of each node with arguments (name, content, children),
#'   same as original node if NULL
#' @param path path to node in tree
#' @param ... additional args to pass to f
#'
#' @examples
#' n2 <- node(name = "D", content = c(1:3), children = list(
#'   node(name = "A", content = array(c(1:6), dim = c(2,3)), children = list()),
#'   node(name = "B", content = array(c(1:6), dim = c(2,3)), children = list()),
#'   node(name = "C", content = array(c(1:6), dim = c(2,3)), children = list())
#' ))
#' summary(n2)
#' # change class of elements so we can see it in summary()
#' summary(rapply_tree(n2, f = function(name, content, path, ...) {
#'   new_content <- if (is(content, "integer")) {
#'     paste("integer", content, collapse = ":")
#'   } else if (is(content, "matrix")) {
#'     paste("matrix", content, collapse = ":")
#'   } else {
#'     "unknown"
#'   }
#'   list(name = name, content = new_content)
#' }))
#'
#' @export rapply_tree
#' @rdname rapply_tree
setMethod("rapply_tree", signature = "node", definition = function(x,
                                                                   f,
                                                                   target_obj_class = NULL,
                                                                   path = character(0),
                                                                   ...) {
  stopifnot(
    is.function(f),
    is.function(target_obj_class) || is.null(target_obj_class),
    is.character(path)
  )
  if (is.null(x)) {
    return(NULL) # this is an empty child
  }
  if (is.null(target_obj_class)) {
    target_obj_class <- function(...) new(class(x), ...)
  }
  new_path <- c(path, x@name)
  new_node_value <- f(name = x@name, content = x@content, path = new_path,
                      is_leaf = length(x@children) == 0, ...)
  target_obj_class(
    name = new_node_value$name,
    content = new_node_value$content,
    children = lapply(x@children, function(child) rapply_tree(child, f, target_obj_class, path = new_path, ...)),
    format_data = x@format_data
  )
})

# displayable ----

#' Returns output to be displayed with cat()
#'
#' @param x  object to apply to
#' @param indent indent with which to print object
#' @param ... other arguments
#'
#' @export displayable
setGeneric(
  "displayable",
  function(x, ...) standardGeneric("displayable"),
  signature = "x"
)

get_indent_str <- function(indent) {
  paste(rep("  ", indent), collapse = "")
}

#' Displayable output of an arbitrary object truncating the output from toString
#'
#' For classes like integer, must use setOldClass("integer") so that S4 method dispatch works
#' and this function gets called.
#'
#' @export displayable
#' @rdname displayable
setMethod("displayable", signature = "ANY", definition = function(x, indent = 0) {
  content <- toString(x)
  if (length(content) > 18) {
    content <- paste0(substr(toString(x), 1, 15), "...")
  }
  paste0(get_indent_str(indent), content)
})

setOldClass("rtable") # needed to overwrite S4 methods targetting it
#' Display an rtable with indent
#'
#' @export displayable
#' @rdname displayable
#'
#' @examples
#' library(rtables)
#' tbl <- rtable(
#'   header = c("Treatement\nN=100", "Comparison\nN=300"),
#'   format = "xx (xx.xx%)",
#'   rrow("A", c(104, .2), c(100, .4)),
#'   rrow("B", c(23, .4), c(43, .5)),
#'   rrow(),
#'   rrow("this is a very long section header"),
#'   rrow("estimate", rcell(55.23, "xx.xx", colspan = 2)),
#'   rrow("95% CI", indent = 1, rcell(c(44.8, 67.4), format = "(xx.x, xx.x)", colspan = 2))
#' )
#' cat(displayable(tbl))
#' cat(displayable(tbl, indent = 1))
#' cat(displayable(tbl, indent = 2))
#' @importFrom utils capture.output
setMethod("displayable", signature = "rtable", definition = function(x, indent = 0) {
  # we don't use indent in rtable because rtables only shifts the first column
  paste0(get_indent_str(indent), capture.output(print(x)), collapse = "\n")
})

#' Display a node recursing into its children
#'
#' @export displayable
#' @rdname displayable
#'
#' @examples
#' n41 <- node(name = "A", content = 4:60, children = list())
#' n42 <- node(name = "B", content = 7:9, children = list())
#' n43 <- node(name = "C", content = 10:12, children = list())
#' n4 <- node(name = "D", content = c(1:3), children = list(n41, n42, n43))
#' cat(displayable(n4))
setMethod("displayable", signature = "node", definition = function(x, indent = 0) {
  if (is.null(x)) {
    return()
  }
  paste(
    paste0(get_indent_str(indent), x@name, ":"),
    paste(lapply(c(list(x@content), x@children), displayable, indent = indent + 1), collapse = "\n"),
    sep = "\n"
  )
})

# Accessor [[ ----

# cannot overwrite `[[` with setGeneric because it dispatches internally
#' Allows to access nodes recursively, each element in index can either be a number or the name of the node
#'
#' Ensure that children are not named identically. If cannot be avoided, access by index
#'
#' Can use summary to get index to get to certain element
#'
#' @param x node object
#' @param i index (possibly missing)
#' @param j index (possibly missing)
#' @param ... deeper level indexes
#' @param return_content whether to return content of node instead of node
#'
#' @examples
#' n2 <- node(name = "D", content = c(1:3), children = list(
#'   node(name = "A", content = array(c(1:6), dim = c(2,3)), children = list()),
#'   node(name = "B", content = array(c(1:6), dim = c(2,3)), children = list()),
#'   node(name = "C", content = array(c(1:6), dim = c(2,3)), children = list())
#' ))
#' summary(n2)
#' summary(n2[[return_content = FALSE]])
#' n2[[]]
#' n2[[return_content = TRUE]]
#' n2[[1]]
#' n2[["A"]]
#' summary(n2[["A", return_content = FALSE]])
#'
#' n3 <- node(name = "D", content = c(1:12), children = list(
#' node(name = "A", content = c(1:6), children = list(
#'   node(name = "A1", content = c(1:3), children = list()),
#'   node(name = "A2", content = c(4:6), children = list())
#' )),
#' node(name = "B", content = c(7:12), children = list(
#'   node(name = "B1", content = c(7:9), children = list()),
#'   node(name = "B2", content = c(10:12), children = list())
#' ))
#' ))
#' cat(displayable(n3))
#' cat(displayable(n3[["A"]]))
#' cat(displayable(n3[["A", return_content = FALSE]]))
#' cat(displayable(n3[["A", "A1"]]))
#' cat(displayable(n3[["A", "A1", return_content = FALSE]]))
setMethod(`[[`, signature = c("node"), function(x, i, j, ..., return_content = TRUE) {
  # i, j are required, but may be missing, so we include i,j in the indices if necessary
  indices <- list(...)
  if (!missing(j)) {
    indices <- c(j, indices)
  }
  if (!missing(i)) {
    indices <- c(i, indices)
  }
  if (length(indices) == 0) {
    return(if (return_content) {
      x@content
    } else {
      x
    })
  }
  # we set the names so that index can also access by name
  children <- x@children
  names(children) <- vapply(children, function(child) child@name, character(1))
  # simple form: children[[ indices[[1]] ]][[ indices[-1] ]]
  do.call(`[[`, args = c(list(x = children[[indices[[1]]]]), indices[-1], list(return_content = return_content)))
})

#' Create an invisible node
#'
#' Node can be accessed through this name, but name will not be displayed when converted with to_rtable.
#' node_name that will not be displayed, also true when converted to rtable
#' The purpose of this is to still allow indexing in the rtable through the name index
#'
#' @param name name of invisible node
#'
#' @return node
#'
#' @export
#'
#' @examples
#' invisible_node_name("root")
invisible_node_name <- function(name) {
  stopifnot(is.character(name))
  structure(name, class = "invisible_node_name")
}

# to_rtable ----

#' Converts the object to an rtable
#'
#' @param x object
#' @param ... additional arguments to pass
#'
#' @export to_rtable
setGeneric(
  "to_rtable",
  function(x) standardGeneric("to_rtable"),
  signature = "x"
)

#' Specific to nodes
#'
#' @export to_rtable
#' @rdname to_rtable
#'
#' @examples
#' n11 <- node(name = "A", content = array(c(1:6), dim = c(2,3)), children = list())
#' n12 <- node(name = "B", content = array(c(1:6), dim = c(2,3)), children = list())
#' n13 <- node(name = "C", content = array(c(1:6), dim = c(2,3)), children = list())
#' n2 <- node(name = "D", content = c(1:3), children = list(n11, n12, n13))
#'
#' # to_rtable(n2) # not working because not a tree of rtables
#'
#' to_rtable(node(
#'   invisible_node_name("received_treatment"),
#'   content = t_summary(structure(1:5, class = "aaa"), factor(LETTERS[c(1,2,1,1,2)]))
#' ))
setMethod("to_rtable", signature = "node", definition = function(x) {
  stopifnot(is.null(x@content) || is_rtable(x@content) || is(x@content, "node"))
  if (is(x@name, "invisible_node_name")) {
    default_children_indent <- 0
    default_content_indent <- 0
  } else {
    default_children_indent <- 1
    default_content_indent <- 1
  }
  tbl <- indent(
    rbindl_rtables(lapply(x@children, to_rtable), gap = x@format_data[["children_gap"]] %||% 1),
    x@format_data[["children_indent"]] %||% default_children_indent
  )
  if (!is.null(x@content)) {
    # rtable or node
    content <- if (is_rtable(x@content)) {
      x@content
    } else {
      to_rtable(x@content)
    }
    tbl <- rbind(
      indent(content, x@format_data[["content_indent"]] %||% default_content_indent),
      tbl,
      gap = x@format_data[["gap_to_children"]] %||% 1
    )
  }
  tbl <- if (is(x@name, "invisible_node_name")) {
    tbl
  } else {
    if (is_empty_rtable(tbl)) {
      stop("Treat case to insert rrow into empty table")
    } else {
      insert_rrow(tbl, rrow(x@name))
    }
  }
  return(tbl)
})

#' Convert rtable to rtable
#'
#' Trivial (identity)
#'
#' @export to_rtable
#' @rdname to_rtable
setMethod("to_rtable", signature = "rtable", definition = function(x) {
  if (is_empty_rtable(x)) {
    "Empty rtable"
  } else {
    x
  }
})

#' Recursively construct a tree
#'
#' @param info_from_parent info passed on to this node from parent
#' @param f function(info_from_parent, path) -> list(name, content, info_to_children_lst)
#'   that returns the name, the content and the info to pass down to the children
#'   (the latter is a named list with one entry per child)
#'   a child can get its name (as desired by the parent) by looking at \code{path[[length(path)]]}
#' @param path to node (character string of node names leading to it)
#'
#' @export
#'
#' @examples
#' i <- 0
#' res <- recursive_construct_tree(
#'   1:7,
#'   function(info_from_parent, path) {
#'     i <<- i + 1
#'     info_to_children_lst <- if (length(info_from_parent) > 1) {
#'       split(info_from_parent, seq_along(info_from_parent) %% 3)
#'     } else {
#'       NULL
#'     }
#'     list(
#'       name = toString(i),
#'       content = info_from_parent,
#'       info_to_children_lst = info_to_children_lst
#'     )
#'   },
#'   path = "root"
#' )
#' cat(displayable(res))
#'
#' res <- recursive_construct_tree(
#'   1:7,
#'   function(info_from_parent, path) {
#'     info_to_children_lst <- if (length(info_from_parent) > 1) {
#'       split(info_from_parent, seq_along(info_from_parent) %% 3)
#'     } else {
#'       NULL
#'     }
#'     list(
#'       name = path[[length(path)]],
#'       content = info_from_parent,
#'       info_to_children_lst = info_to_children_lst
#'     )
#'   },
#'   path = "root"
#' )
#' cat(displayable(res))
recursive_construct_tree <- function(info_from_parent, f, path = "root") {
  node_val <- f(info_from_parent = info_from_parent, path = path)
  node(
    name = node_val$name,
    content = node_val$content,
    # child_name is only suggested, but a different name can be returned
    children = Map(
      function(info_to_child, child_name) recursive_construct_tree(info_to_child, f = f, path = c(path, child_name)),
      node_val$info_to_children_lst,
      names(node_val$info_to_children_lst)
    ),
    format_data = node_val$format_data
  )
}

#' Split list recursively according to by and return the associated tree
#'
#' @param lst list to split, \code{\link{esplit}} will be applied to all list elements
#' @param by_lst list of columns, each of which is a factor to recursively split by
#' @param drop_empty_levels whether to drop empty levels, this happens often when you have, e.g. two factors,
#' one with levels (clA, clB), the other with levels (clA_1, clA_2, clB_1, clB_2) and only the combinations
#' clA-clA_1, clA-clA_2, clB-clB_1, clB-clB_2 appear out of the eight combinations.
#' @param non_leaves_null whether to assign NULL content to any non-leaves
#'
#' @return node object
#'
#' @export
#'
#' @examples
#' tree <- rsplit_to_tree(
#'   1:5,
#'   by_lst = list(factor(c("M", "M", "F", "F", "F")), factor(c("O", "Y", "Y", "Y", "Y")))
#' )
#' summary(tree)
#' cat(displayable(tree))
#'
#' by_lst <- list(
#'   factor(c(rep("clA", 4), rep("clB", 4))),
#'   factor(c(rep(c("A1", "A2"), 2), rep(c("B1", "B2"), 2)))
#' )
#' summary(rsplit_to_tree(1:8, by_lst))
#' summary(rsplit_to_tree(1:8, by_lst, drop_empty_levels = FALSE))
rsplit_to_tree <- function(lst, by_lst, drop_empty_levels = TRUE, non_leaves_null = FALSE) {
  by_lst <- nested_by(by_lst)
  stopifnot(
    is_logical_single(drop_empty_levels),
    is_logical_single(non_leaves_null),
    is_nested_by(by_lst)
  )
  recursive_construct_tree(
    list(content = lst, by_lst = by_lst),
    function(info_from_parent, path) {
      content <- info_from_parent$content
      by_lst <- info_from_parent$by_lst
      info_to_children_lst <- if (is.null(by_lst) || (length(by_lst) == 0)) {
        NULL
      } else {
        split_by <- if (drop_empty_levels) {
          by_drop_empty_cols(by_lst[[1]])
        } else {
          by_lst[[1]]
        }
        # named list
        esplit(list(content = content, by_lst = by_lst[-1]), split_by)
      }
      if (non_leaves_null && (length(info_to_children_lst) != 0)) {
        # has children, so we set content to NULL
        content <- NULL
      }
      list(
        name = path[[length(path)]],
        content = content,
        info_to_children_lst = info_to_children_lst
      )
    }
  )
}

#' It can also be used to remove children by not returning all indices
#'
#' @param node node to sort
#' @param f function(node) that returns the ordered list of new childrens this node will have,
#'   then recurses into the children
#' @return node
#'
#' @export
#'
#' @examples
#' tree <- node("1", 1, list(
#' node("10", 10),
#' node("7", 7),
#' node("6", 6, list(
#'   node("2", 2),
#'   node("1", 1)
#' ))
#' ))
#' cat(displayable(tree))
#' sorted_tree <- rsort_tree(
#'   tree,
#'   function(node) order(vapply(node@children, function(child) child@content, numeric(1)))
#' )
#' cat(displayable(sorted_tree))
rsort_tree <- function(node, f) {
  children_order <- f(node)
  stopifnot(is.numeric(children_order))
  node(
    name = node@name,
    content = node@content,
    children = lapply(node@children[children_order], rsort_tree, f),
    format_data = node@format_data
  )
}

#' Applies f to node at given depth in depth-first order
#'
#' We prefix it full because you can modify the entire node, i.e. also the children and format_data
#' full_apply_from_depth is also possible to be implemented.
#'
#' Note that we don't make an S4 method here for brevity.
#'
#' @param x node to apply to
#' @param f function(node) -> node to apply to all nodes at given depth
#' @param depth depth at which to apply function
#'
#' @return node
#'
#' @export
#'
#' @examples
#' tree <- node("1", 1, list(
#' node("10", 10),
#' node("7", 7),
#' node("6", 6, list(
#'   node("2", 2),
#'   node("1", 1)
#' ))
#' ))
#' cat(displayable(tree))
#' updated_tree <- full_apply_at_depth(
#'   tree,
#'   function(node) {
#'     # Note: copy-on-write for node
#'     node@content <- node@content + 100
#'     node
#'   },
#'   depth = 2
#' )
#' cat(displayable(updated_tree))
full_apply_at_depth <- function(x, f, depth = 0) {
  stopifnot(is(x, "node"))
  stopifnot(is.numeric(depth), depth >= 0)
  if (depth == 0) {
    f(x)
  } else {
    node(
      name = x@name,
      content = x@content,
      children = lapply(x@children, full_apply_at_depth, f = f, depth = depth - 1),
      format_data = x@format_data
    )
  }
}



#' Node Format Data
#'
#'
#' @param gap_to_children row gap between content and children
#' @param children_gap row gaps between children
#' @param children_indent indentation of children wrt to node with respect to current indent level
#' @param content_indent indentation of content of node with respect to current indent level
#' @param left_header header to add to the left of the rtable, is not inherited
#'
#' @export
#'
#'
#' @examples
#' tbl <- function(x) {
#'   rtable(c("A", "B"), rrow(x, "-", "-"))
#' }
#'
#' to_rtable(node("AAA", tbl("r1")))
#' to_rtable(node("AAA", tbl("r1"), children = list(
#'    node("c1", tbl("r2")),
#'    node("c1", tbl("r2")),
#'    node("c1", tbl("r2"))
#' )))
#'
#'
#' to_rtable(node("AAA", tbl("r1"), children = list(
#'    node("c1", tbl("r2")),
#'    node("c1", tbl("r2")),
#'    node("c1", tbl("r2"))
#' ), format_data = node_format_data(children_indent = 0)))
#'
node_format_data <- function(gap_to_children = NULL, children_gap = NULL, children_indent = NULL,
                             content_indent = NULL, left_header = NULL) {

  Filter(Negate(is.null),
         list(gap_to_children = gap_to_children, children_gap = children_gap,
              children_indent = children_indent, content_indent = content_indent,
              left_header = left_header))

}

#' Convert a named nested list to a tree
#'
#' @inheritParams node
#' @param x nested list
#' @param max_depth maximum depth until which to apply it recursively, depth 0 just creates a node with
#'   the list items as children nodes
#'
#' @importFrom utils.nest is_fully_named_list
#'
#' @export
#'
#' @examples
#' lst <- list(
#'   A = list(b = 1, c = iris),
#'   B = list(e = 1:3, C = list(f = 5, g = 9, h = "a"))
#' )
#' n1 <- nested_list_to_tree(lst)
#' summary(n1)
nested_list_to_tree <- function(x, format_data = NULL, max_depth = .Machine$integer.max) {
  stopifnot(is_fully_named_list(x))
  children <- Map(function(xi, namei) {
    if ((max_depth > 0) && identical(class(xi), "list")) {
      # only do this for pure lists, don't do it for data.frames (it is not is(x, "list"))
      n <- nested_list_to_tree(xi, format_data = format_data, max_depth = max_depth - 1)
      n@name <- namei
      n
    } else {
      node(namei, content = xi, children = NULL, format_data = format_data)
    }
  }, x, names(x))
  node(invisible_node_name("root"), content = NULL, children = children, format_data = format_data)
}
