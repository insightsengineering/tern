
# Questions:
# we don't name children with their content name because names may change and this would not get updated
# setClassUnion is broken (mget error), so we will not use node or NULL, set children to NULL to handle empty node
# todo: can we have several calls to setGeneric with different function signatures?



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
#'
#' @exportClass node
#' @name node-class
#' @rdname node-class
setClass("node", slots = c(name = "character", content = "ANY", children = "list"))
# only called when method new is called

#' Children must not be null
#'
#' @name node-validity
#' @rdname node-class
setValidity("node", function(object) {
  all(
    # if it has > 0 children, no child is null
    !any(vapply(object@children, is.null, logical(1))),
    # check all names are unique
    length(union(names(object@children), c())) == length(names(object@children)),
    # check names of children agree with list
    all(names(object@children) == vapply(object@children, function(child) child@name, character(1)))
  )
})

#' Create an object of class node
#'
#' A node is also known as a tree. This function makes sure that the children are properly named.
#' They can then be properly accessed through node_children[['childName']]
#' See the class node for more info about parameters.
#'
#' Note: changing the names of the children after calling this function will cause the children list names
#' to be no longer in-sync. (or should accessor update child names??)
#'
#' @param name name of node
#' @param content content of node
#' @param children children of node
#'
#' @return object of class node
#'
#' @export
#'
#' @examples
#' n11 <- node(name = "A", content = array(c(1:6), dim = c(2,3)), children = list())
#' n12 <- node(name = "B", content = array(c(1:6), dim = c(2,3)), children = list())
#' n13 <- node(name = "C", content = array(c(1:6), dim = c(2,3)), children = list())
#' n2 <- node(name = "D", content = c(1:3), children = list(n11, n12, n13))
#' # incorrect example (children with same name):
#' #node(name = "A", content = c(1:3), children = list("A"))
#' @name node
#' @rdname node-class
node <- function(name, content, children) {
  #names(children) <- vapply(children, function(child) child@name, character(1))
  new("node", name = name, content = content, children = unname(children))
}

#' Create a summary of the object
#'
#' @param x node object
#' @param ... other args
#'
#' @export summary
setGeneric(
  "summary",
  function(x, ...) standardGeneric("summary"),
  signature = "x"
)

#' Creates a summary of a tree to first-depth.
#' It displays the dimension and class of each entry in an rtable.
#'
#' @param index path to node (excluding node name itself)
#'
#' @rdname summary
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
#' @importFrom rtables rtable rcell rrow rbindl_rtables
setMethod("summary", signature = "node", definition = function(x, index = numeric(0)) {
  format_dim_fcn <- function(x, output) {
    paste(x, collapse = " x ")
  }
  rbindl_rtables(c(
    list(rtable(
      header = c("index", "content", "dim"),
      rrow(x@name,
           index,
           class(x@content),
           rcell(dim(x@content), format = format_dim_fcn),
           indent = length(index)
      )
    )),
    Map(function(x, i) {
      summary(x, index = c(index, i))
    }, x@children, seq_along(x@children))
  ))
})

#' Applies the function f to each node in the tree x
#'
#' @param x tree object
#' @param f function to apply to each node in the tree recursively, f(name, content, c(path, name))
#'
#' @export rapply_tree
setGeneric(
  "rapply_tree",
  function(x, f, ...) standardGeneric("rapply_tree"),
  signature = "x"
)
#' f is applied to the content of each node and a new node of class 'target_obj_class' is created
#' with children applied recursively
#' f takes as argument the content and the path to the node and ...
#' todo: also pass object name to f??
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
#'   if (is(content, "integer")) {
#'     paste("integer", content, collapse = ":")
#'   } else if (is(content, "matrix")) {
#'     paste("matrix", content, collapse = ":")
#'   } else {
#'     "unknown"
#'   }
#' }))
#'
#' @export rapply_tree
#' @rdname rapply_tree
setMethod("rapply_tree", signature = "node", definition = function(x, f, target_obj_class = NULL, path = character(0), ...) {
  stopifnot(
    is.function(f),
    is.function(target_obj_class) || is.null(target_obj_class),
    is.character(path)
  )
  if (is.null(x)) {
    return(NULL) # this is an empty child
  }
  if (is.null(target_obj_class)) {
    #print(paste("Selecting class", class(x)))
    target_obj_class <- function(...) new(class(x), ...)
  }
  new_path <- c(path, x@name)
  target_obj_class(
    name = x@name,
    content = f(name = x@name, content = x@content, path = new_path, ...),
    children = lapply(x@children, function(child) rapply_tree(child, f, target_obj_class, path = new_path, ...))
  )
})

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
  #paste0(get_indent_str(indent), utils::capture.output(cat(x))[[1]], "...") # only print first line of cat output
  paste0(get_indent_str(indent), "Default printout:", substr(toString(x), 1, 15), "...")
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
  # todo: handle when name is None
  paste(
    paste0(get_indent_str(indent), x@name),
    paste(lapply(c(list(x@content), x@children), displayable, indent = indent + 1), collapse = "\n"),
    sep = "\n"
  )
})

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
  #browser()
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
  do.call(`[[`, args = c(list(x = children[[ indices[[1]] ]]), indices[-1], list(return_content = return_content)))
})

