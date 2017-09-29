

#' create a table
#' 
#' @param colnames vector with column names
#' @param ... each element is a row
#' 
#' @export
#' 
#' @examples
#' 
#' tbl <- rtable(c("Treatement", "Comparison"),
#'   cell_format("xx (xx.xx%)"),
#'   rrow("Response", c(104, .2), c(100, .4)),
#'   rrow("Non-Response", c(23, .4), c(43, .5)),
#'   empty_row(),
#'   rrow("more"),
#'   rrow("HR", merge_cells(2, cf(3.23, "xx.xx"))),
#'   rrow("95% CI", indent = 2, merge_cells(2, cf(3.23, "xx.xx")))
#' )
#' 
#' tbl
#' 
#' dim(tbl)
#' nrow(tbl)
#' ncol(tbl)
#' 
#' 
#' as_html(tbl)
rtable <- function(col.names, ...) {
  
  ncols <- length(col.names)
  if (ncols <= 1) stop("table needs at least one 1 columns")
  
  ## check if n-cols correct
  rows <- list(...)
  
  check_consistent_ncols(rows, ncols)

  nrow <- sum(vapply(rows, class, character(1)) == "rrow")
  
  structure(
    rows,
    col.names = col.names,
    ncol = ncols,
    nrow = nrow,
    class = "rtable"
  )
}

check_consistent_ncols <- function(rows, ncols) {
  lapply(rows, function(r) {
    if (is(r, "rrow")) {
      if (length(r) != 0) {
        ncells <- vapply(r, function(cell) {
          if (is(cell, "merged_cell")) {
            attr(cell, "ncells")
          } else {
            1
          }
        }, numeric(1))
        
        if (sum(ncells) != ncols) {
          stop(paste("row", attr(r, "label"), "has", sum(ncells), "cells instead of expected", ncols))
        }
      }
    }
  })
  invisible(TRUE)
}

#' @export
dim.rtable <- function(x) {
  as.vector(unlist(attributes(x)[c("nrow", "ncol")]))
}


#' @export
print.rtable <- function(x, ...) {
  print(paste("rtable of dimension:", paste(dim(x), collapse = "x")))
}

#' @export
as_html <- function(x, format = c("xx.xx", "xx.xx / xx.xx")) {
  UseMethod("as_html")  
}

as_html.default <- function(x, format) {
  stop("no as_html method for class ", class(x))
}

as_html.rtable <- function(x, format =  c("xx.xx", "xx.xx / xx.xx")) {
  
}

as_html.row <- function(x, format) {
  
}

as_html.cell_format <- function(x, format) {
  
}

as_html.merged_cell <- function(x, format) {
  
}

as_html.empty_row <- function(x, format) {
  
}


#' table row
#' 
#' @export
rrow <- function(label, ..., format = NULL, indent = 1) {
  structure(
    list(...),
    label = label,
    format = format,
    indent = indent,
    class = "rrow"
  )
}

#' @export
merge_cells <- function(num, cell) {
  structure(
    cell,
    ncells = num,
    class = "merged_cell"
  )
}

#' @export 
cf <- function(cell, format) {
  attr(cell, "format") <- format
  cell
}


#' @export
cell_format <- function(format) {
  structure(
    format,
    class = "cell_format"
  )
}


#' @export
empty_row <- function() {
  structure(
    "-",
    class = "empty_row"
  )
}


