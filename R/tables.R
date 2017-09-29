

#' create a table
#' 
#' @param col.names vector with column names
#' @param ... each element is a row
#' 
#' @importFrom shiny tags
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
#' 
#' as_html(tbl)
rtable <- function(col.names, ...) {
  
  ncol <- length(col.names)
  if (ncol <= 1) stop("table needs at least one 1 columns")
  
  ## check if n-cols correct
  rows <- list(...)
  
  check_consistent_ncols(rows, ncol)

  nrow <- sum(vapply(rows, class, character(1)) == "rrow")
  
  structure(
    rows,
    col.names = col.names,
    ncol = ncol,
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
          stop(paste("row", attr(r, "row.name"), "has", sum(ncells), "cells instead of expected", ncols))
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

#' @export
as_html.default <- function(x, format) {
  stop("no as_html method for class ", class(x))
}

#' @export
as_html.rtable <- function(x, format =  c("xx.xx", "xx.xx / xx.xx")) {
  
  tags$table(
    do.call(tags$tr, lapply(c("", attr(x, "col.names")), tags$th)), 
    lapply(x, as_html)
  )
  
}

#' @export
as_html.rrow <- function(x, format) {
  do.call(tags$tr,
          c(
            list(tags$td(class="rowname", attr(x,"row.name"))),
            lapply(x, function(xi) {
              cell_content <- paste(xi, collapse = " / ")
              if (is(xi, "merged_cell")) {
                as_html.merged_cell(cell_content)
              } else {
                tags$td(cell_content)
              }
            })
          ))
}

#' @export
as_html.cell_format <- function(x, format) {
  NULL
}

#' @export
as_html.merged_cell <- function(x, format) {
  tags$td(colspan = as.character(attr(x, "ncells")), x)
}

#' @export
as_html.empty_row <- function(x, format) {
  tags$tr()
}


#' table row
#' 
#' @export
rrow <- function(row.name, ..., format = NULL, indent = 1) {
  structure(
    list(...),
    row.name = row.name,
    format = format,
    indent = indent,
    class = "rrow"
  )
}

#' @export
merge_cells <- function(num, cell, format = NULL) {
  structure(
    cell,
    format = format,
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


