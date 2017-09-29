

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
#' tbl <- rtable(
#'   col.names = c("Treatement\nN=100", "Comparison\nN=300"),
#'   format = "xx (xx.xx%)",
#'   rrow("Response", c(104, .2), c(100, .4)),
#'   rrow("Non-Response", c(23, .4), c(43, .5)),
#'   empty_row(),
#'   rrow("this is a very long section header"),
#'   rrow("HR", merge_cells(2, cf(3.23, "xx.xx"))),
#'   rrow("95% CI", indent = 2, merge_cells(2, 3.23), format = "xx.x")
#' )
#' 
#' tbl
#' 
#' dim(tbl)
#' nrow(tbl)
#' ncol(tbl)
#' 
#' tbl[[1]][[1]]
#' 
#' as_html(tbl)
#' 
#' Viewer(tbl)
#' 
rtable <- function(col.names, format = NULL, ...) {
  
  ncol <- length(col.names)
  if (ncol <= 1) stop("table needs at least one 1 columns")
  
  ## check if n-cols correct
  rows <- list(...)
  
  check_consistent_ncols(rows, ncol)

  nrow <- length(rows)
  
  structure(
    rows,
    col.names = col.names,
    ncol = ncol,
    nrow = nrow,
    format = format,
    class = "rtable"
  )
}

check_consistent_ncols <- function(rows, ncols) {
  lapply(rows, function(r) {
    if (!is(r, "rrow")) stop("element is not a rrow")
    
    # zero length is possible (label only)
    if (length(r) != 0 && !is(r, "empty_row")) {
      ncells <- n_cells_in_rrow(r)
      if (ncells != ncols) {
        stop(paste("row", attr(r, "row.name"), "has", sum(ncells), "cells instead of expected", ncols))
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
  cat(
    paste(
      "rtable of dimension:", paste(dim(x), collapse = "x"), "\n",
      "currently rtables can only be viewed as html with Viewer()\n"
    )
  )
  
}

#' @export
as_html <- function(x, format, ...) {
  UseMethod("as_html")  
}

#' @export
as_html.default <- function(x, format, ...) {
  stop("no as_html method for class ", class(x))
}

#' @export
as_html.rtable <- function(x, format, ...) {
  
  if (!missing(format)) stop("argument format for as_html.rtable should not be specified")
  
  ncol <- ncol(x)
  
  format <- attr(x, "format")
  
  # split header into lines
  col_headers <- lapply(attr(x, "col.names"), function(colname) {
    els <- unlist(strsplit(colname, "\n", fixed = TRUE))
    y <- Map(function(el, is.last) {
      list(tags$span(el), if (!is.last) tags$br() else NULL)
    }, els, rep(c(FALSE, TRUE), c(length(els) -1, 1)))
    do.call(tagList, y)
  })
  
  tags$table(
    class = "table",
    tags$tr(tagList(tags$th(""), lapply(col_headers, tags$th))), 
    lapply(x, as_html, format=format, ncol = ncol)
  )
  
}

#' @export
as_html.rrow <- function(x, format, ncol, ...) {
  
  if (!is.null(attr(x, "format"))) format <- attr(x, "format")
  
  if (length(x) == 0) {
    tags$tr(
      tags$td(colspan = as.character(ncol+1), class="rowname", attr(x,"row.name"))
    )
  } else {
    do.call(tags$tr,
            c(
              list(tags$td(class="rowname", attr(x,"row.name"))),
              lapply(x, function(xi) {
                if (is(xi, "merged_cell")) {
                  as_html.merged_cell(xi, format)
                } else {
                  tags$td(format_cell(xi, format, output="html"))
                }
              }),
              replicate(ncol - n_cells_in_rrow(x), tags$td(), simplify = FALSE)
            ))   
  }
}


#' @export
as_html.merged_cell <- function(x, format, ...) {
  tags$td(colspan = as.character(attr(x, "ncells")), format_cell(x, format, output="html"))
}

#' @export
as_html.empty_row <- function(x, format, ncol, ...) {
  do.call(tags$tr, replicate(ncol+1, tags$td(), simplify = FALSE))
}


n_cells_in_rrow <- function(r) {
  if (length(r) != 0) {
    ncells <- vapply(r, function(cell) {
      if (is(cell, "merged_cell")) {
        attr(cell, "ncells")
      } else {
        1
      }
    }, numeric(1))
    sum(ncells)
  } else {
    0
  }
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
  
  if (!is.null(attr(cell, "format"))) format <- attr(cell, "format")
  
  structure(
    cell,
    format = format,
    ncells = num,
    class = "merged_cell"
  )
}

#' @export 
cf <- function(cell, format) {
  structure(
    cell,
    format = format
  )
}



#' @export
empty_row <- function() {
  structure(
    "-",
    class = c("empty_row", "rrow")
  )
}


#' @export
Viewer <- function(x, row.names.bold = FALSE) {
  if (!is(x, "rtable")) stop("x is expected to be an rtable")
  
  viewer <- getOption("viewer")
  
  tbl_html <- as_html(x)
  
  sandbox_folder <- file.path(tempdir(), "rtable")
  
  if (!dir.exists(sandbox_folder)) {
    dir.create(sandbox_folder, recursive = TRUE)
    pbs <- file.path(path.package(package = "teal.oncology"), "bootstrap/")
    file.copy(list.files(pbs, full.names = TRUE, recursive = FALSE), sandbox_folder, recursive = TRUE)
    list.files(sandbox_folder)
  }
  
  # get html name
  for (i in 1:10000) {
    htmlFile <- file.path(sandbox_folder, paste0("table", i, ".html"))
    
    if (!file.exists(htmlFile)) {
      break
    } else if (i == 10000) {
      stop("too many html rtables created, restart your session")
    }
  }
  

  html_tbl <- as_html(x)
  
  html_bs <- tags$html(
    lang="en",
    tags$head(
      tags$meta(charset="utf-8"),
      tags$meta("http-equiv"="X-UA-Compatible", content="IE=edge"),
      tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
      tags$title("rtable"),
      tags$link(href="css/bootstrap.min.css", rel="stylesheet"),
      tags$style(type="text/css",
        "td, th { text-align: -webkit-center;}",
         ".rowname {text-align: left;}",
        if (row.names.bold) ".rowname {font-weight: bold;}" else NULL
      )
    ),
    tags$body(
      html_tbl
    )
  )

  cat(
    paste("<!DOCTYPE html>\n",  htmltools::doRenderTags(html_bs)),
    file = htmlFile, append = FALSE
  )

  
  viewer <- getOption("viewer")

  if (!is.null(viewer)) {
    viewer(htmlFile)
  } else {
    utils::browseURL(htmlFile)
  }
  
  
}


#' @export
format_cell <- function(x, format, output = c("html", "ascii")) {
  
  output <- match.arg(output)
  
  if (!is.null(attr(x, "format"))) format <- attr(x, "format")
  
  if (is.null(format)) stop("format is NULL")
  
  l <- if (format %in% c(
    "xx", "xx.", "xx.x", "xx.xx", "xx.xxx",
    "xx%", "xx.x%", "xx.xx%", "xx.xxx%"
  )) {
    1
  } else if (format %in% c(
    "xx / xx", "xx. / xx.", "xx.x / xx.x", "xx.xx / xx.xx",
    "xx (xx%)", "xx (xx.%)", "xx (xx.x%)", "xx (xx.xx%)", 
    "xx. (xx.%)", "xx.x (xx.x%)", "xx.xx (xx.xx%)",
    "(xx, xx)", "(xx., xx.)", "(xx.x, xx.x)", "(xx.xx, xx.xx)"
  )) {
    2
  } else {
    stop("unknown format: ", format)
  }
  
  if (length(x) != l) stop("cell <", paste(x), "> and format ", format, " are of different length")
  
  if (format %in% c()) {
    # output dependent
    paste(x, collapse = " / ")
  } else {
    
    switch(
      format,
      "xx" = as.character(x),
      "xx." = as.character(round(x, 0)),
      "xx.x" = as.character(round(x, 1)),
      "xx.xx" = as.character(round(x, 2)),
      "xx.xxx" = as.character(round(x, 3)),
      "xx%" = paste0(x * 100, "%"),
      "xx.%" = paste0(round(x * 100, 0, "%")),
      "xx.x%" = paste0(round(x * 100, 1), "%"),
      "xx.xx%" = paste0(round(x * 100, 2), "%"),
      "xx.xxx%" = paste0(round(x * 100, 3), "%"),
      "xx / xx" = paste(x, collapse = " / "),
      "xx. / xx." = paste(lapply(x, round, 0)),
      "xx.x / xx.x" = paste(lapply(x, round, 1)),
      "xx.xx / xx.xx" = paste(lapply(x, round, 2)),
      "xx.xxx / xx.xxx" = paste(lapply(x, round, 3)),
      "xx (xx%)" = paste0(x[1], " (", x[2]*100, "%)"),
      "xx (xx.%)" = paste0(x[1], " (", round(x[2]*100, 0), "%)"),
      "xx (xx.x%)" = paste0(x[1], " (", round(x[2]*100, 1), "%)"),
      "xx (xx.xx%)" = paste0(x[1], " (", round(x[2]*100, 2), "%)"),
      "xx. (xx.%)" = paste0(round(x[1],0), " (", round(x[2]*100, 1), "%)"),
      "xx.x (xx.x%)" = paste0(round(x[1],1), " (", round(x[2]*100, 1), "%)"),
      "xx.xx (xx.xx%)" = paste0(round(x[1],2), " (", round(x[2]*100, 2), "%)"),
      "(xx, xx)" = paste0("(",x[1], x[2], ")"),
      "(xx., xx.)" = paste0("(", paste(lapply(x, round, 0), collapse = ", ") , ")"),
      "(xx.x, xx.x)" = paste0("(", paste(lapply(x, round, 1), collapse = ", ") , ")"),
      "(xx.xx, xx.xx)" = paste0("(", paste(lapply(x, round, 2), collapse = ", ") , ")")
    )
    

  }
}

#' @export
as.rtable <- function(x, format = "xx") {
  UseMethod("as.rtable")
}

#' @export
as.rtable.default <- function(x, format) {
  stop("no default implementation for as.rtable")
}
#' convert a table to an rtable
#' 
#' @export
#' 
#' @examples 
#' Viewer(as.rtable(table(iris$Species)))
#' 
#' Viewer(
#'   as.rtable(table(sample(letters[1:4], 200, TRUE), sample(LETTERS[1:4], 200, TRUE))),
#'   row.names.bold = TRUE
#' )
#' 
as.rtable.table <- function(x, format = "xx") {
  
  if (length(dim(x)) == 1) {
    rtable(col.names = names(x), format = format, do.call(rrow, c(list(row.name = "1"), as.list(as.vector(x)))))
  } else {
    X <- as.data.frame.matrix(x)
    do.call(rtable,
            c(list(
              col.names = names(X), format = format
            ),
            Map(function(row, row.name) {
              do.call(rrow, as.list(c(row.name=row.name, setNames(row, NULL))))
            }, as.data.frame(t(X)), rownames(X))
            )
    )
  }
}


