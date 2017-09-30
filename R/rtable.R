

#' Creating Reporting Tables
#' 
#' Reporting tables allow multiple values per cell, cell formatting and mergin 
#' cells. Currently \code{rtable}s can be converted to html.
#' 
#' 
#' @param col.names vector with column names
#' @param ... each element is an \code{\link{rrow}} object
#' @param format a valid format string
#' 
#' @return a \code{rtable} object
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
#'   rrow(),
#'   rrow("this is a very long section header"),
#'   rrow("HR", rcell(3.23, "xx.xx", colspan = 2)),
#'   rrow("95% CI", indent = 1, rcell(c(2.432, 4.3214), format = "(xx.x, xx.x)", colspan = 2))
#' )
#' 
#' tbl
#' 
#' row.names(tbl)
#' names(tbl)
#' 
#' 
#' tbl[1,2]
#' 
#' tbl[3,2]
#' tbl[5,1]
#' tbl[5,2]
#' 
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
#' # colspans
#' 
#' tbl2 <- rtable(
#'   c("A", "B", "C", "D", "E"),
#'   format = "xx",
#'   rrow("r1", 1, 2, 3, 4, 5),
#'   rrow("r2", rcell("sp2", colspan = 2), "sp1", rcell("sp2-2", colspan = 2))
#' )
#' 
#' tbl2
#' 
#' Viewer(tbl2)
#' 
#' tbl2[1,3]
#' tbl2[2,1]
#' tbl2[2,2]
#' tbl2[2,3]
#' tbl2[2,4]
#' tbl2[2,5]
#' 
rtable <- function(col.names, format = NULL, ...) {
  
  ncol <- length(col.names)
  if (ncol <= 1) stop("table needs at least one 1 columns")
  
  ## check if n-cols correct
  rows <- list(...)
  
  if (!all(vapply(rows, is, logical(1), "rrow"))) stop("not all arguments in ... are of class rrow")
  nrow <- length(rows)
  
  check_consistent_ncols(rows, ncol)

  rows_formated <- lapply(rows, function(row) {
   
    row_f <- lapply(row, function(cell) {
      rc <- if (is(cell, "rcell")) {
        if (is.null(attr(cell, "format"))) {
          structure(cell, format = format)
        } else {
          cell
        }
      } else {
        rcell(cell, format = format)
      }
      if (is.null(attr(rc, "format"))) stop("NULL format for cell: ", rc)
      rc
    }) 
    
    attributes(row_f) <- attributes(row)
    row_f
  })
  
  structure(
    rows_formated,
    col.names = col.names,
    ncol = ncol,
    nrow = nrow,
    class = "rtable"
  )
}

#' Reporting Table Row
#' 
#' @export
#' 
#' @examples 
#' 
#' rrow("ABC", c(1,2), c(3,2))
#' 
rrow <- function(row.name, ..., format = NULL, indent = 0) {
  
  cells <- list(...)
  
  cells_f <- lapply(cells, function(cell) {
    if (is(cell, "rcell")) {
      if (is.null(attr(cell, "format"))) {
        structure(cell, format = format)
      } else {
        cell
      }
    } else {
      rcell(cell, format = format)
    }
  })
  
  
  structure(
    cells_f,
    row.name = if (missing(row.name)) NULL else row.name,
    indent = indent,
    class = "rrow"
  )
}

# row <- rrow("ABC", rcell(3.23, format = "xx.x", colspan = 2))
ncells <- function(row) {
  if (length(row) == 0) {
    0 
  } else {
    ni <- vapply(row, function(cell) {
      if (is(cell, "rcell")) {
        attr(cell, "colspan")
      } else {
        1
      }
    }, numeric(1))
    sum(ni)
  } 
}

#' @export
rcell <- function(x, format = NULL, colspan=1) {
  structure(
    x,
    format = format,
    colspan = colspan,
    class = "rcell"
  )
}

check_consistent_ncols <- function(rows, ncols) {
  lapply(rows, function(row) {
    # zero length is possible (label only)
    if (length(row) != 0 && ncells(row) != ncols) {
      stop(paste("row", attr(row, "row.name"), "has", sum(ncells(row)), "cells instead of expected", ncols))
    }
  })
  invisible(TRUE)
}

#' @export
dim.rtable <- function(x) {
  as.vector(unlist(attributes(x)[c("nrow", "ncol")]))
}

#' @export
row.names.rtable <- function(x) {
  vapply(x, function(row) {
    rn <- attr(row, "row.name")
    if (is.null(rn)) "" else rn
  }, character(1))
}

#' @export
names.rtable <- function(x) {
  attr(x, "col.names")
}

#' @export
as_html <- function(x, ...) {
  UseMethod("as_html")  
}

#' @export
as_html.default <- function(x, ...) {
  stop("no as_html method for class ", class(x))
}

#' @export
as_html.rtable <- function(x, ...) {
  
  ncol <- ncol(x)

  # split header into lines
  col_headers <- lapply(attr(x, "col.names"), function(colname) {
    els <- unlist(strsplit(colname, "\n", fixed = TRUE))
    Map(function(el, is.last) {
      tagList(el, if (!is.last) tags$br() else NULL)
    }, els, c(rep(FALSE, length(els) -1), TRUE))
  })

  tags$table(
    class = "table",
    ...,
    tags$tr(tagList(tags$th(""), lapply(col_headers, tags$th, align="center", class="text-center"))), 
    lapply(x, as_html, ncol = ncol)
  )
  
}

#' @export
as_html.rrow <- function(x, ncol, ...) {
  
  indent <- attr(x, "indent")
  row.name <- attr(x,"row.name")
  
  cells <- if (length(x) == 0) {
    tags$td(colspan = as.character(ncol+1), class="rowname", align="left", row.name)
  } else {
    tagList(
      tags$td(class="rowname", align = "left", row.name),
      lapply(x, function(xi) {
        
        colspan <- attr(xi, "colspan")
        if (is.null(colspan)) stop("colspan for rcell is NULL")
        
        cell_content <- format_rcell(xi, output="html")
        if (colspan == 1) {
          tags$td(cell_content, align = "center")
        } else {
          tags$td(cell_content, colspan = as.character(colspan), align = "center")
        }
      }),
      replicate(ncol - ncells(x), tags$td(), simplify = FALSE)
    )
  }
  
  if (indent>0) {
    cells[[1]]$attribs <- c(cells[[1]]$attribs, list(style=paste0("padding-left: ", indent*3, "ch")))
  }
  
  tags$tr(cells)
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
      tags$link(href="css/bootstrap.min.css", rel="stylesheet")
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


formats_1d <- c(
  "xx", "xx.", "xx.x", "xx.xx", "xx.xxx",
  "xx%", "xx.x%", "xx.xx%", "xx.xxx%"
)

formats_2d <- c(
  "xx / xx", "xx. / xx.", "xx.x / xx.x", "xx.xx / xx.xx",
  "xx (xx%)", "xx (xx.%)", "xx (xx.x%)", "xx (xx.xx%)", 
  "xx. (xx.%)", "xx.x (xx.x%)", "xx.xx (xx.xx%)",
  "(xx, xx)", "(xx., xx.)", "(xx.x, xx.x)", "(xx.xx, xx.xx)"
)

#' @export
get_rcell_formats <- function() {
  structure(  
    list(
    "1d" = formats_1d,
    "2d" = formats_2d
    ),
    info = "xx does not modify the element, and xx. rounds a number to 0 digits"
  )
}


#' @export
format_rcell <- function(x, format, output = c("html", "ascii")) {
  
  output <- match.arg(output)
  
  if (missing(format)) format <- attr(x, "format")
  
  if (is.null(x)) stop("format missing")

  l <- if (format %in% formats_1d) {
    1
  } else if (format %in% formats_2d) {
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


#' @export
`[.rtable` <- function(x, i, j, ...) {
  
  if (missing(i) || missing(j)) stop("both i and j need to be defined to access elements in rtable")
  
  row <- x[[i]]
  if (length(row) == 0) {
    NULL # no cell information
  } else {
    nc <- ncol(x)
    nci <- vapply(row, function(cell) attr(cell, "colspan") , numeric(1))
    j2 <- rep(1:length(nci), nci)
    
    row[[j2[j]]]
  }
}



#' @export
print.rtable <- function(x, gap = 8, ...) {
  
  nchar_rownames <- max(vapply(row.names(x), nchar, numeric(1)))
  
  header_row <- do.call(rrow, as.list(c(row.name="", format = "xx", names(x))))
  
  nchar_col <- ceiling(max(unlist(lapply(c(list(header_row), x), function(row) {
    lapply(row, function(cell) {
      nc <- nchar(unlist(strsplit(format_rcell(cell, output = "ascii"), "\n", fixed = TRUE)))
      nc / attr(cell, "colspan")
    })
  }))))
  
  
  txt_header <- row_to_str(header_row, nchar_rownames, nchar_col, gap)
  
  txt_cells <- lapply(x, function(row) {
    row_to_str(row, nchar_rownames, nchar_col, gap)
  })
  

  cat(paste(
    c(
      txt_header,
      paste(rep("-", nchar_rownames + ncol(x)*(nchar_col + gap)), collapse = ""),
      txt_cells
    ),
    collapse = "\n"
  ))
  cat("\n")
}

row_to_str <- function(row, nchar_rownames, nchar_col, gap) {
  
  if (length(row) == 0) {
    rname <- attr(row, "row.name")
    if (is.null(rname)) "" else rname 
  } else {
    cells <- lapply(row, function(cell) {
      unlist(strsplit(format_rcell(cell, output = "ascii"), "\n", fixed = TRUE))
    })
    
    nlines <- max(vapply(cells,length, numeric(1)))
    
    cells_same_length <- lapply(cells, function(x) {
      if (length(x) == nlines) {
        x
      } else {
        c(x, rep(NA_character_, nlines -length(x)))
      }
    })
    
    colspans <- vapply(row, function(cell) attr(cell, "colspan"), numeric(1))
    lines <- as.matrix(Reduce(cbind, cells_same_length))
    
    row_char <- paste0(
      padstr(attr(row, "row.name"), nchar_rownames, "left"),
      paste(unlist(Map(function(cell, colspan) {
        list(spaces(gap), padstr(cell, colspan*nchar_col+ (colspan-1)*gap))
      }, lines[1,], colspans)), collapse = "")
    )
    
    if (nrow(lines) > 1) {
      additional_rows <- apply(lines[-1, , drop=FALSE], 1, function(r) {
        paste0(
          spaces(nchar_rownames),
          paste(unlist(Map(function(cell, colspan) {
            list(spaces(gap), padstr(cell, colspan*nchar_col + (colspan-1)*gap))
          }, r, colspans)), collapse = "")
        )
      })
      row_char <- paste(c(row_char, additional_rows), collapse = "\n")
    }
    row_char
  } 
}


padstr <- function(x, n, just = c("center", "left", "right")) {
 
  just <- match.arg(just)

  if (is.na(x)) x <- ""
    
  nc <- nchar(x)
  if (n < nc) stop(x, " has more than ", n, " characters")
  
  switch(
    just,
    center = {
      pad <- (n-nc)/2  
      paste0(spaces(floor(pad)), x, spaces(ceiling(pad)))
    },
    left = paste0(x, spaces(n-nc)),
    right = paste0(spaces(n-nc), x)
  )
}

spaces <- function(n) {
  paste(rep(" ", n), collapse = "")
}
