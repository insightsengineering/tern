#' Add Titles, Footnotes, Page Number, and a Bounding Box to a Grid Grob
#'
#' This function is useful to label grid grobs (also ggplot2, and lattice plots)
#' with title, footnote, and page nuumbers.
#'
#' @param grob a grid grob object, optionally \code{NULL} if ongly a grob with
#'   the decoration should be shown.
#' @param titles vector of character strings. Vector elements are separated by a
#'   newline and strings are wrapped according to the page with.
#' @param footnotes vector of character string. Same rules as for \code{titles}.
#' @param page page number, if NULL then no page number is displayed.
#' @param npages if page is not NULL then the total number of pages in the plot
#'   is expected.
#' @param border boolean, whether a a border should be drawn around the plot or
#'   not.
#' @param border_padding if border is \code{TRUE} then this expects a unit
#'   object with the padding.
#' @param margins a unit vector with outer margins (bottom, left, top, right).
#' @param pady \code{\link{unit}} object with space between title and plot and
#'   plot and footnote.
#' @param gp a gpar object for all of the plot
#' @param gp_footnote a gpar object for the footnote and page grob
#'
#' @return a grid grob (gTree) 
#'
#' @export
#'
#' @examples
#'
#' titles <- c(
#'   "Edgar Anderson's Iris Data",
#'   paste(
#'     "This famous (Fisher's or Anderson's) iris data set gives the measurements",
#'      "in centimeters of the variables sepal length and width and petal length",
#'      "and width, respectively, for 50 flowers from each of 3 species of iris."
#'   )
#'  )
#'   
#' footnotes <- c(
#'   "The species are Iris setosa, versicolor, and virginica.",
#'   paste(
#'   "iris is a data frame with 150 cases (rows) and 5 variables (columns) named",
#'   "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species."
#'   )
#' )
#'   
#'
#' ## empty plot
#' grid.newpage()
#' grid.draw(
#'   decorate_grob(
#'     NULL,
#'     titles = titles,
#'     footnotes = footnotes,
#'     page = 4,
#'     npages = 10
#'   )
#' )
#'
#' # grid
#' p <- gTree(
#'    children = gList(
#'      rectGrob(),
#'      xaxisGrob(),
#'      yaxisGrob(),
#'      textGrob("Sepal.Length", y = unit(-4, "lines")),
#'      textGrob("Petal.Length", x = unit(-3.5, "lines"), rot = 90),
#'      pointsGrob(iris$Sepal.Length, iris$Petal.Length, gp = gpar(col=iris$Species), pch=16)
#'    ),
#'    vp = vpStack(plotViewport(), dataViewport(xData = iris$Sepal.Length, yData = iris$Petal.Length))
#' )
#' grid.newpage(); grid.draw(p)
#'
#' grid.newpage()
#' grid.draw(
#'   decorate_grob(
#'     grob = p,
#'     titles = titles,
#'     footnotes = footnotes,
#'     page = 1,
#'     npage = 1
#'   )
#' )
#'
#' ## with ggplot2
#' library(ggplot2)
#' p_gg <- with(iris, qplot(Sepal.Length, Petal.Length, col = Species))
#' p_gg
#' p <- ggplotGrob(p_gg)
#' grid.newpage()
#' grid.draw(
#'   decorate_grob(
#'     grob = p,
#'     titles = titles,
#'     footnotes = footnotes,
#'     page = 1,
#'     npage = 1
#'   )
#' )
#'
#' ## with lattice
#' library(lattice)
#' xyplot(Sepal.Length ~ Petal.Length, data = iris, col = iris$Species)
#' p <- grid.grab()
#' grid.newpage()
#' grid.draw(
#'   decorate_grob(
#'     grob = p,
#'     titles = titles,
#'     footnotes = footnotes,
#'     page = 1,
#'     npage = 1
#'   )
#' )
decorate_grob <- function(grob,
                          titles,
                          footnotes,
                          page = NULL,
                          npages = NULL,
                          outer_margins = unit(c(2, 1.5, 3, 1.5), "cm"),
                          plot_margins = unit(rep(1.1, 4), "lines"),
                          plot_padding = unit(rep(1.1, 4), "lines"),
                          bbox_plot = TRUE,
                          gp = NULL,
                          gp_footnote = NULL,
                          vp = NULL) {
  
  if (!is.grob(grob) && !is.null(grob)) stop("grob argument is expected to be a grid grob object or NULL")
  
  if (!is.character(titles)) stop("argument titles is not of type character")
  if (!is.character(footnotes)) stop("argument footnotes is not of type character")
  if (!is.numeric(page) && !is.null(page)) stop("page is not numeric or NULL")
  if (!is.numeric(npages) && !is.null(npages)) stop("npages is not numeric or NULL")
  
  if (!is.unit(outer_margins) && length(outer_margins) != 4)
    stop("margins needs to be a unit object of length 4 (bottom, left, top, right)")
  if (!is.unit(plot_margins) && length(plot_margins) != 4)
    stop("plot_margins needs to be a unit object of length 4 (bottom, left, top, right)")
  if (!is.unit(plot_padding) && length(plot_padding) != 4)
    stop("plot_padding needs to be a unit object of length 4 (bottom, left, top, right)")
  
  
  
  
  
  
  titles <- paste(wrap.text(titles, gp = gp), collapse = "\n")
  footnotes <- paste(wrap.text(footnotes, gp = gp_footnote), collapse = "\n")
  
  titles_grob <- textGrob(titles, x = unit(0, "npc"), just = "left",
                          vp = viewport(layout.pos.col = 1, layout.pos.row = 1))
  
  page_grob <- if (!is.null(page)) {
    if (is.null(npages)) stop("npages expected")
    if (length(page) != 1) stop("page is expected to be of length 1")
    textGrob(paste("Page", page, "of", npages), x = unit(1, "npc"), y = unit(0, "npc"), just = c("right", "bottom"), gp = gp_footnote)
  } else {
    NULL
  }
  
  footnotes_grob <- textGrob(footnotes, x = unit(0, "npc"), y = unit(0, "npc"), just = c("left", "bottom"), gp = gp_footnote)
  
  footnotes_page_grob <- gTree(
    children = gList(footnotes_grob, page_grob),
    viewport(name = "vp_footnotes", layout.pos.col = 1, layout.pos.row = 5)
  )
  
  decorated_plot_grob <- if (border) {
    gTree(
      children = gList(
        rectGrob(),
        gTree(
          children = gList(grob),
          vp = plotViewport(name = "vp_main_plot", margins = convertUnit(border_padding, "lines", valueOnly = TRUE))
        )
      ),
      vp = viewport(layout.pos.col = 1, layout.pos.row = 3)
    )
  } else {
    editGrob(grob, vp = viewport(name = "vp_main_plot", layout.pos.col = 1, layout.pos.row = 3))
  }
  
  gTree(
    children = gList(
      titles_grob,
      decorated_plot_grob,
      footnotes_page_grob
    ),
    vp = vpStack(
      plotViewport(
        name = "margins",
        margins = convertUnit(margins, "lines", valueOnly = TRUE)
      ),
      viewport(
        name = "vp_toplevel_layout",
        layout = grid.layout(
          nrow = 5,
          ncol = 1,
          heights = unit.c(
            unit(1, "grobheight", titles_grob),
            pady,
            unit(1,"null"),
            pady,
            unit.pmax(unit(1, "grobheight", footnotes_grob),
                      unit(1, "grobheight", if (is.null(page_grob)) nullGrob() else page_grob))
          )
        )
      )
    ),
    gp = gp
  )
}


splitString <- function(text) {
  
  availwidth <- convertWidth(unit(1, "npc"), "in", valueOnly = TRUE)
  textwidth <- convertWidth(stringWidth(text), "in", valueOnly = TRUE)
  strings <- strsplit(text, " ")[[1]]
  
  if (textwidth <= availwidth || length(strings) == 1) {
    text
  } else {
    gapwidth <- stringWidth(" ")
    newstring <- strings[1]
    linewidth <- stringWidth(newstring)
    
    for (i in 2:length(words)) {
      width <- stringWidth(strings[i])
      if (convertWidth(linewidth + gapwidth + width, "in", valueOnly = TRUE) < availwidth) {
        sep <- " "
        linewidth <- linewidth + gapwidth + width
      } else {
        sep <- "\n"
        linewidth <- width
      }
      newstring <- paste(newstring, strings[i], sep = sep)
    }
    newstring
  }
} 

#' Split Text According To Available Text Width
#' 
#' Dynamically wrap text
#' 
#' @param text character string
#' @param ... passed on to \code{\link{grob}}
#' 
#' @details
#' This code is taken from R Graphics by Paul Murell, 2nd edition
#' 
#' @noRd
#' 
#' @examples 
#' 
#' grid.newpage()
#' pushViewport(plotViewport())
#' grid.rect()
#' grid.draw(splitTextGrob(text = paste(
#'   "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum vitae",
#'   "dapibus dolor, ac mattis erat. Nunc metus lectus, imperdiet ut enim eu,",
#'   "commodo scelerisque urna. Vestibulum facilisis metus vel nibh tempor, sed",
#'   "elementum sem tempus. Morbi quis arcu condimentum, maximus lorem id,",
#'   "tristique ante. Nullam a nunc dui. Fusce quis lacus nec ante dignissim",
#'   "faucibus nec vitae tellus. Suspendisse mollis et sapien eu ornare. Vestibulum",
#'   "placerat neque nec justo efficitur, ornare varius nulla imperdiet. Nunc justo",
#'   "sapien, vestibulum eget efficitur eget, porttitor id ante. Nulla tempor",
#'   "luctus massa id elementum. Praesent dictum, neque vitae vestibulum malesuada,",
#'   "nunc nisi blandit lacus, sit amet tristique odio dui sit amet velit."
#' )))
#' 
splitTextGrob <- function(text, ...) {
  grob(text = text, cl = "splitText", ...)
}

drawDetails.splitText <- function(x, recording) {
  grid.text(splitString(x$text), x = 0, y = 1, just = c("left", "top"))
}


#' Automatically updates page number
#'
#' @param ... passed on to decorate_grob
#'
#' @export
#'
#' @return closure that increments the page number
#'
#' @examples
#' pf <- decorate_grob_factory(
#'   titles = "This is a test\nHello World",
#'   footnotes = "Here belong the footnotess",
#'   npages = 3
#' )
#'
#' grid.newpage(); grid.draw(pf(NULL))
#' grid.newpage(); grid.draw(pf(NULL))
#' grid.newpage(); grid.draw(pf(NULL))
#' \dontrun{
#' grid.newpage(); grid.draw(pf(NULL))
#' }
decorate_grob_factory <- function(...) {
  args <- list(...)
  if (is.null(args$npages)) stop("nages needs to be specified")
  
  npages <- args$npages
  current_page <- 0
  function(grob) {
    current_page <<- current_page + 1
    if (current_page > npages) stop(paste("current page is", current_page, "but max.", npages, "specified."))
    decorate_grob(grob = grob, page = current_page, ...)
  }
}


#' Decorate all a set of grobs and add the page numbering
#'
#' this uses the decorate_grob_factory
#'
#'
#' @export
#'
#' @examples
#' g <- with(iris, {
#'   list(
#'     ggplotGrob(qplot(Sepal.Length, Sepal.Width, col=Species)),
#'     ggplotGrob(qplot(Sepal.Length, Petal.Length, col=Species)),
#'     ggplotGrob(qplot(Sepal.Length, Petal.Width, col=Species)),
#'     ggplotGrob(qplot(Sepal.Width, Petal.Length, col=Species)),
#'     ggplotGrob(qplot(Sepal.Width, Petal.Width, col=Species)),
#'     ggplotGrob(qplot(Petal.Length, Petal.Width, col=Species))
#'   )
#' })
#' lg <- decorate_grob_set(grobs = g, titles = "Hello\nOne\nTwo\nThree", footnotes = "")
#' grid.newpage(); grid.draw(lg[[1]])
#' grid.newpage(); grid.draw(lg[[2]])
#' grid.newpage(); grid.draw(lg[[6]])
decorate_grob_set <- function(grobs, ...) {
  args <- list(...)
  if (!is.null(args$plot_grob)) stop("do not use the plot_grob argument for this function.")
  n <- length(grobs)
  lgf <- decorate_grob_factory(npages = n, ...)
  lapply(grobs, lgf)
}

#' Return split text that fits onto page width
#'
#'
#' @param txt string (or vector of strins) to be split in multiple lines, not that
#' \code{'\n'} is also split into to lines
#' @param width max with of string, by default the width of the current viewport
#' @param gp graphical parameters for text
#'
#'
#' @return a vector with the new strings
#'
#' @examples
#'
#' library(grid)
#' grid.newpage()
#' pushViewport(plotViewport())
#' grid.rect()
#' text <- "g_uefg_ex02_il05_ip11_CABRTR72_NIS DRAFT Individual Patient Calculated ABR Bar Chart, Treated bleeds, intra−patient comparison, NIS"
#' grid.text(paste(wrap.text(text), collapse = "\n"), just = "left", x = unit(0, "npc"))
wrap.text <- function(txt,
                      width=convertWidth(unit(1,'npc'), 'inch', TRUE),
                      gp=gpar()) {
  
  if (length(txt) == 0) return(character(0))
  
  gstringWidth <- function(label) {
    vapply(label,
           function(lab) convertWidth(grobWidth(textGrob(lab, gp=gp)), 'inch', TRUE),
           numeric(1)
    )
  }
  
  space_width <- gstringWidth(" ")
  width_s <- width - space_width
  
  # splits a string into multiple strings that fit
  splitstr <- function(str) {
    if (gstringWidth(str) > width_s) {
      strs <- unlist(strsplit(str, " ", fixed=TRUE))
      n <- length(strs)
      
      if (n <= 1) {
        str
      } else {
        strsw <- gstringWidth(strs) + space_width
        part <- rep(NA_integer_, n)
        # string partition
        k <- 1
        part[1] <- k
        s <- strsw[1] # current width
        for (i in 2:n) {
          if (s + strsw[i] > width_s) {
            k <- k + 1
            s <- strsw[i]
          } else {
            s <- s + strsw[i]
          }
          part[i] <- k
        }
        as.vector(tapply(strs, part, function(x)paste(x, collapse = " "), simplify = TRUE))
      }
    } else {
      str
    }
  }
  strs_to_split <- unlist(Map(function(x)if (length(x) == 0) "" else x, strsplit(txt, "\n", fixed = TRUE)))
  
  unlist(Map(function(str) splitstr(str), strs_to_split), use.names = FALSE)
}
