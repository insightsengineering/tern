#' Add Titles, Footnotes, Page Number, and a Bounding Box to a Grid Grob
#'
#' This function is useful to label grid grobs (also ggplot2, and lattice plots)
#' with title, footnote, and page nuumbers.
#' 
#' @inheritParams grid::grob
#' @param grob a grid grob object, optionally \code{NULL} if ongly a grob with
#'   the decoration should be shown.
#' @param titles vector of character strings. Vector elements are separated by a
#'   newline and strings are wrapped according to the page with.
#' @param footnotes vector of character string. Same rules as for \code{titles}.
#' @param page string with page numeration, if NULL then no page number is displayed.
#' @param width_titles unit object
#' @param width_footnotes unit object
#' @param border boolean, whether a a border should be drawn around the plot or
#'   not.
#' @param margins unit object of length 4
#' @param padding  unit object of length 4
#' @param outer_margins  unit object of length 4
#' @param gp_titles a gpar object
#' @param gp_footnotes a gpar object
#' 
#' @return a grid grob (gTree) 
#'
#' @export
#' 
#' @template author_waddella
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
#'     page = "Page 4 of 10"
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
#'     page = "Page 6 of 129"
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
#'     page =  "Page 6 of 129"
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
#'     page = "Page 6 of 129"
#'   )
#' )
decorate_grob <- function(grob,
                          titles,
                          footnotes,
                          page = "",
                          width_titles = unit(1, "npc"),
                          width_footnotes = unit(1, "npc") - stringWidth(page),
                          border = TRUE,
                          margins = unit(c(1,0,1,0), "lines"),
                          padding = unit(rep(1, 4), "lines"),
                          outer_margins = unit(c(2, 1.5, 3, 1.5), "cm"),
                          gp_titles = gpar(),
                          gp_footnotes = gpar(fontsize = 8),
                          name = NULL,
                          gp = gpar(),
                          vp = NULL) {
  
  st_titles <- splitTextGrob(titles, x = 0, y = 1, just = c("left", "top"), width = width_titles,
                             vp = viewport(layout.pos.row = 1, layout.pos.col = 1),
                             gp = gp_titles)
  
  
  st_footnotes <- splitTextGrob(footnotes, x = 0, y = 1, just = c("left", "top"), width = width_footnotes,
                                vp = viewport(layout.pos.row = 3, layout.pos.col = 1),
                                gp = gp_footnotes)
  
  g <- gTree(
    grob = grob,
    titles = titles,
    footnotes = footnotes,
    page = page,
    width_titles = width_titles,
    width_footnotes = width_footnotes,
    border = border,
    margins = margins,
    padding = padding,
    outer_margins = outer_margins,
    gp_titles = gp_titles,
    gp_footnotes = gp_footnotes,
    
    children = gList(
      gTree(
        children = gList(
          st_titles,
          gTree(
            children = gList(
              rectGrob(),
              gTree(
                children = gList(
                  grob
                ), 
                vp = plotViewport(margins = padding)
              )
            ),
            vp = vpStack(viewport(layout.pos.row = 2, layout.pos.col = 1), plotViewport(margins = margins))
          ),
          st_footnotes,
          textGrob(page, x = 1, y = 0, just = c("right", "bottom"),
                   vp = viewport(layout.pos.row = 3, layout.pos.col = 1),
                   gp = gp_footnotes)
        ),
        childrenvp = ,
        name = "titles_grob_footnotes",
        vp = vpStack(
          plotViewport(margins = outer_margins),
          viewport(layout = grid.layout(nrow = 3, ncol = 1,
                                        heights = unit.c(
                                          grobHeight(st_titles),
                                          unit(1, "null"),
                                          grobHeight(st_footnotes)
                                        )))
        )
      )
    ),
    name = name,
    gp = gp,
    vp = vp,
    cl = "decoratedGrob"
  )
}


#' @export
validDetails.decoratedGrob <- function(x) {
  
  if (!is.grob(x$grob) && !is.null(x$grob))
    stop("grob argument is expected to be a grid grob object or NULL")
  
  if (!is.character(x$titles)) stop("argument titles is not of type character")
  if (!is.character(x$footnotes)) stop("argument footnotes is not of type character")
  if (!is.character(x$page) && length(x$page) == 1) stop("page is not a character of length 1")
  
  if (!is.unit(x$outer_margins) && length(x$outer_margins) != 4)
    stop("outer_margins needs to be a unit object of length 4 (bottom, left, top, right)")
  if (!is.unit(x$margins) && length(x$margins) != 4)
    stop("margins needs to be a unit object of length 4 (bottom, left, top, right)")
  if (!is.unit(x$padding) && length(x$padding) != 4)
    stop("padding needs to be a unit object of length 4 (bottom, left, top, right)")
  
  x
}


#' @export
widthDetails.decoratedGrob <- function(x) {
  unit(1, "null")
}

#' @export
heightDetails.decoratedGrob <- function(x) {
  unit(1, "null")
}


# Adapted from Paul Murell R Graphics 2nd Edition
# https://www.stat.auckland.ac.nz/~paul/RG2e/interactgrid-splittext.R
splitString <- function(text, width) {
  
  availwidth <- convertWidth(width, "in", valueOnly = TRUE)
  textwidth <- convertWidth(stringWidth(text), "in", valueOnly = TRUE)
  strings <- strsplit(text, " ")[[1]]
  
  if (textwidth <= availwidth || length(strings) == 1) {
    text
  } else {
    gapwidth <- stringWidth(" ")
    newstring <- strings[1]
    linewidth <- stringWidth(newstring)
    
    for (i in 2:length(strings)) {
      str_width <- stringWidth(strings[i])
      if (convertWidth(linewidth + gapwidth + str_width, "in", valueOnly = TRUE) < availwidth) {
        sep <- " "
        linewidth <- linewidth + gapwidth + str_width
      } else {
        sep <- "\n"
        linewidth <- str_width
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
#' @inheritParams grid::grid.text
#' @param text character string
#' @param width a unit object specifying max width of text
#' @param ... passed on to \code{\link{grob}}
#' 
#' @details
#' This code is taken from R Graphics by Paul Murell, 2nd edition
#' 
#' @export
#' 
#' @examples 
#' sg <- splitTextGrob(text = paste(
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
#' ))
#' 
#' grobHeight(sg)
#' 
#' grid.newpage()
#' pushViewport(plotViewport())
#' grid.rect()
#' grid.draw(sg)
#' 
#' grid.rect(
#'  height = grobHeight(sg), width = unit(1, "cm"), gp = gpar(fill = "red")
#' )
#' 
#' # stack splitTextGrob
#' grid.newpage()
#' pushViewport(plotViewport())
#' grid.rect()
#' grid.draw(splitTextGrob(c("Hello, this is a test", "and yet another test"), just = c("left", "top"), x = 0, y = 1))
#' 
#' 
splitTextGrob <- function(text, x = unit(0.5, "npc"), y = unit(0.5, "npc"), width = unit(1, "npc"),
                          just = "centre", hjust = NULL, vjust = NULL, 
                          default.units = "npc", name = NULL, gp = gpar(), vp = NULL) {

  if (!is.unit(x)) 
    x <- unit(x, default.units)
  if (!is.unit(y)) 
    y <- unit(y, default.units)
  
  if (!is.unit(width) || !(length(width) == 1))
    stop("width is supposed to be a unit object of length 1")
  
  ## if it is a fixed unit then we do not need to recalculate when viewport resized
  if (!is(width, "unit.arithmetic") && attr(width, "unit") %in% c("cm", "inches", "mm", "points", "picas", "bigpts",
                                 "dida", "cicero", "scaledpts")) {
    attr(text, "fixed_text") <- paste(vapply(text, splitString, character(1), width = width), collapse = "\n")
  }

  grob(text = text, x = x, y = y, width = width, just = just, hjust = hjust, 
       vjust = vjust, rot = 0, check.overlap = FALSE, 
       name = name, gp = gp, vp = vp, cl = "splitText")
}

#' @export
validDetails.splitText <- function(x) {
  if (!is.character(x$text))
    stop("text is supposed to be of type character")
  
  if (!is.unit(x$width) || !(length(x$width) == 1))
    stop("width is supposed to be a unit object of length 1")
  
  x
}

#' @export
heightDetails.splitText <- function(x) {
  txt <- if (!is.null(attr(x$text, "fixed_text"))) {
    attr(x$text, "fixed_text")
  } else {
    paste(vapply(x$text, splitString, character(1), width = x$width), collapse = "\n")
  }
  stringHeight(txt)
}

#' @export
widthDetails.splitText <- function(x) {
  x$width
}

#' @export
drawDetails.splitText <- function(x, recording) {
  txt <- if (!is.null(attr(x$text, "fixed_text"))) {
    attr(x$text, "fixed_text")
  } else {
    paste(vapply(x$text, splitString, character(1), width = x$width), collapse = "\n")
  }
  
  x$width <- NULL
  x$label <- txt
  x$text <- NULL
  class(x) <- c("text", class(x)[-1])
  
  grid.draw(x)
}
 
#' Automatically updates page number
#'
#' @param ... passed on to decorate_grob
#'
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

