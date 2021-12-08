#' Add Titles, Footnotes, Page Number, and a Bounding Box to a Grid Grob
#'
#' This function is useful to label grid grobs (also \code{ggplot2}, and \code{lattice} plots)
#' with title, footnote, and page numbers.
#'
#' @inheritParams grid::grob
#' @param grob a grid grob object, optionally \code{NULL} if only a \code{grob} with
#'   the decoration should be shown.
#' @param titles vector of character strings. Vector elements are separated by a
#'   newline and strings are wrapped according to the page with.
#' @param footnotes vector of character string. Same rules as for \code{titles}.
#' @param page string with page numeration, if \code{NULL} then no page number is displayed.
#' @param width_titles unit object
#' @param width_footnotes unit object
#' @param border boolean, whether a a border should be drawn around the plot or
#'   not.
#' @param margins unit object of length 4
#' @param padding  unit object of length 4
#' @param outer_margins  unit object of length 4
#' @param gp_titles a \code{gpar} object
#' @param gp_footnotes a \code{gpar} object
#'
#' @details
#' The titles and footnotes will be ragged, i.e. each title will be wrapped
#' individually.
#'
#' \if{html}{
#' The layout can be illustrated as follows:
#'
#' \figure{decorate_grob.png}{options: alt="decorate_grob layout"}
#' }
#'
#' @return a grid grob (\code{gTree})
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
#'     "in centimeters of the variables sepal length and width and petal length",
#'     "and width, respectively, for 50 flowers from each of 3 species of iris."
#'   )
#' )
#'
#' footnotes <- c(
#'   "The species are Iris setosa, versicolor, and virginica.",
#'   paste(
#'     "iris is a data frame with 150 cases (rows) and 5 variables (columns) named",
#'     "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species."
#'   )
#' )
#'
#'
#' ## empty plot
#' library(grid)
#'
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
#'   children = gList(
#'     rectGrob(),
#'     xaxisGrob(),
#'     yaxisGrob(),
#'     textGrob("Sepal.Length", y = unit(-4, "lines")),
#'     textGrob("Petal.Length", x = unit(-3.5, "lines"), rot = 90),
#'     pointsGrob(iris$Sepal.Length, iris$Petal.Length, gp = gpar(col = iris$Species), pch = 16)
#'   ),
#'   vp = vpStack(plotViewport(), dataViewport(xData = iris$Sepal.Length, yData = iris$Petal.Length))
#' )
#' grid.newpage()
#' grid.draw(p)
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
#' ## with \code{ggplot2}
#' library(ggplot2)
#'
#' p_gg <- with(iris, qplot(Sepal.Length, Petal.Length, col = Species))
#' p_gg
#' p <- ggplotGrob(p_gg)
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
#' ## with \code{lattice}
#' library(lattice)
#'
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
#'
#' # no borders
#' library(gridExtra)
#' grid.newpage()
#' grid.draw(
#'   decorate_grob(
#'     tableGrob(
#'       head(mtcars)
#'     ),
#'     titles = "title",
#'     footnotes = "footnote",
#'     border = FALSE
#'   )
#' )
decorate_grob <- function(grob,
                          titles,
                          footnotes,
                          page = "",
                          width_titles = grid::unit(1, "npc"),
                          width_footnotes = grid::unit(1, "npc") - grid::stringWidth(page),
                          border = TRUE,
                          margins = grid::unit(c(1, 0, 1, 0), "lines"),
                          padding = grid::unit(rep(1, 4), "lines"),
                          outer_margins = grid::unit(c(2, 1.5, 3, 1.5), "cm"),
                          gp_titles = grid::gpar(),
                          gp_footnotes = grid::gpar(fontsize = 8),
                          name = NULL,
                          gp = grid::gpar(),
                          vp = NULL) {
  st_titles <- split_text_grob(
    titles,
    x = 0, y = 1,
    just = c("left", "top"),
    width = width_titles,
    vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1),
    gp = gp_titles
  )

  st_footnotes <- split_text_grob(
    footnotes,
    x = 0, y = 1,
    just = c("left", "top"),
    width = width_footnotes,
    vp = grid::viewport(layout.pos.row = 3, layout.pos.col = 1),
    gp = gp_footnotes
  )

  grid::gTree(
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

    children = grid::gList(
      grid::gTree(
        children = grid::gList(
          st_titles,
          grid::gTree(
            children = grid::gList(
              if (border) grid::rectGrob(),
              grid::gTree(
                children = grid::gList(
                  grob
                ),
                vp = grid::plotViewport(margins = padding)
              )
            ),
            vp = grid::vpStack(
              grid::viewport(layout.pos.row = 2, layout.pos.col = 1),
              grid::plotViewport(margins = margins)
            )
          ),
          st_footnotes,
          grid::textGrob(
            page,
            x = 1, y = 0,
            just = c("right", "bottom"),
            vp = grid::viewport(layout.pos.row = 3, layout.pos.col = 1),
            gp = gp_footnotes
          )
        ),
        childrenvp = NULL,
        name = "titles_grob_footnotes",
        vp = grid::vpStack(
          grid::plotViewport(margins = outer_margins),
          grid::viewport(
            layout = grid::grid.layout(
              nrow = 3, ncol = 1,
              heights = grid::unit.c(
                grid::grobHeight(st_titles),
                grid::unit(1, "null"),
                grid::grobHeight(st_footnotes)
              )
            )
          )
        )
      )
    ),
    name = name,
    gp = gp,
    vp = vp,
    cl = "decoratedGrob"
  )
}


#' @importFrom grid validDetails
#' @export
validDetails.decoratedGrob <- function(x) { #nolint #nousage
  stopifnot(
    grid::is.grob(x$grob) || is.null(x$grob),
    is.character(x$titles),
    is.character(x$footnotes),
    is.character(x$page) || length(x$page) != 1,
    grid::is.unit(x$outer_margins) || length(x$outer_margins) == 4,
    grid::is.unit(x$margins) || length(x$margins) == 4,
    grid::is.unit(x$padding) || length(x$padding) == 4
  )

  x
}

#' @importFrom grid widthDetails
#' @export
widthDetails.decoratedGrob <- function(x) { #nolint #nousage
  grid::unit(1, "null")
}

#' @importFrom grid heightDetails
#' @export
heightDetails.decoratedGrob <- function(x) { #nolint #nousage
  grid::unit(1, "null")
}


# Adapted from Paul Murell R Graphics 2nd Edition
# https://www.stat.auckland.ac.nz/~paul/RG2e/interactgrid-splittext.R
split_string <- function(text, width) {
  availwidth <- grid::convertWidth(width, "in", valueOnly = TRUE)
  textwidth <- grid::convertWidth(grid::stringWidth(text), "in", valueOnly = TRUE)
  strings <- strsplit(text, " ")[[1]]

  if (textwidth <= availwidth || length(strings) == 1) {
    text
  } else {
    gapwidth <- grid::stringWidth(" ")
    newstring <- strings[1]
    linewidth <- grid::stringWidth(newstring)

    for (i in 2:length(strings)) {
      str_width <- grid::stringWidth(strings[i])
      if (grid::convertWidth(linewidth + gapwidth + str_width, "in", valueOnly = TRUE) < availwidth) {
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
#' Dynamically wrap text.
#'
#' @inheritParams grid::grid.text
#' @param text character string
#' @param width a unit object specifying max width of text
#'
#' @details
#' This code is taken from R Graphics by \code{Paul Murell}, 2nd edition
#'
#' @export
#'
#' @examples
#' sg <- split_text_grob(text = paste(
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
#' library(grid)
#' grobHeight(sg)
#'
#' grid.newpage()
#' pushViewport(plotViewport())
#' grid.rect()
#' grid.draw(sg)
#'
#' grid.rect(
#'   height = grobHeight(sg), width = unit(1, "cm"), gp = gpar(fill = "red")
#' )
#'
#' # stack split_text_grob
#' grid.newpage()
#' pushViewport(plotViewport())
#' grid.rect()
#' grid.draw(split_text_grob(
#'   c("Hello, this is a test", "and yet another test"),
#'   just = c("left", "top"), x = 0, y = 1
#' ))
split_text_grob <- function(text,
                            x = grid::unit(0.5, "npc"),
                            y = grid::unit(0.5, "npc"),
                            width = grid::unit(1, "npc"),
                            just = "centre",
                            hjust = NULL,
                            vjust = NULL,
                            default.units = "npc", # nolint
                            name = NULL,
                            gp = grid::gpar(),
                            vp = NULL) {
  if (!grid::is.unit(x)) {
    x <- grid::unit(x, default.units)
  }
  if (!grid::is.unit(y)) {
    y <- grid::unit(y, default.units)
  }

  stopifnot(grid::is.unit(width) && length(width) == 1)

  ## if it is a fixed unit then we do not need to recalculate when viewport resized
  if (!inherits(width, "unit.arithmetic") &&
    !is.null(attr(width, "unit")) &&
    attr(width, "unit") %in% c("cm", "inches", "mm", "points", "picas", "bigpts", "dida", "cicero", "scaledpts")) {
    attr(text, "fixed_text") <- paste(vapply(text, split_string, character(1), width = width), collapse = "\n")
  }

  grid::grob(
    text = text,
    x = x, y = y,
    width = width,
    just = just,
    hjust = hjust,
    vjust = vjust,
    rot = 0,
    check.overlap = FALSE,
    name = name,
    gp = gp,
    vp = vp,
    cl = "dynamicSplitText"
  )
}

#' @importFrom grid validDetails
#' @export
validDetails.dynamicSplitText <- function(x) { #nolint #nousage
  stopifnot(
    is.character(x$text),
    grid::is.unit(x$width) && length(x$width) == 1
  )

  x
}

#' @importFrom grid heightDetails
#' @export
heightDetails.dynamicSplitText <- function(x) { #nolint #nousage
  txt <- if (!is.null(attr(x$text, "fixed_text"))) {
    attr(x$text, "fixed_text")
  } else {
    paste(vapply(x$text, split_string, character(1), width = x$width), collapse = "\n")
  }
  stringHeight(txt)
}

#' @importFrom grid widthDetails
#' @export
widthDetails.dynamicSplitText <- function(x) { #nolint #nousage
  x$width
}

#' @importFrom grid drawDetails
#' @export
drawDetails.dynamicSplitText <- function(x, recording) { #nolint #nousage
  txt <- if (!is.null(attr(x$text, "fixed_text"))) {
    attr(x$text, "fixed_text")
  } else {
    paste(vapply(x$text, split_string, character(1), width = x$width), collapse = "\n")
  }

  x$width <- NULL
  x$label <- txt
  x$text <- NULL
  class(x) <- c("text", class(x)[-1])

  grid::grid.draw(x)
}

#' Update Page Number
#'
#' Automatically updates page number.
#'
#' @param npages number of pages in total
#' @param ... passed on to \code{\link{decorate_grob}}
#'
#' @return closure that increments the page number
#'
#' @export
#'
#' @template author_waddella
#'
#' @examples
#' pf <- decorate_grob_factory(
#'   titles = "This is a test\nHello World",
#'   footnotes = "Here belong the footnotess",
#'   npages = 3
#' )
#'
#' library(grid)
#' draw_grob(pf(NULL))
#' draw_grob(pf(NULL))
#' draw_grob(pf(NULL))
decorate_grob_factory <- function(npages, ...) {
  current_page <- 0
  function(grob) {
    current_page <<- current_page + 1
    if (current_page > npages) {
      stop(paste("current page is", current_page, "but max.", npages, "specified."))
    }
    decorate_grob(grob = grob, page = paste("Page", current_page, "of", npages), ...)
  }
}


#' Decorate Set of `grobs` and Add Page Numbering
#'
#' Note that this uses the `decorate_grob_factory` function.
#'
#' @param grobs a list of grid grobs
#' @param ... arguments passed on to \code{\link{decorate_grob}}
#'
#' @export
#'
#' @template author_waddella
#'
#' @examples
#' library(ggplot2)
#' g <- with(iris, {
#'   list(
#'     ggplotGrob(qplot(Sepal.Length, Sepal.Width, col = Species)),
#'     ggplotGrob(qplot(Sepal.Length, Petal.Length, col = Species)),
#'     ggplotGrob(qplot(Sepal.Length, Petal.Width, col = Species)),
#'     ggplotGrob(qplot(Sepal.Width, Petal.Length, col = Species)),
#'     ggplotGrob(qplot(Sepal.Width, Petal.Width, col = Species)),
#'     ggplotGrob(qplot(Petal.Length, Petal.Width, col = Species))
#'   )
#' })
#' lg <- decorate_grob_set(grobs = g, titles = "Hello\nOne\nTwo\nThree", footnotes = "")
#' library(grid)
#' draw_grob(lg[[1]])
#' draw_grob(lg[[2]])
#' draw_grob(lg[[6]])
decorate_grob_set <- function(grobs, ...) {
  n <- length(grobs)
  lgf <- decorate_grob_factory(npages = n, ...)
  lapply(grobs, lgf)
}
