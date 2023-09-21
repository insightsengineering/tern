% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_grid.R
\name{arrange_grobs}
\alias{arrange_grobs}
\title{Arrange Multiple Grobs}
\usage{
arrange_grobs(
  ...,
  grobs = list(...),
  ncol = NULL,
  nrow = NULL,
  padding_ht = grid::unit(2, "line"),
  padding_wt = grid::unit(2, "line"),
  vp = NULL,
  gp = NULL,
  name = NULL
)
}
\arguments{
\item{...}{grobs.}

\item{grobs}{list of grobs.}

\item{ncol}{number of columns in layout.}

\item{nrow}{number of rows in layout.}

\item{padding_ht}{unit of length 1, vertical space between each grob.}

\item{padding_wt}{unit of length 1, horizontal space between each grob.}

\item{vp}{a \code{\link[=viewport]{viewport()}} object (or \code{NULL}).}

\item{gp}{A \code{\link[=gpar]{gpar()}} object.}

\item{name}{a character identifier for the grob.}
}
\value{
A \code{grob}.
}
\description{
Arrange grobs as a new grob with \verb{n*m (rows*cols)} layout.
}
\examples{
library(grid)

\donttest{
num <- lapply(1:9, textGrob)
grid::grid.newpage()
grid.draw(arrange_grobs(grobs = num, ncol = 2))

showViewport()

g1 <- circleGrob(gp = gpar(col = "blue"))
g2 <- circleGrob(gp = gpar(col = "red"))
g3 <- textGrob("TEST TEXT")
grid::grid.newpage()
grid.draw(arrange_grobs(g1, g2, g3, nrow = 2))

showViewport()

grid::grid.newpage()
grid.draw(arrange_grobs(g1, g2, g3, ncol = 3))

grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
vp1 <- grid::viewport(layout.pos.row = 1, layout.pos.col = 2)
grid.draw(arrange_grobs(g1, g2, g3, ncol = 2, vp = vp1))

showViewport()
}
}