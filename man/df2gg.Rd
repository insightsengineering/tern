% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_ggplot.R
\name{df2gg}
\alias{df2gg}
\title{Convert \code{data.frame} object to \code{ggplot} object}
\usage{
df2gg(
  df,
  colwidths = NULL,
  font_size = 10,
  col_labels = TRUE,
  col_lab_fontface = "bold",
  hline = TRUE,
  bg_fill = NULL
)
}
\arguments{
\item{colwidths}{(\code{vector} of \code{numeric})\cr a vector of column widths. Each element's position in
\code{colwidths} corresponds to the column of \code{df} in the same position. If \code{NULL}, column widths
are calculated according to maximum number of characters per column.}

\item{font_size}{(\code{numeric})\cr font size.}

\item{col_labels}{(\code{logical})\cr whether the column names (labels) of \code{df} should be used as the first row
of the output table.}

\item{hline}{(\code{logical})\cr whether a horizontal line should be printed below the first row of the table.}

\item{bg_fill}{(\code{character})\cr table background fill color.}

\item{tbl}{(\code{data.frame})\cr a data frame.}

\item{col_labels_fontface}{(\code{character})\cr fontface to apply to the first row (of column labels
if \code{col_labels = TRUE}).}
}
\value{
a \code{ggplot} object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Given a \code{data.frame} object, performs basic conversion to a \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} object built using
functions from the \code{ggplot2} package.
}
\examples{
\dontrun{
df2gg(head(iris, 5))

df2gg(head(iris, 5), font_size = 15, colwidths = c(1, 1, 1, 1, 1))
}
}
\keyword{internal}
