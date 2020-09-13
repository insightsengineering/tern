#' Attribute
#'
#' Assign value to attribute footnote of object x
#'
#' @param x an object
#' @param value character vector
#' @export
#' @examples
#' x <- table(iris$Species)
#' footnotes(x) <- "Species are equally distributed"
#' attributes(x)

`footnotes<-` <- function(x, value = NULL) { # nolint
  attr(x, "footnote") <- value
  x
}


#' Retrieve value from attribute footnote of object x
#' @param x an object
#' @export
#' @examples
#' x <- table(iris$Species)
#' footnotes(x) <- "Species are equally distributed"
#' footnotes(x)
#'
footnotes <- function(x) {
  attr(x, "footnote")
}

#' Add more footnotes
#' @param x an object
#' @param value character vector
#' @export
#' @examples
#' x <- table(iris$Species)
#' footnotes(x) <- "Species are equally distributed"
#' footnotes(x)
#' add_footnotes(x) <- "Add more footnotes"
#' footnotes(x)

`add_footnotes<-` <- function(x, value) { # nolint
  footnotes(x) <- c(footnotes(x), value)
  x
}
