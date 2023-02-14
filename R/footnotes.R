#' Assign Footnotes
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Assign value to attribute footnote of object `x`.
#'
#' @param x an object
#' @param value character vector
#' @export
#' @examples
#' x <- table(iris$Species)
#' footnotes(x) <- "Species are equally distributed"
#' attributes(x)
`footnotes<-` <- function(x, value = NULL) { # nolint
  dep_msg <- 'Please use current implementation in rtables or directly attr(x, "footnote")'
  lifecycle::deprecate_warn("0.7.11", "footnotes()", details = dep_msg)
  attr(x, "footnote") <- value
  x
}

#' Retrieve Footnotes
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Retrieve value from attribute `footnote` of object `x`.
#'
#' @param x an object
#' @export
#' @examples
#' x <- table(iris$Species)
#' footnotes(x) <- "Species are equally distributed"
#' footnotes(x)
footnotes <- function(x) {
  dep_msg <- 'Please use current implementation in rtables or directly attr(x, "footnote")'
  lifecycle::deprecate_warn("0.7.11", "footnotes()", details = dep_msg)
  attr(x, "footnote")
}

#' Add Footnotes
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' This adds more footnotes.
#'
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
  dep_msg <- 'Please use current implementation in rtables or directly attr(x, "footnote")'
  lifecycle::deprecate_warn("0.7.11", "add_footnotes()", details = dep_msg)
  footnotes(x) <- c(footnotes(x), value)
  x
}
