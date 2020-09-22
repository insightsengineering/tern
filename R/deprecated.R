
#' check is event
#'
#' @param x logical vector
#'
check_is_event <- function(x) {

  stopifnot(
    is.logical(x),
    !any(is.na(x))
  )

  invisible(NULL)
}


check_is_factor <- function(x, allow_na = TRUE) {

  stopifnot(is.factor(x))

  if (!allow_na) {
    if (any(is.na(x))) {
      stop(deparse(substitute(x)), " cannot have any missing data")
    }
  }

  invisible(NULL)
}


check_is_numeric <- function(x, allow_na = TRUE) {

  stopifnot(is.numeric(x))

  if (!allow_na) {
    if (any(is.na(x))) {
      stop(deparse(substitute(x)), " cannot have any missing data")
    }
  }

  invisible(NULL)
}




row_subset <- function(x, rows) { # nousage # nolint

  # similar to subset function, but the latter is only recommended for interactive use
  if (is.data.frame(x)) {
    x[rows, ]
  } else {
    x[rows]
  }
}

#' Check if list or data.frame has elements/variables
#'
#' Checks if list names exist and throws an error otherwise
#'
#' @param data a \code{data.frame} or named list
#' @param names vector with names
#'
#' @return \code{TRUE} if all variables exist and an appropriate error if not.
#'
#' @author Adrian Waddell (waddella), \email{adrian.waddell@roche.com}
#'
#' @noRd
#'
#' @examples
#' # function is not exported
#' `%needs%` <- tern:::`%needs%`
#'
#' iris %needs% c("Sepal.Length", "Petal.Width")
#'
#' \dontrun{
#' iris %needs% "ABC"
#' }
`%needs%` <- function(data, names) { # nousage # nolint
  i <- is.na(match(names, names(data)))

  if (any(i)) {
    msg <- if (sum(i) == 1) {
      paste("variable ", names[i], " does not exist")
    } else {
      paste("variables", paste(names[i], collapse = ", "), "do not exist")
    }
    stop(msg)
  }

  invisible(TRUE)
}
