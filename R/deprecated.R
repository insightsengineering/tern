
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
