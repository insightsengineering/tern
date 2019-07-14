#' check if elements ... have the same dimension
#'
#' @param ... data.frames or vectors
#' @param omit.NULL are \code{NULL} elements in \code{...} to be omitted from the check?
#'
#' @importFrom stats na.omit
#'
#' @noRd
#'
check_same_n <- function(..., omit_null = TRUE) {

  dots <- list(...)

  n_list <- Map(function(x, name) {
    if (is.null(x)) {
      if (omit_null) {
        NA_integer_
      } else {
        stop("arg", name, "is not supposed to be NULL")
      }
    } else if (is.data.frame(x)) {
      nrow(x)
    } else if (is.atomic(x)) {
      length(x)
    } else {
      stop("data structure for ", name, "is currently not supported")
    }
  }, dots, names(dots))

  n <- na.omit(unlist(n_list))

  if (length(unique(n)) > 1) {
    sel <- which(n != n[1])
    stop("dimension missmatch:", paste(names(n)[sel], collapse = ", "), " do not have N=", n[1])
  }

  TRUE
}

check_same_n_l <- function(x) {
  do.call(check_same_n, x)
}


#' Check strata_data
#'
#' strata_data can be NULL or
#'
#' @param x data.frame with valid stratification data
#'
#' @noRd
#'
check_data_frame <- function(x, allow_missing = FALSE) {

  xname <- deparse(substitute(x))

  if (!is.null(x)) {
    if (!is.data.frame(x)) {
      stop(xname, " needs to be either NULL or a data.frame")
    }

    if (!allow_missing) {
      is_missing <- vapply(x, function(var) {
        if (is.numeric(var)) {
          any(is.na(var))
        } else {
          any(is.na(var)) || any(var == "")
        }
      }, logical(1))

      if (any(is_missing)) {
        stop(xname, " can not have any missing values (NA or '')")
      }
    }
  }

  TRUE
}

# if total is non-null then it can not be in the levels of col_by
check_col_by <- function(col_by,
                         col_N, # nolint
                         min_num_levels = 2,
                         total = NULL) {
  stopifnot(
    is.no_by(col_by) || is.factor(col_by),
    !any(is.na(col_by)) && !("" %in% levels(col_by)),
    length(col_N) == nlevels(col_by),
    nlevels(col_by) >= min_num_levels,
    is.null(total) || !(total %in% levels(col_by))
  )

  TRUE
}


check_is_event <- function(x) {
  stopifnot(
    is.logical(x),
    !any(is.na(x))
  )

  TRUE
}


check_is_factor <- function(x, allow_na = TRUE) {
  stopifnot(is.factor(x))

  if (!allow_na) {
    if (any(is.na(x))) {
      stop(deparse(substitute(x)), " cannot have any missing data")
    }
  }

  TRUE
}


check_is_numeric <- function(x, allow_na = TRUE) {
  stopifnot(is.numeric(x))

  if (!allow_na) {
    if (any(is.na(x))) {
      stop(deparse(substitute(x)), " cannot have any missing data")
    }
  }

  TRUE
}
