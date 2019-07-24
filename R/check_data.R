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
    stop("dimension mismatch:", paste(names(n)[sel], collapse = ", "), " do not have N=", n[1])
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
check_col_by_old <- function(col_by,
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

# copied over from utils.nest which is not open-source
# todo: move to utils.nest once request is implemented
all_true <- function(lst, fcn) {
  all(vapply(lst, fcn, TRUE))
}
is.logical.vector_modif <- function(x, min_size = 1) {
  !is.null(x) &&
    is.atomic(x) &&
    length(x) >= min_size &&
    all_true(x, utils.nest::is.logical.single)
}

# checks col_by and col_N to be consistent
# col_by must be a matrix of booleans
# checks that there are no empty levels and at least a specified number of levels
check_col_by <- function(x,
                         col_by,
                         col_N, # nolint
                         min_num_levels = 2) {
  stopifnot(is.data.frame(col_by))

  if (is.data.frame(x)) {
    stopifnot(nrow(col_by) == nrow(x))
  } else {
    stopifnot(nrow(col_by) == length(x))
  }

  stopifnot(
    ncol(col_by) >= min_num_levels,
    length(col_N) == ncol(col_by),
    all(vapply(col_by, function(col) is.logical.vector_modif(col, min_size = 0), logical(1))),
    !any(is.na(col_by)) && !("" %in% colnames(col_by))
  )

  invisible(NULL)
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
