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
  },
  dots, names(dots))

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
#' strata_data can be \code{NULL} or
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
      is_missing <- vapply(
        x,
        function(var) {
          if (is.numeric(var)) {
            any(is.na(var))
          } else {
            any(is.na(var)) || any(var == "")
          }
        },
        logical(1)
      )

      if (any(is_missing)) {
        stop(xname, " can not have any missing values (NA or '')")
      }
    }
  }

  TRUE
}


is_logical_vector_modif <- function(x, min_size = 1) {
  !is.null(x) &&
    is.atomic(x) &&
    length(x) >= min_size &&
    all_true(x, utils.nest::is_logical_single)
}

#' Checks that \code{x}, \code{col_by} and \code{col_N} are consistent in dimensions
#'
#' See \code{\link{t_summary}} variants for how to use it.
#' Also checks that there are no empty levels and at least a specified number of levels.
#' Raises an error on failure.
#'
#' @param x main object which will be queried for length (or rows in case of data.frame)
#' @param col_by matrix of booleans that specifies how to group x (i.e. for each column,
#'   takes all values of x for which the row is true)
#' @param col_N vector that contains number of observations, it can be \code{colSums(col_by)}
#' @param min_num_levels minimum number of columns of \code{col_by}
check_col_by <- function(x,
                         col_by,
                         col_N, # nolint
                         min_num_levels = 2) {
  stopifnot(is.data.frame(col_by))
  stopifnot(is_numeric_vector(col_N))

  if (is.data.frame(x)) {
    stopifnot(nrow(col_by) == nrow(x))
  } else {
    stopifnot(nrow(col_by) == length(x))
  }

  stopifnot(
    ncol(col_by) >= min_num_levels,
    length(col_N) == ncol(col_by),
    all_true(col_by, is_logical_vector),
    !anyNA(col_by) && !("" %in% colnames(col_by))
  )

  invisible(NULL)
}

check_col_by_factor <- function(x,
                                col_by,
                                col_N, # nolint
                                min_num_levels = 2) {
  stopifnot(
    is.factor(col_by),
    !any(is.na(col_by)) && !("" %in% levels(col_by)),
    length(col_N) == nlevels(col_by),
    nlevels(col_by) >= min_num_levels
  )
  if (is.data.frame(x)) {
    stopifnot(nrow(col_by) == nrow(x))
  } else {
    stopifnot(nrow(col_by) == length(x))
  }

  invisible(NULL)
}

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


check_strata <- function(strata_data) {

  if (!is.null(strata_data)) {
    stopifnot(is.data.frame(strata_data) || is_character_vector(strata_data) || is.factor(strata_data))

    if (is.data.frame(strata_data)) {

      strata_type <- vapply(strata_data, FUN = function(x) {
        is_character_vector(x) || is.factor(x)
      }, logical(1))

      if (!all(strata_type)) {
        stop("all stratification factors must be character or factor vector")
      }
    }
  }
}

#' Check input format for stratification variable(s)
#' @param strata_data - data.frame or character vector or factor vector
#' @importFrom utils.nest is_character_vector
#' @noRd
check_strata_levels <- function(strata_data) {

  if (is_character_vector(strata_data) || is.factor(strata_data)) {
    strata_levels <- length(unique(strata_data)) > 1
  }
  else if (is.data.frame(strata_data)) {
    strata_levels <- vapply(strata_data, FUN = function(x) {
      length(unique(x)) > 1
    }, logical(1))
  }

  if (!all(strata_levels)) {
    stop("Not all strata variables have more than 1 level.")
  }
}

check_binary_endpoint_args <- function(rsp, col_by, strata_data){

  check_col_by_factor(rsp, col_by, get_N(col_by), min_num_levels = 2)
  check_same_n(rsp = rsp, col_by = col_by)
  check_is_event(rsp)

  if (!is.null(strata_data)) {
    check_same_n(rsp = rsp, strata_data = strata_data)
    check_data_frame(strata_data)
    check_strata_levels(strata_data)
  }

}


#' check all numbers are in desired range
#' @param x a numeric object
#' @param min a numeric value specifying the lower boundary (exclusive), with default value \code{0}
#' @param max a numeric value specifying the upper boundary (exclusive), with default value \code{1}
#'
#' @importFrom utils.nest is_numeric_single
#' @examples
#' conf.init <- 0.7
#' tern:::check_numeric_range(conf.init)
#'
#' # below produces error
#' quantiles <- c(0, 1.2)
#' \donttest{
#' tern:::check_numeric_range(quantiles)
#' }
check_numeric_range <- function(x, min = 0, max = 1) {
  stopifnot(
    is_numeric_single(min) && is_numeric_single(max),
    is.numeric(x)
  )

  if (!all(x > min & x < max)) {
    stop(paste0("there are numeric values outside the specified range (", min, ", ", max, ")"))
  }

  invisible(NULL)
}

#' Check all row_by variables of appropriate type (factor or data.frame of factors)
#' NA values are not allowed.
#' @param row_by a factor or data.frame of factors
#' @param x a related analysis variable to be split by row_by
#'
#' @examples
#' check_row_by(iris$Species, iris$Sepal.Length)
#'
#' @noRd
check_row_by <- function(row_by, x) {

  check_same_n(x = x, row_by = row_by)

  if (is.atomic(row_by)) {

    check_is_factor(row_by, allow_na = FALSE)

  } else if (is.data.frame(row_by)) {

    stopifnot(!anyNA(row_by))
    rb <- vapply(row_by, is.factor, logical(1))

    if (!all(rb)) {
      stop("Not all row_by variables are factors.")
    }
  }

  invisible(NULL)
}
