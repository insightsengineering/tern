#' Check Element Dimension
#'
#' Checks if the elements in `...` have the same dimension.
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

check_same_n_l <- function(x) { # nousage # nolint
  do.call(check_same_n, x)
}



#' Checks a Data Frame
#'
#' @param x (`data frame`)\cr which should be checked
#' @param allow_missing (`flag`)\cr whether missing data is allowed in the variables
#'
#' @return `TRUE` or fails
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

#' Check Stratification Data
#'
#' Checks whether `strata_data` is either `NULL` (i.e. not provided) or a valid stratification
#' data input.
#'
#' @param strata_data data.frame with valid stratification data
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


#' Checks Numeric Ranges
#'
#' Checks whether all numbers are in desired range.
#'
#' @param x a numeric object
#' @param min a numeric value specifying the lower boundary (exclusive), with default value \code{0}
#' @param max a numeric value specifying the upper boundary (exclusive), with default value \code{1}
#'
#' @noRd
#'
#' @importFrom utils.nest is_numeric_single
#'
#' @examples
#' conf.init <- 0.7
#' tern:::check_numeric_range(conf.init)
#'
#' # below produces error
#' quantiles <- c(0, 1.2)
#' tern:::check_numeric_range(quantiles)
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
