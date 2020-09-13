
#' check is event
#'
#' @importFrom lifecycle deprecate_warn
check_is_event <- function(x) {

  deprecate_warn("0.6.8.9000", "tern::check_is_event")


  stopifnot(
    is.logical(x),
    !any(is.na(x))
  )

  invisible(NULL)
}


check_is_factor <- function(x, allow_na = TRUE) {

  deprecate_warn("0.6.8.9000", "tern::check_is_factor")

  stopifnot(is.factor(x))

  if (!allow_na) {
    if (any(is.na(x))) {
      stop(deparse(substitute(x)), " cannot have any missing data")
    }
  }

  invisible(NULL)
}


check_is_numeric <- function(x, allow_na = TRUE) {

  deprecate_warn("0.6.8.9000", "tern::check_is_numeric")

  stopifnot(is.numeric(x))

  if (!allow_na) {
    if (any(is.na(x))) {
      stop(deparse(substitute(x)), " cannot have any missing data")
    }
  }

  invisible(NULL)
}


check_binary_endpoint_args <- function(rsp, col_by, strata_data) {


  deprecate_warn("0.6.8.9000", "tern::check_binary_endpoint_args")

  check_col_by_factor(rsp, col_by, get_N(col_by), min_num_levels = 2)
  check_same_n(rsp = rsp, col_by = col_by)
  check_is_event(rsp)

  if (!is.null(strata_data)) {
    check_same_n(rsp = rsp, strata_data = strata_data)
    check_data_frame(strata_data)
    check_strata_levels(strata_data)
  }

}

row_subset <- function(x, rows) { # nousage # nolint

  deprecate_warn("0.6.8.9000", "tern::row_subset")


  # similar to subset function, but the latter is only recommended for interactive use
  if (is.data.frame(x)) {
    x[rows, ]
  } else {
    x[rows]
  }
}


