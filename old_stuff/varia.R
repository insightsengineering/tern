#' Add total to \code{col_N}
#'
#' It adds the sum of the vector as the last element.
#'
#' This is necessary when you manually specify \code{col_N} and \code{col_by} uses the total column
#'   (via \code{by_add_total} or similar)
#'
#' @param col_N count
#'
#' @return new count with total count appended to vector
#'
#' @export
#'
#' @examples
#' col_N_add_total(get_N(data.frame(A = c(TRUE, TRUE, FALSE), B = c(FALSE, FALSE, TRUE))))
col_N_add_total <- function(col_N) { # nolint
  c(col_N, sum(col_N))
}

is_logical_vector_modif <- function(x, min_size = 1) { # nousage # nolint
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







# March 2020
t_summary_true <- function(x, # nousage # nolint
                           col_by,
                           col_N = NULL, # nolint
                           total = NULL,
                           row_name = deparse(substitute(x)),
                           denominator = c("N", "n", "omit")) {
  warning("t_summary_true is deprecated, use t_count_true instead")
}


# January 2019

#' NA replace
#'
#' Replace NA values by NA string level
#'
#' @param x factor, possibly with NAs
#' @param na_level name of new level for NAs
#'
#' @return factor with additional NA level
#'
#' @export
#' @examples
#' explicit_na(factor(c(1, 1, 2)))
#' explicit_na(factor(c(1, 1, NA)), "na")
na_as_level <- function(x, na_level = "NA") {
  warning("na_as_level deprecated use explicit_na instead")

  stopifnot(is.factor(x))

  if (any(is.na(x))) {
    if (na_level %in% levels(x)) {
      stop(na_level, " can not be a level of x")
    }
    levels(x) <- c(levels(x), na_level)
    x[is.na(x)] <- na_level
  }
  x
}


#' Variable type
#'
#' Check all row_by variables of appropriate type (factor or data.frame of factors)
#' NA values are not allowed.
#'
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


#' Replace format of `rtables` cells
#'
#' A Generic function to replace format of given `rtables` cells. This is
#' convenient when, for instance, some data should rather be blanked during
#' a post-processing step.
#'
#' @param x A `rtable`.
#' @inheritParams rreplace_format.rrow
#' @inheritParams rreplace_format.rtable
#'
#' @import rtables
#'
#' @export
#' @md
#' @examples
#' library(random.cdisc.data)
#' library(tern)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(cached = TRUE)
#' saved_labels <-  var_labels(ADLB)
#'
#' # For illustration purpose, the example focuses on "Alanine Aminotransferase
#' # Measurement" starting from baseline, while excluding visit at week 1 for
#' # subjects who were prescribed a placebo.
#' ADLB <- subset(
#'   ADLB,
#'   PARAM == "Alanine Aminotransferase Measurement" &
#'     !(ARM == "B: Placebo" & AVISIT == "WEEK 1 DAY 8") & AVISIT != "SCREENING"
#' )
#'
#' # Preprocessing.
#' ADLB_l <- split(
#'   ADLB, f = ifelse(ADLB$AVISIT == "BASELINE", "Baseline", "Follow-up")
#' )
#' ADLB_l <- lapply(ADLB_l, `var_labels<-`, saved_labels)
#' tbls1 <- lapply(X = ADLB_l, function(x) {
#'
#'   tbl <- t_summary_by(
#'     x = compare_in_header(x[, c("AVAL", "CHG")]),
#'     row_by = x$AVISIT,
#'     col_by = x$ARM,
#'     col_N = table(ADSL$ARM),
#'     f_numeric = c("count_n", "mean_sd3", "median_t3", "range_t3")
#'   )
#'   return(tbl)
#'
#' })
#'
#' rreplace_format(tbls1$Baseline, col = 3)
#' rreplace_format(tbls1$Baseline, row = 2, old = NULL, new = "xx%")

rreplace_format <- function(x,
                            row = NULL,
                            col = NULL,
                            old = FALSE,
                            new = function(x, output) "") {
  UseMethod("rreplace_format", x)
}

#' @export
rreplace_format.default <- function(x,
                                    row = NULL,
                                    col = NULL,
                                    old = FALSE,
                                    new = function(x, output) "") {
  stop("No default implementation for `rreplace_format.default`.")
}


#' Formats: rtables
#'
#' Replace format of `rtables`
#'
#' @rdname rreplace_format
#'
#' @inheritParams rreplace_format
#' @inheritParams rreplace_format.rrow
#' @param row A vector of row indexes. Default `NULL` applies to all rows.
#' @export
rreplace_format.rtable <- function(x,
                                   row = NULL,
                                   col = NULL,
                                   old = FALSE,
                                   new = function(x, output) "") {

  if (is.null(row)) row <- seq_len(nrow(x))
  x[row] <- lapply(x[row], rreplace_format, col = col, old = old, new = new)
  return(x)

}

#' Formats: rtable row
#'
#' Replace format of `rtable` row
#'
#' @inheritParams rreplace_format
#' @inheritParams rreplace_format.rtable
#' @param col A vector of columns indexes.
#'   Default `NULL` applies to all columns.
#' @param old A targeted `rtable` format to be replaced. Default `FALSE`,
#'   indicates a replacement independent from current format, otherwise
#'   all `rtables` compatible format are accepted.
#' @param new The replacement format.
#'
#' @export
rreplace_format.rrow <- function(x, # nolint
                                 row = NULL,
                                 col = NULL,
                                 old = FALSE,
                                 new = function(x, output) "") {

  if (old != FALSE || is.null(old)) {
    if (!rtables::is_rcell_format(old, stop_otherwise = FALSE)) {
      stop(
        paste0(
          "The `old` format should be a a valid format string or a format ",
          "function for rcells. To get a list of all valid format strings, use ",
          "`list_rcell_format_labels`"
        )
      )
    }
  }

  if (!rtables::is_rcell_format(new, stop_otherwise = FALSE)) {
    stop(
      paste0(
        "The `new` format should be a a valid format string or a format ",
        "function for rcells. To get a list of all valid format strings, use ",
        "`list_rcell_format_labels`"
      )
    )
  }

  if (!(len_x <- length(x)) > 0)  return(x)
  if (is.null(col)) col <- 1:len_x

  lapply(
    col, function(y) {

      rcell_content <- x[[y]]

      # Current cell format is observed only when `old` specified
      # (note that NULL is a possible value of `old`).
      if (is.null(old) || old != FALSE) {

        rcell_format  <- rtables::get_format(x[[y]])
        attributes(rcell_content) <- NULL

        # It is necessary to catch specifically when the current format is
        # NULL and when demanded `old` is of format NULL.
        if (is.null(rcell_format)) {

          if (is.null(old))  x[[y]] <<- rcell(rcell_content, format = new)
          if (!is.null(old)) x[[y]] <<- x[[y]]

        } else if (!is.null(rcell_format)) {
          if (is.null(old))  x[[y]]  <<- x[[y]]
          if (!is.null(old) & rcell_format %in% old) {
            x[[y]] <<- rcell(rcell_content, format = new)
          }
        }

        # Finally, if no `old` reference is demanded then the format is
        # directly modified.
      } else if (!old) {
        x[[y]] <<- rcell(rcell_content, format = new)
      }

      invisible()
    }
  )
  return(x)

}


#' Get total count
#'
#' Normally, you don't need to use this function as it is the default
#'
#' @param col_by (factor or matrix) to get counts from
#'
#' @return counts per factor level or column of \code{col_by}
#'
#' @export
#'
#' @examples
#' get_N(data.frame(A = c(TRUE, TRUE, FALSE), B = c(FALSE, FALSE, TRUE)))
get_N <- function(col_by) { # nolint
  stopifnot(is.factor(col_by) || is.data.frame(col_by))
  colSums(col_by_to_matrix(col_by))
}


number_rows <- function(x) { # nousage # nolint
  if (is.data.frame(x)) {
    nrow(x)
  } else {
    length(x)
  }
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
