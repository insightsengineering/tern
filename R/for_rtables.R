# todo: move these functions

# todo: document this function
shift_label_table <- function(tbl, term) {
  t_grade <- rtablel(rheader(rrow("", "."), rrow("", "Grade")), c(lapply(row.names(tbl), function(xi) rrow("", xi))))
  attr(t_grade[[1]], "row.name") <- term
  cbind_rtables(t_grade, tbl)
}

# adds row names as new column: this allows to add a header label to the rows
row_names_as_col <- function(tbl, header_label) {
  stopifnot(is.list(header_label) || is_character_vector(header_label))

  nrows_header <- nrow(header(tbl))
  if (missing(header_label)) {
    header_label <- rep("", nrows_header)
  } else if (length(header_label) != nrows_header) {
    stop("dimension mismatch")
  }

  new_header <- do.call(rheader, lapply(header_label, function(x) {
    if (is(x, "rrow")) {
      stopifnot(ncol(x) == 1)
      x
    } else {
      rrow("", x)
    }
  }))

  tbl_rn <- rtablel(header = new_header, c(lapply(row.names(tbl), function(xi) rrow("", xi))))
  cbind_rtables(tbl_rn, tbl)
}

#' Combines two lists of rrows to a list of rrows
#' Also keeps indent.
#'
#' todo: this should eventually replace rtables:::combine_rrows
#'
#' @param x first list of rows, row.names are taken from this list
#' @param y second list
#'
#' @return list of combined rows
#'
combine_rrows_with_indent <- function(x, y) {
  #todo: check for empty rrows
  Map(function(xi, yi) {
    #todo: optionally check row names are the same
    rrowl(attr(xi, "row.name"), c(xi, yi), indent = attr(xi, "indent"))
  }, x, y)
}


# todo: add check !is_r_by to col_by_to_matrix

#' Recursive by
#'
#' When a by of this type is encountered in tern functions, it recursively splits by it
#' \code{is.list} still returns TRUE on the returned object.
#' It is typically used to construct a nested tree from it.
#'
#' The r_by class is used to distinguish the simple_by (which can be a matrix
#' which is a list as well, but in this case a non-recursive interpretation as in col_by)
#' from the recursive
#'
#' @param x a list that specifies recursive split,
#'   e.g. a list of col_bys (which can be matrices or factors)
#'
#' @export
r_by <- function(x) {
  stopifnot(is.list(x))
  structure(x, class = "r_by")
}

#' Check whether is of class r_by
#'
#' @inheritParams r_by
#' @return boolean whether it is an r_by object
is_r_by <- function(x) {
  is(x, "r_by")
}

#' Non-recursive by object
#'
#' This can be used both for row and column grouping.
#'
#' todo: adapt this in rtables/col_by.R to return this class
#'
#' Not called "by" because this function already exists in baseR
#'
#' @param x object to wrap it around
simple_by <- function(x) {
  stopifnot(is.data.frame(x) || is.factor(x) || is(x, "by_all"))
  #todo: maybe have by_all inherit from "simple_by"
  #structure(x, class = "simple_by")
  # todo: need to adapt col_by_to_matrix first as it will not recognize this class, also change is_simple_by
  x
}

#' Check whether is of class simple_by
#'
#' @inheritParams simple_by
#' @return boolean whether it is an simple_by object
is_simple_by <- function(x) {
  #is(x, "simple_by")
  TRUE
}
