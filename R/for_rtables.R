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

#' Combines two lists of \code{rrow} to a list of \code{rrow}
#' Also keeps indent.
#'
#' @param x first list of rows, \code{row.names} are taken from this list
#' @param y second list
#'
#' @return list of combined rows
combine_rrows_with_indent <- function(x, y) { # nousage # nolint
  Map(function(xi, yi) {
    rrowl(attr(xi, "row.name"), c(xi, yi), indent = attr(xi, "indent"))
  }, x, y)
}
