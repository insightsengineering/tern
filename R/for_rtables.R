# todo: document this function
shift_label_table <- function(tbl, term) {
  t_grade <- rtablel(rheader(rrow("", "."), rrow("", "Grade")), c(lapply(row.names(tbl), function(xi) rrow("", xi))))
  attr(t_grade[[1]], "row.name") <- term
  cbind_rtables(t_grade, tbl)
}

row_names_as_col <- function(tbl, header_label) {

  nr_h <- nrow(header(tbl))

  if (missing(header_label)) {
    header_label <- rep("", nr_h)
  } else if (length(header_label) != nr_h) {
    stop("dimension mismatch")
  }

  h <- do.call(rheader, lapply(header_label, function(x) rrow("", x)))

  tbl_rn <- rtablel(header = h, c(lapply(row.names(tbl), function(xi) rrow("", xi))))
  cbind_rtables(tbl_rn, tbl)
}
