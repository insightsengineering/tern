# the functions in this document should eventually be moved to the rtables project

indent_table <- function(x, n) {
  for (i in 1:nrow(x)) {
    attr(x[[i]], "indent") <- attr(x[[i]], "indent") + n
  }
  x
}

shift_label_table <- function(tbl, term) {
  t_grade <- rtablel(rheader(rrow("", "."), rrow("", "Grade")), c(lapply(row.names(tbl), function(xi) rrow("", xi))))
  attr(t_grade[[1]], "row.name") <- term
  cbind_rtables(t_grade, tbl)
}



#' insert rrows at a specific location
#' 
#' @noRd
#' 
#' @examples 
#' tbl <- rtabulate(iris$Sepal.Length, iris$Species)
#' 
#' insert_rrow(tbl, rrow("Hello World"))
insert_rrow <- function(tbl, rrow, at=1) {
  
  if (!is(tbl, "rtable")) stop("tbl is expected to be an rtable")
  if (!is(rrow, "rrow")) stop("rrow is expected to be an rrow")
  
  nr <- nrow(tbl)
  if (at <= 0 || at > nr+1) stop("at is not in [1, nrow(tbl)+1]")
  
  if (length(rrow) != 0 && length(rrow) != ncol(tbl)) stop("rrow has wrong number of colums")
  
  header <- header(tbl)
  
  attributes(tbl) <- NULL
  body <- if (at == 1) {
    c(list(rrow), tbl)
  } else if (at == (nr+1)) {
    c(tbl, list(rrow))
  } else {
    c(tbl[1:(at-1)], rrow, tbl[(at+1):nr])
  }
  
  rtablel(header, body)
  
}

#' cbind two rtables
#' @noRd
#' 
#' @examples 
#' x <- rtable(c("A", "B"), rrow("x row 1", 1,2), rrow("x row 2", 3, 4))
#' 
#' y <- rtable("C", rrow("y row 1", 5), rrow("y row 2", 6))
#' 
#' 
#' cbind_rtables(x, y)
#' 
cbind_rtables <- function(x, y) {
  if (!is(x, "rtable") || !is(y, "rtable")) stop("x and y are not both rtables")
  
  if(nrow(x) != nrow(y)) stop("number of rows missmatch")
  
  header_x <- header(x)
  header_y <- header(y)
  
  if(nrow(header_x) != nrow(header_y)) stop("number of rows missmatch in header")
  
  header <- do.call(rheader, combine_rrows(header_x, header_y))
  
  body <- combine_rrows(unclass(x), unclass(y))
  
  rtablel(header, body)

}

combine_rrows <- function(x,y) {
  
  Map(function(xi, yi) {
    rrowl(attr(xi, "row.name"), c(xi, yi))
  }, x, y)
  
}