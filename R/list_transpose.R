

#' Transpose a list by changing depht 2 elements with depth 1 elements
#' 
#' @export
#' 
#' @param x list of lists or list of vectors
#' 
#' @return list of lists
#' 
#' @examples 
#' 
#' x <- list(c1 = 1:3, c2 = 11:13, c3 = 111:113)
#' list_transpose(x)
#' 
#' x <- list(
#'   c1 = list(row1 = c(1,4), row2 = c("A"), row3 = "U", row4 = 1),
#'   c2 = list(row1 = c(9,23), row2 = c("B"), row3 = "V", row4 = 12),
#'   c3 = list(row1 = c(2,2), row2 = c("C"), row3 = "S", row4 = 99)
#' )
#' list_transpose(x)
#' 
#' x <- list(c1 = list(row1 = c(1,4)))
#' list_transpose(x)
#' 
#' x <- list(c1 = 1:3) 
#' list_transpose(x)
list_transpose <- function(x) {
  
  # length at depth 2
  n2 <- unique(unlist(lapply(x, length)))
  if (length(n2) != 1) stop("the nested lists are not of consistent length")
  
  el_names <- names(x[[1]])
  same_el_names <- vapply(x[-1], function(xi) identical(el_names, names(xi)), logical(1))
  if (!all(same_el_names)) stop("element names do not match in level 2")
  
  y <- lapply(1:n2, function(i) lapply(x, function(xi) xi[[i]]))
  names(y) <- el_names
  
  y
}