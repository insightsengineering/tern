
#' Calculate statistics Pairwise with reference level
#' 
#' 
#' @inheritParams rtables::rrow
#' @param x a vector
#' @param col_by a factor first level is taken as reference level
#' @param FUN a function with two arguments, first argument is the subset of x
#'   and the second argument is a factor with two levels from col_by
#' 
#' 
#' @importFrom stats relevel
#'    
#' @template author_waddella    
#'
#' @return an rtable
#' 
#' @examples 
#' \dontrun{
#' tabulate_pairwise(
#'   x = iris$Sepal.Length,
#'   col_by = iris$Species,
#'   FUN = function(xi, col_by_i) diff(tapply(xi, col_by_i, mean)),
#'   row.name = "diff mean"
#' )
#' }
tabulate_pairwise <- function(x, col_by, FUN, row.name, format=NULL, indent = 0) {

  if (!is.atomic(x) && !is.data.frame(x)) stop("currently x is required to be a vector or data.frame")
  if (!is.factor(col_by)) stop("col_by is expected to be a factor")
  if (length(levels(col_by)) < 2) stop("col_by requires least two levels") 
  if (any(is.na(col_by))) stop("currently no NAs allowed in col_by")
  
  ref_level <- levels(col_by)[1]
  
  row_data <- lapply(levels(col_by)[-1], function(comp_level) {
    
    sel <- col_by %in% c(ref_level, comp_level)
    
    x_sel <- subset(x, sel)
    tmp.col_by_sel <- subset(col_by, sel)
    col_by_sel <- relevel(droplevels(tmp.col_by_sel), ref_level, comp_level)
    
    FUN(x_sel, col_by_sel)
  })
  
  rtable(
    header = rtables:::rtabulate_header(col_by, length(x)),
    rrowl(row.name = row.name, c(list(NULL), row_data), format = format, indent = indent )    
  )
}

