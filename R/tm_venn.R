#' Create a Venn Diagram Plot with 2 groups
#' 
#' @param x boolean has biomarker or not
#' @param y boolean has biomarker of not
#' 
#' 
#'# @importFrom grid 
#' 
#' @export
#' 
#' @return  plot
#' 
#' 
#' @examples 
#' 
#' n <- 100
#' tGE <- sample(c(TRUE, FALSE), n, replace=TRUE, prob = c(.2, .8))
#' IHC <- sample(c(TRUE, FALSE), n, replace=TRUE, prob = c(.6, .4))
#' 
#' \dontrun{
#' library(atezo.data)
#' ASL <- asl(com.roche.cdpt7722.wo29637.rl)
#' 
#' # 
#' names(ASL)
#' 
#' IC <- ASL$IC %in% c("2", "3")
#' TC <- ASL$TC %in% c(2, 3)
#' 
#' venn2(x = IC, y = TC)
#' venn2(x = IC, y = TC, "biomarker IT", "biamrker TC")
#' 
#' }
#' 
venn2 <- function(x, y, xlabel, ylabel) {
  
  if (missing(xlabel)) xlabel <- deparse(substitute(x))
  if (missing(ylabel)) ylabel <- deparse(substitute(y))
  
  if (length(x) != length(y)) stop("x and y need to be of the same length")
  if (!is.logical(x) || !is.logical(y)) stop("x and y need to be boolean")
  
  # what to do with NA?
  
  abs <- table(x, y)
  per <- abs/length(x)
  
  list(abs, per, xlabel = xlabel, ylabel = ylabel)
}

if (FALSE) {

  
}
  