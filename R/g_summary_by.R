

#' Chanage From Baseline Plot
#' 
#' @param x x coordinates
#' @param y y coordinates
#' @param group factor with group information
#' @param y_lower lower bound of error bar, if \code{NULL} no error bar is displayed
#' @param y_upper upper bound of error bar, if \code{NULL} no error bar is displayed
#' @param n_visit for each element of \code{x}
#' @param ylim y limits
#' @param y_line_at draw horizontal lines at
#' @param xlab xlabel
#' @param ylab ylabel
#' @param nlab n_visit label
#' @param title
#' @param draw boolean should plot be drawn
#' @param newpage boolean should plot be drawn on newpage
#' 
#' @retrun grid grob
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' g_summary_by(
#'  x = ANL$VITIT, y = ANL$CHG, group = ANL$ARM,
#'  y_lower , y_upper , 
#'  n_visit = ANL$nvisit,
#'  ylim, yline_at, title 
#' )
#'  
#' }

g_summary_by <- function(x) {
  
  
}