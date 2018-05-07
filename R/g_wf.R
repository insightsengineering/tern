#' Forest plot 
#' 
#' Create a forest plot from any \code{\link[rtables]{rtable}} object that has a
#' column with a single value and a column with 2 values
#' 
#' id, y, col_var, xlab = "ID", ylab = "Value", title, legend.title, add_id = NULL
#'
#' @param id ID variable used as the x-axis label for each bar
#' @param y numeric vector to be plotted
#' @param  col_var categorical vector for bar coloring and legend
#' @param xlab x label. Default is \code{ID}.
#' @param ylab y label. Default is \code{Value}.
#' @param title A string to be displayed as plot title.
#' @param legend.title A string to be displayed as legend title.
#' @param add_id vector of the same length as \code{id} to be added to bar label.
#' 
#' @template author_songy24
#' 
#' @import ggplot2
#' 
#' @export
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL")
#' ARS <- radam("ARS", ADSL = ASL)
#' 
#' ARS_f <- subset(ARS, PARAMCD == "OVRINV")
#' 
#' ASL_f <- ASL %>%
#'   select(USUBJID, STUDYID, ARM, ARMCD, SEX)
#'
#' apchg <- ARS_f %>%
#'   mutate(pchg = rnorm(100, 10, 50))
#'   # Merge pchange and besr response
#' asld <- right_join(apchg, ASL_f %>% select(STUDYID, USUBJID, SEX))
#' head(asld)

ylab = "Maximum SLD % Change"
xlab = "ID"
main = "Swimlane Plot"
id <- asld$USUBJID
y <- asld$pchg
col_var <- asld$AVALC
add_id <- asld$SEX

g_wf <- function(id, y, col_var, xlab = "ID", ylab = "Value", title, legend.title, add_id = NULL){
  
  check_same_N(id = id, col_var = col_var, y = y)
  if(!is.null(add_id)) check_same_N(id = id, add_id)
  data <- data.frame(id, y, col_var)
  data$id <- paste(data$id, paste(add_id, sep = ", "), sep = ",")
  wf <- ggplot(data, aes(x = reorder(id, -y), y = y, fill = col_var)) + 
    xlab(xlab) +
    ylab(ylab) + 
    labs(fill = "Response") +
    ggtitle(main) + 
    geom_col() + 
    geom_text(aes(label = round(y,1)), vjust = -0.2) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  wf
}