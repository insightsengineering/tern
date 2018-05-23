#' Horizontal Waterfall Plot
#' 
#' The waterfall plot visualizes a quatity \code{y} ordered by value with some
#' markup
#' 
#'
#' @param data data frame 
#' @param id_var ID variable name used as the x-axis label for each bar from \code{data}
#' @param y  variable name of the numeric vector to be plotted from \code{data}
#' @param  col_var name of the categorical variable for bar coloring and legend from \code{data}
#' @param xlab x label. Default is \code{ID}.
#' @param ylab y label. Default is \code{Value}.
#' @param title A string to be displayed as plot title.
#' @param legend.title A string to be displayed as legend title.
#' @param add_id variables to be added to the summary table.
#' 
#' @template author_song24
#' 
#' @import ggplot2
#' 
#' @export
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' library(dplyr)
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
#' asld <- right_join(apchg, ASL_f %>% select(STUDYID, USUBJID, SEX, ARMCD))
#' 
#' 
#' asld <- head(asld, 30)
#' 
#' head(asld)
#' 
#' g_wf(
#'   height = asld$pchg,
#'   id = asld$USUBJID,
#'   col = asld$AVALC
#' )
#' 
#' g_wf(
#'   height = asld$pchg,
#'   id = paste("asdfdsfdsfsd",asld$USUBJID),
#'   col = asld$SEX
#' )
#' g_wf(
#'   height = asld$pchg,
#'   id = paste("asdfdsfdsfsd",asld$USUBJID),
#'   xlab = "ID",
#'   ylab = "Percentage Change",
#'   plot.title = "Lahahah"
#' )
#' 
g_wf <- function(height, id, col=NULL, xlab=NULL, ylab=NULL, col.legend.title=NULL, plot.title = NULL){
  
  check_same_N(y=height, id=id)
  
  xlabel <- deparse(substitute(id))
  ylabel <- deparse(substitute(height))
  
  col.label <- if (!missing(col)) deparse(substitute(col))
  
  xlab <- if (is.null(xlab)) xlabel else xlab
  ylab <- if (is.null(ylab)) ylabel else ylab
  col.legend.title <- if (is.null(col.legend.title)) col.label else col.legend.title
  
  
  plot_data <- data.frame(
    height = height,
    id = as.character(id),
    col = if (is.null(col)) "gray90" else to_n(col, length(height)),
    stringsAsFactors = FALSE
  )
  
  plot_data_ord <- plot_data[order(plot_data$height, decreasing = TRUE), ]
  
  p <- ggplot(plot_data_ord, aes(x = factor(id, levels=id), y = height, fill = col)) +
    geom_col() + 
    geom_text(label = format(plot_data_ord$height, digits=2),
              vjust = ifelse(plot_data_ord$height >= 0, -0.5, 1.5)) +
    xlab(xlab) +
    ylab(ylab) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) 

    if (!is.null(col)) {
      p <- p +
        labs(fill = col.legend.title) +
        theme(
          legend.position="bottom",
          legend.background = element_blank(),
          legend.title = element_text(face="bold"),
          legend.box.background = element_rect(colour = "black")
        )
    }
    if (!is.null(plot.title)) {
      p <- p +
        labs( title = plot.title) +
        theme(plot.title = element_text(face = "bold"))
    }
  p


}