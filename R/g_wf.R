#' Horizontal Waterfall Plot
#' 
#' This basic waterfall plot visualizes a quatity \code{height} ordered by value with some
#' markup
#'
#' @param height  numeric vector to be plotted as the waterfall bars
#' @param id vector of IDs used as the x-axis label for the waterall bars 
#' @param col vector of a categorical variable for bar coloring
#' @param xlab x label. Default is \code{ID}.
#' @param ylab y label. Default is \code{Value}.
#' @param title A string to be displayed as plot title.
#' @param col.legend.title A string to be displayed as legend title.
#' 
#' @template author_song24
#' 
#' @importFrom ggplot2 ggplot aes theme geom_col geom_text labs xlab ylab
#'   element_text element_blank element_rect scale_fill_manual
#' 
#' @export
#' 
#' @examples 
#' 
#' 
#' g_wf(height = c(3,5,-1), id = letters[1:3])
#' 
#' g_wf(height = c(3,5,-1), id = letters[1:3], col = c("red", "green", "red"))
#'
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
#' 
#' g_wf(
#'   height = asld$pchg,
#'   id = paste("asdfdsfdsfsd",asld$USUBJID),
#'   xlab = "ID",
#'   ylab = "Percentage Change",
#'   title = "Waterfall plot"
#' )
#' 
g_waterfall <- function(height, id, col=NULL, xlab=NULL, ylab=NULL, col.legend.title=NULL, title = NULL){
  
  if (!is.null(col)) check_same_N(height = height, id=id, col = col) else check_same_N(height = height, id=id)
  
  xlabel <- deparse(substitute(id))
  ylabel <- deparse(substitute(height))
  
  col.label <- if (!missing(col)) deparse(substitute(col))
  
  xlab <- if (is.null(xlab)) xlabel else xlab
  ylab <- if (is.null(ylab)) ylabel else ylab
  col.legend.title <- if (is.null(col.legend.title)) col.label else col.legend.title
  
  
  plot_data <- data.frame(
    height = height,
    id = as.character(id),
    col = if (is.null(col)) "x" else to_n(col, length(height)),
    stringsAsFactors = FALSE
  )
  
  plot_data_ord <- plot_data[order(plot_data$height, decreasing = TRUE), ]
  
  p <- ggplot(plot_data_ord, aes(x = factor(id, levels=id), y = height)) +
    geom_col() + 
    # geom_hline(yintercept = 0) +
    geom_text(label = format(plot_data_ord$height, digits = 2),
              vjust = ifelse(plot_data_ord$height >= 0, -0.5, 1.5)) +
    xlab(xlab) +
    ylab(ylab) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) 

  if (!is.null(col)) {
    p <- p +
      aes(fill = col) +
      # scale_fill_manual(values = col) +
      labs(fill = col.legend.title) +
      theme(
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_text(face="bold"),
        legend.box.background = element_rect(colour = "black")
      )
  }
  
  if (!is.null(title)) {
    p <- p +
      labs( title = title) +
      theme(plot.title = element_text(face = "bold"))
  }
  
  p

}