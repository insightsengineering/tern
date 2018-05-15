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
#' asld <- asld[c(1:30),]
#' head(asld)
#' g_wf(data = asld, id_var = "USUBJID", y = "pchg", col_var = "AVALC", add_var = c("SEX"))

g_wf <- function(data, id_var, y, col_var, xlab = "ID", ylab = "Value", title = "Waterfall Plot", legend.title = "", add_var = NULL){
  
  #check_same_N(id_var = id_var, y = y, col_var = col_var, add_var = add_var)
  plot_data <- data.frame(id = data[[id_var]], y = data[[y]], col_var = data[[col_var]])
  y <- plot_data$y
  id <- plot_data$id
  
  wf <- ggplot(plot_data, aes(x = reorder(id, -y), y = y, fill = col_var)) +
    xlab(xlab) +
    ylab(ylab) + 
    ylim(min(y) + min(y)/10, max(y) + max(y)/10) +
    labs(fill = paste0("Response")) +
    ggtitle(main) + 
    geom_col() + 
    geom_text(y = y + (1/30 * (max(y) - min(y) + (max(y) - min(y))/10)) * sign(y),
              label = format(y, digits=2),
              size = 2.5) +
    theme(axis.text.x = element_text(angle = 0, size = 8),
          axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          legend.title = element_text(size = 8, face = "bold"),
          legend.text = element_text(size = 8),
          plot.margin = unit(c(1,2,1,1), "lines"))

  if(!is.null(add_var)){
    table_data <- data.frame(id = id, value = y, data[add_var])
    table_data$v.name <- table_data$id
    
    table_data_t <- reshape(table_data, 
                            varying = list(c("id", add_var)), 
                            idvar = "v.name", 
                            times = c("id", add_var), timevar = "var", direction = "long")
    
    data_table <- ggplot(table_data_t, aes(x = reorder(v.name, -value), y = factor(var, levels = c(add_var, "id")), label = format(table_data_t[,4]))) +
      geom_text(size = 2.5) + theme_bw() +
      xlab(NULL) + 
      theme(panel.grid.major = element_blank(), legend.position = "none",panel.border = element_blank(), 
            axis.title = element_blank(), 
            axis.text.x = element_blank(), 
            axis.text.y = element_text(face = "bold", size = 8),
            axis.ticks = element_blank()) +
      theme(plot.margin = unit(c(0, 5,0,1), "lines"))
    data_table
    Layout <- grid.layout(nrow = 2, ncol = 1, heights = unit(c(2,
                                                               0.5), c("null", "null")))
    grid.show.layout(Layout)
    vplayout <- function(...) {
      grid.newpage()
      pushViewport(viewport(layout = Layout))
    }
    
    
    subplot <- function(x, y) viewport(layout.pos.row = x,
                                       layout.pos.col = y)
    mmplot <- function(a, b) {
      vplayout()
      print(a, vp = subplot(1, 1))
      print(b, vp = subplot(2, 1))
    }
    mmplot(wf, data_table)
  } else wf
}