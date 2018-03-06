#' Define a kmplot grob
#' 
#' create a grid graphical object for basic KM plot using data from a call of function \code{kmCurveData}
#' 
#' @param title title for plot.
#' @param ... All other arguments will be passed to a call to the \code{kmCurveData()} function.
#' 
#' @import survival
#' @importFrom scales col_factor
#' @import grid
#' 
#' @export
#' 
#' @examples 
#' library(survival)
#' library(scales)
#' library(grid)
#' OS <- data.frame(AVAL = abs(rnorm(200)), 
#'                  CNSR = sample(c(0, 1), 200, TRUE), 
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#' kmGrob(formula_km = Surv(AVAL, 1-CNSR) ~ ARM, data = OS)

kmGrob <- function(title = "Kaplan - Meier Plot", ...){
  
  curve_data <- kmCurveData(...)
  nlines_labels <- curve_data$nlines_labels
  xpos <- curve_data$xpos
  group <- curve_data$group
  xData <- curve_data$xData
  lines_x <- curve_data$lines_x
  lines_y <- curve_data$lines_y
  points_x <- curve_data$points_x
  points_y <- curve_data$points_y
  col_pal <- curve_data$col
  ypos <- curve_data$ypos
  pt_risk <- curve_data$pt_risk
  
  vpplot <- plotViewport(margins = c(3, max(nlines_labels, 4), 3, 2),
                         layout = grid.layout(
                           nrow = 3, ncol = 1, widths = unit(1, "npc"),
                           heights = unit(c(5, 5, length(group)*1.1+4), c("null", "lines", "lines"))),
                         name = "plotarea")
  vpcurve <- dataViewport(xData = xData, yData = c(0,1),
                          layout.pos.col = 1, layout.pos.row = 1, name = "topcurve")
  vptable <- viewport(layout.pos.col = 1, layout.pos.row = 3, name = "bottomtable")
  
  vprisk <- dataViewport(xData = xData, yData = c(0,1), name = "risktable")
  
  vptree <- vpTree(vpplot, vpList(vpcurve, vpStack(vptable,  vprisk)))
  
  lines <- mapply(function(x, y, col){
    linesGrob(x = x, y = y, default.units = "native", gp = gpar(col = col, lwd = 3), vp = vpPath("plotarea", "topcurve"))
  }, lines_x, lines_y, col_pal, SIMPLIFY = FALSE)
  
  points <- mapply(function(x, y, col){
    pointsGrob(x = x, y = y, pch = 3, size = unit(0.5, "char"), gp = gpar(col = col), vp = vpPath("plotarea", "topcurve"))
  }, points_x, points_y, col_pal, SIMPLIFY = FALSE)
  
  ptnumber <- mapply(function(y, col, risk){
    textGrob( label = ifelse(!is.na(risk), as.character(risk), " "),
              x = unit(xpos, "native"),
              y = unit(y, "npc"),
              gp = gpar(col = col),
              vp = vpPath("plotarea", "bottomtable",  "risktable"))
  }, ypos, col_pal, pt_risk, SIMPLIFY = FALSE)
  
  grplabel <-  mapply(function(y, col, grp){
    textGrob( label = grp,
              x = unit(-nlines_labels + 1, "lines"),
              y = unit(y, "npc"),
              just = c("left", "center"),
              gp = gpar(col = col),
              vp =  vpPath("plotarea", "bottomtable",  "risktable"))
  }, ypos, col_pal, names(group), SIMPLIFY = FALSE)
  
  gTree(childrenvp = vptree,
        children =   do.call("gList", 
                             c(list( xaxisGrob(at = xpos, vp = vpPath("plotarea", "topcurve")),
                                     yaxisGrob(vp = vpPath("plotarea", "topcurve")),
                                     rectGrob(vp = vpPath("plotarea", "topcurve")),
                                     textGrob(title, y = unit(1, "npc") + unit(1, "lines"), 
                                              gp = gpar(fontface = "bold", fontsize = 16), vp = vpPath("plotarea", "topcurve")),
                                     textGrob("Survival Probability", x = unit(-3.5, "lines"), rot = 90, vp = vpPath("plotarea", "topcurve")), 
                                     textGrob(label = "Number of Patients at Risk",
                                              x = unit(0, "npc"),
                                              y = unit(1, "npc") + unit(1, "lines"),
                                              just = "left", vp = vpPath("plotarea", "bottomtable")),
                                     xaxisGrob(at = xpos, vp = vpPath("plotarea", "bottomtable",  "risktable")), 
                                     rectGrob(vp = vpPath("plotarea", "bottomtable",  "risktable"))),
                               lines,
                               points,
                               ptnumber,
                               grplabel)),
        cl = "kmGrob")
}


#' Add text annotation on top of a kmgrob
#' 
#' utility function to edit a grid grob by adding a text grob 
#' 
#' @param kmgrob a kmGrob for further editing.
#' @param vp a Grid veiwport object for annotating a text grob.
#' @param x A numeric vector or unit object specifying x-values.
#' @param y A numeric vector or unit object specifying y-values.
#' @param tbl An rtable.
#' @param just The justification of the text relative to its (x, y) location.
#' 
#' @import grid
#' 
#' @export
#' 
#' @examples 
#' library(grid)
#' library(survival)
#' library(rtables)
#' library(scales)
#' library(dplyr)
#' OS <- data.frame(AVAL = abs(rnorm(200)), 
#'                  CNSR = sample(c(0, 1), 200, TRUE), 
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#' a_kmgrob <- kmGrob(formula_km = Surv(AVAL, 1-CNSR) ~ ARM, data = OS, xaxis_by = 0.5)
#' cox_tbl <- coxphAnnoData(Surv(AVAL, 1-CNSR) ~ ARM + strata(RACE), data = OS)
#' addTable(a_kmgrob, 
#'          vp = vpPath("plotarea", "topcurve"), 
#'          x= unit(1, "lines"), y = unit(1, "lines"),
#'          just = c("left", "bottom"),
#'          tbl = cox_tbl ) %>% 
#'          grid.draw()

addTable <- function(kmgrob, vp, x = unit(0.5, "npc") , y = unit(0.5, "npc"), 
                     tbl, just = c("left", "top")){
   
  addGrob(kmgrob,
          textGrob(label = tbl, 
                   x = x,
                   y = y,
                   just = just,
                   gp = gpar(fontfamily = 'mono', fontsize = 8, fontface = "bold"),
                   vp = vp))
}

