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
#' OS <- data.frame(AVAL = abs(rnorm(200)), 
#'                  CNSR = sample(c(0, 1), 200, TRUE), 
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = OS, conf.type = "plain")
#' kmGrob(fit_km = fit_km)
#' 
kmGrob <- function(title = "Kaplan - Meier Plot", fit_km, xaxis_by = NULL ) {
  
  cd <- kmCurveData(fit_km = fit_km, xaxis_by = xaxis_by)
 
  
  vpplot <- plotViewport(margins = c(3, max(cd$nlines_labels, 4), 3, 2),
                         layout = grid.layout(
                           nrow = 3, ncol = 1, widths = unit(1, "npc"),
                           heights = unit(c(5, 5, length(cd$group)*1.1+4), c("null", "lines", "lines"))),
                         name = "plotArea")
  vpcurve <- dataViewport(xData = cd$xData, yData = c(0,1),
                          layout.pos.col = 1, layout.pos.row = 1, name = "topCurve")
  vptable <- viewport(layout.pos.col = 1, layout.pos.row = 3, name = "bottomTable")
  
  vprisk <- dataViewport(xData = cd$xData, yData = c(0,1), name = "riskTable")
  
  vptree <- vpTree(vpplot, vpList(vpcurve, vpStack(vptable,  vprisk)))
  
  lines <- Map(function(x, y, col){
    linesGrob(x = x, y = y, default.units = "native", gp = gpar(col = col, lwd = 3), vp = vpPath("plotArea", "topCurve"))
  }, cd$lines_x, cd$lines_y, cd$colpal)
  
  points <- Map(function(x, y, col){
    pointsGrob(x = x, y = y, pch = 3, size = unit(0.5, "char"), gp = gpar(col = col), vp = vpPath("plotArea", "topCurve"))
  }, cd$points_x, cd$points_y, cd$colpal )
  
  ptnumber <- Map(function(y, col, risk){
    textGrob( label = ifelse(!is.na(risk), as.character(risk), " "),
              x = unit(cd$xpos, "native"),
              y = unit(y, "npc"),
              gp = gpar(col = col),
              vp = vpPath("plotArea", "bottomTable",  "riskTable"))
  }, cd$ypos, cd$colpal, cd$pt_risk )
  
  grplabel <-  Map(function(y, col, grp){
    textGrob( label = grp,
              x = unit(-cd$nlines_labels + 1, "lines"),
              y = unit(y, "npc"),
              just = c("left", "center"),
              gp = gpar(col = col),
              vp =  vpPath("plotArea", "bottomTable",  "riskTable"))
  }, cd$ypos, cd$colpal, names(cd$group) )
  
  gTree(childrenvp = vptree,
                     children =   do.call("gList", 
                                          c(list( xaxisGrob(at = cd$xpos, vp = vpPath("plotArea", "topCurve")),
                                                  yaxisGrob(vp = vpPath("plotArea", "topCurve")),
                                                  rectGrob(vp = vpPath("plotArea", "topCurve")),
                                                  textGrob(title, y = unit(1, "npc") + unit(1, "lines"), 
                                                           gp = gpar(fontface = "bold", fontsize = 16), vp = vpPath("plotArea", "topCurve")),
                                                  textGrob("Survival Probability", x = unit(-3.5, "lines"), rot = 90, vp = vpPath("plotArea", "topCurve")), 
                                                  textGrob(label = "Number of Patients at Risk",
                                                           x = unit(0, "npc"),
                                                           y = unit(1, "npc") + unit(1, "lines"),
                                                           just = "left", vp = vpPath("plotArea", "bottomTable")),
                                                  xaxisGrob(at = cd$xpos, vp = vpPath("plotArea", "bottomTable",  "riskTable")), 
                                                  rectGrob(vp = vpPath("plotArea", "bottomTable",  "riskTable"))),
                                            lines,
                                            points,
                                            ptnumber,
                                            grplabel)),
                  #   gp = gp, vp = vp, name = name,
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
#' library(dplyr)
#' OS <- data.frame(AVAL = abs(rnorm(200)), 
#'                  CNSR = sample(c(0, 1), 200, TRUE), 
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = OS, conf.type = "plain")
#' a_kmgrob <- kmGrob(fit_km, xaxis_by = 0.5)
#' fit_coxph <- coxph(Surv(AVAL, 1-CNSR) ~ ARM + strata(RACE), data = OS, ties = "exact")
#' cox_tbl <- coxphAnnoData(fit_coxph)
#' addTable(a_kmgrob, 
#'          vp = vpPath("plotArea", "topCurve"), 
#'          x= unit(1, "lines"), y = unit(1, "lines"),
#'          just = c("left", "bottom"),
#'          tbl = cox_tbl ) %>% 
#'          grid.draw()
#'          
#'          
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

