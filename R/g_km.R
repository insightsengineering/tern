#' Kaplan-Meier Plot
#' 
#' Create a KM plot for any \code{survfit} object.
#' 
#' @param fit_km a class "survfit" object.
#' @param xaxis_break break interval of x-axis. It takes a numeric vector or \code{NULL}.
#' @param title title for plot.
#' @param draw boolean, should plot be drawn.
#' @param newpage boolean if \code{draw=TRUE} should plot be drawn on a new page.
#' @param anno_km_show show annotation from a survfit object.
#' @param anno_coxph_show show annotation from a coxph object.
#' @param anno_coxph_fit a class "coxph" object.
#' @param anno_coxph_info label information for Cox PH model.
#' 
#' @import dplyr
#' @import survival
#' @import rtables
#' @import grid
#' 
#' @export
#' 
#' @examples 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL")
#' ASL$RACE <- factor(sapply(as.character(ASL$RACE), function(x) if (nchar(x)>9) paste0(substr(x, 1,9), "...") else x))
#' ATE <- radam("ATE", ADSL = ASL)
#' 
#' ATE_f <- subset(ATE, PARAMCD == "OS") 
#' 
#' ANL <- merge(ASL, ATE_f, by = c("USUBJID", "STUDYID"))
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = ANL, conf.type = "plain")
#' ### basic KM plot
#' g_km(fit_km = fit_km)
#' 
#' ### KM plot with coxph annotation only
#' fit_coxph <- coxph(Surv(AVAL, 1-CNSR) ~ ARM + strata(RACE), data = ANL, ties = "exact")
#' g_km(fit_km = fit_km, 
#'      anno_coxph_show = TRUE,
#'      anno_coxph_fit = fit_coxph) 
#'      
#' ### KM plot with both KM and coxph annotations   
#' g_km(fit_km = fit_km, 
#'      anno_km_show = TRUE,
#'      anno_coxph_show = TRUE,
#'      anno_coxph_fit = fit_coxph )     

g_km <- function(fit_km, xaxis_break = NULL, title = "Kaplan - Meier Plot",
                 draw = TRUE, newpage = TRUE,
                 anno_km_show = FALSE, 
                 anno_coxph_show = FALSE,
                 anno_coxph_fit = NULL,
                 anno_coxph_info = "Cox Porportional Model"){
  
  if (!is(fit_km, "survfit")) stop("fit_km needs to be of class survfit")
  grobKm <- kmGrob(fit_km = fit_km, xaxis_break = xaxis_break, title = title)
  
  if (anno_km_show){
    annokm <- kmAnnoData(fit_km)
    grobKm <- grobKm %>% addTable(., 
                                  tbl = annokm,
                                  x = unit(1, "npc") - stringWidth(annokm) - unit(1, "lines"),
                                  y = unit(1, "npc") -  unit(1, "lines"),
                                  just = c("left", "top"))
  }
  
  if (anno_coxph_show){
    if (!is.null(anno_coxph_fit)){
      annocoxph <- coxphAnnoData(anno_coxph_fit, info_coxph = anno_coxph_info )
      grobKm <- grobKm %>% addTable(.,
                                    tbl = annocoxph,
                                    x= unit(1, "lines"), y = unit(1, "lines"),
                                    just = c("left", "bottom"))
    }
  }
  
  if (draw) {
    if (newpage) grid.newpage()
    grid.draw(grobKm)
  }
  invisible(grobKm)
}

 

#' Define a kmplot grob
#' 
#' create a grid graphical object for basic KM plot from a \code{survfit} object.
#' 
#' @param fit_km a class "survfit" object.
#' @param xaxis_break break interval of x-axis. It takes a numeric vector or \code{NULL}.
#' @param title title for plot.
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
#' kmGrob(fit_km = fit_km, xaxis_break = c(0.5, 0.8, 1.5))
#' 
kmGrob <- function(fit_km, xaxis_break = NULL, title = "Kaplan - Meier Plot", 
                   gp = NULL, vp = NULL, name = NULL) {
  
  if (!is(fit_km, "survfit")) stop("fit_km needs to be of class survfit")
  cd <- kmCurveData(fit_km = fit_km, xaxis_break = xaxis_break)
 
  
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
  
  g_kmplot <-  gTree(childrenvp = vptree,
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
                    gp = gp, vp = vp, name = name,
                    cl = "kmGrob")
   g_kmplot
  
}


#' Add text annotation on top of a kmgrob
#' 
#' utility function to edit a grid grob by adding a text grob 
#' 
#' @param kmgrob a kmGrob for further editing.
#' @param tbl An rtable.
#' @param x A numeric vector or unit object specifying x-values.
#' @param y A numeric vector or unit object specifying y-values.
#' @param just The justification of the text relative to its (x, y) location.
#' @param vp a Grid veiwport object for annotating a text grob.
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
#' a_kmgrob <- kmGrob(fit_km, xaxis_break = 0.5)
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
addTable <- function(kmgrob, tbl, x = unit(0.5, "npc") , y = unit(0.5, "npc"), 
                      just = c("left", "top"), vp = vpPath("plotArea", "topCurve")){
   
  addGrob(kmgrob,
          textGrob(label = tbl, 
                   x = x,
                   y = y,
                   just = just,
                   gp = gpar(fontfamily = 'mono', fontsize = 8, fontface = "bold"),
                   vp = vp))
}


#' Draw plot of a grob or list of grobs
#' 
#' utility function to draw a grob or a list of grobs
#' 
#' @param grobs a grob or a list of grobs.
#' @param ncol number of plots per column.
#' @param newpage boolean if \code{draw=TRUE} should plot be drawn on a new page
#' 
#' @import gridExtra
#' @import grid
#' @import dplyr
#' 
#' @export
#' 
#' @examples 
#' library(dplyr)
#' library(gridExtra)
#' library(grid)
#' ptext <- textGrob("test text here")
#' prect <- rectGrob(width = unit(0.5, "npc"), height = unit(0.5, "npc"))
#' grobDraw(ptext)
#' grobDraw(prect)
#' grobDraw(list(ptext, prect))
       
      
grobDraw <- function(grobs, ncol = 1,  newpage = TRUE){
  
  if (newpage) grid.newpage()
  
  if (is(grobs, "grob")) {
    grobs %>% grid.draw()
  } else if (is(grobs, "list")){
    if (length(grobs) < 1){
      stop("grobs can't be a list of NULL")
    } else {
      
      gl <- Map(function(x){
        is(x, "grob")
      }, grobs)
      
      if (all(gl)){
        arrangeGrob(grobs = grobs, ncol = ncol) %>% grid.draw()
      } else {
        stop("all list elements must be grob")
      }
    }
  } else {
    stop("grobs can only be a grob or list of grobs")
  }
  
}
