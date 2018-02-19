#' Forest plot (table + graph)
#' 
#' @param x rtable from forest_rsp function or from forest_tte function
#' @param arm.ref string used to label reference arm
#' @param arm.comp string used to label comparable arm
#' @param anl.model the regression method used for analysis. Valid options are \t{survival} and \t{logistic}
#' @param time.unit string used to display the unit of median survival time. default is "month". 
#' @param padx gap between two columns
#' @param cex multiplier applied to overall fontsize
#' 
#' @author Yuyao Song (songy24), \email{yuyao.song@roche.com}
#' 
#' @import grid
#' 
#' @export
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL")
#' ATE <- radam("ATE", ADSL = ASL)
#' 
#' ATE_f <- ATE %>% filter(PARAMCD == "OS") 
#' 
#' ANL <- merge(ASL %>% select(USUBJID, STUDYID, SEX, RACE, ARM), ATE_f)
#' 
#' tbl <- t_forest_tte(
#'   tte = ANL$AVAL,
#'   is_event = ANL$CNSR == 0,
#'   col_by = factor(ANL$ARM), 
#'   group_data = as.data.frame(lapply(ANL[, c("SEX", "RACE")], as.factor))
#' )
#' 
#' g <- g_forest(tbl = tbl, i_col_est = 8, i_col_ci = 9, header_forest = c("Treatement Better", "Comparison Better"))
#' 
#' library(grid)
#' grid.newpage()
#' grid.draw(g)
#' 
#' 
#' 
#' 
#' 
#' library(random.cdisc.data)
#' ASL <- radam("ASL")
#' ARS <- radam("ARS", ADSL = ASL)
#' 
#' ARS_f <- ARS %>% filter(PARAMCD == "OVRSPI") 
#' 
#' ASL_f <- right_join(ASL %>% select(USUBJID, STUDYID, SEX, RACE, ARM),
#'                         ARS_f %>% select(USUBJID, STUDYID))
#' 
#' tbl <- t_forest_rsp(
#'   response = ARS_f$AVAL,
#'   event = ARS_f$AVALC %in% c("CR","PR"),
#'   arm = ASL_f$ARM, 
#'   group_data = ASL_f %>% select("SEX", "RACE")
#' )
#' 
#' g_forest(tbl)
#' 
g_forest <- function(tbl, i_col_est, i_col_ci, header_forest, padx = unit(0, "lines"), cex = 1) {
  

  
  vp <- vpTree(
    parent = viewport(
      name = "forestplot",
      layout = grid.layout(
        nrow = 1, ncol = 11,
        widths = unit.c(
          stringWidth(rn[which.max(nchar(rn))]) + 1 * padx,
          stringWidth("xxxxx") + 2 * padx,
          stringWidth("xxxxx") + 2 * padx,
          stringWidth("xxxxx") + 2 * padx,
          stringWidth("xx.xx") + 2 * padx,
          stringWidth("xxxxx") + 2 * padx,
          stringWidth("xxxxx") + 2 * padx,
          stringWidth("xx.xx") + 2 * padx,
          stringWidth("xx.xx") + 2 * padx,
          stringWidth("xx.xx - xx.xx") + 2 * padx,
          unit(1, "null")
        )
      )
    ),
    children = vpList(
      viewport(name = "col_1", layout.pos.col=1, layout.pos.row=1),
      viewport(name = "col_2", layout.pos.col=2, layout.pos.row=1),
      viewport(name = "col_3", layout.pos.col=3, layout.pos.row=1),
      viewport(name = "col_4", layout.pos.col=4, layout.pos.row=1),
      viewport(name = "col_5", layout.pos.col=5, layout.pos.row=1),
      viewport(name = "col_6", layout.pos.col=6, layout.pos.row=1),
      viewport(name = "col_7", layout.pos.col=7, layout.pos.row=1),
      viewport(name = "col_8", layout.pos.col=8, layout.pos.row=1),
      viewport(name = "col_9", layout.pos.col=9, layout.pos.row=1),
      viewport(name = "col_10", layout.pos.col=10, layout.pos.row=1),
      dataViewport(name = "col_11", layout.pos.col=11, layout.pos.row=1,
                   xData = c(-2,2), yData = c(0,1))
    )
  )
  
  grid.newpage()
  pushViewport(viewport(gp = gpar(cex = cex)))
  
  pushViewport(plotViewport(margins = c(3,2,14,2)))
  
  pushViewport(vp)
  
  # grid.ls(viewports = TRUE)
  seekViewport("forestplot")
  
  # break arm labels to muliple lines as needed
  arm.ref <- wrap_text(arm.ref, width = unit(4, "cm"), collapse = "\n")
  arm.comp <- wrap_text(arm.comp, width = unit(4, "cm"), collapse = "\n")
  
  # need once: mid-line OR/HR = 1
  grid.xaxis(at = c(log(0.1), log(0.5), log(1), log(2), log(5), log(10)), label = c(0.1, 0.5, 1, 2, 5, 10), vp = vpPath("col_11"))
  grid.lines(x = unit(c(0,0), "native"), y = unit(c(0, 1), "npc"), vp = vpPath("col_11"),
             gp = gpar(lty = 2))  
  
  # Add Header
  if(anl.model == "logistic"){
    draw_header("Baseline Risk Factors","Total n", "n", "Resp.n", "Resp.Rate (%)", "n", "Resp.n", "Resp.Rate (%)", "Odds Ratio", "95% CI", arm.ref,arm.comp)
  } else {
    draw_header("Baseline Risk Factors","Total n", "n", "Events", paste("Median", " (", time.unit, ")", sep = ""), 
                    "n", "Events", paste("Median", " (", time.unit, ")", sep = ""), "Hazard Ratio", "95% CI", arm.comp, arm.ref, hazard.ratio = TRUE)
  }
  
  # Add table contents
  for (i in 1:nrow(x)){
    if (!is.null(x[i,1])) {
      x1 <- row.names(x)[i]; x2 <- x[i, 1]; x3 <- x[i, 2]; x4 <- x[i, 3]; x5 <- round(x[i, 4], 1); x6 <- x[i, 5]; x7 <- x[i, 6]; x8 <- round(x[i, 7], 1); 
      x9 <- ifelse(is.numeric(x[i, 8]),  round(x[i, 8], 2), x[i, 8]) 
      x10 <- paste("(", paste(round(x[i, 9],2), collapse = ", "), ")", sep = "")
      if (is.numeric(x[i, 8])) x11 <-  c(log(abs(x[i,8])), log(abs(x[i,9]))) else x11 <- c(999.99, log(abs(x[i,9])))
      draw_row(i, nrow(x), x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, add_hline = TRUE, fontface = 1)
    } else if (is.null(x[i,1]) & row.names(x)[i] != "") {
      draw_row(i, nrow(x), row.names(x)[i], "", "", "", "", "", "", "", "", "", c(NA, NA, NA), FALSE, 2)
    } else {
      draw_row(i, nrow(x), "", "", "", "", "", "", "", "", "", "", c(NA, NA, NA), FALSE,2)
    }
  }
}

draw_header <- function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, hazard.ratio= FALSE) {
  
  library(stringr)
  
  ypos = unit(1, "npc")+unit(1, "lines")

  #Baseline Risk factor header
  grid.text(x1, 
            x = unit(0, "npc"), 
            y = ypos + unit(0.5, "lines"), 
            vp = vpPath("col_1"), 
            just = "left", 
            gp = gpar(fontsize = 10, fontface = 2))
  
  #Statistics column header
  mapply(function(panel, i) grid.text(i, 
                                      y = ypos, 
                                      vp = vpPath(panel), 
                                      gp = gpar(fontsize = 10, fontface = 2), 
                                      rot = 90, 
                                      just = c("left", "center")),
         paste("col", c(2:10), sep = "_"), c(x2, x3, x4, x5, x6, x7, x8, x9, x10), SIMPLIFY =FALSE, USE.NAMES = FALSE)
  
  #Treatment Arms as column headers
  mapply(function(panels, i) grid.text(i,
                                       x = unit(0.5, "native"),
                                       y = ypos + unit(6.75 + str_count(i, "\n"), "lines"),
                                       vp = vpPath(ifelse(hazard.ratio == FALSE, panels[1], panels[2])),
                                       gp = gpar(fontsize = 10 ,fontface = 2)),
        list(c("col_4", "col_7"), c("col_7", "col_4")), c(x11, x12), SIMPLIFY =FALSE, USE.NAMES = FALSE)
  
  #Statistics header dividing lines between ARM and header
  mapply(function(panel, loc) grid.lines(x = unit(loc, "native"), 
                                         y = ypos + unit(5, "lines"), 
                                         vp = vpPath(panel), 
                                         gp = gpar(lty = 1, lwd = 2)),
         paste("col", c(3:8), sep = "_"), list(c(0, 1), c(0, 1), c(0,0.9), c(0.1,1), c(0,1), c(0,1)), SIMPLIFY =FALSE, USE.NAMES = FALSE)
  
  #Line between header and table
  grid.lines(unit(c(0,1), "npc"),
             y = ypos - unit(0.5, "lines"), 
             gp = gpar(col = "black", lty = 1, lwd = 2))
  
 
  #Forest plot header
  mapply(function(loc, i) {
                grid.text(i, 
                          x = unit(loc, "native"), 
                          y = ypos + unit(str_count(i, "\n") + 1.5, "lines"),
                          vp = vpPath("col_11"), 
                          gp = gpar(fontsize = 10, fontface = 2))
                grid.text("Better", 
                          x = unit(loc, "native"), 
                          y = ypos + unit(.5, "lines"),
                          vp = vpPath("col_11"), 
                          gp = gpar(fontsize = 10, fontface = 2))},
         c(-1, 1), c(x11, x12), SIMPLIFY =FALSE, USE.NAMES = FALSE)


}

draw_row <- function(i,n, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, add_hline = FALSE, fontface = 1) {
  ypos <- unit(1 - i/(n+1), "npc")
  
  indent_x1 <- if (fontface == 1 && x1 != "ALL") 1 else 0
  
  grid.text(x1, 
            x = unit(0, "npc") + unit(indent_x1, "lines"),
            y = ypos, vp = vpPath("col_1"),
            gp = gpar(fontsize = 10 , fontface = fontface),
            just = "left")
  
  mapply(function(panel, i) grid.text(i, 
                                      y = ypos, 
                                      vp = vpPath(panel), 
                                      gp = gpar(fontsize = 10 , fontface = fontface)),
         paste("col", c(2:10), sep = "_"), c(x2, x3, x4, x5, x6, x7, x8, x9, x10), SIMPLIFY =FALSE, USE.NAMES = FALSE)
  
  #Only draw if the CI is within the range of 0.1-10
  if (!any(is.na(x11)) && !is.na(x11[1]) && x11[2] <= log(10) && x11[3] >= log(0.1)){
    if ((is.na(x11[2]) || x11[2] < log(0.1)) && x11[3] <= log(10) ){
      grid.lines(x = unit(c(log(0.1), x11[3]), "native"), 
                 y = unit.c(ypos, ypos), 
                 arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "first"), 
                 vp = vpPath("col_11"), 
                 gp =gpar(col = "blue", lwd = 2)) 
    }
    if (x11[2] >= log(0.1) && (is.na(x11[3]) || x11[3] > log(10))){
      grid.lines(x = unit(c(x11[2], log(10)), "native"), 
                 y = unit.c(ypos, ypos), 
                 arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "last"), 
                 vp = vpPath("col_11"), 
                 gp =gpar(col = "blue", lwd = 2)) 
    }
    if (x11[2] >= log(0.1) && x11[3] <= log(10) ){
      grid.lines(x = unit(c(x11[2], x11[3]), "native"), 
                 y = unit.c(ypos, ypos), 
                 vp = vpPath("col_11"), 
                 gp =gpar(col = "blue", lwd = 2)) 
    }
    if (x11[2] < log(0.1) && x11[3] > log(10) ){
      grid.lines(x = unit(c(log(0.1), log(10)), "native"), 
                 y = unit.c(ypos, ypos), 
                 arrow = arrow(angle = 30, length = unit(0.5, "lines"), ends = "both"), 
                 vp = vpPath("col_11"), 
                 gp =gpar(col = "blue", lwd = 2)) 
    }
    if (x11[1] >= log(0.1) && x11[1] <= log(10)){
      grid.circle(x = unit(x11[1], "native"),
                  y = ypos, r = unit(1/3.5, "lines"), 
                  vp = vpPath("col_11"),
                  gp = gpar(col = "blue", fill = "blue"))
    }
  }    
  if (add_hline) {
    grid.lines(unit(c(0,1), "npc"), y = unit.c(ypos, ypos) - unit(1/(2*n-2), "npc"), gp = gpar(col = "grey", lty = 1, lwd = 0.3))
  }
  
}