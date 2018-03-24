#' Add text annotation on top of a kmgrob
#' 
#' utility function to edit a grid grob by adding a text grob 
#' 
#' @param kmgrob a kmGrob for further editing.
#' @param tbl An \code{\link[rtables]{rtable}}.
#' @param x A numeric vector or unit object specifying x-values.
#' @param y A numeric vector or unit object specifying y-values.
#' @param just The justification of the text relative to its (x, y) location.
#' @param vp a Grid veiwport object for annotating a text grob.
#' 
#' 
#' @template author_wangh107
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
#' a_kmgrob <- kmGrob(fit_km, xticks = 0.5)
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
  
  tblstr <- toString(tbl, gap = 1)
  addGrob(kmgrob,
          textGrob(label = tblstr , 
                   x = x,
                   y = y,
                   just = just,
                   gp = gpar(fontfamily = 'mono', fontsize = 8, fontface = "bold"),
                   vp = vp))
}


