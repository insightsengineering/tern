#' Add an rtable as Text Annotation to a Gird Grob
#' 
#' utility function to edit a grid grob by adding a text grob 
#' 
#' @param grob a kmGrob for further editing
#' @param tbl an \code{\link[rtables]{rtable}}
#' @param x numeric vector or unit object specifying x-values
#' @param y numeric vector or unit object specifying y-values
#' @param just The justification of the text relative to its (x, y) location
#' @param vp grid viewport object for annotating a text grob
#' 
#' 
#' @template author_wangh107
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
#' 
#'                                     
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = OS, conf.type = "plain")
#' 
#' a_kmgrob <- g_km(fit_km, xticks = 0.5, draw = FALSE)
#' 
#' fit_coxph <- coxph(Surv(AVAL, 1-CNSR) ~ ARM + strata(RACE), data = OS, ties = "exact")
#' cox_tbl <- t_coxph(fit_coxph)
#' 
#' p_tbl <- addTable(
#'   a_kmgrob, 
#'   vp = vpPath("plotArea", "topCurve"), 
#'   x= unit(1, "lines"), y = unit(1, "lines"),
#'   just = c("left", "bottom"),
#'   tbl = cox_tbl 
#' )
#' 
#' grid.newpage()
#' grid.draw(p_tbl)
#' 
addTable <- function(grob, tbl, x = unit(0.5, "npc") , y = unit(0.5, "npc"), 
                     just = c("left", "top"), vp = vpPath("plotArea", "topCurve")){
  
  tblstr <- toString(tbl, gap = 1)
  addGrob(grob,
          textGrob(label = tblstr , 
                   x = x,
                   y = y,
                   just = just,
                   gp = gpar(fontfamily = 'mono', fontsize = 8, fontface = "bold"),
                   vp = vp))
}


