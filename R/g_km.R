#' Kaplan-Meier Plot
#' 
#' Create a KM plot for any \code{\link[survival]{survfit}} object.
#' 
#' @inheritParams g_forest
#' @param fit_km a class \code{\link{survfit}} object.
#' @param col a vector of color for each line.
#' @param xticks break interval of x-axis. It takes a numeric vector or
#'   \code{NULL}.
#'   
#' 
#' @template author_wangh107
#' 
#' @import grid
#' @importFrom gridExtra arrangeGrob
#' 
#' @export
#' 
#' @examples 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL")
#' ASL$RACE <- factor(sapply(as.character(ASL$RACE), function(x) {
#'    if (nchar(x)>9) paste0(substr(x, 1,9), "...") else x
#' }))
#' ATE <- radam("ATE", ADSL = ASL)
#' 
#' ATE_f <- subset(ATE, PARAMCD == "OS") 
#' 
#' ANL <- merge(ASL, ATE_f, by = c("USUBJID", "STUDYID"))
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = ANL, conf.type = "plain")
#' g_km(fit_km = fit_km)
#'  g_km(fit_km = fit_km, col = c("black", "red"))
   

g_km <- function(fit_km, col = NULL, xticks = NULL, title = "Kaplan - Meier Plot",
                 draw = TRUE, newpage = TRUE ){
  
  if (!is(fit_km, "survfit")) stop("fit_km needs to be of class survfit")
  grobKm <- kmGrob(fit_km = fit_km, col = col, xticks = xticks, title = title)
 
  
  if (draw) {
    if (newpage) grid.newpage()
    grid.draw(grobKm)
  }
  invisible(grobKm)
}

 

#' Define a kmplot grob
#' 
#' create a grid graphical object for basic KM plot from a \code{\link{survfit}} object.
#' 
#' @param fit_km a class \code{\link{survfit}} object.
#' @param col a vector of color for each line.
#' @param xticks break interval of x-axis. It takes a numeric vector or \code{NULL}.
#' @param title title for plot.
#' 
#' @importFrom scales col_factor
#' @import grid
#' 
#' @noRd
#' 
#' @examples 
#' OS <- data.frame(AVAL = abs(rnorm(200)), 
#'                  CNSR = sample(c(0, 1), 200, TRUE), 
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = OS, conf.type = "plain")
#' kmGrob(fit_km = fit_km, xticks = c(0.5, 0.8, 1.5))
#' 
kmGrob <- function(fit_km, col = NULL, xticks = NULL, title = "Kaplan - Meier Plot", 
                   gp = NULL, vp = NULL, name = NULL) {
  
  if (!is(fit_km, "survfit")) stop("fit_km needs to be of class survfit")
  cd <- kmCurveData(fit_km = fit_km, xticks = xticks)
  if (!is.null(col)) {
    if (length(col) != length(cd$group)) stop("Number of color is not equal to number of lines")
    cd$colpal <- col
  }
  
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


#' Prepare necessary data for Kaplan-Meier Plot 
#' 
#' Return a list of data for primitive element of grid drawing
#' 
#' @param fit_km a class \code{\link{survfit}} object.
#' @param xticks break interval of x-axis. It takes a numeric vector or \code{NULL}.
#' 
#' @importFrom scales col_factor
#' @importFrom utils head tail  
#' 
#' @noRd
#' 
#' @template author_wangh107
#' 
#' 
#' @examples 
#' 
#' 
#' OS <- data.frame(AVAL = abs(rnorm(200)), 
#'                  CNSR = sample(c(0, 1), 200, TRUE), 
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = OS, conf.type = "plain")
#' kmCurveData(fit_km, xticks = c(0.5, 0.8, 1.5))
#' 
#' 
kmCurveData <- function(fit_km, xticks = NULL) {
  
  if (!is(fit_km, "survfit")) stop("fit_km needs to be of class survfit")
  
  ngroup <- length(fit_km$strata)
  if (ngroup > 9) stop("unfortunately we currently do not have more than 9 colors to encode different groups")
  
  # extract kmplot relevant data
  df <- data.frame(
    time = fit_km$time,
    surv = fit_km$surv,
    n.risk = fit_km$n.risk,
    n.censor = fit_km$n.censor,
    n.event = fit_km$n.event,
    std.err = fit_km$std.err,
    upper = fit_km$upper,
    lower = fit_km$lower,
    group = factor(rep(names(fit_km$strata), fit_km$strata), levels = names(fit_km$strata))
  )
  
  # split by group
  df_s <- split(df, df$group)
  group <- fit_km$strata
  
  ### width of label in Risk table
  tmp.labels <- names(group)
  tmp.label <- tmp.labels[which.max(nchar(tmp.labels))[1]]
  nlines_labels <- convertWidth(stringWidth(tmp.label), "lines", TRUE) + 2
  
  
  ### interval in x-axis
  if (length(xticks) <= 1){
    xpos <- seq(0, floor(max(df$time)), by = ifelse(is.null(xticks), 
                                                    max(1, floor(max(df$time)/10)), 
                                                    xticks))
  } else {
    xpos <-  c(0, xticks)
  }
  
  ypos <- 1 - 1:length(df_s)/(length(df_s) + 1)
  
  pt_risk <- lapply(df_s, function(x){
    n.r <- vapply(xpos, function(xi) {
      if (xi <= min(x$time)){
        i <- head(which(x$time >= xi), 1)
        x$n.risk[i]
      } else if (xi > max(x$time)){
        NA
      } else{
        i <- tail(which(x$time <= xi), 1)
        x$n.risk[i] - x$n.censor[i] - x$n.event[i]
      }
    }, numeric(1))
  })
  
  
  xData <- c(0, df$time)
  lines_x <- lapply(df_s, function(x){
    c(0, rep(x$time, each = 2))
  })
  lines_y <- lapply(df_s, function(x){
    c(rep(c(1, head(x$surv, -1)), each = 2), tail(x$surv, 1))
  })
  
  points_x <- lapply(df_s, function(x){
    x[x$n.censor !=0, "time"]
  })
  
  points_y <- lapply(df_s, function(x){
    x[x$n.censor !=0, "surv"]
  })
  
  col_pal <- scales::col_factor("Set1", domain = names(df_s))
  colpal <- col_pal(names(df_s))
  return(list(nlines_labels = nlines_labels,
              xpos = xpos,
              xData = xData,
              lines_x = lines_x, 
              lines_y = lines_y,
              points_x = points_x,
              points_y = points_y,
              colpal = colpal,
              group = group,
              ypos = ypos,
              pt_risk = pt_risk))
}


