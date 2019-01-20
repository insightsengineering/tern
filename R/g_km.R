#' Kaplan-Meier Plot
#' 
#' Create a KM plot for any \code{\link[survival]{survfit}} object.
#' 
#' @param fit_km a class \code{\link{survfit}} object.
#' @param xticks break interval of x-axis. It takes a numeric vector or \code{NULL}.
#' @param col line color.
#' @param lty line type.
#' @param lwd line width.
#' @param censor.show \code{TRUE/FALSE} to show censored.
#' @param pch point type for censored.
#' @param size size of censored point, a class of \code{unit}.
#' @param title title for plot.
#' @param xlab a string for label of x-axis
#' @param ylab a string for label of y-axis.
#' @param gp a abject of class \code{gpar}.
#' @param vp a grid \code{viewport}.
#' @param name A character value to uniquely identify the object.
#' @param draw draw the plot on device.
#' @param newpage open a new draw page.
#' 
#' @template author_wangh107
#' 
#' @import grid
#' 
#' @export
#' 
#' @examples 
#' library(random.cdisc.data)
#' library(tern)
#' library(dplyr)
#' ASL <- radsl()
#' ASL$RACE <- factor(sapply(as.character(ASL$RACE), function(x) {
#'    if (nchar(x)>9) paste0(substr(x, 1,9), "...") else x
#' }))
#' ATE <- radte(ADSL = ASL)
#' 
#' ATE_f <- subset(ATE, PARAMCD == "OS") 
#' 
#' ANL <- merge(ASL[,c("USUBJID", "STUDYID", "SEX")],
#'  ATE_f, by = c("USUBJID", "STUDYID"))
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = ANL, conf.type = "plain")
#' 
#' p <- g_km(fit_km = fit_km)
#' grid.newpage()
#' grid.draw(p)
#' 
#' p <- g_km(fit_km = fit_km, col = c("black", "red", "blue"), lty = c(1, 2, 3))
#' y <-  textGrob(label = toString(tern:::t_km(fit_km), gap = 1), 
#'                    x = unit(0.5, "npc"),
#'                    y = unit(0.9, "npc"),
#'                    just = c("left", "top"),
#'                    gp = gpar(fontfamily = 'mono', fontsize = 8, fontface = "bold"))
#' p <- addGrob(p, gTree(children = gList(y), vp = vpPath("mainPlot", "kmCurve", "curvePlot")))                  
#' grid.newpage()
#' grid.draw(p)
#' 
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ 1, data = ANL, conf.type = "plain")
#' p <- g_km(fit_km, xlab = "Duration (Days)", col = "green" )
#' grid.newpage()
#' grid.draw(p)
#' 
g_km <- function(fit_km,  xticks = NULL, col = NA, lty = 1, lwd = 1,
                 censor.show = TRUE, pch = 3, size = unit(0.5, "char"),
                 title = "Kaplan - Meier Plot", xlab = "Days", ylab = "Survival Probability",
                 gp = NULL, vp = NULL, name = NULL ){
  
  kmdata <- kmCurveData(fit_km = fit_km, xticks = xticks)
  ### margins
  tmp.labels <- names(kmdata$group)
  tmp.label <- tmp.labels[which.max(nchar(tmp.labels))]
  nlines_labels <- convertWidth(stringWidth(tmp.label), "lines", TRUE) + 2
  
  childrenvp <- vpTree(
    plotViewport(margins = c(3, max(nlines_labels, 4), 3, 2),
                 layout = grid.layout(
                   nrow = 3, ncol = 1, widths = unit(1, "npc"),
                   heights = unit(c(5, 8, length(kmdata$group) + 2), c("null", "lines", "lines"))
                 ), name = "mainPlot"),
    vpList(
      viewport(layout.pos.row = 1, layout.pos.col = 1, name = "kmCurve"),
      viewport(layout.pos.row = 2, layout.pos.col = 1, name = "xLab"),
      viewport(layout.pos.row = 3, layout.pos.col = 1, name = "riskTable")
    )
  )
  
  ### km curve 
  kmgrob <- kmCurveGrob(kmdata, col = col, lty = lty, lwd = lwd, 
                        censor.show = censor.show, pch = pch, size = size, 
                        vp = vpPath("mainPlot", "kmCurve"))
  ### pt at risk
  label <- do.call("c", kmdata$pt_risk)
  label[which(is.na(label))] <- ""
  x <- rep(kmdata$xpos, length(kmdata$group))
  group <- rep(names(kmdata$pt_risk), each = length(kmdata$xpos))
  group <- factor(group, levels = names(kmdata$pt_risk))
  riskgrob <- labelPanelGrob(label = label, x = x, group = group, col = col,
                             vp = vpPath("mainPlot", "riskTable"))
  
  col <- to_group_color(col, kmdata$group)
  kmGrob <-   gTree( kmdata = kmdata, vp = vp, name = name, gp = gp,
                     childrenvp = childrenvp,
                     children = gList(
                       kmgrob,
                       textGrob(ylab, x = unit(-3.5, "lines"), rot = 90, 
                                vp = vpPath("mainPlot", "kmCurve", "curvePlot")),
                       textGrob(title, y = unit(1, "npc") + unit(1, "lines"), 
                                gp = gpar(fontface = "bold", fontsize = 16), 
                                vp = vpPath("mainPlot", "kmCurve", "curvePlot")),
                       textGrob(xlab, x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
                                just = "top", vp = vpPath("mainPlot", "xLab")),
                       riskgrob,
                       textGrob(levels(group), x = unit(-nlines_labels + 1, "lines"),
                                y = unit(1:nlevels(group)/(nlevels(group)+1), "npc"),
                                just = "left",  gp = gpar(col = col), 
                                vp = vpPath("mainPlot", "riskTable", "labelPlot")),
                       textGrob(label = "Number of Patients at Risk",
                                x = unit(0, "npc"),
                                y = unit(1, "npc") + unit(1, "lines"),
                                just = "left", vp = vpPath("mainPlot", "riskTable", "labelPlot")),
                       xaxisGrob(at = unique(x), vp = vpPath("mainPlot", "riskTable", "labelPlot"))

                     ),
                     cl = "kmGrob")
  invisible(kmGrob)
}

 

#' Define a kmplot grob
#' 
#' create a grid graphical object for basic KM plot from a \code{\link{survfit}} object.
#' 
#' @param kmdata a class \code{\link{kmCurveData}} object.
#' @param fit_km a class \code{\link{survfit}} object.
#' @param xticks break interval of x-axis. It takes a numeric vector or \code{NULL}.
#' @param col line color.
#' @param lty line type.
#' @param lwd line width.
#' @param censor.show \code{TRUE/FALSE} to show censored.
#' @param pch point type for censored.
#' @param size size of censored point, a class of \code{unit}.
#' @param gp a abject of class \code{gpar}.
#' @param vp a grid \code{viewport}.
#' @param name A character value to uniquely identify the object.
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
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ 1, data = OS)                 
#' kmdata <- tern:::kmCurveData(fit_km, xticks = c(0.5, 1.0, 2.0))               
#' kmgrob <- tern:::kmCurveGrob(kmdata, col = "blue") 
#' grid.newpage()
#' pushViewport(plotViewport())
#' grid.draw(kmgrob)         
#'        
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = OS, conf.type = "plain")
#' kmgrob <- tern:::kmCurveGrob(fit_km = fit_km, xticks = c(0.5, 0.8, 1.5))
#' grid.newpage()
#' pushViewport(plotViewport())
#' grid.draw(kmgrob)
#' 
kmCurveGrob <- function(kmdata, fit_km,  xticks = NULL, 
                        col = NA, lty = 1, lwd = 1,
                        censor.show = TRUE, pch = 3, size = unit(0.5, "char"),
                        gp = NULL, vp = NULL, name = NULL) {
  
  if (!missing(kmdata) && !is(kmdata, "kmCurveData")) 
    stop("kmdata is not a kmCurveData object")
  if (!missing(fit_km) && !is(fit_km, "survfit"))
    stop("fit_km needs to be of class survfit")
  if (missing(kmdata) && missing(fit_km))
    stop("kmdata and fit_km can not be both missing")
  
  if (missing(kmdata)) kmdata <- kmCurveData(fit_km = fit_km, xticks = xticks)

  ngroup <- length(kmdata$group)
  
  lwd <- to_n(lwd, ngroup)
  lty <- to_n(lty, ngroup)
  
  col <- to_group_color(col, kmdata$group)
  
  lines <- Map(function(x, y, col_i, lty_i, lwd_i){
    linesGrob(x = x, y = y, default.units = "native", 
              gp = gpar(col = col_i, lwd = lwd_i, lty = lty_i),
              vp =  "curvePlot")
  }, kmdata$lines_x, kmdata$lines_y, col, lty, lwd)
  
  
  if (censor.show){
    pch <- to_n(pch, ngroup)
    size <- list(to_n(size, ngroup))
    points <- Map(function(x, y, col_i, pch_i, size_i){
      pointsGrob(x = x, y = y, pch = pch_i, size = size_i,
                 default.units = "native",
                 gp = gpar(col = col_i),
                 vp =  "curvePlot")
    }, kmdata$points_x, kmdata$points_y, col, pch, size)
 
  }
  
  gTree(
    kmdata = kmdata, vp = vp, name = name, gp = gp,
    childrenvp = dataViewport(xData = kmdata$xpos, yData = c(0, 1), name = "curvePlot"),
    children = do.call("gList",
                       c(list(
                         xaxisGrob(at = kmdata$xpos, vp = "curvePlot"),
                         yaxisGrob(vp = "curvePlot"),
                         rectGrob(vp =  "curvePlot")),
                         lines,
                         if (censor.show) points)),
    cl = "kmCurveGrob"
  )
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
#' tern:::kmCurveData(fit_km, xticks = c(0.5, 0.8, 1.5))
#' 
#' 
kmCurveData <- function(fit_km, xticks = NULL) {
  
  if (!is(fit_km, "survfit")) stop("fit_km needs to be of class survfit")
  # extract kmplot relevant data
  if (is.null(fit_km$strata)) {
     
    df <- data.frame(
      time = fit_km$time,
      surv = fit_km$surv,
      n.risk = fit_km$n.risk,
      n.censor = fit_km$n.censor,
      n.event = fit_km$n.event,
      std.err = fit_km$std.err,
      upper = fit_km$upper,
      lower = fit_km$lower,
      group = rep("All", length(fit_km$time))
    )
    
    group <- "All"
    names(group) <- "All"
  } else {
     
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
    
    group <- fit_km$strata
  }
  
  # split by group
  df_s <- split(df, df$group)

  ### interval in x-axis
  if (length(xticks) <= 1){
    xpos <- seq(0, floor(max(df$time)), by = ifelse(is.null(xticks), 
                                                    max(1, floor(max(df$time)/10)), 
                                                    xticks))
  } else {
    xpos <-  c(0, xticks)
  }

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
  
  
  #xData <- c(0, df$time)
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

  structure(list(xpos = xpos,
                # xData = xData,
                 lines_x = lines_x, 
                 lines_y = lines_y,
                 points_x = points_x,
                 points_y = points_y,
                 group = group,
                 pt_risk = pt_risk), 
            class = "kmCurveData")
}

 
