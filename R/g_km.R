#' Kaplan-Meier Plot
#'
#' Create a KM plot for any \code{\link[survival]{survfit}} object.
#'
#' @param fit_km a class \code{\link{survfit}} object.
#' @param xticks break interval of x-axis. It takes a numeric vector or \code{NULL}.
#' @param col line color.
#' @param lty line type.
#' @param lwd line width.
#' @param censor_show \code{TRUE/FALSE} to show censored.
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
#' library(dplyr)
#'
#' ADSL <- cadsl
#' levels(ADSL$RACE) <- strtrim(levels(ADSL$RACE), 10)
#'
#' ADTTE <- cadtte
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")
#'
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = ADTTE_f, conf.type = "plain")
#'
#' g_km(fit_km = fit_km)
#' plot(fit_km)
#'
#' p <- g_km(fit_km = fit_km, col = c("black", "red", "blue"), lty = c(1, 2, 3), draw = FALSE)
#'
#' g_tkm <-  textGrob(label = toString(t_km(fit_km), gap = 1),
#'                    x = unit(1, "npc") - unit(2, "lines"),
#'                    y = unit(1, "npc") - unit(2, "lines"),
#'                    just = c("right", "top"),
#'                    gp = gpar(fontfamily = 'mono', fontsize = 8, fontface = "bold"))
#'
#' p_t <- addGrob(p, gTree(children = gList(g_tkm), vp = vpPath("mainPlot", "kmCurve", "curvePlot")))
#'
#' grid.newpage()
#' grid.draw(p_t)
#'
#' g_km(fit_km, xlab = "Duration (Days)", col = "green")
g_km <- function(fit_km,
                 xticks = NULL,
                 col = NA,
                 lty = 1,
                 lwd = 1,
                 censor_show = TRUE,
                 pch = 3,
                 size = unit(0.5, "char"),
                 title = "Kaplan - Meier Plot",
                 xlab = "Days",
                 ylab = "Survival Probability",
                 draw = TRUE,
                 newpage = TRUE,
                 gp = NULL,
                 vp = NULL,
                 name = NULL) {

  kmdata <- km_curve_data(fit_km = fit_km, xticks = xticks)
  ### margins
  tmp_labels <- names(kmdata$group)
  tmp_label <- tmp_labels[which.max(nchar(tmp_labels))]
  nlines_labels <- convertWidth(stringWidth(tmp_label), "lines", TRUE) + 2

  childrenvp <- vpTree(
    plotViewport(
      margins = c(3, max(nlines_labels, 4), 3, 2),
      layout = grid.layout(
        nrow = 3, ncol = 1, widths = unit(1, "npc"),
        heights = unit(c(5, 8, length(kmdata$group) + 2), c("null", "lines", "lines"))
      ),
      name = "mainPlot"
    ),
    vpList(
      viewport(layout.pos.row = 1, layout.pos.col = 1, name = "kmCurve"),
      viewport(layout.pos.row = 2, layout.pos.col = 1, name = "xLab"),
      viewport(layout.pos.row = 3, layout.pos.col = 1, name = "riskTable")
    )
  )

  ### km curve
  kmgrob <- km_curve_grob(
    kmdata,
    col = col,
    lty = lty,
    lwd = lwd,
    censor_show = censor_show,
    pch = pch,
    size = size,
    vp = vpPath("mainPlot", "kmCurve")
  )
  ### pt at risk
  label <- do.call("c", kmdata$pt_risk)
  label[which(is.na(label))] <- ""
  x <- rep(kmdata$xpos, length(kmdata$group))
  group <- rep(names(kmdata$pt_risk), each = length(kmdata$xpos))
  group <- factor(group, levels = names(kmdata$pt_risk))
  riskgrob <- label_panel_grob(
    label = label,
    x = x,
    group = group,
    col = col,
    vp = vpPath("mainPlot", "riskTable")
  )

  col <- to_group_color(col, names(kmdata$group))
  km_grob <- gTree(
    kmdata = kmdata,
    vp = vp,
    name = name,
    gp = gp,
    childrenvp = childrenvp,
    children = gList(
      kmgrob,
      textGrob(
        ylab,
        x = unit(-3.5, "lines"),
        rot = 90,
        vp = vpPath("mainPlot", "kmCurve", "curvePlot")
      ),
      textGrob(
        title,
        y = unit(1, "npc") + unit(1, "lines"),
        gp = gpar(fontface = "bold", fontsize = 16),
        vp = vpPath("mainPlot", "kmCurve", "curvePlot")
      ),
      textGrob(
        xlab,
        x = unit(0.5, "npc"),
        y = unit(0.5, "npc"),
        just = "top",
        vp = vpPath("mainPlot", "xLab")
      ),
      riskgrob,
      textGrob(
        levels(group),
        x = unit(-nlines_labels + 1, "lines"),
        y = unit(1:nlevels(group) / (nlevels(group) + 1), "npc"),
        just = "left",
        gp = gpar(col = col),
        vp = vpPath("mainPlot", "riskTable", "labelPlot")),
      textGrob(
        label = "Number of Patients at Risk",
        x = unit(0, "npc"),
        y = unit(1, "npc") + unit(1, "lines"),
        just = "left", vp = vpPath("mainPlot", "riskTable", "labelPlot")
      ),
      xaxisGrob(at = unique(x), vp = vpPath("mainPlot", "riskTable", "labelPlot"))
    ),
    cl = "kmGrob"
  )
  if (draw) {
    if (newpage) {
      grid.newpage()
    }
    grid.draw(km_grob)
  }

  invisible(km_grob)
}



#' Define a kmplot grob
#'
#' create a grid graphical object for basic KM plot from a \code{\link{survfit}} object.
#'
#' @param kmdata a class \code{\link{km_curve_data}} object.
#' @param fit_km a class \code{\link{survfit}} object.
#' @param xticks break interval of x-axis. It takes a numeric vector or \code{NULL}.
#' @param col line color.
#' @param lty line type.
#' @param lwd line width.
#' @param censor_show \code{TRUE/FALSE} to show censored.
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
#'
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ 1, data = OS)
#' kmdata <- tern:::km_curve_data(fit_km, xticks = c(0.5, 1.0, 2.0))
#' kmgrob <- tern:::km_curve_grob(kmdata, col = "blue")
#' grid.newpage()
#' pushViewport(plotViewport())
#' grid.draw(kmgrob)
#'
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = OS, conf.type = "plain")
#' kmgrob <- tern:::km_curve_grob(fit_km = fit_km, xticks = c(0.5, 0.8, 1.5))
#' grid.newpage()
#' pushViewport(plotViewport())
#' grid.draw(kmgrob)
km_curve_grob <- function(kmdata,
                          fit_km,
                          xticks = NULL,
                          col = NA,
                          lty = 1,
                          lwd = 1,
                          censor_show = TRUE,
                          pch = 3,
                          size = unit(0.5, "char"),
                          gp = NULL,
                          vp = NULL,
                          name = NULL) {
  stopifnot(
    missing(kmdata) || is(kmdata, "km_curve_data"),
    missing(fit_km) || is(fit_km, "survfit"),
    !missing(kmdata) || !missing(fit_km)
  )

  if (missing(kmdata)) {
    kmdata <- km_curve_data(fit_km = fit_km, xticks = xticks)
  }

  ngroup <- length(kmdata$group)

  lwd <- to_n(lwd, ngroup)
  lty <- to_n(lty, ngroup)

  col <- to_group_color(col, names(kmdata$group))

  lines <- Map(function(x, y, col_i, lty_i, lwd_i) {
    linesGrob(
      x = x,
      y = y,
      default.units = "native",
      gp = gpar(col = col_i, lwd = lwd_i, lty = lty_i),
      vp =  "curvePlot"
    )
  }, kmdata$lines_x, kmdata$lines_y, col, lty, lwd)


  if (censor_show) {
    pch <- to_n(pch, ngroup)
    size <- list(to_n(size, ngroup))
    points <- Map(function(x, y, col_i, pch_i, size_i) {
      pointsGrob(
        x = x,
        y = y,
        pch = pch_i,
        size = size_i,
        default.units = "native",
        gp = gpar(col = col_i),
        vp =  "curvePlot"
      )
    }, kmdata$points_x, kmdata$points_y, col, pch, size)

  }

  gTree(
    kmdata = kmdata,
    vp = vp,
    name = name,
    gp = gp,
    childrenvp = dataViewport(xData = kmdata$xpos, yData = c(0, 1), name = "curvePlot"),
    children = do.call(
      "gList",
      c(
        list(
          xaxisGrob(
            at = kmdata$xpos,
            vp = "curvePlot"
          ),
          yaxisGrob(vp = "curvePlot"),
          rectGrob(vp =  "curvePlot")
        ),
        lines,
        if (censor_show) points
      )
    ),
    cl = "km_curve_grob"
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
#' OS <- data.frame(AVAL = abs(rnorm(200)),
#'                  CNSR = sample(c(0, 1), 200, TRUE),
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#'
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = OS, conf.type = "plain")
#'
#' tern:::km_curve_data(fit_km, xticks = c(0.5, 0.8, 1.5))
km_curve_data <- function(fit_km, xticks = NULL) {
  stopifnot(is(fit_km, "survfit"))

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
  if (length(xticks) <= 1) {
    xpos <- seq(0, floor(max(df$time)), by = ifelse(is.null(xticks),
                                                    max(1, floor(max(df$time) / 10)),
                                                    xticks))
  } else {
    xpos <-  c(0, xticks)
  }

  pt_risk <- lapply(df_s, function(x) {
    vapply(xpos, function(xi) {
      if (xi <= min(x$time)) {
        i <- head(which(x$time >= xi), 1)
        x$n.risk[i]
      } else if (xi > max(x$time)) {
        NA
      } else {
        i <- tail(which(x$time <= xi), 1)
        x$n.risk[i] - x$n.censor[i] - x$n.event[i]
      }
    }, numeric(1))
  })


  lines_x <- lapply(df_s, function(x) {
    c(0, rep(x$time, each = 2))
  })
  lines_y <- lapply(df_s, function(x) {
    c(rep(c(1, head(x$surv, -1)), each = 2), tail(x$surv, 1))
  })

  points_x <- lapply(df_s, function(x) {
    x[x$n.censor != 0, "time"]
  })

  points_y <- lapply(df_s, function(x) {
    x[x$n.censor != 0, "surv"]
  })

  structure(list(xpos = xpos,
                 lines_x = lines_x,
                 lines_y = lines_y,
                 points_x = points_x,
                 points_y = points_y,
                 group = group,
                 pt_risk = pt_risk),
            class = "km_curve_data")
}
