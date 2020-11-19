#' Kaplan-Meier Plot
#'
#' @param data (`data.frame`)\cr survival data as pre-processed by `h_data_plot`.
#' @param fit_km (`survfit`)\cr see [survival::survfit()].
#' @param xticks (`numeric` or `number`)\cr
#'   numeric vector of ticks or single number with spacing
#'   between ticks on the x axis.
#'   If `NULL`, [labeling::extended()] is used to determine
#'   an optimal tick position on the x axis.
#' @param censor_show (`flag`)\cr whether to show censored.
#' @param xlab (`string`)\cr label of x-axis.
#' @param ylab (`string`)\cr label of y-axis.
#' @param title (`string`)\cr title for plot.
#' @param draw  (`flag`)\cr draw the plot on device.
#' @param newpage (`flag`)\cr open a new page on the graphic device.
#' @param col (`character`)\cr lines colors. Length of a vector should be equal
#'   to number of strata from [survival::survfit()].
#' @param lty (`numeric`)\cr line type. Length of a vector should be equal
#'   to number of strata from [survival::survfit()].
#' @param lwd (`numeric`)\cr line width. Length of a vector should be equal
#'   to number of strata from [survival::survfit()].
#' @param pch (`numeric`)\cr point type for censored.
#' @param size (`numeric`)\cr size of censored point, a class of `unit`.
#' @param max_time (`numeric`)\cr maximum value of X axis range (`NULL` for
#'    default)
#' @param ggtheme (`theme`)\cr a graphical theme as provided by `ggplot2` to
#'   control outlook of the Kaplan-Meier curve.
#' @param annot_at_risk (`flag`)\cr compute and add the annotation table
#'   reporting the number of patient at risk matching the main grid of the
#'   Kaplan-Meier curve.
#' @param annot_surv_med (`flag`)\cr compute and add the annotation table
#'   on the top right corner of the Kaplan-Meier curve estimating the
#'   median survival time per group.
#'
#' @name kaplan_meier
#'
NULL

#' Kaplan-Meier Plot
#'
#' From a survival model, a graphic is rendered along with tabulated annotation
#' including the number of patient at risk at given time and the median survival
#' per group.
#'
#' @inheritParams grid::gTree
#' @inheritParams kaplan_meier
#' @export
#' @examples
#'
#' library(random.cdisc.data)
#' library(dplyr)
#' library(survival)
#' library(grid)
#'
#' fit_km <- radtte(cached = TRUE) %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#'
#' # 1. Example - basic option
#'
#' res <- g_km(fit_km = fit_km)
#' res <- g_km(fit_km = fit_km, col = c("grey25", "grey50", "grey75"))
#' res <- g_km(fit_km = fit_km, ggtheme = ggplot2::theme_minimal())
#' res <- g_km(fit_km = fit_km, ggtheme = ggplot2::theme_minimal(), lty = 1:3)
#' res <- g_km(fit_km = fit_km, max = 2000)
#'
#' # 2. Example - Arrange several KM curve on a single graph device
#'
#' # 2.1 Use case: A general graph on the top, a zoom on the bottom.
#' grid::grid.newpage()
#' lyt <- grid.layout(nrow = 2, ncol = 1) %>%
#'   grid::viewport(layout = .) %>%
#'   pushViewport()
#'
#' res <- g_km(
#'   fit_km = fit_km, newpage = FALSE, annot_surv_med = FALSE,
#'   vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1)
#' )
#' res <- g_km(
#'   fit_km = fit_km, max = 1000, newpage = FALSE, annot_surv_med = FALSE,
#'   ggtheme = ggplot2::theme_dark(),
#'   vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1)
#' )
#'
#' # 2.1 Use case: No annotations on top, annotated graph on bottom
#' grid::grid.newpage()
#' lyt <- grid.layout(nrow = 2, ncol = 1) %>%
#'   grid::viewport(layout = .) %>%
#'   pushViewport()
#'
#' res <- g_km(
#'   fit_km = fit_km, newpage = FALSE,
#'   annot_surv_med = FALSE, annot_at_risk = FALSE,
#'   vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1)
#' )
#' res <- g_km(
#'   fit_km = fit_km, max = 2000, newpage = FALSE, annot_surv_med = FALSE,
#'   annot_at_risk = TRUE,
#'   ggtheme = ggplot2::theme_dark(),
#'   vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1)
#' )
#'
g_km <- function(fit_km,
                 xticks = NULL,
                 col = NULL,
                 lty = NULL,
                 lwd = .5,
                 censor_show = TRUE,
                 pch = "|",
                 size = 3,
                 max_time = NULL,
                 xlab = "Days",
                 ylab = "Survival Probability",
                 title = "Kaplan-Meier Plot",
                 draw = TRUE,
                 newpage = TRUE,
                 gp = NULL,
                 vp = NULL,
                 name = NULL,
                 # New arguments:
                 ggtheme = NULL,
                 annot_at_risk = TRUE,
                 annot_surv_med = TRUE) {

  data_plot <- h_data_plot(
    fit_km = fit_km,
    xticks = xticks,
    max_time = max_time
  )
  xticks <- h_xticks(data = data_plot, xticks = xticks)
  gg <- h_ggkm(
    data = data_plot,
    censor_show = censor_show,
    xticks = xticks,
    xlab = xlab,
    ylab = ylab,
    title = title,
    lwd = lwd,
    lty = lty,
    col = col,
    ggtheme = ggtheme
  )

  if (annot_at_risk) {
    g_el <- h_decompose_gg(gg) # nolint
    # This is the content of the table that will be below the graph.
    annot_tbl <- summary(fit_km, time = xticks)
    annot_tbl <- if (is.null(fit_km$strata)) {
      data.frame(
        n.risk = annot_tbl$n.risk,
        time = annot_tbl$time,
        strata = "Data"
      )
    } else {
      data.frame(
        n.risk = annot_tbl$n.risk,
        time = annot_tbl$time,
        strata = annot_tbl$strata
      )
    }

    grobs_patient <- h_grob_tbl_at_risk(data = data_plot, annot_tbl = annot_tbl)

    lyt <- h_km_layout(data = data_plot, g_el = g_el)

    km_grob <- grid::gTree(
      vp = grid::viewport(layout = lyt, height = .95, width = .95),
      children = grid::gList(

        # The Kaplan - Meier curve (top-right corner).
        grid::gTree(
          vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2),
          children = grid::gList(g_el$panel)
        ),

        # Survfit summary table (top-right corner).
        if (annot_surv_med) {
          grid::gTree(
            vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2),
            children = h_grob_median_surv(fit_km = fit_km)
          )
        },

        # Add the y-axis annotation (top-left corner).
        grid::gTree(
          vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1),
          children = h_grob_y_annot(ylab = g_el$ylab, yaxis = g_el$yaxis)
        ),

        # Add the x-axis annotation (second row below the Kaplan Meier Curve).
        grid::gTree(
          vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2),
          children = grid::gList(rbind(g_el$xaxis, g_el$xlab))
        ),

        # Add the legend.
        grid::gTree(
          vp = grid::viewport(layout.pos.row = 3, layout.pos.col = 2),
          children = grid::gList(g_el$guide)
        ),

        # Add the table with patient-at-risk numbers.
        grid::gTree(
          vp = grid::viewport(layout.pos.row = 4, layout.pos.col = 2),
          children = grobs_patient$at_risk
        ),

        # Add the table with patient-at-risk labels.
        grid::gTree(
          vp = grid::viewport(layout.pos.row = 4, layout.pos.col = 1),
          children = grobs_patient$label
        ),

        # Add the x-axis for the table.
        grid::gTree(
          vp = grid::viewport(layout.pos.row = 5, layout.pos.col = 2),
          children = grid::gList(rbind(g_el$xaxis, g_el$xlab))
        )
      )
    )

    result <- grid::gTree(
      vp = vp,
      gp = gp,
      name = name,
      children = grid::gList(km_grob)
    )

  } else {
    result <- grid::gTree(
      vp = vp,
      gp = gp,
      name = name,
      children = grid::gList(ggplot2::ggplotGrob(gg))
    )
    lyt <- NULL
  }

  if (newpage & draw) grid::grid.newpage()
  if (draw) grid::grid.draw(result)

  return(
    list(
      lyt = lyt,
      result = result
    )
  )
}

#' Helper function: tidy survival fit
#'
#' Convert the survival fit data into a data frame designed for plotting
#' within `g_km`.
#'
#' @inheritParams kaplan_meier
#' @examples vignette("Design of KMG01")
#'
#' \dontrun{
#' library(random.cdisc.data)
#' library(dplyr)
#' library(survival)
#' radtte(cached = TRUE) %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
#'   tern:::h_data_plot()
#' }
#'
h_data_plot <- function(fit_km,
                        xticks = NULL,
                        max_time = NULL) {
  y <- broom::tidy(fit_km)
  y$censor <- ifelse(y$n.censor > 0, y$estimate, NA)
  if (!is.null(xticks)) {
    y <- y[y$time <= max(xticks), ]
  }
  if (!is.null(max_time)) {
    y <- y[y$time <= max(max_time), ]
  }
  if (is.null(fit_km$strata)) {
    y$strata <- "Data"
  }
  y
}

#' Helper function: x tick positions
#'
#' Calculate the positions of ticks on the x-axis. However, if `xticks` already
#' exists it is kept as is. It is based on the same function `ggplot2` relies on,
#' and is required in the graphic and the patient-at-risk annotation table.
#'
#' @inheritParams kaplan_meier
#'
#' @examples
#'
#' \dontrun{
#' library(random.cdisc.data)
#' library(dplyr)
#' library(survival)
#' data <- radtte(cached = TRUE) %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
#'   h_data_plot()
#'
#' h_xticks(data)
#' h_xticks(data, xticks = seq(0, 3000, 500))
#' h_xticks(data, xticks = 500)
#' }
#'
h_xticks <- function(data, xticks = NULL) {
  if (is.null(xticks)) {
    labeling::extended(range(data$time)[1], range(data$time)[2], m = 5)
  } else if (is.number(xticks)) {
    seq(0, max(data$time), xticks)
  } else if (is.numeric(xticks)) {
    xticks
  } else {
    stop(
      paste(
        "xticks should be either `NULL`",
        "or a single number (interval between x ticks)",
        "or a numeric vector (position of ticks on the x axis)"
      )
    )
  }
}

#' Helper function: KM plot
#'
#' Draw the Kaplan-Meier plot using `ggplot2`.
#'
#' @inheritParams kaplan_meier
#' @examples
#' \dontrun{
#'
#' library(random.cdisc.data)
#' library(dplyr)
#' library(survival)
#' fit_km <- radtte(cached = TRUE) %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#' data_plot <- h_data_plot(fit_km = fit_km)
#' xticks <- h_xticks(data = data_plot)
#' gg <- h_ggkm(
#'   data = data_plot,
#'   censor_show = TRUE,
#'   xticks = xticks,
#'   xlab = "Days",
#'   ylab = "Survival Probability",
#'   title = "Survival"
#' )
#'
#' gg
#' }
#'
h_ggkm <- function(data,
                   xticks = NULL,
                   censor_show,
                   xlab,
                   ylab,
                   title,
                   lwd = 1,
                   lty = NULL,
                   pch = "|",
                   size = 3,
                   col = NULL,
                   ggtheme = NULL) {

  assert_that(
    (is.null(lty) || is.number(lty) || is.numeric(lty))
  )

  gg <- {
    ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes_string(
        x = "time",
        y = "estimate",
        ymin = "conf.low",
        ymax = "conf.high",
        color = "strata",
        fill = "strata"
      )
    ) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_ribbon(alpha = .3, lty = 0)
  }

  gg <- if (is.null(lty)) {
    gg +
      ggplot2::geom_step(lwd = lwd)
  } else if (is.number(lty)){
    gg +
      ggplot2::geom_step(lwd = lwd, lty = lty)
  } else if (is.numeric(lty)) {
    gg +
      ggplot2::geom_step(mapping = aes_string(linetype = "strata"), lwd = lwd) +
      ggplot2::scale_linetype_manual(values = lty)
  }

  gg <- gg +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = xlab, y = ylab, title = title)

  if (!is.null(col)) {
    gg <- gg +
      ggplot2::scale_color_manual(values = col) +
      ggplot2::scale_fill_manual(values = col)
  }
  if (censor_show) {
    gg <- gg +
      ggplot2::geom_point(
        data = data[!is.na(data$censor), ],
        ggplot2::aes_string(x = "time", y = "censor"),
        shape = pch, size = size
      )
  }
  if (!is.null(xticks)) {
    gg <- gg + ggplot2::scale_x_continuous(breaks = xticks)
  }

  if (!is.null(ggtheme)) {
    gg <- gg + ggtheme
  }
  gg + ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(size = 2)
  )
}


#' `ggplot` Decomposition
#'
#' The elements composing the `ggplot` are extracted and organized in a
#' list containing:
#' the panel (`panel`),
#' the y-axis and its label (`yaxis`, `ylab`),
#' idem for the x-axis (`xaxis`, `xlab`),
#' the legend (`guide`).
#'
#' @param gg (`ggplot`)\cr a graphic to decompose.
#' @examples
#'
#' \dontrun{
#' library(random.cdisc.data)
#' library(dplyr)
#' library(survival)
#' library(grid)
#'
#' fit_km <- radtte(cached = TRUE) %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#' data_plot <- h_data_plot(fit_km = fit_km)
#' xticks <- h_xticks(data = data_plot)
#' gg <- h_ggkm(
#'   data = data_plot,
#'   censor_show = TRUE,
#'   xticks = xticks, xlab = "Days", ylab = "Survival Probability",
#'   title = "tt"
#' )
#'
#' g_el <- h_decompose_gg(gg)
#' grid.newpage()
#' grid.rect(gp = gpar(lty = 1, col = "red", fill = "gray85", lwd = 5))
#' grid.draw(g_el$panel)
#'
#' grid.newpage()
#' grid.rect(gp = gpar(lty = 1, col = "royalblue", fill = "gray85", lwd = 5))
#' grid.draw(with(g_el, cbind(ylab, yaxis)))
#' }
#'
h_decompose_gg <- function(gg) {
  g_el <- ggplot2::ggplotGrob(gg)
  y <- c(
    panel = "panel",
    yaxis = "axis-l",
    xaxis = "axis-b",
    xlab = "xlab-b",
    ylab = "ylab-l",
    guide = "guide"
  )
  lapply(X = y, function(x) gtable::gtable_filter(g_el, x))
}


#' Helper: KM Layout
#'
#' Prepares a (5 rows) x (2 cols) layout for the Kaplan-Meier curve.
#'
#' @inheritParams kaplan_meier
#' @param g_el (`list` of `gtable`)\cr list as obtained by `h_decompose_gg()`.
#'
#' @details
#' The layout corresponds to a grid of two columns and five rows of unequal
#' dimensions. Most of the dimension are fixed, only the curve is flexible and
#' will accommodate with the remaining free space.
#' - The left column gets the annotation of the ggplot (y-axis) and the
#'   names of the strata for the patient at risk tabulation.
#'   The main constraint is about the width of the columns which must allow the
#'   writing of the strata name.
#' - The right column receive the ggplot, the legend, the x-axis and the
#' patient at risk table.
#'
#' @examples
#' \dontrun{
#' fit_km <- radtte(cached = TRUE) %>%
#' filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#' data_plot <- h_data_plot(fit_km = fit_km)
#' xticks <- h_xticks(data = data_plot)
#' gg <- h_ggkm(
#'   data = data_plot,
#'   censor_show = TRUE,
#'   xticks = xticks, xlab = "Days", ylab = "Survival Probability",
#'   title = "tt"
#' )
#' g_el <- h_decompose_gg(gg)
#' lyt <- h_km_layout(data = data_plot, g_el = g_el)
#' grid.show.layout(lyt)
#' }
#'
h_km_layout <- function(data, g_el) {
  txtlines <- levels(as.factor(data$strata))
  nlines <- nlevels(as.factor(data$strata))
  col_annot_width <- max(
    c(
      as.numeric(grid::convertX(g_el$yaxis$width + g_el$ylab$width, "pt")),
      as.numeric(
        grid::convertX(
          grid::stringWidth(txtlines) + grid::unit(7, "pt"), "pt"
        )
      )
    )
  )
  grid::grid.layout(
    nrow = 5, ncol = 2,
    widths = grid::unit(c(col_annot_width, 1), c("pt", "null")),
    heights = grid::unit(
      c(
        1,
        grid::convertX(with(g_el, xaxis$height + ylab$width), "pt"),
        grid::convertX(g_el$guide$heights, "pt"),
        nlines + 1,
        grid::convertX(with(g_el, xaxis$height + ylab$width), "pt")
      ),
      c(
        "null",
        "pt",
        "pt",
        "lines",
        "pt"
      )
    )
  )
}

#' Helper: Patient-at-Risk Grobs
#'
#' Two Graphical Objects are obtained, one corresponding to row labeling and
#' the second to the number of patient at risk.
#'
#' @inheritParams kaplan_meier
#' @param annot_tbl (`data.frame`)\cr annotation as prepared
#'   by [survival::summary.survfit()] which includes the number of
#'   patients at risk at given time points.
#'
#' @examples
#'
#' \dontrun{
#' fit_km <- radtte(cached = TRUE) %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#'
#' data_plot <- h_data_plot(fit_km = fit_km)
#'
#' xticks <- h_xticks(data = data_plot)
#'
#' gg <- h_ggkm(
#'   data = data_plot,
#'   censor_show = TRUE,
#'   xticks = xticks, xlab = "Days", ylab = "Survival Probability",
#'   title = "tt"
#' )
#'
#' # The annotation table reports the patient at risk for a given strata and
#' # time (`xticks`).
#' annot_tbl <- summary(fit_km, time = xticks)
#' annot_tbl <- if (is.null(fit_km$strata)) {
#'   with(annot_tbl, data.frame(n.risk = n.risk, time = time, strata = "Data"))
#' } else {
#'   with(annot_tbl, data.frame(n.risk = n.risk, time = time, strata = strata))
#' }
#'
#' # The annotation table is transformed into a grob.
#' tbl <- h_grob_tbl_at_risk(data = data_plot, annot_tbl = annot_tbl)
#'
#' # For the representation, the layout is estimated for which the decomposition
#' # of the graphic element is necessary.
#' g_el <- h_decompose_gg(gg)
#' lyt <- h_km_layout(data = data_plot, g_el = g_el)
#'
#' grid.newpage()
#' pushViewport(viewport(layout = lyt, height = .95, width = .95))
#' grid.rect(gp = gpar(lty = 1, col = "purple", fill = "gray85", lwd = 1))
#' pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 2))
#' grid.rect(gp = gpar(lty = 1, col = "orange", fill = "gray85", lwd = 1))
#' grid.draw(tbl$at_risk)
#' popViewport()
#' pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 1))
#' grid.rect(gp = gpar(lty = 1, col = "green3", fill = "gray85", lwd = 1))
#' grid.draw(tbl$label)
#' }
#'
h_grob_tbl_at_risk <- function(data, annot_tbl) {
  txtlines <- levels(as.factor(data$strata))
  nlines <- nlevels(as.factor(data$strata))
  vp_table <- grid::plotViewport(margins = grid::unit(c(0, 0, 0, 0), "lines"))
  gb_table_left_annot <- grid::gList(
    grid::rectGrob(
      x = 0, y = grid::unit(c(1:nlines) - 1, "lines"),
      gp = grid::gpar(fill = c("gray95", "gray90"), alpha = 1, col = "white"),
      height = grid::unit(1, "lines"), just = "bottom", hjust = 0
    ),
    grid::textGrob(
      label = unique(annot_tbl$strata),
      x = .95,
      y = grid::unit(unique(as.numeric(annot_tbl$strata)) - .5, "native"),
      hjust = 1,
      gp = grid::gpar(fontface = "italic", fontsize = 10)
    )
  )
  gb_patient_at_risk <- grid::gList(
    grid::rectGrob(
      x = 0, y = grid::unit(c(1:nlines) - 1, "lines"),
      gp = grid::gpar(fill = c("gray95", "gray90"), alpha = 1, col = "white"),
      height = grid::unit(1, "lines"), just = "bottom", hjust = 0
    ),
    grid::textGrob(
      label = annot_tbl$n.risk,
      x = grid::unit(annot_tbl$time, "native"),
      y = grid::unit(as.numeric(annot_tbl$strata) - .5, "line") # maybe native
    )
  )
  list(
    at_risk = grid::gList(
      grid::gTree(
        vp = vp_table,
        children = grid::gList(
          grid::gTree(
            vp = grid::dataViewport(
              xData = data$time,
              yscale = c(0, nlines + 1),
              extension = c(0.05, 0)
            ),
            children = grid::gList(gb_patient_at_risk)
          )
        )
      )
    ),
    label = grid::gList(
      grid::gTree(
        vp = grid::viewport(width = max(grid::stringWidth(txtlines))),
        children = grid::gList(
          grid::gTree(
            vp = grid::dataViewport(
              xscale = 0:1,
              yscale = c(0, nlines + 1),
              extension = c(0.0, 0)
            ),
            children = grid::gList(gb_table_left_annot)
          )
        )
      )
    )
  )
}

#' Helper Function: Survival Estimations
#'
#' Transform a survival fit to a table with groups in rows characterized
#' by N, median and 95% confidence interval.
#'
#' @inheritParams kaplan_meier
#' @examples
#' \dontrun{
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' adtte <- radtte(cached = TRUE) %>%
#'   filter(PARAMCD == "OS")
#'
#' fit <- survival::survfit(
#'   form = survival::Surv(AVAL, 1 - CNSR) ~ ARMCD,
#'   data = adtte
#' )
#' h_tbl_median_surv(fit_km = fit)
#' }
#'
h_tbl_median_surv <- function(fit_km) {
  y <- if (is.null(fit_km$strata)) {
    as.data.frame(t(summary(fit_km)$table), row.names = "Data")
  } else {
    as.data.frame(summary(fit_km)$table)
  }
  y$records <- round(y$records)
  y$median <- signif(y$median, 4)
  y$`CI` <- paste0( # nolint
    "(", signif(y$`0.95LCL`, 4), ", ", signif(y$`0.95UCL`, 4), ")"
  )
  setNames(
    y[c("records", "median", "CI")],
    c("N", "Median", "95% CI")
  )
}


#' Helper Function: Survival Estimation Grob
#'
#' The survival fit is transformed in a grob containing a table with groups in
#' rows characterized by N, median and 95% confidence interval.
#'
#' @inheritParams kaplan_meier
#' @param ttheme (`list`)\cr see [gridExtra::ttheme_default()].
#'
#' @examples
#' \dontrun{
#' library(random.cdisc.data)
#' library(dplyr)
#' library(survival)
#'
#' grid.newpage()
#' grid.rect(gp = gpar(lty = 1, col = "pink", fill = "gray85", lwd = 1))
#' radtte(cached = TRUE) %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
#'   h_grob_median_surv %>%
#'   grid.draw
#' }
#'
h_grob_median_surv <- function(fit_km,
                               ttheme = gridExtra::ttheme_default()) {
  data <- h_tbl_median_surv(fit_km) # nolint
  gt <- gridExtra::tableGrob(d = data, theme = ttheme)
  vp <- grid::viewport(
    x = grid::unit(1, "npc"),
    y = grid::unit(1, "npc"),
    height = sum(gt$heights),
    width = sum(gt$widths),
    just = c("right", "top")
  )

  grid::gList(
    grid::gTree(
      vp = vp,
      children = grid::gList(gt)
    )
  )
}

#' Helper: Grid Object with y-axis Annotation
#'
#' Build the y-axis annotation from a decomposed `ggplot`.
#'
#' @param ylab (`gtable`)\cr the y-lab as a graphical object derived from
#'   a `ggplot`.
#' @param yaxis (`gtable`)\cr the y-axis as a graphical object derived from
#'   a `ggplot`.
#'
#' @examples
#' \dontrun{
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' fit_km <- radtte(cached = TRUE) %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#' data_plot <- h_data_plot(fit_km = fit_km)
#' xticks <- h_xticks(data = data_plot)
#' gg <- h_ggkm(
#'   data = data_plot,
#'   censor_show = TRUE,
#'   xticks = xticks, xlab = "Days", ylab = "Survival Probability",
#'   title = "title"
#' )
#'
#' g_el <- h_decompose_gg(gg)
#'
#' grid.newpage()
#' pvp <- plotViewport(margins = c(5, 4, 2, 20))
#' pushViewport(pvp)
#' grid.draw(h_grob_y_annot(ylab = g_el$ylab, yaxis = g_el$yaxis))
#' grid.rect(gp = gpar(lty = 1, col = "gray35", fill = NA))
#' }
#'
h_grob_y_annot <- function(ylab, yaxis) {
  grid::gList(
    grid::gTree(
      vp = grid::viewport(
        width = grid::convertX(yaxis$width + ylab$width, "pt"),
        x = grid::unit(1, "npc"),
        just = "right"
      ),
      children = grid::gList(cbind(ylab, yaxis))
    )
  )
}
