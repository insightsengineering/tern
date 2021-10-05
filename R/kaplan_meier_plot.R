#' Kaplan-Meier Plot
#'
#'
#' @param df (`data frame`)\cr data set containing all analysis variables.
#' @param variables (named `list`) of variable names. Details are: \cr
#' * `tte`: variable indicating time-to-event duration values (`numeric`).
#' * `is_event`: event variable (`logical`) \cr `TRUE` if event, `FALSE` if time to event is censored.
#' * `arm`: the treatment group variable (`factor`).
#' * `strat`: (`character` or `NULL`) variable names indicating stratification factors.
#' @param control_surv a (`list`) of parameters for comparison details, specified by using \cr
#'    the helper function [control_surv_timepoint]. Some possible parameter options are: \cr
#' * `conf_level`: (`proportion`)\cr confidence level of the interval for survival rate.
#' * `conf_type`: (`string`) \cr "plain" (default), "log", "log-log" for confidence interval type, \cr
#'    see more in [survival::survfit()]. Note that the option "none" is no longer supported.
#' @param data (`data.frame`)\cr survival data as pre-processed by `h_data_plot`.
#' @param xticks (`numeric`, `number`, or `NULL`)\cr
#'   numeric vector of ticks or single number with spacing
#'   between ticks on the x axis.
#'   If `NULL` (default), [labeling::extended()] is used to determine
#'   an optimal tick position on the x axis.
#' @param yval (`string`)\cr value of y-axis. Should be either
#'   `Survival` (default) or `Failure` probability.
#' @param censor_show (`flag`)\cr whether to show censored.
#' @param xlab (`string`)\cr label of x-axis.
#' @param ylab (`string`)\cr label of y-axis.
#' @param title (`string`)\cr title for plot.
#' @param col (`character`)\cr lines colors. Length of a vector should be equal
#'   to number of strata from [survival::survfit()].
#' @param lty (`numeric`)\cr line type. Length of a vector should be equal
#'   to number of strata from [survival::survfit()].
#' @param lwd (`numeric`)\cr line width. Length of a vector should be equal
#'   to number of strata from [survival::survfit()].
#' @param pch (`numeric`, `string`)\cr value or character of points symbol to indicate censored cases.
#' @param size (`numeric`)\cr size of censored point, a class of `unit`.
#' @param max_time (`numeric`)\cr maximum value to show on X axis.
#' Only data values less than or up to to this threshold value will be plotted.(`NULL` for
#'    default)
#' @param font_size (`number`) \cr font size to be used.
#' @param ci_ribbon (`flag`)\cr draw the confidence interval around the Kaplan-Meier curve.
#' @param ggtheme (`theme`)\cr a graphical theme as provided by `ggplot2` to
#'   control outlook of the Kaplan-Meier curve.
#' @param annot_at_risk (`flag`)\cr compute and add the annotation table
#'   reporting the number of patient at risk matching the main grid of the
#'   Kaplan-Meier curve.
#' @param annot_surv_med (`flag`)\cr compute and add the annotation table
#'   on the Kaplan-Meier curve estimating the median survival time per group.
#' @param annot_coxph (`flag`)\cr add the annotation table from a [survival::coxph()] model.
#' @param control_coxph_pw (`list`) \cr parameters for comparison details, specified by using \cr
#'    the helper function [control_coxph()]. Some possible parameter options are: \cr
#' * `pval_method`: (`string`) \cr p-value method for testing hazard ratio = 1.
#'   Default method is "log-rank", can also be set to "wald" or "likelihood".
#' * `ties`: (`string`) \cr specifying the method for tie handling. Default is "efron",
#'   can also be set to "breslow" or "exact". See more in [survival::coxph()]
#' * `conf_level`: (`proportion`)\cr confidence level of the interval for HR.
#' @param position_coxph `numeric` \cr x and y positions for plotting [survival::coxph()] model.
#' @param position_surv_med `numeric` \cr x and y positions for plotting annotation table
#'    estimating median survival time per group
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
#' @return a `grob` of class `gTree`.
#'
#' @inheritParams grid::gTree
#' @inheritParams kaplan_meier
#' @inheritParams argument_convention
#'
#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid gList gTree grid.draw grid.newpage textGrob unit viewport
#' @importFrom gridExtra ttheme_default
#' @importFrom survival Surv survfit
#' @importFrom stats as.formula
#'
#' @export
#'
#' @examples
#'\dontrun{
#' library(scda)
#' library(dplyr)
#' library(ggplot2)
#' library(survival)
#' library(grid)
#'
#' df <- synthetic_cdisc_data("latest")$adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(is_event = CNSR == 0)
#' variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")
#' # 1. Example - basic option
#'
#' res <- g_km(df = df, variables = variables)
#' res <- g_km(df = df, variables = variables, yval = "Failure")
#' res <- g_km(
#'   df = df,
#'   variables = variables,
#'   control_surv = control_surv_timepoint(conf_level = 0.9),
#'   col = c("grey25", "grey50", "grey75")
#' )
#' res <- g_km(df = df, variables = variables, ggtheme = theme_minimal())
#' res <- g_km(df = df, variables = variables, ggtheme = theme_minimal(), lty = 1:3)
#' res <- g_km(df = df, variables = variables, max = 2000)
#'
#' # 2. Example - Arrange several KM curve on a single graph device
#'
#' # 2.1 Use case: A general graph on the top, a zoom on the bottom.
#' grid.newpage()
#' lyt <- grid.layout(nrow = 2, ncol = 1) %>%
#'   viewport(layout = .) %>%
#'   pushViewport()
#'
#' res <- g_km(
#'   df = df, variables = variables, newpage = FALSE, annot_surv_med = FALSE,
#'   vp = viewport(layout.pos.row = 1, layout.pos.col = 1)
#' )
#' res <- g_km(
#'   df = df, variables = variables, max = 1000, newpage = FALSE, annot_surv_med = FALSE,
#'   ggtheme = theme_dark(),
#'   vp = viewport(layout.pos.row = 2, layout.pos.col = 1)
#' )
#'
#' # 2.1 Use case: No annotations on top, annotated graph on bottom
#' grid.newpage()
#' lyt <- grid.layout(nrow = 2, ncol = 1) %>%
#'   viewport(layout = .) %>%
#'   pushViewport()
#'
#' res <- g_km(
#'   df = df, variables = variables, newpage = FALSE,
#'   annot_surv_med = FALSE, annot_at_risk = FALSE,
#'   vp = viewport(layout.pos.row = 1, layout.pos.col = 1)
#' )
#' res <- g_km(
#'   df = df, variables = variables, max = 2000, newpage = FALSE, annot_surv_med = FALSE,
#'   annot_at_risk = TRUE,
#'   ggtheme = theme_dark(),
#'   vp = viewport(layout.pos.row = 2, layout.pos.col = 1)
#' )
#'
#' # Add annotation from a pairwise coxph analysis
#' g_km(
#'   df = df, variables = variables,
#'   annot_coxph = TRUE
#' )
#'
#' g_km(
#'   df = df, variables = c(variables, list(strat = "SEX")),
#'   font_size = 15,
#'   annot_coxph = TRUE,
#'   control_coxph = control_coxph(pval_method = "wald", ties = "exact", conf_level = 0.99),
#'   position_coxph = c(0.4, 0.5)
#' )
#'
#' # Change position of the treatment group annotation table.
#' g_km(
#'   df = df, variables = c(variables, list(strat = "SEX")),
#'   font_size = 15,
#'   annot_coxph = TRUE,
#'   control_coxph = control_coxph(pval_method = "wald", ties = "exact", conf_level = 0.99),
#'   position_surv_med = c(1, 0.7)
#' )
#'}
g_km <- function(df,
                 variables,
                 control_surv = control_surv_timepoint(),
                 col = NULL,
                 lty = NULL,
                 lwd = .5,
                 censor_show = TRUE,
                 pch = 3,
                 size = 2,
                 max_time = NULL,
                 xticks = NULL,
                 xlab = "Days",
                 yval = c("Survival", "Failure"),
                 ylab = paste(yval, "Probability"),
                 title = NULL,
                 draw = TRUE,
                 newpage = TRUE,
                 gp = NULL,
                 vp = NULL,
                 name = NULL,
                 font_size = 12,
                 ci_ribbon = FALSE,
                 ggtheme = NULL,
                 annot_at_risk = TRUE,
                 annot_surv_med = TRUE,
                 annot_coxph = FALSE,
                 control_coxph_pw = control_coxph(),
                 position_coxph = c(0, 0.05),
                 position_surv_med = c(0.9, 0.9)) {
  assert_that(
    is.list(variables),
    all(c("tte", "arm", "is_event") %in% names(variables)),
    is.string(title) || is.null(title)
  )
  tte <- variables$tte
  is_event <- variables$is_event
  arm <- variables$arm
  assert_that(
    is_df_with_variables(df, list(tte = tte, is_event = is_event, arm = arm)),
    is_numeric_vector(df[[tte]]),
    is_valid_factor(df[[arm]]),
    is_logical_vector(df[[is_event]])
  )
  armval <- as.character(unique(df[[arm]]))
  if (length(armval) > 1) {
    armval <- NULL
  }
  yval <- match.arg(yval)
  formula <- as.formula(paste0("Surv(", tte, ", ", is_event, ") ~ ", arm))
  fit_km <- survfit(
    formula = formula,
    data = df,
    conf.int = control_surv$conf_level,
    conf.type = control_surv$conf_type
  )
  data_plot <- h_data_plot(
    fit_km = fit_km,
    armval = armval,
    max_time = max_time
  )

  xticks <- h_xticks(data = data_plot, xticks = xticks, max_time = max_time)
  gg <- h_ggkm(
    data = data_plot,
    censor_show = censor_show,
    pch = pch,
    size = size,
    xticks = xticks,
    xlab = xlab,
    yval = yval,
    ylab = ylab,
    title = title,
    max_time = max_time,
    lwd = lwd,
    lty = lty,
    col = col,
    ggtheme = ggtheme,
    ci_ribbon = ci_ribbon
  )

 g_el <- h_decompose_gg(gg) # nolint

 if (annot_at_risk) {
   # This is the content of the table that will be below the graph.
   annot_tbl <- summary(fit_km, time = xticks)
   annot_tbl <- if (is.null(fit_km$strata)) {
     data.frame(
       n.risk = annot_tbl$n.risk,
       time = annot_tbl$time,
       strata = as.factor(armval)
     )
   } else {
     strata_lst <- strsplit(sub("=", "equals", levels(annot_tbl$strata)), "equals")
     levels(annot_tbl$strata) <- matrix(unlist(strata_lst), ncol = 2, byrow = TRUE)[, 2]
     data.frame(
       n.risk = annot_tbl$n.risk,
       time = annot_tbl$time,
       strata = annot_tbl$strata
     )
 }

   grobs_patient <- h_grob_tbl_at_risk(
     data = data_plot,
     annot_tbl = annot_tbl,
     xlim = max(max_time, data_plot$time, xticks)
   )

 }

 if (annot_at_risk | annot_surv_med | annot_coxph) {

   lyt <- h_km_layout(data = data_plot, g_el = g_el, title = title, annot_at_risk = annot_at_risk) # nolint
   ttl_row <- as.numeric(!is.null(title))
   km_grob <- gTree(
     vp = viewport(layout = lyt, height = .95, width = .95),
     children = gList(
       # Title.
       if (!is.null(ttl_row)) {
         gTree(
           vp = viewport(layout.pos.row = 1, layout.pos.col = 2),
           children =  gList(textGrob(label = title, x = unit(0, "npc"), hjust = 0))
         )
       },

       # The Kaplan - Meier curve (top-right corner).
       gTree(
         vp = viewport(layout.pos.row = 1 + ttl_row, layout.pos.col = 2),
         children = gList(g_el$panel)
       ),

       # Survfit summary table (top-right corner).
        if (annot_surv_med) {
          gTree(
            vp = viewport(layout.pos.row = 1 + ttl_row, layout.pos.col = 2),
            children = h_grob_median_surv(
              fit_km = fit_km,
              armval = armval,
              x = position_surv_med[1],
              y = position_surv_med[2],
              ttheme = ttheme_default(
                base_size = font_size
              )
            )
          )
        },

        if (annot_coxph) {
          gTree(
            vp = viewport(layout.pos.row = 1 + ttl_row, layout.pos.col = 2),
            children = h_grob_coxph(
              df = df,
              variables = variables,
              control_coxph_pw = control_coxph_pw,
              x = position_coxph[1],
              y = position_coxph[2],
              ttheme = ttheme_default(
                base_size = font_size,
                padding = unit(c(1, .5), "lines"),
                core = list(bg_params = list(fill = c("grey95", "grey90"), alpha = .5))
              )
            )
          )
        },

        # Add the y-axis annotation (top-left corner).
        gTree(
          vp = viewport(layout.pos.row = 1 + ttl_row, layout.pos.col = 1),
          children = h_grob_y_annot(ylab = g_el$ylab, yaxis = g_el$yaxis)
        ),

        # Add the x-axis annotation (second row below the Kaplan Meier Curve).
        gTree(
          vp = viewport(layout.pos.row = 2 + ttl_row, layout.pos.col = 2),
          children = gList(rbind(g_el$xaxis, g_el$xlab))
        ),

        # Add the legend.
        gTree(
          vp = viewport(layout.pos.row = 3 + ttl_row, layout.pos.col = 2),
          children = gList(g_el$guide)
        ),

        # Add the table with patient-at-risk numbers.
       if (annot_at_risk) {
        gTree(
          vp = viewport(layout.pos.row = 4 + ttl_row, layout.pos.col = 2),
          children = grobs_patient$at_risk
        )
       },

       if (annot_at_risk) {
        gTree(
          vp = viewport(layout.pos.row = 4 + ttl_row, layout.pos.col = 1),
          children = grobs_patient$label
        )
       },

       if (annot_at_risk) {
        # Add the x-axis for the table.
        gTree(
          vp = viewport(layout.pos.row = 5 + ttl_row, layout.pos.col = 2),
          children = gList(rbind(g_el$xaxis, g_el$xlab))
        )
       }
      )
    )

    result <- gTree(
      vp = vp,
      gp = gp,
      name = name,
      children = gList(km_grob)
    )

  } else {
    result <- gTree(
      vp = vp,
      gp = gp,
      name = name,
      children = gList(ggplot2::ggplotGrob(gg))
    )
  }

  if (newpage & draw) grid.newpage()
  if (draw) grid.draw(result)

  invisible(result)
}

#' Helper function: tidy survival fit
#'
#' Convert the survival fit data into a data frame designed for plotting
#' within `g_km`.
#'
#' This starts from the [broom::tidy()] result, and then:
#' - post-processes the `strata` column into a factor,
#' - extends each stratum by an additional first row with time 0
#' and probability 1 so that downstream plot lines start at those coordinates,
#' - adds a `censor` column,
#' - filters the rows before `max_time`.
#'
#' @inheritParams kaplan_meier
#' @param fit_km (`survfit`)\cr result of [survival::survfit()].
#' @param armval (`string`) \cr used as strata name when treatment arm
#' variable only has one level. Default is "All".
#' @importFrom broom tidy
#' @examples
#'
#' \dontrun{
#' library(scda)
#' library(dplyr)
#' library(survival)
#'
#' # Test with multiple arms
#' synthetic_cdisc_data("latest")$adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
#'   tern:::h_data_plot()
#'
#' # Test with single arm
#' synthetic_cdisc_data("latest")$adtte %>%
#'   filter(PARAMCD == "OS", ARMCD == "ARM B") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
#'   tern:::h_data_plot(armval = "ARM B")
#' }
#'
h_data_plot <- function(fit_km,
                        armval = "All",
                        max_time = NULL) {
  y <- tidy(fit_km)

  if (!is.null(fit_km$strata)) {
    fit_km_var_level <- strsplit(sub("=", "equals", names(fit_km$strata)), "equals")
    strata_levels <- vapply(fit_km_var_level, FUN = "[", FUN.VALUE = "a", i = 2)
    strata_var_level <- strsplit(sub("=", "equals", y$strata), "equals")
    y$strata <- factor(
      vapply(strata_var_level, FUN = "[", FUN.VALUE = "a", i = 2),
      levels = strata_levels
    )
  } else {
    y$strata <- armval
  }

  y_by_strata <- split(y, y$strata)
  y_by_strata_extended <- lapply(
    y_by_strata,
    FUN = function(tbl) {
      first_row <- tbl[1L, ]
      first_row$time <- 0
      # nolint start
      first_row$n.risk <- sum(first_row[, c("n.risk", "n.event", "n.censor")])
      first_row$n.event <- first_row$n.censor <- 0
      first_row$estimate <- first_row$conf.high <- first_row$conf.low <- 1
      first_row$std.error <- 0
      # nolint end
      rbind(
        first_row,
        tbl
      )
    }
  )
  y <- do.call(rbind, y_by_strata_extended)

  y$censor <- ifelse(y$n.censor > 0, y$estimate, NA)
  if (!is.null(max_time)) {
    y <- y[y$time <= max(max_time), ]
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
#' @importFrom labeling extended
#'
#' @examples
#'
#' \dontrun{
#' library(scda)
#' library(dplyr)
#' library(survival)
#'
#' data <- synthetic_cdisc_data("latest")$adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
#'   h_data_plot()
#'
#' h_xticks(data)
#' h_xticks(data, xticks = seq(0, 3000, 500))
#' h_xticks(data, xticks = 500)
#' h_xticks(data, xticks = 500, max_time = 6000)
#' h_xticks(data, xticks = c(0, 500), max_time = 300)
#' h_xticks(data, xticks = 500, max_time = 300)
#' }
#'
h_xticks <- function(data, xticks = NULL, max_time = NULL) {
  if (is.null(xticks)) {
    if (is.null(max_time)) {
      extended(range(data$time)[1], range(data$time)[2], m = 5)
    } else {
      extended(range(data$time)[1], max(range(data$time)[2], max_time), m = 5)
    }
  } else if (is.number(xticks)) {
    if (is.null(max_time)) {
      seq(0, max(data$time), xticks)
    } else {
      seq(0, max(data$time, max_time), xticks)
    }
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
#' library(scda)
#' library(dplyr)
#' library(survival)
#'
#' fit_km <- synthetic_cdisc_data("latest")$adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#' data_plot <- tern:::h_data_plot(fit_km = fit_km)
#' xticks <- tern:::h_xticks(data = data_plot)
#' gg <- tern:::h_ggkm(
#'   data = data_plot,
#'   censor_show = TRUE,
#'   xticks = xticks,
#'   xlab = "Days",
#'   yval = "Survival",
#'   ylab = "Survival Probability",
#'   title = "Survival"
#' )
#'
#' gg
#' }
#'
h_ggkm <- function(data,
                   xticks = NULL,
                   yval,
                   censor_show,
                   xlab,
                   ylab,
                   title,
                   max_time = NULL,
                   lwd = 1,
                   lty = NULL,
                   pch = 3,
                   size = 2,
                   col = NULL,
                   ci_ribbon = FALSE,
                   ggtheme = NULL) {

  assert_that(
    (is.null(lty) || is.number(lty) || is.numeric(lty))
  )

  # change estimates of survival to estimates of failure (1 - survival)
  if (yval == "Failure") {
    data$estimate <- 1 - data$estimate
    data[c("conf.high", "conf.low")] <- list(1 - data$conf.low, 1 - data$conf.high)
    data$censor <- 1 - data$censor
  }

  gg <- {
    ggplot(
      data = data,
      mapping = aes_string(
        x = "time",
        y = "estimate",
        ymin = "conf.low",
        ymax = "conf.high",
        color = "strata",
        fill = "strata"
      )
    ) +
      geom_hline(yintercept = 0)
  }

  if (ci_ribbon) {
    gg <- gg + geom_ribbon(alpha = .3, lty = 0)
  }

  gg <- if (is.null(lty)) {
    gg +
      geom_step(lwd = lwd)
  } else if (is.number(lty)) {
    gg +
      geom_step(lwd = lwd, lty = lty)
  } else if (is.numeric(lty)) {
    gg +
      geom_step(mapping = aes_string(linetype = "strata"), lwd = lwd) +
      scale_linetype_manual(values = lty)
  }

  gg <- gg +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = xlab, y = ylab, title = title)

  if (!is.null(col)) {
    gg <- gg +
      scale_color_manual(values = col) +
      scale_fill_manual(values = col)
  }
  if (censor_show) {
    dt <- data[data$n.censor != 0, ]
    dt$censor_lbl <- factor("Censored")

    gg <- gg + geom_point(
      data = dt,
      aes_string(
        x = "time",
        y = "censor",
        shape = "censor_lbl"
      ),
      size = size,
      show.legend = TRUE,
      inherit.aes = TRUE
    ) +
      scale_shape_manual(name = NULL, values = pch) +
      guides(
        shape = guide_legend(override.aes = list(linetype = NA)),
        fill = guide_legend(override.aes = list(shape = NA))
      )
  }


  if (!is.null(max_time) && !is.null(xticks)) {
    gg <- gg + scale_x_continuous(breaks = xticks, limits = c(min(0, xticks), max(c(xticks, max_time))))
  }
  else if (!is.null(xticks)) {
    if (max(data$time) <= max(xticks)) {
      gg <- gg + scale_x_continuous(breaks = xticks, limits = c(min(0, min(xticks)), max(xticks)))
    } else {
      gg <- gg + scale_x_continuous(breaks = xticks)
    }
  }
  else if (!is.null(max_time)) {
    gg <- gg + scale_x_continuous(limits = c(0, max_time))
  }


  if (!is.null(ggtheme)) {
    gg <- gg + ggtheme
  }

  gg + theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.x = element_line(size = 2)
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
#'
#' @importFrom ggplot2 ggplotGrob
#' @importFrom gtable gtable_filter
#'
#' @examples
#'
#' \dontrun{
#' library(scda)
#' library(dplyr)
#' library(survival)
#' library(grid)
#'
#' fit_km <- synthetic_cdisc_data("latest")$adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#' data_plot <- h_data_plot(fit_km = fit_km)
#' xticks <- h_xticks(data = data_plot)
#' gg <- h_ggkm(
#'   data = data_plot,
#'   yval = "Survival",
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
  g_el <- ggplotGrob(gg)
  y <- c(
    panel = "panel",
    yaxis = "axis-l",
    xaxis = "axis-b",
    xlab = "xlab-b",
    ylab = "ylab-l",
    guide = "guide"
  )
  lapply(X = y, function(x) gtable_filter(g_el, x))
}


#' Helper: KM Layout
#'
#' Prepares a (5 rows) x (2 cols) layout for the Kaplan-Meier curve.
#'
#' @inheritParams kaplan_meier
#' @param g_el (`list` of `gtable`)\cr list as obtained by `h_decompose_gg()`.
#' @param annot_at_risk (`flag`)\cr compute and add the annotation table
#'   reporting the number of patient at risk matching the main grid of the
#'   Kaplan-Meier curve.
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
#' @importFrom grid convertX grid.layout stringWidth unit
#'
#' @examples
#' \dontrun{
#' library(scda)
#' library(dplyr)
#' library(survival)
#'
#' fit_km <- synthetic_cdisc_data("latest")$adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#' data_plot <- h_data_plot(fit_km = fit_km)
#' xticks <- h_xticks(data = data_plot)
#' gg <- h_ggkm(
#'   data = data_plot,
#'   censor_show = TRUE,
#'   xticks = xticks, xlab = "Days", ylab = "Survival Probability",
#'   title = "tt", yval = "Survival"
#' )
#' g_el <- h_decompose_gg(gg)
#' lyt <- h_km_layout(data = data_plot, g_el = g_el, title = "t")
#' grid.show.layout(lyt)
#' }
#'
h_km_layout <- function(data, g_el, title, annot_at_risk) {
  txtlines <- levels(as.factor(data$strata))
  nlines <- nlevels(as.factor(data$strata))
  col_annot_width <- max(
    c(
      as.numeric(convertX(g_el$yaxis$width + g_el$ylab$width, "pt")),
      as.numeric(
        convertX(
          stringWidth(txtlines) + unit(7, "pt"), "pt"
        )
      )
    )
  )

  if (is.null(title)) {
    grid.layout(
      nrow = ifelse(annot_at_risk, 5, 4), ncol = 2,
      widths = unit(c(col_annot_width, 1), c("pt", "null")),
      heights = unit(
        c(
          1,
          convertX(with(g_el, xaxis$height + ylab$width), "pt"),
          convertX(g_el$guide$heights, "pt"),
          nlines + 1,
          convertX(with(g_el, xaxis$height + ylab$width), "pt")
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
  } else {
    grid.layout(
      nrow = ifelse(annot_at_risk, 6, 5), ncol = 2,
      widths = unit(c(col_annot_width, 1), c("pt", "null")),
      heights = unit(
        c(
          1,
          1,
          convertX(with(g_el, xaxis$height + ylab$width), "pt"),
          convertX(g_el$guide$heights, "pt"),
          nlines + 1,
          convertX(with(g_el, xaxis$height + ylab$width), "pt")
        ),
        c(
          "lines",
          "null",
          "pt",
          "pt",
          "lines",
          "pt"
        )
      )
    )
  }
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
#' @param xlim (`numeric`)\cr the maximum value on the x-axis (used to
#'   ensure the at risk table aligns with the KM graph).
#'
#' @importFrom grid dataViewport gList gpar gTree plotViewport rectGrob stringWidth textGrob unit viewport
#'
#' @examples
#'
#' \dontrun{
#' library(scda)
#' library(dplyr)
#' library(survival)
#' library(grid)
#'
#' fit_km <- synthetic_cdisc_data("latest")$adtte %>%
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
#'   title = "tt", yval = "Survival"
#' )
#'
#' # The annotation table reports the patient at risk for a given strata and
#' # time (`xticks`).
#' annot_tbl <- summary(fit_km, time = xticks)
#' if (is.null(fit_km$strata)) {
#'   annot_tbl <- with(annot_tbl, data.frame(n.risk = n.risk, time = time, strata = "All"))
#' } else {
#'   strata_lst <- strsplit(sub("=", "equals", levels(annot_tbl$strata)), "equals")
#'   levels(annot_tbl$strata) <- matrix(unlist(strata_lst), ncol = 2, byrow = TRUE)[, 2]
#'   annot_tbl <- data.frame(
#'     n.risk = annot_tbl$n.risk,
#'     time = annot_tbl$time,
#'     strata = annot_tbl$strata
#'     )
#' }
#'
#' # The annotation table is transformed into a grob.
#' tbl <- h_grob_tbl_at_risk(data = data_plot, annot_tbl = annot_tbl, xlim = max(xticks))
#'
#' # For the representation, the layout is estimated for which the decomposition
#' # of the graphic element is necessary.
#' g_el <- h_decompose_gg(gg)
#' lyt <- h_km_layout(data = data_plot, g_el = g_el, title = "t")
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
h_grob_tbl_at_risk <- function(data, annot_tbl, xlim) {
  txtlines <- levels(as.factor(data$strata))
  nlines <- nlevels(as.factor(data$strata))
  y_str_unit <- as.numeric(annot_tbl$strata)
  vp_table <- plotViewport(margins = unit(c(0, 0, 0, 0), "lines"))
  gb_table_left_annot <- gList(
    rectGrob(
      x = 0, y = unit(c(1:nlines) - 1, "lines"),
      gp = gpar(fill = c("gray95", "gray90"), alpha = 1, col = "white"),
      height = unit(1, "lines"), just = "bottom", hjust = 0
    ),
    textGrob(
      label = unique(annot_tbl$strata),
      x = .95,
      y = unit(
        (max(unique(y_str_unit)) - unique(y_str_unit)) + .5,
        "native"
      ),
      hjust = 1,
      gp = gpar(fontface = "italic", fontsize = 10)
    )
  )
  gb_patient_at_risk <- gList(
    rectGrob(
      x = 0, y = unit(c(1:nlines) - 1, "lines"),
      gp = gpar(fill = c("gray95", "gray90"), alpha = 1, col = "white"),
      height = unit(1, "lines"), just = "bottom", hjust = 0
    ),
    textGrob(
      label = annot_tbl$n.risk,
      x = unit(annot_tbl$time, "native"),
      y = unit(
        (max(y_str_unit) - y_str_unit) + .5
        , "line"
      ) # maybe native
    )
  )

  list(
    at_risk = gList(
      gTree(
        vp = vp_table,
        children = gList(
          gTree(
            vp = dataViewport(
              xscale = c(0, xlim) + c(-0.05, 0.05) * xlim,
              yscale = c(0, nlines + 1),
              extension = c(0.05, 0)
            ),
            children = gList(gb_patient_at_risk)
          )
        )
      )
    ),
    label = gList(
      gTree(
        vp = viewport(width = max(stringWidth(txtlines))),
        children = gList(
          gTree(
            vp = dataViewport(
              xscale = 0:1,
              yscale = c(0, nlines + 1),
              extension = c(0.0, 0)
            ),
            children = gList(gb_table_left_annot)
          )
        )
      )
    )
  )
}

#' Helper Function: Survival Estimations
#'
#' Transform a survival fit to a table with groups in rows characterized
#' by N, median and confidence interval.
#'
#' @inheritParams h_data_plot
#' @importFrom stats setNames
#' @examples
#' \dontrun{
#' library(scda)
#' library(dplyr)
#' library(survival)
#'
#' adtte <- synthetic_cdisc_data("latest")$adtte %>%
#'   filter(PARAMCD == "OS")
#'
#' fit <- survfit(
#'   form = Surv(AVAL, 1 - CNSR) ~ ARMCD,
#'   data = adtte
#' )
#' h_tbl_median_surv(fit_km = fit)
#' }
#'
h_tbl_median_surv <- function(fit_km, armval = "All") {
  y <- if (is.null(fit_km$strata)) {
    as.data.frame(t(summary(fit_km)$table), row.names = armval)
  } else {
    tbl <- summary(fit_km)$table
    rownames_lst <- strsplit(sub("=", "equals", rownames(tbl)), "equals")
    rownames(tbl) <- matrix(unlist(rownames_lst), ncol = 2, byrow = TRUE)[, 2]
    as.data.frame(tbl)
  }
  conf.int <- summary(fit_km)$conf.int #nolint
  y$records <- round(y$records)
  y$median <- signif(y$median, 4)
  y$`CI` <- paste0( # nolint
    "(", signif(y[[paste0(conf.int, "LCL")]], 4), ", ", signif(y[[paste0(conf.int, "UCL")]], 4), ")"
  )
  setNames(
    y[c("records", "median", "CI")],
    c("N", "Median", f_conf_level(conf.int))
  )
}


#' Helper Function: Survival Estimation Grob
#'
#' The survival fit is transformed in a grob containing a table with groups in
#' rows characterized by N, median and 95% confidence interval.
#'
#' @inheritParams kaplan_meier
#' @param ttheme (`list`)\cr see [gridExtra::ttheme_default()].
#' @param x a `numeric` value between 0 and 1 specifying x-location.
#' @param y a `numeric` value between 0 and 1 specifying y-location.
#' @inheritParams h_data_plot
#'
#' @importFrom grid gList gTree unit viewport
#' @importFrom gridExtra tableGrob ttheme_default
#'
#' @examples
#' \dontrun{
#' library(scda)
#' library(dplyr)
#' library(survival)
#' library(grid)
#'
#' grid.newpage()
#' grid.rect(gp = gpar(lty = 1, col = "pink", fill = "gray85", lwd = 1))
#' synthetic_cdisc_data("latest")$adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(form = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
#'   h_grob_median_surv %>%
#'   grid.draw()
#' }
#'
h_grob_median_surv <- function(fit_km,
                               armval = "All",
                               x = 0.9,
                               y = 0.9,
                               ttheme = ttheme_default()) {
  data <- h_tbl_median_surv(fit_km, armval = armval) # nolint
  gt <- gridExtra::tableGrob(d = data, theme = ttheme)
  vp <- viewport(
    x = unit(x, "npc") + unit(1, "lines"),
    y = unit(y, "npc") + unit(1.5, "lines"),
    height = sum(gt$heights),
    width = sum(gt$widths),
    just = c("right", "top")
  )

  gList(
    gTree(
      vp = vp,
      children = gList(gt)
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
#' @importFrom grid convertX gList gTree unit viewport
#'
#' @examples
#' \dontrun{
#' library(scda)
#' library(dplyr)
#' library(survival)
#' library(grid)
#'
#' fit_km <- synthetic_cdisc_data("latest")$adtte %>%
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
  gList(
    gTree(
      vp = viewport(
        width = convertX(yaxis$width + ylab$width, "pt"),
        x = unit(1, "npc"),
        just = "right"
      ),
      children = gList(cbind(ylab, yaxis))
    )
  )
}

#' Helper Function: Pairwise CoxPH table
#'
#' Create an rtable of pairwise stratified or unstratified CoxPH analysis results.
#'
#' @inheritParams g_km
#' @examples
#' \dontrun{
#' library(scda)
#' library(dplyr)
#'
#' adtte <- synthetic_cdisc_data("latest")$adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(is_event = CNSR == 0)
#'
#' tern:::h_tbl_coxph_pairwise(
#'   df = adtte,
#'   variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
#'   control_coxph_pw = control_coxph(conf_level = 0.9))
#' }
#'
h_tbl_coxph_pairwise <- function(df,
                                 variables,
                                 control_coxph_pw = control_coxph()
) {
  assert_that(is_df_with_variables(df, as.list(unlist(variables))))
  arm <- variables$arm
  df[[arm]] <- factor(df[[arm]])
  ref_group <- levels(df[[arm]])[1]
  comp_group <- levels(df[[arm]])[-1]
  results <- Map(function(comp) {
    res <- s_coxph_pairwise(
      df = df[df[[arm]] == comp, , drop = FALSE],
      .ref_group = df[df[[arm]] == ref_group, , drop = FALSE],
      .in_ref_col = FALSE,
      .var = variables$tte,
      is_event = variables$is_event,
      strat = variables$strat,
      control = control_coxph_pw
    )
    res_df <- data.frame(
      hr = format(round(res$hr, 2), nsmall = 2),
      hr_ci = paste0( # nolint
        "(", format(round(res$hr_ci[1], 2), nsmall = 2), ", ",
        format(round(res$hr_ci[2], 2), nsmall = 2), ")"
      ),
      pvalue = if (res$pvalue < 0.0001) "<0.0001" else format(round(res$pvalue, 4), 4),
      stringsAsFactors = FALSE
    )
    colnames(res_df) <- c("HR", vapply(res[c("hr_ci", "pvalue")], obj_label, FUN.VALUE = "character"))
    row.names(res_df) <- comp
    res_df
  }, comp_group)
  do.call(rbind, results)
}
#' Helper Function: CoxPH Grob
#' Grob of rtable output from [h_tbl_coxph_pairwise]
#'
#' @inheritParams h_grob_median_surv
#' @param ... arguments will be passed to [h_tbl_coxph_pairwise()].
#' @param x a `numeric` value between 0 and 1 specifying x-location.
#' @param y a `numeric` value between 0 and 1 specifying y-location.
#'
#' @importFrom grid gList gTree unit viewport
#' @importFrom gridExtra tableGrob ttheme_default
#'
#' @examples
#' \dontrun{
#' library(scda)
#' library(dplyr)
#' library(survival)
#' library(grid)
#'
#' grid.newpage()
#' grid.rect(gp = gpar(lty = 1, col = "pink", fill = "gray85", lwd = 1))
#' data <- synthetic_cdisc_data("latest")$adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(is_event = CNSR == 0)
#' tbl_grob <- tern:::h_grob_coxph(
#'    df = data,
#'    variables = list(tte = "AVAL", is_event = "is_event", arm = "ARMCD"),
#'    control_coxph_pw = control_coxph(conf_level = 0.9), x = 0.5, y = 0.5)
#' grid.draw(tbl_grob)
#' }
#'
h_grob_coxph <- function(...,
                         x = 0,
                         y = 0,
                         ttheme = ttheme_default(
                           base_size = 12,
                           padding = unit(c(1, .5), "lines"),
                           core = list(bg_params = list(fill = c("grey95", "grey90"), alpha = .5))
                         )
) {
  data <- h_tbl_coxph_pairwise(...) # nolint
  gt <- gridExtra::tableGrob(d = data, theme = ttheme)
  vp <- viewport(
    x = unit(x, "npc") + unit(1, "lines"),
    y = unit(y, "npc") + unit(1.5, "lines"),
    height =  sum(gt$heights),
    width =  sum(gt$widths),
    just = c("left", "bottom")
  )

  gList(
    gTree(
      vp = vp,
      children = gList(gt)
    )
  )
}
