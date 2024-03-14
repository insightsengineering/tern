#' Kaplan-Meier Plot
#'
#' @description `r lifecycle::badge("stable")`
#'
#' From a survival model, a graphic is rendered along with tabulated annotation
#' including the number of patient at risk at given time and the median survival
#' per group.
#'
#' @inheritParams grid::gTree
#' @inheritParams argument_convention
#' @param df (`data.frame`)\cr data set containing all analysis variables.
#' @param variables (named `list`)\cr variable names. Details are:
#'   * `tte` (`numeric`)\cr variable indicating time-to-event duration values.
#'   * `is_event` (`logical`)\cr event variable. `TRUE` if event, `FALSE` if time to event is censored.
#'   * `arm` (`factor`)\cr the treatment group variable.
#'   * `strat` (`character` or `NULL`)\cr variable names indicating stratification factors.
#' @param control_surv (`list`)\cr parameters for comparison details, specified by using
#'   the helper function [control_surv_timepoint()]. Some possible parameter options are:
#'   * `conf_level` (`proportion`)\cr confidence level of the interval for survival rate.
#'   * `conf_type` (`string`)\cr `"plain"` (default), `"log"`, `"log-log"` for confidence interval type,
#'     see more in [survival::survfit()]. Note that the option "none" is no longer supported.
#' @param xticks (`numeric`, `number`, or `NULL`)\cr numeric vector of ticks or single number with spacing
#'   between ticks on the x axis. If `NULL` (default), [labeling::extended()] is used to determine
#'   an optimal tick position on the x axis.
#' @param yval (`string`)\cr value of y-axis. Options are `Survival` (default) and `Failure` probability.
#' @param censor_show (`flag`)\cr whether to show censored.
#' @param xlab (`string`)\cr label of x-axis.
#' @param ylab (`string`)\cr label of y-axis.
#' @param ylim (`vector` of `numeric`)\cr vector of length 2 containing lower and upper limits for the y-axis.
#'   If `NULL` (default), the minimum and maximum y-values displayed are used as limits.
#' @param title (`string`)\cr title for plot.
#' @param footnotes (`string`)\cr footnotes for plot.
#' @param col (`character`)\cr lines colors. Length of a vector should be equal
#'   to number of strata from [survival::survfit()].
#' @param lty (`numeric`)\cr line type. Length of a vector should be equal
#'   to number of strata from [survival::survfit()].
#' @param lwd (`numeric`)\cr line width. Length of a vector should be equal
#'   to number of strata from [survival::survfit()].
#' @param pch (`numeric`, `string`)\cr value or character of points symbol to indicate censored cases.
#' @param size (`numeric`)\cr size of censored point, a class of `unit`.
#' @param max_time (`numeric`)\cr maximum value to show on X axis. Only data values less than or up to
#'   this threshold value will be plotted (defaults to `NULL`).
#' @param font_size (`number`)\cr font size to be used.
#' @param ci_ribbon (`flag`)\cr draw the confidence interval around the Kaplan-Meier curve.
#' @param ggtheme (`theme`)\cr a graphical theme as provided by `ggplot2` to control outlook of the Kaplan-Meier curve.
#' @param annot_at_risk (`flag`)\cr compute and add the annotation table reporting the number of patient at risk
#'   matching the main grid of the Kaplan-Meier curve.
#' @param annot_at_risk_title (`flag`)\cr whether the "Patients at Risk" title should be added above the `annot_at_risk`
#'   table. Has no effect if `annot_at_risk` is `FALSE`. Defaults to `TRUE`.
#' @param annot_surv_med (`flag`)\cr compute and add the annotation table on the Kaplan-Meier curve estimating the
#'   median survival time per group.
#' @param annot_coxph (`flag`)\cr add the annotation table from a [survival::coxph()] model.
#' @param annot_stats (`string`)\cr statistics annotations to add to the plot. Options are
#'   `median` (median survival follow-up time) and `min` (minimum survival follow-up time).
#' @param annot_stats_vlines (`flag`)\cr add vertical lines corresponding to each of the statistics
#'   specified by `annot_stats`. If `annot_stats` is `NULL` no lines will be added.
#' @param control_coxph_pw (`list`)\cr parameters for comparison details, specified by using
#'   the helper function [control_coxph()]. Some possible parameter options are:
#'   * `pval_method` (`string`)\cr p-value method for testing hazard ratio = 1.
#'     Default method is `"log-rank"`, can also be set to `"wald"` or `"likelihood"`.
#'   * `ties` (`string`)\cr method for tie handling. Default is `"efron"`,
#'     can also be set to `"breslow"` or `"exact"`. See more in [survival::coxph()]
#'   * `conf_level` (`proportion`)\cr confidence level of the interval for HR.
#' @param ref_group_coxph (`character`)\cr level of arm variable to use as reference group in calculations for
#'   `annot_coxph` table. If `NULL` (default), uses the first level of the arm variable.
#' @param annot_coxph_ref_lbls (`flag`)\cr whether the reference group should be explicitly printed in labels for the
#'   `annot_coxph` table. If `FALSE` (default), only comparison groups will be printed in `annot_coxph` table labels.
#' @param position_coxph (`numeric`)\cr x and y positions for plotting [survival::coxph()] model.
#' @param position_surv_med (`numeric`)\cr x and y positions for plotting annotation table estimating median survival
#'   time per group.
#' @param width_annots (named `list` of `unit`s)\cr a named list of widths for annotation tables with names `surv_med`
#'   (median survival time table) and `coxph` ([survival::coxph()] model table), where each value is the width
#'   (in units) to implement when printing the annotation table.
#'
#' @return A `grob` of class `gTree`.
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(ggplot2)
#' library(survival)
#' library(grid)
#' library(nestcolor)
#'
#' df <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(is_event = CNSR == 0)
#' variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")
#'
#' # 1. Example - basic option
#'
#' g_km(df = df, variables = variables)
#' g_km(df = df, variables = variables, yval = "Failure")
#' g_km(
#'   df = df,
#'   variables = variables,
#'   control_surv = control_surv_timepoint(conf_level = 0.9),
#'   col = c("grey25", "grey50", "grey75"),
#'   annot_at_risk_title = FALSE
#' )
#' g_km(df = df, variables = variables, ggtheme = theme_minimal())
#' res <- g_km(df = df, variables = variables, ggtheme = theme_minimal(), lty = 1:3)
#' res <- g_km(df = df, variables = variables, max = 2000)
#' res <- g_km(
#'   df = df,
#'   variables = variables,
#'   annot_stats = c("min", "median"),
#'   annot_stats_vlines = TRUE
#' )
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
#' # Change widths/sizes of surv_med and coxph annotation tables.
#' g_km(
#'   df = df, variables = c(variables, list(strat = "SEX")),
#'   annot_coxph = TRUE,
#'   width_annots = list(surv_med = grid::unit(2, "in"), coxph = grid::unit(3, "in"))
#' )
#'
#' g_km(
#'   df = df, variables = c(variables, list(strat = "SEX")),
#'   font_size = 15,
#'   annot_coxph = TRUE,
#'   control_coxph = control_coxph(pval_method = "wald", ties = "exact", conf_level = 0.99),
#'   position_coxph = c(0.5, 0.5)
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
#' }
#'
#' @export
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
                   ylim = NULL,
                   title = NULL,
                   footnotes = NULL,
                   draw = lifecycle::deprecated(),
                   newpage = lifecycle::deprecated(),
                   gp = lifecycle::deprecated(),
                   vp = lifecycle::deprecated(),
                   name = lifecycle::deprecated(),
                   font_size = 10,
                   ci_ribbon = FALSE,
                   ggtheme = NULL,
                   annot_at_risk = TRUE,
                   annot_at_risk_title = TRUE,
                   annot_surv_med = TRUE,
                   annot_coxph = FALSE,
                   annot_stats = NULL,
                   annot_stats_vlines = FALSE, ###### TODO
                   control_coxph_pw = control_coxph(),
                   ref_group_coxph = NULL,
                   control_annot_surv_med = control_surv_med_annot(),
                   control_annot_coxph = control_coxph_annot(),
                   annot_coxph_ref_lbls = lifecycle::deprecated(),
                   position_coxph = lifecycle::deprecated(),
                   position_surv_med = lifecycle::deprecated(),
                   width_annots = lifecycle::deprecated()) {
  # Deprecated argument warnings
  if (lifecycle::is_present(draw)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(draw)",
      details = "This argument is no longer used since the plot is now generated as a ggplot2 object."
    )
  }
  if (lifecycle::is_present(newpage)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(newpage)",
      details = "This argument is no longer used since the plot is now generated as a ggplot2 object."
    )
  }
  if (lifecycle::is_present(gp)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(gp)",
      details = "This argument is no longer used since the plot is now generated as a ggplot2 object."
    )
  }
  if (lifecycle::is_present(vp)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(vp)",
      details = "This argument is no longer used since the plot is now generated as a ggplot2 object."
    )
  }
  if (lifecycle::is_present(name)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(name)",
      details = "This argument is no longer used since the plot is now generated as a ggplot2 object."
    )
  }
  checkmate::assert_list(variables)
  checkmate::assert_subset(c("tte", "arm", "is_event"), names(variables))
  checkmate::assert_numeric(ylim, len = 2, null.ok = TRUE)
  checkmate::assert_numeric(font_size, len = 1)
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_string(footnotes, null.ok = TRUE)
  checkmate::assert_logical(ci_ribbon, len = 1, any.missing = FALSE)
  checkmate::assert_subset(annot_stats, c("median", "min"))
  checkmate::assert_logical(annot_stats_vlines)

  tte <- variables$tte
  is_event <- variables$is_event
  arm <- variables$arm

  assert_valid_factor(df[[arm]])
  armval <- as.character(unique(df[[arm]]))
  assert_df_with_variables(df, list(tte = tte, is_event = is_event, arm = arm))
  checkmate::assert_logical(df[[is_event]], min.len = 1, any.missing = FALSE)
  checkmate::assert_numeric(df[[tte]], min.len = 1, any.missing = FALSE)
  checkmate::assert_vector(col, null.ok = TRUE, len = length(armval))
  checkmate::assert_vector(lty, null.ok = TRUE)
  checkmate::assert_numeric(lwd)

  if (annot_coxph && length(armval) < 2) {
    stop(paste(
      "When `annot_coxph` = TRUE, `df` must contain at least 2 levels of `variables$arm`",
      "in order to calculate the hazard ratio."
    ))
  }

  # process model
  yval <- match.arg(yval)
  formula <- stats::as.formula(paste0("survival::Surv(", tte, ", ", is_event, ") ~ ", arm))
  fit_km <- ggsurvfit::survfit2(
    formula = formula,
    data = df,
    conf.int = control_surv$conf_level,
    conf.type = control_surv$conf_type
  )
  data_plot <- ggsurvfit::tidy_survfit(fit_km)

  # add x-ticks
  xticks <- h_xticks(data = data_plot, xticks = xticks, max_time = max_time)
  if (is.null(max_time)) max_time <- max(xticks)

  # set plot type
  p_type <- if (yval == "Failure") "risk" else if (yval == "Survival") "survival" else yval

  # initialize ggplot
  gg <- ggsurvfit::ggsurvfit(fit_km, type = p_type, linetype_aes = !is.null(lty)) +
    ggsurvfit::scale_ggsurvfit(
      x_scales = list(limits = c(0, max_time), breaks = xticks),
      y_scales = list(limits = ylim, label = NULL)
    ) +
    scale_linetype_manual(values = lty) +
    ggplot2::labs(title = title, x = xlab, y = ylab, caption = footnotes) +
    theme(
      axis.text = element_text(size = font_size),
      axis.title = element_text(size = font_size),
      legend.text = element_text(size = font_size),
      legend.box.background = element_rect(colour = "black", linewidth = 0.75),
      legend.margin = margin(c(1, 5, 2, 5)),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  # add censor marks
  if (censor_show) gg <- gg + ggsurvfit::add_censor_mark(shape = pch, size = size)

  # add ci ribbon
  if (ci_ribbon) gg <- gg + ggsurvfit::add_confidence_interval()

  ###
  if (!is.null(annot_stats)) {
    if ("min" %in% annot_stats) gg <- gg + ggsurvfit::add_quantile(y_value = 0)
    if ("median" %in% annot_stats) gg <- gg + ggsurvfit::add_quantile(y_value = 0.5)
  }

  # control aesthetics
  if (!is.null(col)) {
    gg <- gg +
      scale_color_manual(values = col) +
      scale_fill_manual(values = col)
  }
  if (!is.null(lwd)) gg <- gg + scale_linewidth_manual(values = lwd)
  if (!is.null(ggtheme)) gg <- gg + ggtheme

  # add at risk annotation table
  if (annot_at_risk) {
    annot_tbl <- summary(fit_km, times = xticks, extend = TRUE)
    annot_tbl <- if (is.null(fit_km$strata)) {
      data.frame(
        n.risk = annot_tbl$n.risk,
        time = annot_tbl$time,
        strata = armval
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

    at_risk_tbl <- as.data.frame(tidyr::pivot_wider(annot_tbl, names_from = "time", values_from = "n.risk")[, -1])
    at_risk_tbl[is.na(at_risk_tbl)] <- 0
    rownames(at_risk_tbl) <- levels(annot_tbl$strata)

    at_risk <- df2gg(
      at_risk_tbl,
      col_labels = FALSE, hline = FALSE,
      colwidths = rep(1, ncol(at_risk_tbl))
    ) +
      labs(title = if (annot_at_risk_title) "Patients at Risk:" else NULL, x = xlab) +
      theme_bw(base_size = font_size) +
      theme(
        plot.title = element_text(size = font_size, vjust = 3, face = "bold"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = font_size, face = "italic"),
        axis.text.x = element_text(size = font_size),
        axis.line.x = element_line()
      ) +
      coord_cartesian(clip = "off", ylim = c(0.5, nrow(at_risk_tbl)))
    at_risk <- suppressMessages(at_risk +
      scale_x_continuous(expand = c(0.025 , 0), breaks = seq_along(at_risk_tbl) - 0.5, labels = xticks) +
      scale_y_continuous(labels = rev(levels(annot_tbl$strata)), breaks = seq_len(nrow(at_risk_tbl)))
    )

    gg <- cowplot::plot_grid(
      gg,
      at_risk,
      align = "v",
      axis = "tblr",
      ncol = 1,
      rel_heights = c(0.75, 0.25)
    )
  }

  # add median survival time annotation table
  if (annot_surv_med) {
    surv_med_tbl <- h_tbl_median_surv(fit_km = fit_km, armval = armval)
    scale_arm_lbls <- max(nchar(rownames(surv_med_tbl))) / 5
    bg_fill <- if (isTRUE(control_annot_surv_med[["fill"]])) "#00000020" else control_annot_surv_med[["fill"]]

    dfgg <- df2gg(surv_med_tbl, colwidths = c(1, 1, 2), bg_fill = bg_fill) +
      theme(
        axis.text.y = element_text(size = font_size, face = "italic"),
        plot.margin = margin(0, 2, 0, 5)
      ) +
      coord_cartesian(clip = "off", ylim = c(0.5, nrow(surv_med_tbl) + 1.5))
    dfgg <- suppressMessages(dfgg +
      scale_x_continuous(expand = c(0.025 , 0)) +
      scale_y_continuous(labels = rev(rownames(surv_med_tbl)), breaks = seq_len(nrow(surv_med_tbl)))
    )

    gg <- cowplot::ggdraw(gg) +
      cowplot::draw_plot(
        dfgg,
        control_annot_surv_med[["x"]],
        control_annot_surv_med[["y"]],
        width = control_annot_surv_med[["w"]],
        height = control_annot_surv_med[["h"]],
        vjust = 0.5,
        hjust = 0.5
      )
  }

  # add coxph annotation table
  if (annot_coxph) {
    coxph_tbl <- h_tbl_coxph_pairwise(
      df = df,
      variables = variables,
      ref_group_coxph = ref_group_coxph,
      control_coxph_pw = control_coxph_pw,
      annot_coxph_ref_lbls = control_annot_coxph[["ref_lbls"]]
    )
    scale_arm_lbls <- max(nchar(rownames(coxph_tbl))) / 10
    bg_fill <- if (isTRUE(control_annot_coxph[["fill"]])) "#00000020" else control_annot_coxph[["fill"]]

    dfgg <- df2gg(coxph_tbl, colwidths = c(1.1, 1, 3), bg_fill = bg_fill) +
      theme(
        axis.text.y = element_text(size = font_size, face = "italic"),
        plot.margin = margin(0, 2, 0, 5)
      ) +
      coord_cartesian(clip = "off", ylim = c(0.5, nrow(coxph_tbl) + 1.5))
    dfgg <- suppressMessages(dfgg +
      scale_x_continuous(expand = c(0.025 , 0)) +
      scale_y_continuous(labels = rev(rownames(coxph_tbl)), breaks = seq_len(nrow(coxph_tbl)))
    )

    gg <- cowplot::ggdraw(gg) +
      cowplot::draw_plot(
        dfgg,
        control_annot_coxph[["x"]],
        control_annot_coxph[["y"]],
        width = control_annot_coxph[["w"]],
        height = control_annot_coxph[["h"]],
        vjust = 0.5,
        hjust = 0.5
      )
  }

  gg
}

#' Control Functions for Kaplan-Meier Plot Annotation Tables
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Auxiliary functions for controlling arguments for formatting the annotation tables that can be added to plots
#' generated via [g_km()].
#'
#' @param x (`proportion`)\cr x-coordinate for center of annotation table.
#' @param y (`proportion`)\cr y-coordinate for center of annotation table.
#' @param w (`proportion`)\cr relative width of the annotation table.
#' @param h (`proportion`)\cr relative height of the annotation table.
#' @param fill (`logical` or `character`)\cr whether the annotation table should have a background fill color.
#'   Can also be a color code to use as the background fill color. If `TRUE`, color code defaults to `"#00000020"`.
#'
#' @return A list of components with the same names as the arguments.
#'
#' @seealso [g_km()]
#'
#' @name control_annot
NULL

#' @describeIn control_annot Control function for formatting the median survival time annotation table. This annotation
#'   table can be added in [g_km()] by setting `annot_surv_med=TRUE`, and can be configured using the
#'   `control_surv_med_annot()` function by setting it as the `control_annot_surv_med` argument.
#'
#'
#' @examples
#' control_surv_med_annot()
#'
#' @export
control_surv_med_annot <- function(x = 0.8, y = 0.85, w = 0.32, h = 0.16, fill = TRUE) {
  assert_proportion_value(x)
  assert_proportion_value(y)
  assert_proportion_value(w)
  assert_proportion_value(h)

  list(x = x, y = y, w = w, h = h, fill = fill)
}

#' @describeIn control_annot Control function for formatting the Cox-PH annotation table. This annotation table can be
#'   added in [g_km()] by setting `annot_coxph=TRUE`, and can be configured using the `control_coxph_annot()` function
#'   by setting it as the `control_annot_coxph` argument.
#'
#' @param ref_lbls (`logical`)\cr whether the reference group should be explicitly printed in labels for the
#'   annotation table. If `FALSE` (default), only comparison groups will be printed in the table labels.
#'
#' @examples
#' control_annot_coxph()
#'
#' @export
control_coxph_annot <- function(x = 0.29, y = 0.51, w = 0.4, h = 0.125, fill = TRUE, ref_lbls = FALSE) {
  checkmate::assert_logical(ref_lbls, any.missing = FALSE)

  res <- c(control_surv_med_annot(x = x, y = y, w = w, h = h), list(ref_lbls = ref_lbls))
  res
}
