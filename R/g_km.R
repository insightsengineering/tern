#' Kaplan-Meier plot
#'
#' @description `r lifecycle::badge("stable")`
#'
#' From a survival model, a graphic is rendered along with tabulated annotation
#' including the number of patient at risk at given time and the median survival
#' per group.
#'
#' @inheritParams argument_convention
#' @param variables (named `list`)\cr variable names. Details are:
#'   * `tte` (`numeric`)\cr variable indicating time-to-event duration values.
#'   * `is_event` (`logical`)\cr event variable. `TRUE` if event, `FALSE` if time to event is censored.
#'   * `arm` (`factor`)\cr the treatment group variable.
#'   * `strata` (`character` or `NULL`)\cr variable names indicating stratification factors.
#' @param control_surv (`list`)\cr parameters for comparison details, specified by using
#'   the helper function [control_surv_timepoint()]. Some possible parameter options are:
#'   * `conf_level` (`proportion`)\cr confidence level of the interval for survival rate.
#'   * `conf_type` (`string`)\cr `"plain"` (default), `"log"`, `"log-log"` for confidence interval type,
#'     see more in [survival::survfit()]. Note that the option "none" is no longer supported.
#' @param col (`character`)\cr lines colors. Length of a vector should be equal
#'   to number of strata from [survival::survfit()].
#' @param lty (`numeric`)\cr line type. If a vector is given, its length should be equal to the number of strata from
#'   [survival::survfit()].
#' @param lwd (`numeric`)\cr line width. If a vector is given, its length should be equal to the number of strata from
#'   [survival::survfit()].
#' @param censor_show (`flag`)\cr whether to show censored observations.
#' @param pch (`string`)\cr name of symbol or character to use as point symbol to indicate censored cases.
#' @param size (`numeric(1)`)\cr size of censored point symbols.
#' @param max_time (`numeric(1)`)\cr maximum value to show on x-axis. Only data values less than or up to
#'   this threshold value will be plotted (defaults to `NULL`).
#' @param xticks (`numeric` or `NULL`)\cr numeric vector of tick positions or a single number with spacing
#'   between ticks on the x-axis. If `NULL` (default), [labeling::extended()] is used to determine
#'   optimal tick positions on the x-axis.
#' @param xlab (`string`)\cr x-axis label.
#' @param yval (`string`)\cr type of plot, to be plotted on the y-axis. Options are `Survival` (default) and `Failure`
#'   probability.
#' @param ylab (`string`)\cr y-axis label.
#' @param title (`string`)\cr plot title.
#' @param footnotes (`string`)\cr plot footnotes.
#' @param font_size (`numeric(1)`)\cr font size to use for all text.
#' @param ci_ribbon (`flag`)\cr whether the confidence interval should be drawn around the Kaplan-Meier curve.
#' @param annot_at_risk (`flag`)\cr compute and add the annotation table reporting the number of patient at risk
#'   matching the main grid of the Kaplan-Meier curve.
#' @param annot_at_risk_title (`flag`)\cr whether the "Patients at Risk" title should be added above the `annot_at_risk`
#'   table. Has no effect if `annot_at_risk` is `FALSE`. Defaults to `TRUE`.
#' @param annot_surv_med (`flag`)\cr compute and add the annotation table on the Kaplan-Meier curve estimating the
#'   median survival time per group.
#' @param annot_coxph (`flag`)\cr whether to add the annotation table from a [survival::coxph()] model.
#' @param annot_stats (`string` or `NULL`)\cr statistics annotations to add to the plot. Options are
#'   `median` (median survival follow-up time) and `min` (minimum survival follow-up time).
#' @param annot_stats_vlines (`flag`)\cr add vertical lines corresponding to each of the statistics
#'   specified by `annot_stats`. If `annot_stats` is `NULL` no lines will be added.
#' @param control_coxph_pw (`list`)\cr parameters for comparison details, specified using the helper function
#'   [control_coxph()]. Some possible parameter options are:
#'   * `pval_method` (`string`)\cr p-value method for testing hazard ratio = 1.
#'     Default method is `"log-rank"`, can also be set to `"wald"` or `"likelihood"`.
#'   * `ties` (`string`)\cr method for tie handling. Default is `"efron"`,
#'     can also be set to `"breslow"` or `"exact"`. See more in [survival::coxph()]
#'   * `conf_level` (`proportion`)\cr confidence level of the interval for HR.
#' @param ref_group_coxph (`string` or `NULL`)\cr level of arm variable to use as reference group in calculations for
#'   `annot_coxph` table. If `NULL` (default), uses the first level of the arm variable.
#' @param control_annot_surv_med (`list`)\cr parameters to control the position and size of the annotation table added
#'   to the plot when `annot_surv_med = TRUE`, specified using the [control_surv_med_annot()] function. Parameter
#'   options are: `x`, `y`, `w`, `h`, and `fill`. See [control_surv_med_annot()] for details.
#' @param control_annot_coxph (`list`)\cr parameters to control the position and size of the annotation table added
#'   to the plot when `annot_coxph = TRUE`, specified using the [control_coxph_annot()] function. Parameter
#'   options are: `x`, `y`, `w`, `h`, `fill`, and `ref_lbls`. See [control_coxph_annot()] for details.
#' @param legend_pos (`numeric(2)` or `NULL`)\cr vector containing x- and y-coordinates, respectively, for the legend
#'   position relative to the KM plot area. If `NULL` (default), the legend is positioned in the bottom right corner of
#'   the plot, or the middle right of the plot if needed to prevent overlapping.
#' @param rel_height_plot (`proportion`)\cr proportion of total figure height to allocate to the Kaplan-Meier plot.
#'   Relative height of patients at risk table is then `1 - rel_height_plot`. If `annot_at_risk = FALSE` or
#'   `as_list = TRUE`, this parameter is ignored.
#' @param ggtheme (`theme`)\cr a graphical theme as provided by `ggplot2` to format the Kaplan-Meier plot.
#' @param as_list (`flag`)\cr whether the two `ggplot` objects should be returned as a list when `annot_at_risk = TRUE`.
#'   If `TRUE`, a named list with two elements, `plot` and `table`, will be returned. If `FALSE` (default) the patients
#'   at risk table is printed below the plot via [cowplot::plot_grid()].
#' @param draw `r lifecycle::badge("deprecated")` This function no longer generates `grob` objects.
#' @param newpage `r lifecycle::badge("deprecated")` This function no longer generates `grob` objects.
#' @param gp `r lifecycle::badge("deprecated")` This function no longer generates `grob` objects.
#' @param vp `r lifecycle::badge("deprecated")` This function no longer generates `grob` objects.
#' @param name `r lifecycle::badge("deprecated")` This function no longer generates `grob` objects.
#' @param annot_coxph_ref_lbls `r lifecycle::badge("deprecated")` Please use the `ref_lbls` element of
#'   `control_annot_coxph` instead.
#' @param position_coxph `r lifecycle::badge("deprecated")`  Please use the `x` and `y` elements of
#'   `control_annot_coxph` instead.
#' @param position_surv_med `r lifecycle::badge("deprecated")` Please use the `x` and `y` elements of
#'   `control_annot_surv_med` instead.
#' @param width_annots `r lifecycle::badge("deprecated")` Please use the `w` element of `control_annot_surv_med`
#'   (for `surv_med`) and `control_annot_coxph` (for `coxph`)."
#'
#' @return A `ggplot` Kaplan-Meier plot and (optionally) summary table.
#'
#' @examples
#' library(dplyr)
#' library(nestcolor)
#'
#' df <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(is_event = CNSR == 0)
#' variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARMCD")
#'
#' # Basic examples
#' g_km(df = df, variables = variables)
#' g_km(df = df, variables = variables, yval = "Failure")
#'
#' # Examples with customization parameters applied
#' g_km(
#'   df = df,
#'   variables = variables,
#'   control_surv = control_surv_timepoint(conf_level = 0.9),
#'   col = c("grey25", "grey50", "grey75"),
#'   annot_at_risk_title = FALSE,
#'   lty = 1:3,
#'   font_size = 8
#' )
#' g_km(
#'   df = df,
#'   variables = variables,
#'   annot_stats = c("min", "median"),
#'   annot_stats_vlines = TRUE,
#'   max_time = 3000,
#'   ggtheme = ggplot2::theme_minimal()
#' )
#'
#' # Example with pairwise Cox-PH analysis annotation table, adjusted annotation tables
#' g_km(
#'   df = df, variables = variables,
#'   annot_coxph = TRUE,
#'   control_coxph = control_coxph(pval_method = "wald", ties = "exact", conf_level = 0.99),
#'   control_annot_coxph = control_coxph_annot(x = 0.26, w = 0.35),
#'   control_annot_surv_med = control_surv_med_annot(x = 0.8, y = 0.9, w = 0.35)
#' )
#'
#' @aliases kaplan_meier
#' @export
g_km <- function(df,
                 variables,
                 control_surv = control_surv_timepoint(),
                 col = NULL,
                 lty = NULL,
                 lwd = 0.5,
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
                 font_size = 10,
                 ci_ribbon = FALSE,
                 annot_at_risk = TRUE,
                 annot_at_risk_title = TRUE,
                 annot_surv_med = TRUE,
                 annot_coxph = FALSE,
                 annot_stats = NULL,
                 annot_stats_vlines = FALSE,
                 control_coxph_pw = control_coxph(),
                 ref_group_coxph = NULL,
                 control_annot_surv_med = control_surv_med_annot(),
                 control_annot_coxph = control_coxph_annot(),
                 legend_pos = NULL,
                 rel_height_plot = 0.75,
                 ggtheme = NULL,
                 as_list = FALSE,
                 draw = lifecycle::deprecated(),
                 newpage = lifecycle::deprecated(),
                 gp = lifecycle::deprecated(),
                 vp = lifecycle::deprecated(),
                 name = lifecycle::deprecated(),
                 annot_coxph_ref_lbls = lifecycle::deprecated(),
                 position_coxph = lifecycle::deprecated(),
                 position_surv_med = lifecycle::deprecated(),
                 width_annots = lifecycle::deprecated()) {
  # Deprecated argument warnings
  if (lifecycle::is_present(draw)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(draw)",
      details = "This argument is no longer used since the plot is now generated as a `ggplot2` object."
    )
  }
  if (lifecycle::is_present(newpage)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(newpage)",
      details = "This argument is no longer used since the plot is now generated as a `ggplot2` object."
    )
  }
  if (lifecycle::is_present(gp)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(gp)",
      details = "This argument is no longer used since the plot is now generated as a `ggplot2` object."
    )
  }
  if (lifecycle::is_present(vp)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(vp)",
      details = "This argument is no longer used since the plot is now generated as a `ggplot2` object."
    )
  }
  if (lifecycle::is_present(name)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(name)",
      details = "This argument is no longer used since the plot is now generated as a `ggplot2` object."
    )
  }
  if (lifecycle::is_present(annot_coxph_ref_lbls)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(annot_coxph_ref_lbls)",
      details = "Please specify this setting using the 'ref_lbls' element of control_annot_coxph."
    )
    control_annot_coxph[["ref_lbls"]] <- annot_coxph_ref_lbls
  }
  if (lifecycle::is_present(position_coxph)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(position_coxph)",
      details = "Please specify this setting using the 'x' and 'y' elements of control_annot_coxph."
    )
    control_annot_coxph[["x"]] <- position_coxph[1]
    control_annot_coxph[["y"]] <- position_coxph[2]
  }
  if (lifecycle::is_present(position_surv_med)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(position_surv_med)",
      details = "Please specify this setting using the 'x' and 'y' elements of control_annot_surv_med."
    )
    control_annot_surv_med[["x"]] <- position_surv_med[1]
    control_annot_surv_med[["y"]] <- position_surv_med[2]
  }
  if (lifecycle::is_present(width_annots)) {
    lifecycle::deprecate_warn(
      "0.9.4", "g_km(width_annots)",
      details = paste(
        "Please specify widths of annotation tables relative to the plot area using the 'w' element of",
        "control_annot_surv_med (for surv_med) and control_annot_coxph (for coxph)."
      )
    )
    control_annot_surv_med[["w"]] <- as.numeric(width_annots[["surv_med"]])
    control_annot_coxph[["w"]] <- as.numeric(width_annots[["coxph"]])
  }

  checkmate::assert_list(variables)
  checkmate::assert_subset(c("tte", "arm", "is_event"), names(variables))
  checkmate::assert_logical(censor_show, len = 1)
  checkmate::assert_numeric(size, len = 1)
  checkmate::assert_numeric(max_time, len = 1, null.ok = TRUE)
  checkmate::assert_numeric(xticks, null.ok = TRUE)
  checkmate::assert_character(xlab, len = 1, null.ok = TRUE)
  checkmate::assert_character(yval)
  checkmate::assert_character(ylab, null.ok = TRUE)
  checkmate::assert_numeric(ylim, finite = TRUE, any.missing = FALSE, len = 2, sorted = TRUE, null.ok = TRUE)
  checkmate::assert_character(title, len = 1, null.ok = TRUE)
  checkmate::assert_character(footnotes, len = 1, null.ok = TRUE)
  checkmate::assert_numeric(font_size, len = 1)
  checkmate::assert_logical(ci_ribbon, len = 1)
  checkmate::assert_logical(annot_at_risk, len = 1)
  checkmate::assert_logical(annot_at_risk_title, len = 1)
  checkmate::assert_logical(annot_surv_med, len = 1)
  checkmate::assert_logical(annot_coxph, len = 1)
  checkmate::assert_subset(annot_stats, c("median", "min"))
  checkmate::assert_logical(annot_stats_vlines)
  checkmate::assert_list(control_coxph_pw)
  checkmate::assert_character(ref_group_coxph, len = 1, null.ok = TRUE)
  checkmate::assert_list(control_annot_surv_med)
  checkmate::assert_list(control_annot_coxph)
  checkmate::assert_numeric(legend_pos, finite = TRUE, any.missing = FALSE, len = 2, null.ok = TRUE)
  assert_proportion_value(rel_height_plot)
  checkmate::assert_logical(as_list)

  tte <- variables$tte
  is_event <- variables$is_event
  arm <- variables$arm
  assert_valid_factor(df[[arm]])
  armval <- as.character(unique(df[[arm]]))
  assert_df_with_variables(df, list(tte = tte, is_event = is_event, arm = arm))
  checkmate::assert_logical(df[[is_event]], min.len = 1)
  checkmate::assert_numeric(df[[tte]], min.len = 1)
  checkmate::assert_vector(col, len = length(armval), null.ok = TRUE)
  checkmate::assert_vector(lty, null.ok = TRUE)
  checkmate::assert_numeric(lwd, len = 1, null.ok = TRUE)

  if (annot_coxph && length(armval) < 2) {
    stop(paste(
      "When `annot_coxph` = TRUE, `df` must contain at least 2 levels of `variables$arm`",
      "in order to calculate the hazard ratio."
    ))
  }

  # process model
  yval <- match.arg(yval)
  formula <- stats::as.formula(paste0("survival::Surv(", tte, ", ", is_event, ") ~ ", arm))
  fit_km <- survival::survfit(
    formula = formula,
    data = df,
    conf.int = control_surv$conf_level,
    conf.type = control_surv$conf_type
  )
  data <- h_data_plot(fit_km, armval = armval, max_time = max_time)

  # calculate x-ticks
  xticks <- h_xticks(data = data, xticks = xticks, max_time = max_time)

  # change estimates of survival to estimates of failure (1 - survival)
  if (yval == "Failure") {
    data[c("estimate", "conf.low", "conf.high", "censor")] <- list(
      1 - data$estimate, 1 - data$conf.low, 1 - data$conf.high, 1 - data$censor
    )
  }

  # derive y-axis limits
  if (is.null(ylim)) {
    if (!is.null(max_time)) {
      y_lwr <- min(data[data$time < max_time, ][["estimate"]])
      y_upr <- max(data[data$time < max_time, ][["estimate"]])
    } else {
      y_lwr <- min(data[["estimate"]])
      y_upr <- max(data[["estimate"]])
    }
    ylim <- c(y_lwr, y_upr)
  }

  # initialize ggplot
  gg_plt <- ggplot(
    data = data,
    mapping = aes(
      x = .data[["time"]],
      y = .data[["estimate"]],
      ymin = .data[["conf.low"]],
      ymax = .data[["conf.high"]],
      color = .data[["strata"]],
      fill = .data[["strata"]]
    )
  ) +
    theme_bw(base_size = font_size) +
    scale_y_continuous(limits = ylim, expand = c(0.025, 0)) +
    labs(title = title, x = xlab, y = ylab, caption = footnotes) +
    theme(
      axis.text = element_text(size = font_size),
      axis.title = element_text(size = font_size),
      legend.title = element_blank(),
      legend.text = element_text(size = font_size),
      legend.box.background = element_rect(fill = "white", linewidth = 0.5),
      legend.background = element_blank(),
      legend.position = "inside",
      legend.spacing.y = unit(-0.02, "npc"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  # derive x-axis limits
  if (!is.null(max_time) && !is.null(xticks)) {
    gg_plt <- gg_plt + scale_x_continuous(
      breaks = xticks, limits = c(min(0, xticks), max(c(xticks, max_time))), expand = c(0.025, 0)
    )
  } else if (!is.null(xticks)) {
    if (max(data$time) <= max(xticks)) {
      gg_plt <- gg_plt + scale_x_continuous(
        breaks = xticks, limits = c(min(0, min(xticks)), max(xticks)), expand = c(0.025, 0)
      )
    } else {
      gg_plt <- gg_plt + scale_x_continuous(breaks = xticks, expand = c(0.025, 0))
    }
  } else if (!is.null(max_time)) {
    gg_plt <- gg_plt + scale_x_continuous(limits = c(0, max_time), expand = c(0.025, 0))
  }

  # set legend position
  if (!is.null(legend_pos)) {
    gg_plt <- gg_plt + theme(legend.position.inside = legend_pos)
  } else {
    max_time2 <- sort(
      data$time,
      partial = nrow(data) - length(armval) - 1
    )[nrow(data) - length(armval) - 1]

    y_rng <- ylim[2] - ylim[1]

    if (yval == "Survival" && all(data$estimate[data$time == max_time2] > ylim[1] + 0.09 * y_rng) &&
      all(data$estimate[data$time == max_time2] < ylim[1] + 0.5 * y_rng)) { # nolint
      gg_plt <- gg_plt +
        theme(
          legend.position.inside = c(1, 0.5),
          legend.justification = c(1.1, 0.6)
        )
    } else {
      gg_plt <- gg_plt +
        theme(
          legend.position.inside = c(1, 0),
          legend.justification = c(1.1, -0.4)
        )
    }
  }

  # add lines
  gg_plt <- if (is.null(lty)) {
    gg_plt + geom_step(linewidth = lwd, na.rm = TRUE)
  } else if (length(lty) == 1) {
    gg_plt + geom_step(linewidth = lwd, lty = lty, na.rm = TRUE)
  } else {
    gg_plt +
      geom_step(aes(lty = .data[["strata"]]), linewidth = lwd, na.rm = TRUE) +
      scale_linetype_manual(values = lty)
  }

  # add censor marks
  if (censor_show) {
    gg_plt <- gg_plt + geom_point(
      data = data[data$n.censor != 0, ],
      aes(x = .data[["time"]], y = .data[["censor"]], shape = "Censored"),
      size = size,
      na.rm = TRUE
    ) +
      scale_shape_manual(name = NULL, values = pch) +
      guides(fill = guide_legend(override.aes = list(shape = NA)))
  }

  # add ci ribbon
  if (ci_ribbon) gg_plt <- gg_plt + geom_ribbon(alpha = 0.3, lty = 0, na.rm = TRUE)

  # control aesthetics
  if (!is.null(col)) {
    gg_plt <- gg_plt +
      scale_color_manual(values = col) +
      scale_fill_manual(values = col)
  }
  if (!is.null(ggtheme)) gg_plt <- gg_plt + ggtheme

  # annotate with stats (text/vlines)
  if (!is.null(annot_stats)) {
    if ("median" %in% annot_stats) {
      fit_km_all <- survival::survfit(
        formula = stats::as.formula(paste0("survival::Surv(", tte, ", ", is_event, ") ~ ", 1)),
        data = df,
        conf.int = control_surv$conf_level,
        conf.type = control_surv$conf_type
      )
      gg_plt <- gg_plt +
        annotate(
          "text",
          size = font_size / .pt, col = 1, lineheight = 0.95,
          x = stats::median(fit_km_all) + 0.07 * max(data$time),
          y = ifelse(yval == "Survival", 0.65, 0.35),
          label = paste("Median F/U:\n", round(stats::median(fit_km_all), 1), tolower(df$AVALU[1]))
        )
      if (annot_stats_vlines) {
        gg_plt <- gg_plt +
          annotate(
            "segment",
            x = stats::median(fit_km_all), xend = stats::median(fit_km_all), y = -Inf, yend = Inf,
            linetype = 2, col = "darkgray"
          )
      }
    }
    if ("min" %in% annot_stats) {
      min_fu <- min(df[[tte]])
      gg_plt <- gg_plt +
        annotate(
          "text",
          size = font_size / .pt, col = 1, lineheight = 0.95,
          x = min_fu + max(data$time) * 0.07,
          y = ifelse(yval == "Survival", 0.96, 0.05),
          label = paste("Min. F/U:\n", round(min_fu, 1), tolower(df$AVALU[1]))
        )
      if (annot_stats_vlines) {
        gg_plt <- gg_plt +
          annotate(
            "segment",
            linetype = 2, col = "darkgray",
            x = min_fu, xend = min_fu, y = Inf, yend = -Inf
          )
      }
    }
    gg_plt <- gg_plt + guides(fill = guide_legend(override.aes = list(shape = NA, label = "")))
  }

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

    gg_at_risk <- df2gg(
      at_risk_tbl,
      font_size = font_size, col_labels = FALSE, hline = FALSE,
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
        axis.text.y = element_text(size = font_size, face = "italic", hjust = 1),
        axis.text.x = element_text(size = font_size),
        axis.line.x = element_line()
      ) +
      coord_cartesian(clip = "off", ylim = c(0.5, nrow(at_risk_tbl)))
    gg_at_risk <- suppressMessages(
      gg_at_risk +
        scale_x_continuous(expand = c(0.025, 0), breaks = seq_along(at_risk_tbl) - 0.5, labels = xticks) +
        scale_y_continuous(labels = rev(levels(annot_tbl$strata)), breaks = seq_len(nrow(at_risk_tbl)))
    )

    if (!as_list) {
      gg_plt <- cowplot::plot_grid(
        gg_plt,
        gg_at_risk,
        align = "v",
        axis = "tblr",
        ncol = 1,
        rel_heights = c(rel_height_plot, 1 - rel_height_plot)
      )
    }
  }

  # add median survival time annotation table
  if (annot_surv_med) {
    surv_med_tbl <- h_tbl_median_surv(fit_km = fit_km, armval = armval)
    bg_fill <- if (isTRUE(control_annot_surv_med[["fill"]])) "#00000020" else control_annot_surv_med[["fill"]]

    gg_surv_med <- df2gg(surv_med_tbl, font_size = font_size, colwidths = c(1, 1, 2), bg_fill = bg_fill) +
      theme(
        axis.text.y = element_text(size = font_size, face = "italic", hjust = 1),
        plot.margin = margin(0, 2, 0, 5)
      ) +
      coord_cartesian(clip = "off", ylim = c(0.5, nrow(surv_med_tbl) + 1.5))
    gg_surv_med <- suppressMessages(
      gg_surv_med +
        scale_x_continuous(expand = c(0.025, 0)) +
        scale_y_continuous(labels = rev(rownames(surv_med_tbl)), breaks = seq_len(nrow(surv_med_tbl)))
    )

    gg_plt <- cowplot::ggdraw(gg_plt) +
      cowplot::draw_plot(
        gg_surv_med,
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
    bg_fill <- if (isTRUE(control_annot_coxph[["fill"]])) "#00000020" else control_annot_coxph[["fill"]]

    gg_coxph <- df2gg(coxph_tbl, font_size = font_size, colwidths = c(1.1, 1, 3), bg_fill = bg_fill) +
      theme(
        axis.text.y = element_text(size = font_size, face = "italic", hjust = 1),
        plot.margin = margin(0, 2, 0, 5)
      ) +
      coord_cartesian(clip = "off", ylim = c(0.5, nrow(coxph_tbl) + 1.5))
    gg_coxph <- suppressMessages(
      gg_coxph +
        scale_x_continuous(expand = c(0.025, 0)) +
        scale_y_continuous(labels = rev(rownames(coxph_tbl)), breaks = seq_len(nrow(coxph_tbl)))
    )

    gg_plt <- cowplot::ggdraw(gg_plt) +
      cowplot::draw_plot(
        gg_coxph,
        control_annot_coxph[["x"]],
        control_annot_coxph[["y"]],
        width = control_annot_coxph[["w"]],
        height = control_annot_coxph[["h"]],
        vjust = 0.5,
        hjust = 0.5
      )
  }

  if (as_list) {
    list(plot = gg_plt, table = gg_at_risk)
  } else {
    gg_plt
  }
}
