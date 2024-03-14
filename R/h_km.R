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

#' Helper function: x-tick positions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Calculate the positions of ticks on the x-axis. However, if `xticks` already
#' exists it is kept as is. It is based on the same function `ggplot2` relies on,
#' and is required in the graphic and the patient-at-risk annotation table.
#'
#' @inheritParams g_km
#' @inheritParams h_ggkm
#'
#' @return A vector of positions to use for x-axis ticks on a `ggplot` object.
#'
#' @examples
#' library(dplyr)
#' library(survival)
#'
#' data <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
#'   h_data_plot()
#'
#' h_xticks(data)
#' h_xticks(data, xticks = seq(0, 3000, 500))
#' h_xticks(data, xticks = 500)
#' h_xticks(data, xticks = 500, max_time = 6000)
#' h_xticks(data, xticks = c(0, 500), max_time = 300)
#' h_xticks(data, xticks = 500, max_time = 300)
#'
#' @export
h_xticks <- function(data, xticks = NULL, max_time = NULL) {
  if (is.null(xticks)) {
    if (is.null(max_time)) {
      labeling::extended(range(data$time)[1], range(data$time)[2], m = 5)
    } else {
      labeling::extended(range(data$time)[1], max(range(data$time)[2], max_time), m = 5)
    }
  } else if (checkmate::test_number(xticks)) {
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

#' Helper Function: Survival Estimations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Transform a survival fit to a table with groups in rows characterized by N, median and confidence interval.
#'
#' @inheritParams h_data_plot
#'
#' @return A summary table with statistics `N`, `Median`, and `XX% CI` (`XX` taken from `fit_km`).
#'
#' @examples
#' library(dplyr)
#' library(survival)
#'
#' adtte <- tern_ex_adtte %>% filter(PARAMCD == "OS")
#' fit <- survfit(
#'   formula = Surv(AVAL, 1 - CNSR) ~ ARMCD,
#'   data = adtte
#' )
#' h_tbl_median_surv(fit_km = fit)
#'
#' @export
h_tbl_median_surv <- function(fit_km, armval = "All") {
  y <- if (is.null(fit_km$strata)) {
    as.data.frame(t(summary(fit_km)$table), row.names = armval)
  } else {
    tbl <- summary(fit_km)$table
    rownames_lst <- strsplit(sub("=", "equals", rownames(tbl)), "equals")
    rownames(tbl) <- matrix(unlist(rownames_lst), ncol = 2, byrow = TRUE)[, 2]
    as.data.frame(tbl)
  }
  conf.int <- summary(fit_km)$conf.int # nolint
  y$records <- round(y$records)
  y$median <- signif(y$median, 4)
  y$`CI` <- paste0(
    "(", signif(y[[paste0(conf.int, "LCL")]], 4), ", ", signif(y[[paste0(conf.int, "UCL")]], 4), ")"
  )
  stats::setNames(
    y[c("records", "median", "CI")],
    c("N", "Median", f_conf_level(conf.int))
  )
}

#' Helper Function: Pairwise Cox-PH table
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Create a `data.frame` of pairwise stratified or unstratified Cox-PH analysis results.
#'
#' @inheritParams g_km
#' @param annot_coxph_ref_lbls (`flag`)\cr whether the reference group should be explicitly printed in labels for the
#'   `annot_coxph` table. If `FALSE` (default), only comparison groups will be printed in `annot_coxph` table labels.
#'
#' @return A `data.frame` containing statistics `HR`, `XX% CI` (`XX` taken from `control_coxph_pw`),
#'   and `p-value (log-rank)`.
#'
#' @examples
#' library(dplyr)
#'
#' adtte <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(is_event = CNSR == 0)
#'
#' h_tbl_coxph_pairwise(
#'   df = adtte,
#'   variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
#'   control_coxph_pw = control_coxph(conf_level = 0.9)
#' )
#'
#' @export
h_tbl_coxph_pairwise <- function(df,
                                 variables,
                                 ref_group_coxph = NULL,
                                 control_coxph_pw = control_coxph(),
                                 annot_coxph_ref_lbls = FALSE) {
  if ("strat" %in% names(variables)) {
    warning(
      "Warning: the `strat` element name of the `variables` list argument to `h_tbl_coxph_pairwise() ",
      "was deprecated in tern 0.9.3.\n  ",
      "Please use the name `strata` instead of `strat` in the `variables` argument."
    )
    variables[["strata"]] <- variables[["strat"]]
  }

  assert_df_with_variables(df, variables)
  checkmate::assert_choice(ref_group_coxph, levels(df[[variables$arm]]), null.ok = TRUE)
  checkmate::assert_flag(annot_coxph_ref_lbls)

  arm <- variables$arm
  df[[arm]] <- factor(df[[arm]])

  ref_group <- if (!is.null(ref_group_coxph)) ref_group_coxph else levels(df[[variables$arm]])[1]
  comp_group <- setdiff(levels(df[[arm]]), ref_group)

  results <- Map(function(comp) {
    res <- s_coxph_pairwise(
      df = df[df[[arm]] == comp, , drop = FALSE],
      .ref_group = df[df[[arm]] == ref_group, , drop = FALSE],
      .in_ref_col = FALSE,
      .var = variables$tte,
      is_event = variables$is_event,
      strata = variables$strata,
      control = control_coxph_pw
    )
    res_df <- data.frame(
      hr = format(round(res$hr, 2), nsmall = 2),
      hr_ci = paste0(
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
  if (annot_coxph_ref_lbls) names(results) <- paste(comp_group, "vs.", ref_group)

  do.call(rbind, results)
}

## Deprecated Functions ----

#' Helper function: tidy survival fit
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Convert the survival fit data into a data frame designed for plotting
#' within `g_km`.
#'
#' This starts from the [broom::tidy()] result, and then:
#'   * Post-processes the `strata` column into a factor.
#'   * Extends each stratum by an additional first row with time 0 and probability 1 so that
#'     downstream plot lines start at those coordinates.
#'   * Adds a `censor` column.
#'   * Filters the rows before `max_time`.
#'
#' @inheritParams g_km
#' @param fit_km (`survfit`)\cr result of [survival::survfit()].
#' @param armval (`string`)\cr used as strata name when treatment arm variable only has one level. Default is `"All"`.
#'
#' @return A `tibble` with columns `time`, `n.risk`, `n.event`, `n.censor`, `estimate`, `std.error`, `conf.high`,
#'   `conf.low`, `strata`, and `censor`.
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(survival)
#'
#' # Test with multiple arms
#' tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
#'   h_data_plot()
#'
#' # Test with single arm
#' tern_ex_adtte %>%
#'   filter(PARAMCD == "OS", ARMCD == "ARM B") %>%
#'   survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
#'   h_data_plot(armval = "ARM B")
#' }
#'
#' @export
h_data_plot <- function(fit_km,
                        armval = "All",
                        max_time = NULL) {
  lifecycle::deprecate_warn(
    "0.9.4",
    "h_data_plot()",
    details = "`g_km` now generates `ggplot` objects. This function is no longer used within `tern`."
  )

  y <- broom::tidy(fit_km)

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
      first_row$n.risk <- sum(first_row[, c("n.risk", "n.event", "n.censor")])
      first_row$n.event <- first_row$n.censor <- 0
      first_row$estimate <- first_row$conf.high <- first_row$conf.low <- 1
      first_row$std.error <- 0
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

#' Helper function: KM plot
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Draw the Kaplan-Meier plot using `ggplot2`.
#'
#' @inheritParams g_km
#' @param data (`data.frame`)\cr survival data as pre-processed by `h_data_plot`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(survival)
#'
#' fit_km <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#' data_plot <- h_data_plot(fit_km = fit_km)
#' xticks <- h_xticks(data = data_plot)
#' gg <- h_ggkm(
#'   data = data_plot,
#'   censor_show = TRUE,
#'   xticks = xticks,
#'   xlab = "Days",
#'   yval = "Survival",
#'   ylab = "Survival Probability",
#'   title = "Survival"
#' )
#' gg
#' }
#'
#' @export
h_ggkm <- function(data,
                   xticks = NULL,
                   yval = "Survival",
                   censor_show,
                   xlab,
                   ylab,
                   ylim = NULL,
                   title,
                   footnotes = NULL,
                   max_time = NULL,
                   lwd = 1,
                   lty = NULL,
                   pch = 3,
                   size = 2,
                   col = NULL,
                   ci_ribbon = FALSE,
                   ggtheme = nestcolor::theme_nest()) {
  lifecycle::deprecate_warn(
    "0.9.4",
    "h_ggkm()",
    details = "`g_km` now generates `ggplot` objects. This function is no longer used within `tern`."
  )
  checkmate::assert_numeric(lty, null.ok = TRUE)
  checkmate::assert_character(col, null.ok = TRUE)

  if (is.null(ylim)) {
    data_lims <- data
    if (yval == "Failure") data_lims[["estimate"]] <- 1 - data_lims[["estimate"]]
    if (!is.null(max_time)) {
      y_lwr <- min(data_lims[data_lims$time < max_time, ][["estimate"]])
      y_upr <- max(data_lims[data_lims$time < max_time, ][["estimate"]])
    } else {
      y_lwr <- min(data_lims[["estimate"]])
      y_upr <- max(data_lims[["estimate"]])
    }
    ylim <- c(y_lwr, y_upr)
  }
  checkmate::assert_numeric(ylim, finite = TRUE, any.missing = FALSE, len = 2, sorted = TRUE)

  # change estimates of survival to estimates of failure (1 - survival)
  if (yval == "Failure") {
    data$estimate <- 1 - data$estimate
    data[c("conf.high", "conf.low")] <- list(1 - data$conf.low, 1 - data$conf.high)
    data$censor <- 1 - data$censor
  }

  gg <- {
    ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(
        x = .data[["time"]],
        y = .data[["estimate"]],
        ymin = .data[["conf.low"]],
        ymax = .data[["conf.high"]],
        color = .data[["strata"]],
        fill = .data[["strata"]]
      )
    ) +
      ggplot2::geom_hline(yintercept = 0)
  }

  if (ci_ribbon) {
    gg <- gg + ggplot2::geom_ribbon(alpha = .3, lty = 0)
  }

  gg <- if (is.null(lty)) {
    gg +
      ggplot2::geom_step(linewidth = lwd)
  } else if (checkmate::test_number(lty)) {
    gg +
      ggplot2::geom_step(linewidth = lwd, lty = lty)
  } else if (is.numeric(lty)) {
    gg +
      ggplot2::geom_step(mapping = ggplot2::aes(linetype = .data[["strata"]]), linewidth = lwd) +
      ggplot2::scale_linetype_manual(values = lty)
  }

  gg <- gg +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::labs(x = xlab, y = ylab, title = title, caption = footnotes)

  if (!is.null(col)) {
    gg <- gg +
      ggplot2::scale_color_manual(values = col) +
      ggplot2::scale_fill_manual(values = col)
  }
  if (censor_show) {
    dt <- data[data$n.censor != 0, ]
    dt$censor_lbl <- factor("Censored")

    gg <- gg + ggplot2::geom_point(
      data = dt,
      ggplot2::aes(
        x = .data[["time"]],
        y = .data[["censor"]],
        shape = .data[["censor_lbl"]]
      ),
      size = size,
      show.legend = TRUE,
      inherit.aes = TRUE
    ) +
      ggplot2::scale_shape_manual(name = NULL, values = pch) +
      ggplot2::guides(
        shape = ggplot2::guide_legend(override.aes = list(linetype = NA)),
        fill = ggplot2::guide_legend(override.aes = list(shape = NA))
      )
  }

  if (!is.null(max_time) && !is.null(xticks)) {
    gg <- gg + ggplot2::scale_x_continuous(breaks = xticks, limits = c(min(0, xticks), max(c(xticks, max_time))))
  } else if (!is.null(xticks)) {
    if (max(data$time) <= max(xticks)) {
      gg <- gg + ggplot2::scale_x_continuous(breaks = xticks, limits = c(min(0, min(xticks)), max(xticks)))
    } else {
      gg <- gg + ggplot2::scale_x_continuous(breaks = xticks)
    }
  } else if (!is.null(max_time)) {
    gg <- gg + ggplot2::scale_x_continuous(limits = c(0, max_time))
  }

  if (!is.null(ggtheme)) {
    gg <- gg + ggtheme
  }

  gg + ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    legend.key.height = unit(0.02, "npc"),
    panel.grid.major.x = ggplot2::element_line(linewidth = 2)
  )
}

#' `ggplot` Decomposition
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' The elements composing the `ggplot` are extracted and organized in a `list`.
#'
#' @param gg (`ggplot`)\cr a graphic to decompose.
#'
#' @return A named `list` with elements:
#'   * `panel`: The panel.
#'   * `yaxis`: The y-axis.
#'   * `xaxis`: The x-axis.
#'   * `xlab`: The x-axis label.
#'   * `ylab`: The y-axis label.
#'   * `guide`: The legend.
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(survival)
#' library(grid)
#'
#' fit_km <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#' data_plot <- h_data_plot(fit_km = fit_km)
#' xticks <- h_xticks(data = data_plot)
#' gg <- h_ggkm(
#'   data = data_plot,
#'   yval = "Survival",
#'   censor_show = TRUE,
#'   xticks = xticks, xlab = "Days", ylab = "Survival Probability",
#'   title = "tt",
#'   footnotes = "ff"
#' )
#'
#' g_el <- h_decompose_gg(gg)
#' grid::grid.newpage()
#' grid.rect(gp = grid::gpar(lty = 1, col = "red", fill = "gray85", lwd = 5))
#' grid::grid.draw(g_el$panel)
#'
#' grid::grid.newpage()
#' grid.rect(gp = grid::gpar(lty = 1, col = "royalblue", fill = "gray85", lwd = 5))
#' grid::grid.draw(with(g_el, cbind(ylab, yaxis)))
#' }
#'
#' @export
h_decompose_gg <- function(gg) {
  lifecycle::deprecate_warn(
    "0.9.4",
    "h_decompose_gg()",
    details = "`g_km` now generates `ggplot` objects. This function is no longer used within `tern`."
  )
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
#' @description `r lifecycle::badge("deprecated")`
#'
#' Prepares a (5 rows) x (2 cols) layout for the Kaplan-Meier curve.
#'
#' @inheritParams g_km
#' @inheritParams h_ggkm
#' @param g_el (`list` of `gtable`)\cr list as obtained by `h_decompose_gg()`.
#' @param annot_at_risk (`flag`)\cr compute and add the annotation table reporting the number of
#'   patient at risk matching the main grid of the Kaplan-Meier curve.
#'
#' @return A grid layout.
#'
#' @details The layout corresponds to a grid of two columns and five rows of unequal dimensions. Most of the
#'   dimension are fixed, only the curve is flexible and will accommodate with the remaining free space.
#'   * The left column gets the annotation of the `ggplot` (y-axis) and the names of the strata for the patient
#'     at risk tabulation. The main constraint is about the width of the columns which must allow the writing of
#'     the strata name.
#'   * The right column receive the `ggplot`, the legend, the x-axis and the patient at risk table.
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(survival)
#' library(grid)
#'
#' fit_km <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#' data_plot <- h_data_plot(fit_km = fit_km)
#' xticks <- h_xticks(data = data_plot)
#' gg <- h_ggkm(
#'   data = data_plot,
#'   censor_show = TRUE,
#'   xticks = xticks, xlab = "Days", ylab = "Survival Probability",
#'   title = "tt", footnotes = "ff", yval = "Survival"
#' )
#' g_el <- h_decompose_gg(gg)
#' lyt <- h_km_layout(data = data_plot, g_el = g_el, title = "t", footnotes = "f")
#' grid.show.layout(lyt)
#' }
#'
#' @export
h_km_layout <- function(data, g_el, title, footnotes, annot_at_risk = TRUE, annot_at_risk_title = TRUE) {
  lifecycle::deprecate_warn(
    "0.9.4",
    "h_km_layout()",
    details = "`g_km` now generates `ggplot` objects. This function is no longer used within `tern`."
  )
  txtlines <- levels(as.factor(data$strata))
  nlines <- nlevels(as.factor(data$strata))
  col_annot_width <- max(
    c(
      as.numeric(grid::convertX(g_el$yaxis$widths + g_el$ylab$widths, "pt")),
      as.numeric(
        grid::convertX(
          grid::stringWidth(txtlines) + grid::unit(7, "pt"), "pt"
        )
      )
    )
  )

  ttl_row <- as.numeric(!is.null(title))
  foot_row <- as.numeric(!is.null(footnotes))
  no_tbl_ind <- c()
  ht_x <- c()
  ht_units <- c()

  if (ttl_row == 1) {
    no_tbl_ind <- c(no_tbl_ind, TRUE)
    ht_x <- c(ht_x, 2)
    ht_units <- c(ht_units, "lines")
  }

  no_tbl_ind <- c(no_tbl_ind, rep(TRUE, 3), rep(FALSE, 2))
  ht_x <- c(
    ht_x,
    1,
    grid::convertX(with(g_el, xaxis$heights + ylab$widths), "pt") + grid::unit(5, "pt"),
    grid::convertX(g_el$guide$heights, "pt") + grid::unit(2, "pt"),
    1,
    nlines + 0.5,
    grid::convertX(with(g_el, xaxis$heights + ylab$widths), "pt")
  )
  ht_units <- c(
    ht_units,
    "null",
    "pt",
    "pt",
    "lines",
    "lines",
    "pt"
  )

  if (foot_row == 1) {
    no_tbl_ind <- c(no_tbl_ind, TRUE)
    ht_x <- c(ht_x, 1)
    ht_units <- c(ht_units, "lines")
  }
  if (annot_at_risk) {
    no_at_risk_tbl <- rep(TRUE, 6 + ttl_row + foot_row)
    if (!annot_at_risk_title) {
      no_at_risk_tbl[length(no_at_risk_tbl) - 2 - foot_row] <- FALSE
    }
  } else {
    no_at_risk_tbl <- no_tbl_ind
  }

  grid::grid.layout(
    nrow = sum(no_at_risk_tbl), ncol = 2,
    widths = grid::unit(c(col_annot_width, 1), c("pt", "null")),
    heights = grid::unit(
      x = ht_x[no_at_risk_tbl],
      units = ht_units[no_at_risk_tbl]
    )
  )
}

#' Helper: Patient-at-Risk Grobs
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Two graphical objects are obtained, one corresponding to row labeling and the second to the table of
#' numbers of patients at risk. If `title = TRUE`, a third object corresponding to the table title is
#' also obtained.
#'
#' @inheritParams g_km
#' @inheritParams h_ggkm
#' @param annot_tbl (`data.frame`)\cr annotation as prepared by [survival::summary.survfit()] which
#'   includes the number of patients at risk at given time points.
#' @param xlim (`numeric`)\cr the maximum value on the x-axis (used to
#'   ensure the at risk table aligns with the KM graph).
#' @param title (`flag`)\cr whether the "Patients at Risk" title should be added above the `annot_at_risk`
#'   table. Has no effect if `annot_at_risk` is `FALSE`. Defaults to `TRUE`.
#'
#' @return A named `list` of two `gTree` objects if `title = FALSE`: `at_risk` and `label`, or three
#'   `gTree` objects if `title = TRUE`: `at_risk`, `label`, and `title`.
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(survival)
#' library(grid)
#'
#' fit_km <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#'
#' data_plot <- h_data_plot(fit_km = fit_km)
#'
#' xticks <- h_xticks(data = data_plot)
#'
#' gg <- h_ggkm(
#'   data = data_plot,
#'   censor_show = TRUE,
#'   xticks = xticks, xlab = "Days", ylab = "Survival Probability",
#'   title = "tt", footnotes = "ff", yval = "Survival"
#' )
#'
#' # The annotation table reports the patient at risk for a given strata and
#' # times (`xticks`).
#' annot_tbl <- summary(fit_km, times = xticks)
#' if (is.null(fit_km$strata)) {
#'   annot_tbl <- with(annot_tbl, data.frame(n.risk = n.risk, time = time, strata = "All"))
#' } else {
#'   strata_lst <- strsplit(sub("=", "equals", levels(annot_tbl$strata)), "equals")
#'   levels(annot_tbl$strata) <- matrix(unlist(strata_lst), ncol = 2, byrow = TRUE)[, 2]
#'   annot_tbl <- data.frame(
#'     n.risk = annot_tbl$n.risk,
#'     time = annot_tbl$time,
#'     strata = annot_tbl$strata
#'   )
#' }
#'
#' # The annotation table is transformed into a grob.
#' tbl <- h_grob_tbl_at_risk(data = data_plot, annot_tbl = annot_tbl, xlim = max(xticks))
#'
#' # For the representation, the layout is estimated for which the decomposition
#' # of the graphic element is necessary.
#' g_el <- h_decompose_gg(gg)
#' lyt <- h_km_layout(data = data_plot, g_el = g_el, title = "t", footnotes = "f")
#'
#' grid::grid.newpage()
#' pushViewport(viewport(layout = lyt, height = .95, width = .95))
#' grid.rect(gp = grid::gpar(lty = 1, col = "purple", fill = "gray85", lwd = 1))
#' pushViewport(viewport(layout.pos.row = 3:4, layout.pos.col = 2))
#' grid.rect(gp = grid::gpar(lty = 1, col = "orange", fill = "gray85", lwd = 1))
#' grid::grid.draw(tbl$at_risk)
#' popViewport()
#' pushViewport(viewport(layout.pos.row = 3:4, layout.pos.col = 1))
#' grid.rect(gp = grid::gpar(lty = 1, col = "green3", fill = "gray85", lwd = 1))
#' grid::grid.draw(tbl$label)
#' }
#'
#' @export
h_grob_tbl_at_risk <- function(data, annot_tbl, xlim, title = TRUE) {
  lifecycle::deprecate_warn(
    "0.9.4",
    "h_grob_tbl_at_risk()",
    details = "`g_km` now generates `ggplot` objects. This function is no longer used within `tern`."
  )
  txtlines <- levels(as.factor(data$strata))
  nlines <- nlevels(as.factor(data$strata))
  y_int <- annot_tbl$time[2] - annot_tbl$time[1]
  annot_tbl <- expand.grid(
    time = seq(0, xlim, y_int),
    strata = unique(annot_tbl$strata)
  ) %>% dplyr::left_join(annot_tbl, by = c("time", "strata"))
  annot_tbl[is.na(annot_tbl)] <- 0
  y_str_unit <- as.numeric(annot_tbl$strata)
  vp_table <- grid::plotViewport(margins = grid::unit(c(0, 0, 0, 0), "lines"))
  if (title) {
    gb_table_title <- grid::gList(
      grid::textGrob(
        label = "Patients at Risk:",
        x = 1,
        y = grid::unit(0.2, "native"),
        gp = grid::gpar(fontface = "bold", fontsize = 10)
      )
    )
  }
  gb_table_left_annot <- grid::gList(
    grid::rectGrob(
      x = 0, y = grid::unit(c(1:nlines) - 1, "lines"),
      gp = grid::gpar(fill = c("gray95", "gray90"), alpha = 1, col = "white"),
      height = grid::unit(1, "lines"), just = "bottom", hjust = 0
    ),
    grid::textGrob(
      label = unique(annot_tbl$strata),
      x = 0.5,
      y = grid::unit(
        (max(unique(y_str_unit)) - unique(y_str_unit)) + 0.75,
        "native"
      ),
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
      y = grid::unit(
        (max(y_str_unit) - y_str_unit) + .5,
        "line"
      ) # maybe native
    )
  )

  ret <- list(
    at_risk = grid::gList(
      grid::gTree(
        vp = vp_table,
        children = grid::gList(
          grid::gTree(
            vp = grid::dataViewport(
              xscale = c(0, xlim) + c(-0.05, 0.05) * xlim,
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

  if (title) {
    ret[["title"]] <- grid::gList(
      grid::gTree(
        vp = grid::viewport(width = max(grid::stringWidth(txtlines))),
        children = grid::gList(
          grid::gTree(
            vp = grid::dataViewport(
              xscale = 0:1,
              yscale = c(0, 1),
              extension = c(0, 0)
            ),
            children = grid::gList(gb_table_title)
          )
        )
      )
    )
  }

  ret
}

#' Helper Function: Survival Estimation Grob
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' The survival fit is transformed in a grob containing a table with groups in
#' rows characterized by N, median and 95% confidence interval.
#'
#' @inheritParams g_km
#' @inheritParams h_data_plot
#' @param ttheme (`list`)\cr see [gridExtra::ttheme_default()].
#' @param x (`numeric`)\cr a value between 0 and 1 specifying x-location.
#' @param y (`numeric`)\cr a value between 0 and 1 specifying y-location.
#' @param width (`unit`)\cr width (as a unit) to use when printing the grob.
#'
#' @return A `grob` of a table containing statistics `N`, `Median`, and `XX% CI` (`XX` taken from `fit_km`).
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(survival)
#' library(grid)
#'
#' grid::grid.newpage()
#' grid.rect(gp = grid::gpar(lty = 1, col = "pink", fill = "gray85", lwd = 1))
#' tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .) %>%
#'   h_grob_median_surv() %>%
#'   grid::grid.draw()
#' }
#'
#' @export
h_grob_median_surv <- function(fit_km,
                               armval = "All",
                               x = 0.9,
                               y = 0.9,
                               width = grid::unit(0.3, "npc"),
                               ttheme = gridExtra::ttheme_default()) {
  lifecycle::deprecate_warn(
    "0.9.4",
    "h_grob_median_surv()",
    details = "`g_km` now generates `ggplot` objects. This function is no longer used within `tern`."
  )
  data <- h_tbl_median_surv(fit_km, armval = armval)

  width <- grid::convertUnit(grid::unit(as.numeric(width), grid::unitType(width)), "in")
  height <- width * (nrow(data) + 1) / 12

  w <- paste(" ", c(
    rownames(data)[which.max(nchar(rownames(data)))],
    sapply(names(data), function(x) c(x, data[[x]])[which.max(nchar(c(x, data[[x]])))])
  ))
  w_unit <- grid::convertWidth(grid::stringWidth(w), "in", valueOnly = TRUE)

  w_txt <- sapply(1:64, function(x) {
    graphics::par(ps = x)
    graphics::strwidth(w[4], units = "in")
  })
  f_size_w <- which.max(w_txt[w_txt < as.numeric((w_unit / sum(w_unit)) * width)[4]])

  h_txt <- sapply(1:64, function(x) {
    graphics::par(ps = x)
    graphics::strheight(grid::stringHeight("X"), units = "in")
  })
  f_size_h <- which.max(h_txt[h_txt < as.numeric(grid::unit(as.numeric(height) / 4, grid::unitType(height)))])

  if (ttheme$core$fg_params$fontsize == 12) {
    ttheme$core$fg_params$fontsize <- min(f_size_w, f_size_h)
    ttheme$colhead$fg_params$fontsize <- min(f_size_w, f_size_h)
    ttheme$rowhead$fg_params$fontsize <- min(f_size_w, f_size_h)
  }

  gt <- gridExtra::tableGrob(
    d = data,
    theme = ttheme
  )
  gt$widths <- ((w_unit / sum(w_unit)) * width)
  gt$heights <- rep(grid::unit(as.numeric(height) / 4, grid::unitType(height)), nrow(gt))

  vp <- grid::viewport(
    x = grid::unit(x, "npc") + grid::unit(1, "lines"),
    y = grid::unit(y, "npc") + grid::unit(1.5, "lines"),
    height = height,
    width = width,
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
#' @description `r lifecycle::badge("deprecated")`
#'
#' Build the y-axis annotation from a decomposed `ggplot`.
#'
#' @param ylab (`gtable`)\cr the y-lab as a graphical object derived from a `ggplot`.
#' @param yaxis (`gtable`)\cr the y-axis as a graphical object derived from a `ggplot`.
#'
#' @return a `gTree` object containing the y-axis annotation from a `ggplot`.
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(survival)
#' library(grid)
#'
#' fit_km <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   survfit(formula = Surv(AVAL, 1 - CNSR) ~ ARMCD, data = .)
#' data_plot <- h_data_plot(fit_km = fit_km)
#' xticks <- h_xticks(data = data_plot)
#' gg <- h_ggkm(
#'   data = data_plot,
#'   censor_show = TRUE,
#'   xticks = xticks, xlab = "Days", ylab = "Survival Probability",
#'   title = "title", footnotes = "footnotes", yval = "Survival"
#' )
#'
#' g_el <- h_decompose_gg(gg)
#'
#' grid::grid.newpage()
#' pvp <- grid::plotViewport(margins = c(5, 4, 2, 20))
#' pushViewport(pvp)
#' grid::grid.draw(h_grob_y_annot(ylab = g_el$ylab, yaxis = g_el$yaxis))
#' grid.rect(gp = grid::gpar(lty = 1, col = "gray35", fill = NA))
#' }
#'
#' @export
h_grob_y_annot <- function(ylab, yaxis) {
  lifecycle::deprecate_warn(
    "0.9.4",
    "h_grob_y_annot()",
    details = "`g_km` now generates `ggplot` objects. This function is no longer used within `tern`."
  )
  grid::gList(
    grid::gTree(
      vp = grid::viewport(
        width = grid::convertX(yaxis$widths + ylab$widths, "pt"),
        x = grid::unit(1, "npc"),
        just = "right"
      ),
      children = grid::gList(cbind(ylab, yaxis))
    )
  )
}

#' Helper Function: Cox-PH Grob
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Grob of `rtable` output from [h_tbl_coxph_pairwise()]
#'
#' @inheritParams h_grob_median_surv
#' @param ... arguments will be passed to [h_tbl_coxph_pairwise()].
#' @param x (`numeric`)\cr a value between 0 and 1 specifying x-location.
#' @param y (`numeric`)\cr a value between 0 and 1 specifying y-location.
#' @param width (`unit`)\cr width (as a unit) to use when printing the grob.
#'
#' @return A `grob` of a table containing statistics `HR`, `XX% CI` (`XX` taken from `control_coxph_pw`),
#'   and `p-value (log-rank)`.
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(survival)
#' library(grid)
#'
#' grid::grid.newpage()
#' grid.rect(gp = grid::gpar(lty = 1, col = "pink", fill = "gray85", lwd = 1))
#' data <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(is_event = CNSR == 0)
#' tbl_grob <- h_grob_coxph(
#'   df = data,
#'   variables = list(tte = "AVAL", is_event = "is_event", arm = "ARMCD"),
#'   control_coxph_pw = control_coxph(conf_level = 0.9), x = 0.5, y = 0.5
#' )
#' grid::grid.draw(tbl_grob)
#' }
#'
#' @export
h_grob_coxph <- function(...,
                         x = 0,
                         y = 0,
                         width = grid::unit(0.4, "npc"),
                         ttheme = gridExtra::ttheme_default(
                           padding = grid::unit(c(1, .5), "lines"),
                           core = list(bg_params = list(fill = c("grey95", "grey90"), alpha = .5))
                         )) {
  lifecycle::deprecate_warn(
    "0.9.4",
    "h_grob_coxph()",
    details = "`g_km` now generates `ggplot` objects. This function is no longer used within `tern`."
  )
  data <- h_tbl_coxph_pairwise(...)

  width <- grid::convertUnit(grid::unit(as.numeric(width), grid::unitType(width)), "in")
  height <- width * (nrow(data) + 1) / 12

  w <- paste("    ", c(
    rownames(data)[which.max(nchar(rownames(data)))],
    sapply(names(data), function(x) c(x, data[[x]])[which.max(nchar(c(x, data[[x]])))])
  ))
  w_unit <- grid::convertWidth(grid::stringWidth(w), "in", valueOnly = TRUE)

  w_txt <- sapply(1:64, function(x) {
    graphics::par(ps = x)
    graphics::strwidth(w[4], units = "in")
  })
  f_size_w <- which.max(w_txt[w_txt < as.numeric((w_unit / sum(w_unit)) * width)[4]])

  h_txt <- sapply(1:64, function(x) {
    graphics::par(ps = x)
    graphics::strheight(grid::stringHeight("X"), units = "in")
  })
  f_size_h <- which.max(h_txt[h_txt < as.numeric(grid::unit(as.numeric(height) / 4, grid::unitType(height)))])

  if (ttheme$core$fg_params$fontsize == 12) {
    ttheme$core$fg_params$fontsize <- min(f_size_w, f_size_h)
    ttheme$colhead$fg_params$fontsize <- min(f_size_w, f_size_h)
    ttheme$rowhead$fg_params$fontsize <- min(f_size_w, f_size_h)
  }

  tryCatch(
    expr = {
      gt <- gridExtra::tableGrob(
        d = data,
        theme = ttheme
      ) # ERROR 'data' must be of a vector type, was 'NULL'
      gt$widths <- ((w_unit / sum(w_unit)) * width)
      gt$heights <- rep(grid::unit(as.numeric(height) / 4, grid::unitType(height)), nrow(gt))
      vp <- grid::viewport(
        x = grid::unit(x, "npc") + grid::unit(1, "lines"),
        y = grid::unit(y, "npc") + grid::unit(1.5, "lines"),
        height = height,
        width = width,
        just = c("left", "bottom")
      )
      grid::gList(
        grid::gTree(
          vp = vp,
          children = grid::gList(gt)
        )
      )
    },
    error = function(w) {
      message(paste(
        "Warning: Cox table will not be displayed as there is",
        "not any level to be compared in the arm variable."
      ))
      return(
        grid::gList(
          grid::gTree(
            vp = NULL,
            children = NULL
          )
        )
      )
    }
  )
}
