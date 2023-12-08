#' Survival Time Point Analysis
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Summarize patients' survival rate and difference of survival rates between groups at a time point.
#'
#' @inheritParams argument_convention
#' @inheritParams s_surv_time
#' @param time_point (`number`)\cr survival time point of interest.
#' @param control (`list`)\cr parameters for comparison details, specified by using the helper function
#'   [control_surv_timepoint()]. Some possible parameter options are:
#'   * `conf_level` (`proportion`)\cr confidence level of the interval for survival rate.
#'   * `conf_type` (`string`)\cr confidence interval type. Options are "plain" (default), "log", "log-log",
#'     see more in [survival::survfit()]. Note option "none" is no longer supported.
#'   * `time_point` (`number`)\cr survival time point of interest.
#' @param method (`string`)\cr either `surv` (survival estimations),
#'   `surv_diff` (difference in survival with the control) or `both`.
#' @param table_names_suffix (`string`)\cr optional suffix for the `table_names` used for the `rtables` to
#'   avoid warnings from duplicate table names.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("surv_timepoint")`
#'   to see available statistics for this function.
#' @param .indent_mods (named `vector` of `integer`)\cr indent modifiers for the labels. Each element of the vector
#'   should be a name-value pair with name corresponding to a statistic specified in `.stats` and value the indentation
#'   for that statistic's row label.
#'
#' @name survival_timepoint
#' @order 1
NULL

#' @describeIn survival_timepoint Statistics function which analyzes survival rate.
#'
#' @return
#' * `s_surv_timepoint()` returns the statistics:
#'   * `pt_at_risk`: Patients remaining at risk.
#'   * `event_free_rate`: Event-free rate (%).
#'   * `rate_se`: Standard error of event free rate.
#'   * `rate_ci`: Confidence interval for event free rate.
#'
#' @keywords internal
s_surv_timepoint <- function(df,
                             .var,
                             time_point,
                             is_event,
                             control = control_surv_timepoint()) {
  checkmate::assert_string(.var)
  assert_df_with_variables(df, list(tte = .var, is_event = is_event))
  checkmate::assert_numeric(df[[.var]], min.len = 1, any.missing = FALSE)
  checkmate::assert_number(time_point)
  checkmate::assert_logical(df[[is_event]], min.len = 1, any.missing = FALSE)

  conf_type <- control$conf_type
  conf_level <- control$conf_level

  formula <- stats::as.formula(paste0("survival::Surv(", .var, ", ", is_event, ") ~ 1"))
  srv_fit <- survival::survfit(
    formula = formula,
    data = df,
    conf.int = conf_level,
    conf.type = conf_type
  )
  s_srv_fit <- summary(srv_fit, times = time_point, extend = TRUE)
  df_srv_fit <- as.data.frame(s_srv_fit[c("time", "n.risk", "surv", "lower", "upper", "std.err")])
  if (df_srv_fit[["n.risk"]] == 0) {
    pt_at_risk <- event_free_rate <- rate_se <- NA_real_
    rate_ci <- c(NA_real_, NA_real_)
  } else {
    pt_at_risk <- df_srv_fit$n.risk
    event_free_rate <- df_srv_fit$surv
    rate_se <- df_srv_fit$std.err
    rate_ci <- c(df_srv_fit$lower, df_srv_fit$upper)
  }
  list(
    pt_at_risk = formatters::with_label(pt_at_risk, "Patients remaining at risk"),
    event_free_rate = formatters::with_label(event_free_rate * 100, "Event Free Rate (%)"),
    rate_se = formatters::with_label(rate_se * 100, "Standard Error of Event Free Rate"),
    rate_ci = formatters::with_label(rate_ci * 100, f_conf_level(conf_level))
  )
}

#' @describeIn survival_timepoint Formatted analysis function which is used as `afun` in `surv_timepoint()`
#'   when `method = "surv"`.
#'
#' @return
#' * `a_surv_timepoint()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_surv_timepoint <- make_afun(
  s_surv_timepoint,
  .indent_mods = c(
    pt_at_risk = 0L,
    event_free_rate = 0L,
    rate_se = 1L,
    rate_ci = 1L
  ),
  .formats = c(
    pt_at_risk = "xx",
    event_free_rate = "xx.xx",
    rate_se = "xx.xx",
    rate_ci = "(xx.xx, xx.xx)"
  )
)

#' @describeIn survival_timepoint Statistics function which analyzes difference between two survival rates.
#'
#' @return
#' * `s_surv_timepoint_diff()` returns the statistics:
#'   * `rate_diff`: Event-free rate difference between two groups.
#'   * `rate_diff_ci`: Confidence interval for the difference.
#'   * `ztest_pval`: p-value to test the difference is 0.
#'
#' @keywords internal
s_surv_timepoint_diff <- function(df,
                                  .var,
                                  .ref_group,
                                  .in_ref_col,
                                  time_point,
                                  control = control_surv_timepoint(),
                                  ...) {
  if (.in_ref_col) {
    return(
      list(
        rate_diff = formatters::with_label("", "Difference in Event Free Rate"),
        rate_diff_ci = formatters::with_label("", f_conf_level(control$conf_level)),
        ztest_pval = formatters::with_label("", "p-value (Z-test)")
      )
    )
  }
  data <- rbind(.ref_group, df)
  group <- factor(rep(c("ref", "x"), c(nrow(.ref_group), nrow(df))), levels = c("ref", "x"))
  res_per_group <- lapply(split(data, group), function(x) {
    s_surv_timepoint(df = x, .var = .var, time_point = time_point, control = control, ...)
  })

  res_x <- res_per_group[[2]]
  res_ref <- res_per_group[[1]]
  rate_diff <- res_x$event_free_rate - res_ref$event_free_rate
  se_diff <- sqrt(res_x$rate_se^2 + res_ref$rate_se^2)

  qs <- c(-1, 1) * stats::qnorm(1 - (1 - control$conf_level) / 2)
  rate_diff_ci <- rate_diff + qs * se_diff
  ztest_pval <- if (is.na(rate_diff)) {
    NA
  } else {
    2 * (1 - stats::pnorm(abs(rate_diff) / se_diff))
  }
  list(
    rate_diff = formatters::with_label(rate_diff, "Difference in Event Free Rate"),
    rate_diff_ci = formatters::with_label(rate_diff_ci, f_conf_level(control$conf_level)),
    ztest_pval = formatters::with_label(ztest_pval, "p-value (Z-test)")
  )
}

#' @describeIn survival_timepoint Formatted analysis function which is used as `afun` in `surv_timepoint()`
#'   when `method = "surv_diff"`.
#'
#' @return
#' * `a_surv_timepoint_diff()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_surv_timepoint_diff <- make_afun(
  s_surv_timepoint_diff,
  .formats = c(
    rate_diff = "xx.xx",
    rate_diff_ci = "(xx.xx, xx.xx)",
    ztest_pval = "x.xxxx | (<0.0001)"
  )
)

#' @describeIn survival_timepoint Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `surv_timepoint()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_surv_timepoint()` and/or `s_surv_timepoint_diff()` to the table layout depending on
#'   the value of `method`.
#'
#' @examples
#' library(dplyr)
#'
#' adtte_f <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(
#'     AVAL = day2month(AVAL),
#'     is_event = CNSR == 0
#'   )
#'
#' # Survival at given time points.
#' basic_table() %>%
#'   split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
#'   add_colcounts() %>%
#'   surv_timepoint(
#'     vars = "AVAL",
#'     var_labels = "Months",
#'     is_event = "is_event",
#'     time_point = 7
#'   ) %>%
#'   build_table(df = adtte_f)
#'
#' # Difference in survival at given time points.
#' basic_table() %>%
#'   split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
#'   add_colcounts() %>%
#'   surv_timepoint(
#'     vars = "AVAL",
#'     var_labels = "Months",
#'     is_event = "is_event",
#'     time_point = 9,
#'     method = "surv_diff",
#'     .indent_mods = c("rate_diff" = 0L, "rate_diff_ci" = 2L, "ztest_pval" = 2L)
#'   ) %>%
#'   build_table(df = adtte_f)
#'
#' # Survival and difference in survival at given time points.
#' basic_table() %>%
#'   split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
#'   add_colcounts() %>%
#'   surv_timepoint(
#'     vars = "AVAL",
#'     var_labels = "Months",
#'     is_event = "is_event",
#'     time_point = 9,
#'     method = "both"
#'   ) %>%
#'   build_table(df = adtte_f)
#'
#' @export
#' @order 2
surv_timepoint <- function(lyt,
                           vars,
                           time_point,
                           is_event,
                           control = control_surv_timepoint(),
                           method = c("surv", "surv_diff", "both"),
                           na_str = default_na_str(),
                           nested = TRUE,
                           ...,
                           table_names_suffix = "",
                           var_labels = "Time",
                           show_labels = "visible",
                           .stats = c(
                             "pt_at_risk", "event_free_rate", "rate_ci",
                             "rate_diff", "rate_diff_ci", "ztest_pval"
                           ),
                           .formats = NULL,
                           .labels = NULL,
                           .indent_mods = if (method == "both") {
                             c(rate_diff = 1L, rate_diff_ci = 2L, ztest_pval = 2L)
                           } else {
                             c(rate_diff_ci = 1L, ztest_pval = 1L)
                           }) {
  method <- match.arg(method)
  checkmate::assert_string(table_names_suffix)

  extra_args <- list(time_point = time_point, is_event = is_event, control = control, ...)

  f <- list(
    surv = c("pt_at_risk", "event_free_rate", "rate_se", "rate_ci"),
    surv_diff = c("rate_diff", "rate_diff_ci", "ztest_pval")
  )
  .stats <- h_split_param(.stats, .stats, f = f)
  .formats <- h_split_param(.formats, names(.formats), f = f)
  .labels <- h_split_param(.labels, names(.labels), f = f)
  .indent_mods <- h_split_param(.indent_mods, names(.indent_mods), f = f)

  afun_surv <- make_afun(
    a_surv_timepoint,
    .stats = .stats$surv,
    .formats = .formats$surv,
    .labels = .labels$surv,
    .indent_mods = .indent_mods$surv
  )

  afun_surv_diff <- make_afun(
    a_surv_timepoint_diff,
    .stats = .stats$surv_diff,
    .formats = .formats$surv_diff,
    .labels = .labels$surv_diff,
    .indent_mods = .indent_mods$surv_diff
  )

  time_point <- extra_args$time_point

  for (i in seq_along(time_point)) {
    extra_args[["time_point"]] <- time_point[i]

    if (method %in% c("surv", "both")) {
      lyt <- analyze(
        lyt,
        vars,
        var_labels = paste(time_point[i], var_labels),
        table_names = paste0("surv_", time_point[i], table_names_suffix),
        show_labels = show_labels,
        afun = afun_surv,
        na_str = na_str,
        nested = nested,
        extra_args = extra_args
      )
    }

    if (method %in% c("surv_diff", "both")) {
      lyt <- analyze(
        lyt,
        vars,
        var_labels = paste(time_point[i], var_labels),
        table_names = paste0("surv_diff_", time_point[i], table_names_suffix),
        show_labels = ifelse(method == "both", "hidden", show_labels),
        afun = afun_surv_diff,
        na_str = na_str,
        nested = nested,
        extra_args = extra_args
      )
    }
  }
  lyt
}
