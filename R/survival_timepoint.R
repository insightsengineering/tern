#' Survival Time Point Analysis
#'
#' Summarize patient's survival rate and difference of survival rates between groups at a time point.
#'
#' @inheritParams argument_convention
#' @inheritParams s_surv_time
#' @param time_point (`number`) \cr survival time point of interest.
#' @param control a (`list`) of parameters for comparison details, specified by using \cr
#'    the helper function [control_surv_timepoint]. Some possible parameter options are: \cr
#' * `conf_level`: (`proportion`)\cr confidence level of the interval for survival rate.
#' * `conf_type`: (`string`) \cr "plain" (default), "log", "log-log" for confidence interval type, \cr
#'    see more in [survival::survfit()]. Note that the option "none" is no longer supported.
#' * `time_point`: (`number`) \cr survival time point of interest.
#'
#' @name survival_timepoint
#'
NULL

#' @describeIn survival_timepoint Statistics Function which analyzes survival rate.
#'
#' @export
#'
#' @return The statistics are:
#' * `pt_at_risk` : patients remaining at risk.
#' * `event_free_rate` : event free rate (%).
#' * `rate_se` : standard error of event free rate.
#' * `rate_ci` : confidence interval for event free rate.
#' @examples
#' library(scda)
#' library(dplyr)
#'
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#' ADTTE_f <- ADTTE %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(
#'     AVAL = day2month(AVAL),
#'     is_event = CNSR == 0
#'   )
#' df <- ADTTE_f %>%
#'   filter(ARMCD == "ARM A")
#'
#' s_surv_timepoint(df, .var = "AVAL", time_point = 7, is_event = "is_event")
s_surv_timepoint <- function(df,
                             .var,
                             time_point,
                             is_event,
                             control = control_surv_timepoint()) {
  assertthat::assert_that(
    is_df_with_variables(df, list(tte = .var, is_event = is_event)),
    assertthat::is.string(.var),
    utils.nest::is_numeric_vector(df[[.var]]),
    utils.nest::is_numeric_single(time_point),
    utils.nest::is_logical_vector(df[[is_event]])
  )
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
    pt_at_risk = with_label(pt_at_risk, "Patients remaining at risk"),
    event_free_rate = with_label(event_free_rate * 100, "Event Free Rate (%)"),
    rate_se = with_label(rate_se * 100, "Standard Error of Event Free Rate"),
    rate_ci = with_label(rate_ci * 100, f_conf_level(conf_level))
  )
}

#' @describeIn survival_timepoint Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' a_surv_timepoint(df, .var = "AVAL", time_point = 7, is_event = "is_event")
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

#' @describeIn survival_timepoint Statistics Function which analyzes difference between two survival rates.
#' @return The statistics are:
#' * `rate_diff` : event free rate difference between two groups.
#' * `rate_diff_ci` : confidence interval for the difference.
#' * `ztest_pval` : p-value to test the difference is 0.
#' @export
#' @examples
#' df_ref_group <- ADTTE_f %>%
#'   filter(ARMCD == "ARM B")
#'
#' s_surv_timepoint_diff(df, df_ref_group, .in_ref_col = TRUE, .var = "AVAL", is_event = "is_event")
#' s_surv_timepoint_diff(
#'   df,
#'   df_ref_group,
#'   .in_ref_col = FALSE,
#'   .var = "AVAL",
#'   time_point = 7,
#'   is_event = "is_event"
#' )
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
        rate_diff = with_label("", "Difference in Event Free Rate"),
        rate_diff_ci = with_label("", f_conf_level(control$conf_level)),
        ztest_pval = with_label("", "p-value (Z-test)")
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
    rate_diff = with_label(rate_diff, "Difference in Event Free Rate"),
    rate_diff_ci = with_label(rate_diff_ci, f_conf_level(control$conf_level)),
    ztest_pval = with_label(ztest_pval, "p-value (Z-test)")
  )
}

#' @describeIn survival_timepoint Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' a_surv_timepoint_diff(
#'   df,
#'   df_ref_group,
#'   .in_ref_col = FALSE,
#'   .var = "AVAL",
#'   time_point = 7,
#'   is_event = "is_event"
#' )
a_surv_timepoint_diff <- make_afun(
  s_surv_timepoint_diff,
  .indent_mods = c(
    rate_diff = 1L,
    rate_diff_ci = 2L,
    ztest_pval = 2L
  ),
  .formats = c(
    rate_diff = "xx.xx",
    rate_diff_ci = "(xx.xx, xx.xx)",
    ztest_pval = "x.xxxx | (<0.0001)"
  )
)

#' @describeIn survival_timepoint Analyze Function which adds the survival rate analysis to the input layout.
#'   Note that additional formatting arguments can be used here.
#' @inheritParams argument_convention
#' @param method (`string`)\cr either `surv` (survival estimations),
#'   `surv_diff` (difference in survival with the control) or `both`.
#' @param table_names_suffix (`string`)\cr optional suffix for the `table_names` used for the rtables to
#'   avoid warnings from duplicate table names.
#' @export
#' @examples
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
#'   build_table(df = ADTTE_f)
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
#'   build_table(df = ADTTE_f)
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
#'   build_table(df = ADTTE_f)
surv_timepoint <- function(lyt,
                           vars,
                           ...,
                           table_names_suffix = "",
                           var_labels = "Time",
                           show_labels = "visible",
                           method = c("surv", "surv_diff", "both"),
                           .stats = c(
                             "pt_at_risk", "event_free_rate", "rate_ci",
                             "rate_diff", "rate_diff_ci", "ztest_pval"
                           ),
                           .formats = NULL,
                           .labels = NULL,
                           .indent_mods = NULL) {
  method <- match.arg(method)
  assertthat::assert_that(assertthat::is.string(table_names_suffix))

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

  time_point <- list(...)$time_point

  for (i in seq_along(time_point)) {
    tpt <- time_point[i]

    if (method %in% c("surv", "both")) {
      lyt <- analyze(
        lyt,
        vars,
        var_labels = paste(tpt, var_labels),
        table_names = paste0("surv_", tpt, table_names_suffix),
        show_labels = show_labels,
        afun = afun_surv,
        extra_args = list(
          is_event = list(...)$is_event,
          control = list(...)$control,
          time_point = tpt
        )
      )
    }

    if (method %in% c("surv_diff", "both")) {
      lyt <- analyze(
        lyt,
        vars,
        var_labels = paste(tpt, var_labels),
        table_names = paste0("surv_diff_", tpt, table_names_suffix),
        show_labels = ifelse(method == "both", "hidden", show_labels),
        afun = afun_surv_diff,
        extra_args = list(
          is_event = list(...)$is_event,
          control = list(...)$control,
          time_point = tpt
        )
      )
    }
  }
  lyt
}
