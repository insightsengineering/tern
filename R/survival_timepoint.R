#' Survival Time Point Analysis
#'
#' Summarize patient's survival rate and difference of survival rates between groups at a time point.
#'
#' @md
#' @inheritParams argument_convention
#' @inheritParams s_surv_time
#' @param control a (`list`) of parameters for comparison details, specified by using \cr
#'    the helper function [control_surv_timepoint]. Some possible parameter options are: \cr
#' * `conf_level`: (`proportion`)\cr confidence level of the interval for survival rate.
#' * `conf_type`: (`string`) \cr "plain" (default), "none", "log", "log-log" for confidence interval type, \cr
#'    see more in [survival::survfit()]
#' * `time_point`: (`number`) \cr survival time point of interest.
#'
#' @template formatting_arguments
#'
#' @name survival_timepoint
#'
NULL

#' @describeIn survival_timepoint Statistics Function which analyzes survival rate.
#' @export
#' @return The statistics are:
#' * `pt_at_risk` : patients remaining at risk.
#' * `event_free_rate` : event free rate (%).
#' * `rate_se` : standard error of event free rate.
#' * `rate_ci` : confidence interval for event free rate.
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' ADTTE <- radtte(cached = TRUE)
#' ADTTE_f <- ADTTE %>%
#'  dplyr::filter(PARAMCD == "OS") %>%
#'  dplyr::mutate(is_event = CNSR == 0)
#' df <- ADTTE_f %>% dplyr::filter(ARMCD == "ARM A")
#' s_surv_timepoint(df, .var = "AVAL", is_event = "is_event")
#'
s_surv_timepoint <- function(df,
                             .var,
                             is_event,
                             control = control_surv_timepoint()) {
  assert_that(
    is_df_with_variables(df, list(tte = .var, is_event = is_event)),
    is.string(.var),
    is_numeric_vector(df[[.var]]),
    is_logical_vector(df[[is_event]])
  )
  conf_type <- control$conf_type
  conf_level <- control$conf_level
  time_point <- control$time_point

  formula <- as.formula(paste0("survival::Surv(", .var, ", ", is_event, ") ~ 1"))
  srv_fit <- survival::survfit(
    formula = formula,
    data = df,
    conf.int = conf_level,
    conf.type = conf_type
  )
  s_srv_fit <- summary(srv_fit, times = time_point)
  df_srv_fit <- as.data.frame(s_srv_fit[c("time", "n.risk", "surv", "lower", "upper", "std.err")])
  if (dim(df_srv_fit)[1] == 0) {
    pt_at_risk <- event_free_rate <- rate_se <- NA
    rate_ci <- c(NA, NA)
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

#' @describeIn survival_timepoint Analyze Function which adds the survival rate analysis to the input layout.
#'   Note that additional formatting arguments can be used here.
#' @export
#' @examples
#'
#' split_cols_by(lyt = NULL, var = "ARMCD", ref_group = "ARM A") %>%
#'  add_colcounts() %>%
#'  surv_timepoint(
#'    vars = "AVAL",
#'    var_labels = "7 Months",
#'    is_event = "is_event",
#'    control = control_surv_timepoint(time_point = 7)
#'  ) %>%
#'  build_table(df = ADTTE_f)
#'

surv_timepoint  <- function(lyt,
                            vars,
                            var_labels = "Months",
                            .stats = c("pt_at_risk", "event_free_rate", "rate_ci"),
                            ...) {
  a_surv_timepoint <- format_wrap_df(
    s_surv_timepoint,
    indent_mods = c("pt_at_risk" = 0L, "event_free_rate" = 0L, "rate_ci" = 2L),
    formats = c("pt_at_risk" = "xx", "event_free_rate" = "xx.xx", "rate_ci" = "(xx.xx, xx.xx)")
  )
  analyze(
    lyt,
    vars,
    var_labels = var_labels,
    show_labels = "visible",
    afun = a_surv_timepoint,
    extra_args = c(list(.stats = .stats), list(...))
  )
}


#' @describeIn survival_timepoint Statistics Function which analyzes difference between two survival rates.
#' @param conf_level confidence level for difference of two risk rates
#' @return The statistics are:
#' * `rate_diff` : event free rate difference between two groups.
#' * `rate_diff_ci` : confidence interval for the difference.
#' * `ztest_pval` : p-value to test the difference is 0.
#' @export
#' @examples
#'
#' df_ref_group <- ADTTE_f %>% dplyr::filter(ARMCD == "ARM B")
#' s_surv_timepoint_diff(df, df_ref_group, .in_ref_col = TRUE, .var = "AVAL", is_event = "is_event")
#' s_surv_timepoint_diff(df, df_ref_group, .in_ref_col = FALSE, .var = "AVAL", is_event = "is_event")
#'

s_surv_timepoint_diff <- function(df,
                                  .var,
                                  .ref_group,
                                  .in_ref_col,
                                  conf_level = 0.95,
                                  ...) {
  if (.in_ref_col) {
    return(
      list(
        rate_diff = with_label("", "Difference in Event Free Rate"),
        rate_diff_ci = with_label("", f_conf_level(conf_level)),
        ztest_pval = with_label("", "p-value (Z-test)")
      )
    )
  }
  data <- rbind(.ref_group, df)
  group <- factor(rep(c("ref", "x"), c(nrow(.ref_group), nrow(df))), levels = c("ref", "x"))
  res_per_group <- lapply(split(data, group), function(x){
    s_surv_timepoint(df = x, .var = .var, ...)
  })

  res_x <- res_per_group[[2]]
  res_ref <- res_per_group[[1]]
  rate_diff <- if (length(res_x$event_free_rate) == 0 || length(res_ref$event_free_rate) == 0) {
    NA
  } else {
    res_x$event_free_rate - res_ref$event_free_rate
  }
  se_diff <- if (length(res_x$rate_se) == 0 || length(res_ref$rate_se) == 0) {
    NA
  } else {
    sqrt(res_x$rate_se ^ 2 + res_ref$rate_se ^ 2)
  }
  qs <- c(-1, 1) * stats::qnorm(1 - (1 - conf_level) / 2)
  rate_diff_ci <- rate_diff + qs * se_diff
  ztest_pval <- if (is.na(rate_diff)) {
    NA
  } else {
    2 * (1 - stats::pnorm(abs(rate_diff) / se_diff))
  }
  list(
    rate_diff = with_label(rate_diff, "Difference in Event Free Rate"),
    rate_diff_ci = with_label(rate_diff_ci, f_conf_level(conf_level)),
    ztest_pval = with_label(ztest_pval, "p-value (Z-test)")
  )
}

#' @describeIn survival_timepoint Analyze Function which adds the difference between two survival rates analysis
#'   to the input layout. Note that additional formatting arguments can be used here.
#' @export
#' @examples
#'
#' split_cols_by(lyt = NULL, var = "ARMCD", ref_group = "ARM A") %>%
#'  add_colcounts() %>%
#'  surv_timepoint_diff(
#'    vars = "AVAL",
#'    var_labels = "9 Months",
#'    is_event = "is_event",
#'    control = control_surv_timepoint(time_point = 9),
#'    .indent_mods = c("rate_diff" = 0L, "rate_diff_ci" = 2L, "ztest_pval" = 2L)
#'  ) %>%
#'  build_table(df = ADTTE_f)
#'
#' split_cols_by(lyt = NULL, var = "ARMCD", ref_group = "ARM A") %>%
#'  add_colcounts() %>%
#'  surv_timepoint(
#'    vars = "AVAL",
#'    var_labels = "7 Months",
#'    is_event = "is_event",
#'    control = control_surv_timepoint(time_point = 7)
#'  ) %>%
#'  surv_timepoint_diff(
#'    vars = "AVAL",
#'    show_label = "hidden",
#'    is_event = "is_event",
#'    control = control_surv_timepoint(time_point = 7)
#'  ) %>%
#'  build_table(df = ADTTE_f)
#'
surv_timepoint_diff <- function(lyt,
                                vars,
                                var_labels = "Time",
                                show_labels = "visible",
                                ...) {
  a_surv_timepoint_diff <- format_wrap_df(
    s_surv_timepoint_diff,
    indent_mods = c("rate_diff" = 2L, "rate_diff_ci" = 4L, "ztest_pval" = 4L),
    formats = c("rate_diff" = "xx.xx", "rate_diff_ci" = "(xx.xx, xx.xx)", "ztest_pval" = "x.xxxx | (<0.0001)")
  )
  analyze(
    lyt,
    vars,
    var_labels = var_labels,
    show_labels = show_labels,
    afun = a_surv_timepoint_diff,
    extra_args = list(...)
  )
}
