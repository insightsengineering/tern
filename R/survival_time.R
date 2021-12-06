#' Survival Time Analysis
#'
#' Summarize median survival time and CIs, percentiles of survival times, \cr
#' survival time range of censored/event patients.
#'
#' @md
#' @inheritParams argument_convention
#' @param control a (`list`) of parameters for comparison details, specified by using \cr
#'    the helper function [control_surv_time]. Some possible parameter options are: \cr
#' * `conf_level`: (`proportion`)\cr confidence level of the interval for survival time.
#' * `conf_type`: (`string`) \cr "plain" (default), "log", "log-log" for confidence interval type, \cr
#'    see more in [survival::survfit()]. Note option, "none" is not supported.
#' * `quantiles`: numeric vector of length two to specify the quantiles of survival time.
#'
#' @name survival_time
#'
NULL

#' @describeIn survival_time Statistics Function which analyzes survival times.
#'  `range_censor` and `range_event`.
#'
#' @export
#'
#' @return The statistics are:
#' * `median` : median survival time.
#' * `median_ci` : confidence interval for median time.
#' * `quantiles` : survival time for two specified quantiles.
#' * `range_censor` : survival time range for censored observations.
#' * `range_event` : survival time range for observations with events.
#' * `range` : survival time range for all observations.
#' @examples
#' library(scda)
#' library(dplyr)
#'
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#' ADTTE_f <- ADTTE %>%
#'   dplyr::filter(PARAMCD == "OS") %>%
#'   dplyr::mutate(
#'     AVAL = day2month(AVAL),
#'     is_event = CNSR == 0
#'   )
#' df <- ADTTE_f %>% dplyr::filter(ARMCD == "ARM A")
#' s_surv_time(df, .var = "AVAL", is_event = "is_event")
#'
s_surv_time <- function(df,
                        .var,
                        is_event,
                        control = control_surv_time()) {
  assertthat::assert_that(
    is_df_with_variables(df, list(tte = .var, is_event = is_event)),
    assertthat::is.string(.var),
    is_numeric_vector(df[[.var]]),
    is_logical_vector(df[[is_event]])
  )

  conf_type <- control$conf_type
  conf_level <- control$conf_level
  quantiles <- control$quantiles

  formula <- stats::as.formula(paste0("survival::Surv(", .var, ", ", is_event, ") ~ 1"))
  srv_fit <- survival::survfit(
    formula = formula,
    data = df,
    conf.int = conf_level,
    conf.type = conf_type
  )
  srv_tab <- summary(srv_fit, extend = TRUE)$table
  srv_qt_tab <- stats::quantile(srv_fit, probs = quantiles)$quantile
  range_censor <- range_noinf(df[[.var]][!df[[is_event]]], na.rm = TRUE)
  range_event <- range_noinf(df[[.var]][df[[is_event]]], na.rm = TRUE)
  range <- range_noinf(df[[.var]], na.rm = TRUE)
  list(
    median = with_label(unname(srv_tab["median"]), "Median"),
    median_ci = with_label(unname(srv_tab[paste0(srv_fit$conf.int, c("LCL", "UCL"))]), f_conf_level(conf_level)),
    quantiles = with_label(unname(srv_qt_tab), paste0(quantiles[1] * 100, "% and ", quantiles[2] * 100, "%-ile")),
    range_censor = with_label(range_censor, "Range (censored)"),
    range_event = with_label(range_event, "Range (event)"),
    range = with_label(range, "Range")
  )
}

#' @describeIn survival_time Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' a_surv_time(df, .var = "AVAL", is_event = "is_event")
#'
a_surv_time <- make_afun(
  s_surv_time,
  .indent_mods = c(
    "median" = 0L,
    "median_ci" = 1L,
    "quantiles" = 0L,
    "range_censor" = 0L,
    "range_event" = 0L,
    "range" = 0L
  ),
  .formats = c(
    "median" = "xx.x",
    "median_ci" = "(xx.x, xx.x)",
    "quantiles" = "xx.x, xx.x",
    "range_censor" = "xx.x to xx.x",
    "range_event" = "xx.x to xx.x",
    "range" = "xx.x to xx.x"
  )
)

#' @describeIn survival_time Analyze Function which adds the survival times analysis
#'   to the input layout. Note that additional formatting arguments can be used here.
#' @inheritParams argument_convention
#' @export
#' @examples
#' basic_table() %>%
#' split_cols_by(var = "ARMCD") %>%
#'   add_colcounts() %>%
#'   surv_time(
#'     vars = "AVAL",
#'     var_labels = "Survival Time (Months)",
#'     is_event = "is_event",
#'     control = control_surv_time(conf_level = 0.9, conf_type = "log-log")
#'   ) %>%
#'   build_table(df = ADTTE_f)
#'
surv_time <- function(lyt,
                      vars,
                      ...,
                      var_labels = "Time to Event",
                      table_names = vars,
                      .stats = c("median", "median_ci", "quantiles", "range_censor", "range_event"),
                      .formats = NULL,
                      .labels = NULL,
                      .indent_mods = NULL) {
  afun <- make_afun(
    a_surv_time,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )
  analyze(
    lyt,
    vars,
    var_labels = var_labels,
    show_labels = "visible",
    table_names = table_names,
    afun = afun,
    extra_args = list(...)
  )
}
