#' Pairwise CoxPH model
#'
#' Summarize p-value, HR and CIs from stratified or unstratified CoxPH model.
#'
#' @md
#' @inheritParams argument_convention
#' @inheritParams s_surv_time
#' @param strat (`character` or `NULL`) variable names indicating stratification factors.
#' @param control (`list`) \cr parameters for comparison details, specified by using \cr
#'    the helper function [control_coxph()]. Some possible parameter options are: \cr
#' * `pval_method`: (`string`) \cr p-value method for testing hazard ratio = 1.
#'   Default method is "log-rank", can also be set to "wald" or "likelihood".
#' * `ties`: (`string`) \cr specifying the method for tie handling. Default is "efron",
#'   can also be set to "breslow" or "exact". See more in [survival::coxph()]
#' * `conf_level`: (`proportion`)\cr confidence level of the interval for HR.
#'
#' @name survival_coxph_pairwise
#'
NULL

#' @describeIn survival_coxph_pairwise Statistics Function which analyzes HR, CIs of HR and p-value with coxph model.
#'
#' @export
#'
#' @return The statistics are:
#' * `pvalue` : p-value to test HR = 1.
#' * `hr` : hazard ratio.
#' * `hr_ci` : confidence interval for hazard ratio.
#' * `n_tot` : total number of observations
#' * `n_tot_events` : total number of events
#'
#' @examples
#' library(scda)
#' library(dplyr)
#'
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#' ADTTE_f <- ADTTE %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(is_event = CNSR == 0)
#' df <- ADTTE_f %>%
#'   filter(ARMCD == "ARM A")
#' df_ref_group <- ADTTE_f %>%
#'   filter(ARMCD == "ARM B")
#'
#' s_coxph_pairwise(df, df_ref_group, .in_ref_col = FALSE, .var = "AVAL", is_event = "is_event")
s_coxph_pairwise <- function(df,
                             .ref_group,
                             .in_ref_col,
                             .var,
                             is_event,
                             strat = NULL,
                             control = control_coxph()) {
  assertthat::assert_that(
    is_df_with_variables(df, list(tte = .var, is_event = is_event)),
    assertthat::is.string(.var),
    is.numeric(df[[.var]]),
    is.logical(df[[is_event]])
  )
  pval_method <- control$pval_method
  ties <- control$ties
  conf_level <- control$conf_level

  if (.in_ref_col) {
    return(
      list(
        pvalue = formatters::with_label("", paste0("p-value (", pval_method, ")")),
        hr = formatters::with_label("", "Hazard Ratio"),
        hr_ci = formatters::with_label("", f_conf_level(conf_level)),
        n_tot = formatters::with_label("", "Total n"),
        n_tot_events = formatters::with_label("", "Total events")
      )
    )
  }
  data <- rbind(.ref_group, df)
  group <- factor(rep(c("ref", "x"), c(nrow(.ref_group), nrow(df))), levels = c("ref", "x"))

  df_cox <- data.frame(
    tte = data[[.var]],
    is_event = data[[is_event]],
    arm = group
  )
  if (is.null(strat)) {
    formula_cox <- survival::Surv(tte, is_event) ~ arm
  } else {
    formula_cox <- stats::as.formula(
      paste0(
        "survival::Surv(tte, is_event) ~ arm + strata(",
        paste(strat, collapse = ","),
        ")"
      )
    )
    df_cox <- cbind(df_cox, data[strat])
  }
  cox_fit <- survival::coxph(
    formula = formula_cox,
    data = df_cox,
    ties = ties
  )
  sum_cox <- summary(cox_fit, conf.int = conf_level, extend = TRUE)
  pval <- switch(pval_method,
    "wald" = sum_cox$waldtest["pvalue"],
    "log-rank" = sum_cox$sctest["pvalue"], # Score (logrank) test,
    "likelihood" = sum_cox$logtest["pvalue"]
  )
  list(
    pvalue = formatters::with_label(unname(pval), paste0("p-value (", pval_method, ")")),
    hr = formatters::with_label(sum_cox$conf.int[1, 1], "Hazard Ratio"),
    hr_ci = formatters::with_label(unname(sum_cox$conf.int[1, 3:4]), f_conf_level(conf_level)),
    n_tot = formatters::with_label(sum_cox$n, "Total n"),
    n_tot_events = formatters::with_label(sum_cox$nevent, "Total events")
  )
}

#' @describeIn survival_coxph_pairwise Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' a_coxph_pairwise(df, df_ref_group, .in_ref_col = FALSE, .var = "AVAL", is_event = "is_event")
a_coxph_pairwise <- make_afun(
  s_coxph_pairwise,
  .indent_mods = c(pvalue = 0L, hr = 0L, hr_ci = 1L, n_tot = 0L, n_tot_events = 0L),
  .formats = c(
    pvalue = "x.xxxx | (<0.0001)",
    hr = "xx.xx",
    hr_ci = "(xx.xx, xx.xx)",
    n_tot = "xx.xx",
    n_tot_events = "xx.xx"
  )
)


#' @describeIn survival_coxph_pairwise Analyze Function which adds the pairwise coxph analysis
#'   to the input layout. Note that additional formatting arguments can be used here.
#' @export
#' @examples
#'
#' basic_table() %>%
#'   split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
#'   add_colcounts() %>%
#'   coxph_pairwise(
#'     vars = "AVAL",
#'     is_event = "is_event",
#'     var_labels = "Unstratified Analysis"
#'   ) %>%
#'   build_table(df = ADTTE_f)
#'
#' basic_table() %>%
#'   split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
#'   add_colcounts() %>%
#'   coxph_pairwise(
#'     vars = "AVAL",
#'     is_event = "is_event",
#'     var_labels = "Stratified Analysis",
#'     strat = "SEX",
#'     control = control_coxph(pval_method = "wald")
#'   ) %>%
#'   build_table(df = ADTTE_f)
coxph_pairwise <- function(lyt,
                           vars,
                           ...,
                           var_labels = "CoxPH",
                           show_labels = "visible",
                           table_names = vars,
                           .stats = c("pvalue", "hr", "hr_ci"),
                           .formats = NULL,
                           .labels = NULL,
                           .indent_mods = NULL) {
  afun <- make_afun(
    a_coxph_pairwise,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )
  analyze(
    lyt,
    vars,
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names,
    afun = afun,
    extra_args = list(...)
  )
}
