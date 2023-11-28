#' Pairwise `CoxPH` model
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Summarize p-value, HR and CIs from stratified or unstratified `CoxPH` model.
#'
#' @inheritParams argument_convention
#' @inheritParams s_surv_time
#' @param strat (`character` or `NULL`)\cr variable names indicating stratification factors.
#' @param control (`list`)\cr parameters for comparison details, specified by using the helper function
#'   [control_coxph()]. Some possible parameter options are:
#'   * `pval_method` (`string`)\cr p-value method for testing hazard ratio = 1. Default method is `"log-rank"` which
#'     comes from [survival::survdiff()], can also be set to `"wald"` or `"likelihood"` (from [survival::coxph()]).
#'   * `ties` (`string`)\cr specifying the method for tie handling. Default is `"efron"`,
#'     can also be set to `"breslow"` or `"exact"`. See more in [survival::coxph()]
#'   * `conf_level` (`proportion`)\cr confidence level of the interval for HR.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("coxph_pairwise")`
#'   to see available statistics for this function.
#'
#' @name survival_coxph_pairwise
#' @order 1
NULL

#' @describeIn survival_coxph_pairwise Statistics function which analyzes HR, CIs of HR and p-value of a `coxph` model.
#'
#' @return
#' * `s_coxph_pairwise()` returns the statistics:
#'   * `pvalue`: p-value to test HR = 1.
#'   * `hr`: Hazard ratio.
#'   * `hr_ci`: Confidence interval for hazard ratio.
#'   * `n_tot`: Total number of observations.
#'   * `n_tot_events`: Total number of events.
#'
#' @keywords internal
s_coxph_pairwise <- function(df,
                             .ref_group,
                             .in_ref_col,
                             .var,
                             is_event,
                             strat = NULL,
                             control = control_coxph()) {
  checkmate::assert_string(.var)
  checkmate::assert_numeric(df[[.var]])
  checkmate::assert_logical(df[[is_event]])
  assert_df_with_variables(df, list(tte = .var, is_event = is_event))
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
  orginal_survdiff <- survival::survdiff(
    formula_cox,
    data = df_cox
  )
  log_rank_pvalue <- 1 - pchisq(orginal_survdiff$chisq, length(orginal_survdiff$n) - 1)

  pval <- switch(pval_method,
    "wald" = sum_cox$waldtest["pvalue"],
    "log-rank" = log_rank_pvalue, # pvalue from original log-rank test survival::survdiff()
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

#' @describeIn survival_coxph_pairwise Formatted analysis function which is used as `afun` in `coxph_pairwise()`.
#'
#' @return
#' * `a_coxph_pairwise()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
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

#' @describeIn survival_coxph_pairwise Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `coxph_pairwise()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_coxph_pairwise()` to the table layout.
#'
#' @examples
#' library(dplyr)
#'
#' adtte_f <- tern_ex_adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(is_event = CNSR == 0)
#'
#' df <- adtte_f %>% filter(ARMCD == "ARM A")
#' df_ref_group <- adtte_f %>% filter(ARMCD == "ARM B")
#'
#' basic_table() %>%
#'   split_cols_by(var = "ARMCD", ref_group = "ARM A") %>%
#'   add_colcounts() %>%
#'   coxph_pairwise(
#'     vars = "AVAL",
#'     is_event = "is_event",
#'     var_labels = "Unstratified Analysis"
#'   ) %>%
#'   build_table(df = adtte_f)
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
#'   build_table(df = adtte_f)
#'
#' @export
#' @order 2
coxph_pairwise <- function(lyt,
                           vars,
                           na_str = default_na_str(),
                           nested = TRUE,
                           ...,
                           var_labels = "CoxPH",
                           show_labels = "visible",
                           table_names = vars,
                           .stats = c("pvalue", "hr", "hr_ci"),
                           .formats = NULL,
                           .labels = NULL,
                           .indent_mods = NULL) {
  extra_args <- list(...)

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
    na_str = na_str,
    nested = nested,
    extra_args = extra_args
  )
}
