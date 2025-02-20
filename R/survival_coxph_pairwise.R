#' Analyze a pairwise Cox-PH model
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [coxph_pairwise()] creates a layout element to analyze a pairwise Cox-PH model.
#'
#' This function can return statistics including p-value, hazard ratio (HR), and HR confidence intervals from both
#' stratified and unstratified Cox-PH models. The variable(s) to be analyzed is specified via the `vars` argument and
#' any stratification factors via the `strata` argument.
#'
#' @inheritParams argument_convention
#' @inheritParams s_surv_time
#' @param strata (`character` or `NULL`)\cr variable names indicating stratification factors.
#' @param strat `r lifecycle::badge("deprecated")` Please use the `strata` argument instead.
#' @param control (`list`)\cr parameters for comparison details, specified by using the helper function
#'   [control_coxph()]. Some possible parameter options are:
#'   * `pval_method` (`string`)\cr p-value method for testing the null hypothesis that hazard ratio = 1. Default
#'     method is `"log-rank"` which comes from [survival::survdiff()], can also be set to `"wald"` or `"likelihood"`
#'     (from [survival::coxph()]).
#'   * `ties` (`string`)\cr specifying the method for tie handling. Default is `"efron"`,
#'     can also be set to `"breslow"` or `"exact"`. See more in [survival::coxph()].
#'   * `conf_level` (`proportion`)\cr confidence level of the interval for HR.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("coxph_pairwise"), type = "sh")``
#'
#' @name survival_coxph_pairwise
#' @order 1
NULL

#' @describeIn survival_coxph_pairwise Statistics function which analyzes HR, CIs of HR, and p-value of a Cox-PH model.
#'
#' @return
#' * `s_coxph_pairwise()` returns the statistics:
#'   * `pvalue`: p-value to test the null hypothesis that hazard ratio = 1.
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
                             strata = NULL,
                             strat = lifecycle::deprecated(),
                             control = control_coxph(),
                             ...) {
  if (lifecycle::is_present(strat)) {
    lifecycle::deprecate_warn("0.9.4", "s_coxph_pairwise(strat)", "s_coxph_pairwise(strata)")
    strata <- strat
  }

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
        pvalue = formatters::with_label(as.list(NULL), paste0("p-value (", pval_method, ")")),
        hr = formatters::with_label(as.list(NULL), "Hazard Ratio"),
        hr_ci = formatters::with_label(as.list(NULL), f_conf_level(conf_level)),
        hr_ci_3d = formatters::with_label(as.list(NULL), paste0("Hazard Ratio (", f_conf_level(conf_level), ")")),
        n_tot = formatters::with_label(as.list(NULL), "Total n"),
        n_tot_events = formatters::with_label(as.list(NULL), "Total events")
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
  if (is.null(strata)) {
    formula_cox <- survival::Surv(tte, is_event) ~ arm
  } else {
    formula_cox <- stats::as.formula(
      paste0(
        "survival::Surv(tte, is_event) ~ arm + strata(",
        paste(strata, collapse = ","),
        ")"
      )
    )
    df_cox <- cbind(df_cox, data[strata])
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
    hr_ci_3d = formatters::with_label(
      c(sum_cox$conf.int[1, 1], unname(sum_cox$conf.int[1, 3:4])),
      paste0("Hazard Ratio (", f_conf_level(conf_level), ")")
    ),
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
a_coxph_pairwise <- function(df,
                             ...,
                             .stats = NULL,
                             .stat_names = NULL,
                             .formats = NULL,
                             .labels = NULL,
                             .indent_mods = NULL) {
  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)
  extra_afun_params <- retrieve_extra_afun_params(names(dots_extra_args$.additional_fun_parameters))
  dots_extra_args$.additional_fun_parameters <- NULL

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_coxph_pairwise,
    custom_stat_fnc_list = NULL,
    args_list = c(
      df = list(df),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in formatting defaults
  .stats <- get_stats("coxph_pairwise", stats_in = .stats)
  x_stats <- x_stats[.stats]
  .formats <- get_formats_from_stats(.stats, .formats)
  .labels <- get_labels_from_stats(
    .stats, .labels, tern_defaults = c(lapply(x_stats, attr, "label"), tern_default_labels)
  )
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods)


  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, extra_afun_params$.df_row, extra_afun_params$.var)

  # Get and check statistical names
  .stat_names <- get_stat_names(x_stats, .stat_names)

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = .labels %>% .unlist_keep_nulls(),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

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
#'     strata = "SEX",
#'     control = control_coxph(pval_method = "wald")
#'   ) %>%
#'   build_table(df = adtte_f)
#'
#' @export
#' @order 2
coxph_pairwise <- function(lyt,
                           vars,
                           strata = NULL,
                           control = control_coxph(),
                           na_str = default_na_str(),
                           nested = TRUE,
                           ...,
                           var_labels = "CoxPH",
                           show_labels = "visible",
                           table_names = vars,
                           .stats = c("pvalue", "hr", "hr_ci"),
                           .stat_names = NULL,
                           .formats = NULL,
                           .labels = NULL,
                           .indent_mods = NULL) {
  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    strata = list(strata), control = list(control),
    ...
  )

  # Append additional info from layout to the analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_coxph_pairwise) <- c(formals(a_coxph_pairwise), extra_args[[".additional_fun_parameters"]])

  analyze(
    lyt = lyt,
    vars = vars,
    afun = a_coxph_pairwise,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names
  )
}
