#' Survival Time Analysis
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Summarize median survival time and CIs, percentiles of survival times, survival
#' time range of censored/event patients.
#'
#' @inheritParams argument_convention
#' @param control (`list`)\cr parameters for comparison details, specified by using the helper function
#'   [control_surv_time()]. Some possible parameter options are:
#'   * `conf_level` (`proportion`)\cr confidence level of the interval for survival time.
#'   * `conf_type` (`string`)\cr confidence interval type. Options are "plain" (default), "log", or "log-log",
#'     see more in [survival::survfit()]. Note option "none" is not supported.
#'   * `quantiles` (`numeric`)\cr vector of length two to specify the quantiles of survival time.
#' @param ref_fn_censor (`flag`)\cr whether referential footnotes indicating censored observations should be printed
#'   when the `range` statistic is included.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("surv_time")`
#'   to see available statistics for this function.
#' @param .indent_mods (named `vector` of `integer`)\cr indent modifiers for the labels. Each element of the vector
#'   should be a name-value pair with name corresponding to a statistic specified in `.stats` and value the indentation
#'   for that statistic's row label.
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
#' df <- adtte_f %>% filter(ARMCD == "ARM A")
#'
#' @name survival_time
#' @order 1
NULL

#' @describeIn survival_time Statistics function which analyzes survival times.
#'
#' @return
#' * `s_surv_time()` returns the statistics:
#'   * `median`: Median survival time.
#'   * `median_ci`: Confidence interval for median time.
#'   * `quantiles`: Survival time for two specified quantiles.
#'   * `range_censor`: Survival time range for censored observations.
#'   * `range_event`: Survival time range for observations with events.
#'   * `range`: Survival time range for all observations.
#'
#' @keywords internal
s_surv_time <- function(df,
                        .var,
                        is_event,
                        control = control_surv_time()) {
  checkmate::assert_string(.var)
  assert_df_with_variables(df, list(tte = .var, is_event = is_event))
  checkmate::assert_numeric(df[[.var]], min.len = 1, any.missing = FALSE)
  checkmate::assert_logical(df[[is_event]], min.len = 1, any.missing = FALSE)

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
    median = formatters::with_label(unname(srv_tab["median"]), "Median"),
    median_ci = formatters::with_label(
      unname(srv_tab[paste0(srv_fit$conf.int, c("LCL", "UCL"))]), f_conf_level(conf_level)
    ),
    quantiles = formatters::with_label(
      unname(srv_qt_tab), paste0(quantiles[1] * 100, "% and ", quantiles[2] * 100, "%-ile")
    ),
    range_censor = formatters::with_label(range_censor, "Range (censored)"),
    range_event = formatters::with_label(range_event, "Range (event)"),
    range = formatters::with_label(range, "Range")
  )
}

#' @describeIn survival_time Formatted analysis function which is used as `afun` in `surv_time()`.
#'
#' @return
#' * `a_surv_time()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' a_surv_time(
#'   df,
#'   .df_row = df,
#'   .var = "AVAL",
#'   is_event = "is_event"
#' )
#'
#' @export
a_surv_time <- function(df,
                        labelstr = "",
                        .var = NULL,
                        .df_row = NULL,
                        is_event,
                        control = control_surv_time(),
                        ref_fn_censor = TRUE,
                        .stats = NULL,
                        .formats = NULL,
                        .labels = NULL,
                        .indent_mods = NULL,
                        na_str = default_na_str()) {
  x_stats <- s_surv_time(
    df = df, .var = .var, is_event = is_event, control = control
  )
  rng_censor_lwr <- x_stats[["range_censor"]][1]
  rng_censor_upr <- x_stats[["range_censor"]][2]

  # Use method-specific defaults
  fmts <- c(median_ci = "(xx.x, xx.x)", quantiles = "xx.x, xx.x", range = "xx.x to xx.x")
  lbls <- c(median_ci = "95% CI", range = "Range", range_censor = "Range (censored)", range_event = "Range (event)")
  lbls_custom <- .labels
  .formats <- c(.formats, fmts[setdiff(names(fmts), names(.formats))])
  .labels <- c(.labels, lbls[setdiff(names(lbls), names(lbls_custom))])

  # Fill in with formatting defaults if needed
  .stats <- get_stats("surv_time", stats_in = .stats)
  .formats <- get_formats_from_stats(.stats, .formats)
  .labels <- get_labels_from_stats(.stats, .labels) %>% labels_use_control(control, lbls_custom)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods)

  x_stats <- x_stats[.stats]

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, .df_row, .var)

  cell_fns <- setNames(vector("list", length = length(x_stats)), .labels)
  if ("range" %in% names(x_stats) && ref_fn_censor) {
    if (x_stats[["range"]][1] == rng_censor_lwr && x_stats[["range"]][2] == rng_censor_upr) {
      cell_fns[[.labels[["range"]]]] <- "Censored observations: range minimum & maximum"
    } else if (x_stats[["range"]][1] == rng_censor_lwr) {
      cell_fns[[.labels[["range"]]]] <- "Censored observation: range minimum"
    } else if (x_stats[["range"]][2] == rng_censor_upr) {
      cell_fns[[.labels[["range"]]]] <- "Censored observation: range maximum"
    }
  }

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = .labels,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .format_na_strs = na_str,
    .cell_footnotes = cell_fns
  )
}

#' @describeIn survival_time Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `surv_time()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_surv_time()` to the table layout.
#'
#' @examples
#' basic_table() %>%
#'   split_cols_by(var = "ARMCD") %>%
#'   add_colcounts() %>%
#'   surv_time(
#'     vars = "AVAL",
#'     var_labels = "Survival Time (Months)",
#'     is_event = "is_event",
#'     control = control_surv_time(conf_level = 0.9, conf_type = "log-log")
#'   ) %>%
#'   build_table(df = adtte_f)
#'
#' @export
#' @order 2
surv_time <- function(lyt,
                      vars,
                      is_event,
                      control = control_surv_time(),
                      ref_fn_censor = TRUE,
                      na_str = default_na_str(),
                      nested = TRUE,
                      ...,
                      var_labels = "Time to Event",
                      show_labels = "visible",
                      table_names = vars,
                      .stats = c("median", "median_ci", "quantiles", "range"),
                      .formats = NULL,
                      .labels = NULL,
                      .indent_mods = c(median_ci = 1L)) {
  extra_args <- list(
    .stats = .stats, .formats = .formats, .labels = .labels, .indent_mods = .indent_mods, na_str = na_str,
    is_event = is_event, control = control, ref_fn_censor = ref_fn_censor, ...
  )

  analyze(
    lyt = lyt,
    vars = vars,
    afun = a_surv_time,
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args
  )
}
