#' Incidence rate
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Estimate the event rate adjusted for person-years at risk, otherwise known
#' as incidence rate. Primary analysis variable is the person-years at risk.
#'
#' @inheritParams argument_convention
#' @param control (`list`)\cr parameters for estimation details, specified by using
#'   the helper function [control_incidence_rate()]. Possible parameter options are:
#'   * `conf_level` (`proportion`)\cr confidence level for the estimated incidence rate.
#'   * `conf_type` (`string`)\cr `normal` (default), `normal_log`, `exact`, or `byar`
#'     for confidence interval type.
#'   * `input_time_unit` (`string`)\cr `day`, `week`, `month`, or `year` (default)
#'     indicating time unit for data input.
#'   * `num_pt_year` (`numeric`)\cr time unit for desired output (in person-years).
#' @param n_events (`string`)\cr name of integer variable indicating whether an event has been observed (1) or not (0).
#' @param id_var (`string`)\cr name of variable used as patient identifier if `"n_unique"` is included in `.stats`.
#'   Defaults to `"USUBJID"`.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("estimate_incidence_rate")`
#'   to see available statistics for this function.
#' @param summarize (`flag`)\cr whether the function should act as an analyze function (`summarize = FALSE`), or a
#'   summarize function (`summarize = TRUE`). Defaults to `FALSE`.
#' @param label_fmt (`string`)\cr how labels should be formatted after a row split occurs if `summarize = TRUE`. The
#'   string should use `"%s"` to represent row split levels, and `"%.labels"` to represent labels supplied to the
#'   `.labels` argument. Defaults to `"%s - %.labels"`.
#'
#' @seealso [control_incidence_rate()] and helper functions [h_incidence_rate].
#'
#' @name incidence_rate
#' @order 1
NULL

#' @describeIn incidence_rate Statistics function which estimates the incidence rate and the
#'   associated confidence interval.
#'
#' @return
#' * `s_incidence_rate()` returns the following statistics:
#'   - `person_years`: Total person-years at risk.
#'   - `n_events`: Total number of events observed.
#'   - `rate`: Estimated incidence rate.
#'   - `rate_ci`: Confidence interval for the incidence rate.
#'   - `n_unique`: Total number of patients with at least one event observed.
#'   - `n_rate`: Total number of events observed & estimated incidence rate.
#'
#' @keywords internal
s_incidence_rate <- function(df,
                             .var,
                             n_events,
                             is_event = lifecycle::deprecated(),
                             id_var = "USUBJID",
                             control = control_incidence_rate()) {
  if (lifecycle::is_present(is_event)) {
    lifecycle::deprecate_warn(
      "0.9.6", "s_incidence_rate(is_event)", "s_incidence_rate(n_events)"
    )
    n_events <- as.numeric(is_event)
  }

  assert_df_with_variables(df, list(tte = .var, n_events = n_events))
  checkmate::assert_string(.var)
  checkmate::assert_string(n_events)
  checkmate::assert_string(id_var)
  checkmate::assert_numeric(df[[.var]], any.missing = FALSE)
  checkmate::assert_integerish(df[[n_events]], any.missing = FALSE)

  n_unique <- n_available(unique(df[[id_var]][df$n_events == 1]))
  input_time_unit <- control$input_time_unit
  num_pt_year <- control$num_pt_year
  conf_level <- control$conf_level
  person_years <- sum(df[[.var]], na.rm = TRUE) * (
    1 * (input_time_unit == "year") +
      1 / 12 * (input_time_unit == "month") +
      1 / 52.14 * (input_time_unit == "week") +
      1 / 365.24 * (input_time_unit == "day")
  )
  n_events <- sum(df[[n_events]], na.rm = TRUE)

  result <- h_incidence_rate(
    person_years,
    n_events,
    control
  )
  list(
    person_years = formatters::with_label(person_years, "Total patient-years at risk"),
    n_events = formatters::with_label(n_events, "Number of adverse events observed"),
    rate = formatters::with_label(result$rate, paste("AE rate per", num_pt_year, "patient-years")),
    rate_ci = formatters::with_label(result$rate_ci, f_conf_level(conf_level)),
    n_unique = formatters::with_label(n_unique, "Total number of patients with at least one adverse event"),
    n_rate = formatters::with_label(
      c(n_events, result$rate),
      paste("Number of adverse events observed (AE rate per", num_pt_year, "patient-years)")
    )
  )
}

#' @describeIn incidence_rate Formatted analysis function which is used as `afun` in `estimate_incidence_rate()`.
#'
#' @return
#' * `a_incidence_rate()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_incidence_rate <- function(df,
                             labelstr = "",
                             .var,
                             .df_row,
                             n_events,
                             id_var = "USUBJID",
                             control = control_incidence_rate(),
                             .stats = NULL,
                             .formats = c(
                               "person_years" = "xx.x",
                               "n_events" = "xx",
                               "rate" = "xx.xx",
                               "rate_ci" = "(xx.xx, xx.xx)",
                               "n_unique" = "xx",
                               "n_rate" = "xx (xx.x)"
                             ),
                             .labels = NULL,
                             .indent_mods = NULL,
                             na_str = default_na_str(),
                             label_fmt = "%s - %.labels") {
  checkmate::assert_string(label_fmt)

  x_stats <- s_incidence_rate(
    df = df, .var = .var, n_events = n_events, id_var = id_var, control = control
  )
  if (is.null(unlist(x_stats))) {
    return(NULL)
  }
  if (is.null(.formats)) .formats <- formals()$.formats %>% eval()
  if (is.null(.labels)) .labels <- sapply(x_stats, \(x) attributes(x)$label)
  if (nzchar(labelstr) > 0) {
    .labels <- sapply(.labels, \(x) gsub("%.labels", x, gsub("%s", labelstr, label_fmt)))
  }

  # Fill in with formatting defaults if needed
  .stats <- get_stats("estimate_incidence_rate", stats_in = .stats)
  .formats <- get_formats_from_stats(.stats, .formats)
  .labels <- get_labels_from_stats(.stats, .labels)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods)

  x_stats <- x_stats[.stats]

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, .df_row, .var)

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .format_na_strs = na_str
  )
}

#' @describeIn incidence_rate Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `estimate_incidence_rate()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_incidence_rate()` to the table layout.
#'
#' @examples
#' library(dplyr)
#'
#' df <- data.frame(
#'   USUBJID = as.character(seq(6)),
#'   CNSR = c(0, 1, 1, 0, 0, 0),
#'   AVAL = c(10.1, 20.4, 15.3, 20.8, 18.7, 23.4),
#'   ARM = factor(c("A", "A", "A", "B", "B", "B"))
#' ) %>%
#'   mutate(n_events = 1 - CNSR)
#'
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   estimate_incidence_rate(
#'     vars = "AVAL",
#'     n_events = "n_events",
#'     control = control_incidence_rate(
#'       input_time_unit = "month",
#'       num_pt_year = 100
#'     )
#'   ) %>%
#'   build_table(df)
#'
#' @export
#' @order 2
estimate_incidence_rate <- function(lyt,
                                    vars,
                                    n_events,
                                    id_var = "USUBJID",
                                    control = control_incidence_rate(),
                                    na_str = default_na_str(),
                                    nested = TRUE,
                                    summarize = FALSE,
                                    label_fmt = "%s - %.labels",
                                    ...,
                                    show_labels = "hidden",
                                    table_names = vars,
                                    .stats = NULL,
                                    .formats = NULL,
                                    .labels = NULL,
                                    .indent_mods = NULL) {
  extra_args <- c(
    list(.stats = .stats, .formats = .formats, .labels = .labels, .indent_mods = .indent_mods, na_str = na_str),
    list(n_events = n_events, id_var = id_var, control = control, label_fmt = label_fmt, ...)
  )

  if (!summarize) {
    analyze(
      lyt,
      vars,
      show_labels = show_labels,
      table_names = table_names,
      afun = a_incidence_rate,
      na_str = na_str,
      nested = nested,
      extra_args = extra_args
    )
  } else {
    summarize_row_groups(
      lyt,
      vars,
      cfun = a_incidence_rate,
      na_str = na_str,
      extra_args = extra_args
    )
  }
}
