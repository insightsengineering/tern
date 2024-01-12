#' Incidence Rate
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
#' @param n_events (`integer`)\cr number of events observed.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("estimate_incidence_rate")`
#'   to see available statistics for this function.
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
#'
#' @keywords internal
s_incidence_rate <- function(df,
                             .var,
                             n_events,
                             is_event,
                             control = control_incidence_rate()) {
  if (!missing(is_event)) {
    warning("argument is_event will be deprecated. Please use n_events.")

    if (missing(n_events)) {
      assert_df_with_variables(df, list(tte = .var, is_event = is_event))
      checkmate::assert_string(.var)
      checkmate::assert_logical(df[[is_event]], any.missing = FALSE)
      checkmate::assert_numeric(df[[.var]], any.missing = FALSE)
      n_events <- is_event
    }
  } else {
    assert_df_with_variables(df, list(tte = .var, n_events = n_events))
    checkmate::assert_string(.var)
    checkmate::assert_numeric(df[[.var]], any.missing = FALSE)
    checkmate::assert_integer(df[[n_events]], any.missing = FALSE)
  }

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
    rate_ci = formatters::with_label(result$rate_ci, f_conf_level(conf_level))
  )
}

#' @describeIn incidence_rate Formatted analysis function which is used as `afun`
#'   in `estimate_incidence_rate()`.
#'
#' @return
#' * `a_incidence_rate()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_incidence_rate <- make_afun(
  s_incidence_rate,
  .formats = c(
    "person_years" = "xx.x",
    "n_events" = "xx",
    "rate" = "xx.xx",
    "rate_ci" = "(xx.xx, xx.xx)"
  )
)

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
#'   mutate(is_event = CNSR == 0) %>%
#'   mutate(n_events = as.integer(is_event))
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
                                    control = control_incidence_rate(),
                                    na_str = default_na_str(),
                                    nested = TRUE,
                                    ...,
                                    show_labels = "hidden",
                                    table_names = vars,
                                    .stats = NULL,
                                    .formats = NULL,
                                    .labels = NULL,
                                    .indent_mods = NULL) {
  extra_args <- list(n_events = n_events, control = control, ...)

  afun <- make_afun(
    a_incidence_rate,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )

  analyze(
    lyt,
    vars,
    show_labels = show_labels,
    table_names = table_names,
    afun = afun,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args
  )
}

#' Helper Functions for Incidence Rate
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param control (`list`)\cr parameters for estimation details, specified by using
#'   the helper function [control_incidence_rate()]. Possible parameter options are:
#'   * `conf_level`: (`proportion`)\cr confidence level for the estimated incidence rate.
#'   * `conf_type`: (`string`)\cr `normal` (default), `normal_log`, `exact`, or `byar`
#'     for confidence interval type.
#'   * `input_time_unit`: (`string`)\cr `day`, `week`, `month`, or `year` (default)
#'     indicating time unit for data input.
#'   * `num_pt_year`: (`numeric`)\cr time unit for desired output (in person-years).
#' @param person_years (`numeric`)\cr total person-years at risk.
#' @param alpha (`numeric`)\cr two-sided alpha-level for confidence interval.
#' @param n_events (`integer`)\cr number of events observed.
#'
#' @return Estimated incidence rate `rate` and associated confidence interval `rate_ci`.
#'
#' @seealso [incidence_rate]
#'
#' @name h_incidence_rate
NULL

#' @describeIn h_incidence_rate Helper function to estimate the incidence rate and
#'   associated confidence interval based on the normal approximation for the
#'   incidence rate. Unit is one person-year.
#'
#' @examples
#' h_incidence_rate_normal(200, 2)
#'
#' @export
h_incidence_rate_normal <- function(person_years,
                                    n_events,
                                    alpha = 0.05) {
  checkmate::assert_number(person_years)
  checkmate::assert_number(n_events)
  assert_proportion_value(alpha)

  est <- n_events / person_years
  se <- sqrt(est / person_years)
  ci <- est + c(-1, 1) * stats::qnorm(1 - alpha / 2) * se

  list(rate = est, rate_ci = ci)
}

#' @describeIn h_incidence_rate Helper function to estimate the incidence rate and
#'   associated confidence interval based on the normal approximation for the
#'   logarithm of the incidence rate. Unit is one person-year.
#'
#' @examples
#' h_incidence_rate_normal_log(200, 2)
#'
#' @export
h_incidence_rate_normal_log <- function(person_years,
                                        n_events,
                                        alpha = 0.05) {
  checkmate::assert_number(person_years)
  checkmate::assert_number(n_events)
  assert_proportion_value(alpha)

  rate_est <- n_events / person_years
  rate_se <- sqrt(rate_est / person_years)
  lrate_est <- log(rate_est)
  lrate_se <- rate_se / rate_est
  ci <- exp(lrate_est + c(-1, 1) * stats::qnorm(1 - alpha / 2) * lrate_se)

  list(rate = rate_est, rate_ci = ci)
}

#' @describeIn h_incidence_rate Helper function to estimate the incidence rate and
#'   associated exact confidence interval. Unit is one person-year.
#'
#' @examples
#' h_incidence_rate_exact(200, 2)
#'
#' @export
h_incidence_rate_exact <- function(person_years,
                                   n_events,
                                   alpha = 0.05) {
  checkmate::assert_number(person_years)
  checkmate::assert_number(n_events)
  assert_proportion_value(alpha)

  est <- n_events / person_years
  lcl <- stats::qchisq(p = (alpha) / 2, df = 2 * n_events) / (2 * person_years)
  ucl <- stats::qchisq(p = 1 - (alpha) / 2, df = 2 * n_events + 2) / (2 * person_years)

  list(rate = est, rate_ci = c(lcl, ucl))
}

#' @describeIn h_incidence_rate Helper function to estimate the incidence rate and
#'   associated `Byar`'s confidence interval. Unit is one person-year.
#'
#' @examples
#' h_incidence_rate_byar(200, 2)
#'
#' @export
h_incidence_rate_byar <- function(person_years,
                                  n_events,
                                  alpha = 0.05) {
  checkmate::assert_number(person_years)
  checkmate::assert_number(n_events)
  assert_proportion_value(alpha)

  est <- n_events / person_years
  seg_1 <- n_events + 0.5
  seg_2 <- 1 - 1 / (9 * (n_events + 0.5))
  seg_3 <- stats::qnorm(1 - alpha / 2) * sqrt(1 / (n_events + 0.5)) / 3
  lcl <- seg_1 * ((seg_2 - seg_3)^3) / person_years
  ucl <- seg_1 * ((seg_2 + seg_3) ^ 3) / person_years # styler: off

  list(rate = est, rate_ci = c(lcl, ucl))
}

#' @describeIn h_incidence_rate Helper function to estimate the incidence rate and
#'   associated confidence interval.
#'
#' @keywords internal
h_incidence_rate <- function(person_years,
                             n_events,
                             control = control_incidence_rate()) {
  alpha <- 1 - control$conf_level
  est <- switch(control$conf_type,
    normal = h_incidence_rate_normal(person_years, n_events, alpha),
    normal_log = h_incidence_rate_normal_log(person_years, n_events, alpha),
    exact = h_incidence_rate_exact(person_years, n_events, alpha),
    byar = h_incidence_rate_byar(person_years, n_events, alpha)
  )

  num_pt_year <- control$num_pt_year
  list(
    rate = est$rate * num_pt_year,
    rate_ci = est$rate_ci * num_pt_year
  )
}
