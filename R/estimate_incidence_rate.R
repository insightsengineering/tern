#' Incidence rate
#'
#' Estimate the event rate adjusted for person-years at risk, otherwise known
#' as incidence rate. Primary analysis variable is the person-years at risk.
#'
#' @inheritParams argument_convention
#' @param control (`list`) \cr parameters for estimation details, specified by using
#'   the helper function [control_incidence_rate()]. Possible parameter options are: \cr
#' * `conf_level`: (`proportion`) \cr confidence level for the estimated incidence rate.
#' * `conf_type`: (`string`) \cr `normal` (default), `normal_log`, `exact`, or `byar`
#'   for confidence interval type.
#' * `time_unit_input`: (`string`) \cr `day`, `month`, or `year` (default)
#'   indicating time unit for data input.
#' * `time_unit_output`: (`numeric`) \cr time unit for desired output (in person-years).
#' @param person_years (`numeric`) \cr total person-years at risk.
#' @param num_events (`numeric`) \cr total number of events observed.
#' @param alpha (`numeric`) \cr two-sided alpha-level for confidence interval.
#'
#' @name incidence_rate
#' @order 1
#'
NULL

#' Control function for incidence rate
#'
#' This is an auxiliary function for controlling arguments for the incidence rate, used
#' internally to specify details in [s_incidence_rate()].
#'
#' @inheritParams argument_convention
#' @param time_unit_input (`string`) \cr `day`, `month`, or `year` (default)
#'   indicating time unit for data input.
#' @param time_unit_output (`numeric`) \cr time unit for desired output (in person-years).
#' @param conf_type (`string`) \cr `normal` (default), `normal_log`, `exact`, or `byar`
#'   for confidence interval type.
#' @return A list of components with the same name as the arguments.
#' @export
#' @examples
#' control_incidence_rate(0.9, "exact", "month", 100)
#'
control_incidence_rate <- function(conf_level = 0.95,
                                   conf_type = c("normal", "normal_log", "exact", "byar"),
                                   time_unit_input = c("year", "day", "month"),
                                   time_unit_output = 1) {

  conf_type <- match.arg(conf_type)
  time_unit_input <- match.arg(time_unit_input)
  assert_that(
    is_proportion(conf_level),
    is.number(time_unit_output)
  )

  list(
    conf_level = conf_level,
    conf_type = conf_type,
    time_unit_input = time_unit_input,
    time_unit_output = time_unit_output
  )

}

#' @describeIn incidence_rate helper function to estimate the incidence rate and
#'   associated confidence interval based on the normal approximation for the
#'   incidence rate. Unit is one person-year.
#' @export
#' @order 2
#' @examples
#' h_incidence_rate_normal(200, 2)
#'
h_incidence_rate_normal <- function(person_years,
                                    num_events,
                                    alpha = 0.05) {

  assert_that(
    is.number(person_years),
    is.number(num_events),
    is_proportion(alpha)
  )

  est <- num_events / person_years
  se <- sqrt(est / person_years)
  ci <- est + c(-1, 1) * qnorm(1 - alpha / 2) * se

  list(rate = est, rate_ci = ci)

}

#' @describeIn incidence_rate helper function to estimate the incidence rate and
#'   associated confidence interval based on the normal approximation for the
#'   logarithm of the incidence rate. Unit is one person-year.
#' @export
#' @order 2
#' @examples
#' h_incidence_rate_normal_log(200, 2)
#'
h_incidence_rate_normal_log <- function(person_years,
                                        num_events,
                                        alpha = 0.05) {

  assert_that(
    is.number(person_years),
    is.number(num_events),
    is_proportion(alpha)
  )

  rate_est <- num_events / person_years
  rate_se <- sqrt(rate_est / person_years)
  lrate_est <- log(rate_est)
  lrate_se <- rate_se / rate_est
  ci <- exp(lrate_est + c(-1, 1) * qnorm(1 - alpha / 2) * lrate_se)

  list(rate = rate_est, rate_ci = ci)

}

#' @describeIn incidence_rate helper function to estimate the incidence rate and
#'   associated exact confidence interval. Unit is one person-year.
#' @export
#' @order 2
#' @examples
#' h_incidence_rate_exact(200, 2)
#'
h_incidence_rate_exact <- function(person_years,
                                   num_events,
                                   alpha = 0.05) {

  assert_that(
    is.number(person_years),
    is.number(num_events),
    is_proportion(alpha)
  )

  est <- num_events / person_years
  lcl <- qchisq(p = (alpha) / 2, df = 2 * num_events) / (2 * person_years)
  ucl <- qchisq(p = 1 - (alpha) / 2, df = 2 * num_events + 2) / (2 * person_years)

  list(rate = est, rate_ci = c(lcl, ucl))

}

#' @describeIn incidence_rate helper function to estimate the incidence rate and
#'   associated Byar's confidence interval. Unit is one person-year.
#' @export
#' @order 2
#' @examples
#' h_incidence_rate_byar(200, 2)
#'
h_incidence_rate_byar <- function(person_years,
                                  num_events,
                                  alpha = 0.05) {

  assert_that(
    is.number(person_years),
    is.number(num_events),
    is_proportion(alpha)
  )

  est <- num_events / person_years
  seg_1 <- num_events + 0.5
  seg_2 <- 1 - 1 / (9 * (num_events + 0.5))
  seg_3 <- qnorm(1 - alpha / 2) * sqrt(1 / (num_events + 0.5)) / 3
  lcl <- seg_1 * ((seg_2 - seg_3)^3) / person_years
  ucl <- seg_1 * ((seg_2 + seg_3)^3) / person_years

  list(rate = est, rate_ci = c(lcl, ucl))

}

#' @describeIn incidence_rate incidence_rate helper function to estimate the incidence rate and
#'   associated confidence interval.
#' @export
#' @order 3
#' @examples
#' h_incidence_rate(200, 2)
#'
#' h_incidence_rate(
#'   200,
#'   2,
#'   control_incidence_rate(
#'     conf_level = 0.9,
#'     conf_type = "normal_log",
#'     time_unit_output = 100)
#' )
#'
h_incidence_rate <- function(person_years,
                             num_events,
                             control = control_incidence_rate()) {

  alpha <- 1 - control$conf_level
  est <- switch(
    control$conf_type,
    normal = h_incidence_rate_normal(person_years, num_events, alpha),
    normal_log = h_incidence_rate_normal_log(person_years, num_events, alpha),
    exact = h_incidence_rate_exact(person_years, num_events, alpha),
    byar = h_incidence_rate_byar(person_years, num_events, alpha)
  )

  time_unit_output <- control$time_unit_output
  list(
    rate = est$rate * time_unit_output,
    rate_ci = est$rate_ci * time_unit_output
  )

}

#' @describeIn incidence_rate statistics function which estimates the incidence rate and the
#'   associated confidence interval.
#' @return The statistics are:
#'   * `person_years`: total person-years at risk
#'   * `num_events`: total number of events observed
#'   * `rate`: estimated incidence rate
#'   * `rate_ci`: confidence interval for the incidence rate
#' @export
#' @order 4
#' @examples
#'
#' library(dplyr)
#' df <- data.frame(
#'   USUBJID = as.character(seq(6)),
#'   CNSR = c(0, 1, 1, 0, 0, 0),
#'   AVAL = c(10.1, 20.4, 15.3, 20.8, 18.7, 23.4),
#'   ARM = factor(c("A", "A", "A", "B", "B", "B"))
#' ) %>%
#'   dplyr::mutate(is_event = CNSR == 0)
#'
#' s_incidence_rate(
#'   df,
#'   .var = "AVAL",
#'   is_event = "is_event",
#'   control = control_incidence_rate(
#'     time_unit_input = "month",
#'     time_unit_output = 100
#'   )
#' )
#'
s_incidence_rate <- function(df,
                             .var,
                             is_event,
                             control = control_incidence_rate()) {

  assert_that(
    is_df_with_variables(df, list(tte = .var, is_event = is_event)),
    is.string(.var),
    is_numeric_vector(df[[.var]]),
    is_logical_vector(df[[is_event]])
  )

  time_unit_input <- control$time_unit_input
  time_unit_output <- control$time_unit_output
  conf_level <- control$conf_level
  person_years <- sum(df[[.var]], na.rm = TRUE) * (
    1 * (time_unit_input == "year") +
      1 / 12 * (time_unit_input == "month") +
      1 / 365.24 * (time_unit_input == "day")
  )
  num_events <- sum(df[[is_event]], na.rm = TRUE)

  result <- h_incidence_rate(
    person_years,
    num_events,
    control
  )
  list(
    person_years = with_label(person_years, "Total patient-years at risk"),
    num_events = with_label(num_events, "Number of adverse events observed"),
    rate = with_label(result$rate, paste("AE rate per", time_unit_output, "patient-years")),
    rate_ci = with_label(result$rate_ci, f_conf_level(conf_level))
  )
}

#' @describeIn incidence_rate Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' a_incidence_rate(
#'   df,
#'   .var = "AVAL",
#'   is_event = "is_event",
#'   control = control_incidence_rate(time_unit_input = "month", time_unit_output = 100)
#' )
#'
a_incidence_rate <- make_afun(
  s_incidence_rate,
  .formats = c(
    "person_years" = "xx.x",
    "num_events" = "xx",
    "rate" = "xx.xx",
    "rate_ci" = "(xx.xx, xx.xx)"
  )
)

#' @describeIn incidence_rate layout creating function which adds analyze rows using the statistics
#' function `s_incidence_rate` and desired format.
#' @export
#' @order 5
#' @examples
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   estimate_incidence_rate(
#'     vars = "AVAL",
#'     is_event = "is_event",
#'     control = control_incidence_rate(
#'       time_unit_input = "month",
#'       time_unit_output = 100
#'     )
#'   ) %>%
#'   build_table(df, col_counts = table(df$ARM))
#'
estimate_incidence_rate <- function(lyt,
                                    vars,
                                    ...,
                                    show_labels = "hidden",
                                    table_names = vars,
                                    .stats = NULL,
                                    .formats = NULL,
                                    .labels = NULL,
                                    .indent_mods = NULL) {
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
    extra_args = list(...)
  )
}
