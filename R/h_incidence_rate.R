#' Helper functions for incidence rate
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
#' @param person_years (`numeric(1)`)\cr total person-years at risk.
#' @param alpha (`numeric(1)`)\cr two-sided alpha-level for confidence interval.
#' @param n_events (`integer(1)`)\cr number of events observed.
#'
#' @return Estimated incidence rate, `rate`, and associated confidence interval, `rate_ci`.
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
#'   associated Byar's confidence interval. Unit is one person-year.
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
