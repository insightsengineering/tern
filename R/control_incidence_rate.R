#' Control function for incidence rate
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is an auxiliary function for controlling arguments for the incidence rate, used
#' internally to specify details in `s_incidence_rate()`.
#'
#' @inheritParams argument_convention
#' @param conf_type (`string`)\cr `normal` (default), `normal_log`, `exact`, or `byar`
#'   for confidence interval type.
#' @param input_time_unit (`string`)\cr `day`, `week`, `month`, or `year` (default)
#'   indicating time unit for data input.
#' @param n_pt_years_rate (`numeric`)\cr number of patient-years to use when calculating AE rate.
#' @param time_unit_input `r lifecycle::badge("deprecated")` Please use the `input_time_unit` argument instead.
#' @param time_unit_output `r lifecycle::badge("deprecated")` Please use the `n_pt_years_rate` argument instead.
#'
#' @return A list of components with the same names as the arguments.
#'
#' @seealso [incidence_rate]
#'
#' @examples
#' control_incidence_rate(0.9, "exact", "month", 100)
#'
#' @export
control_incidence_rate <- function(conf_level = 0.95,
                                   conf_type = c("normal", "normal_log", "exact", "byar"),
                                   input_time_unit = c("year", "day", "week", "month"),
                                   n_pt_years_rate = 1,
                                   time_unit_input = lifecycle::deprecated(),
                                   time_unit_output = lifecycle::deprecated()) {
  if (lifecycle::is_present(time_unit_input)) {
    lifecycle::deprecate_warn(
      "0.8.3", "control_incidence_rate(time_unit_input)", "control_incidence_rate(input_time_unit)"
    )
    input_time_unit <- time_unit_input
  }
  if (lifecycle::is_present(time_unit_output)) {
    lifecycle::deprecate_warn(
      "0.8.3", "control_incidence_rate(time_unit_output)", "control_incidence_rate(n_pt_years_rate)"
    )
    n_pt_years_rate <- time_unit_output
  }

  conf_type <- match.arg(conf_type)
  input_time_unit <- match.arg(input_time_unit)
  checkmate::assert_number(n_pt_years_rate)
  assert_proportion_value(conf_level)

  list(
    conf_level = conf_level,
    conf_type = conf_type,
    input_time_unit = input_time_unit,
    n_pt_years_rate = n_pt_years_rate
  )
}
