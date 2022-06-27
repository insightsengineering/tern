#' Control function for incidence rate
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is an auxiliary function for controlling arguments for the incidence rate, used
#' internally to specify details in `s_incidence_rate()`.
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
#' @seealso [incidence_rate()], [estimate_incidence_rate()]
control_incidence_rate <- function(conf_level = 0.95,
                                   conf_type = c("normal", "normal_log", "exact", "byar"),
                                   time_unit_input = c("year", "day", "week", "month"),
                                   time_unit_output = 1) {
  conf_type <- match.arg(conf_type)
  time_unit_input <- match.arg(time_unit_input)
  assertthat::assert_that(
    is_proportion(conf_level),
    assertthat::is.number(time_unit_output)
  )

  list(
    conf_level = conf_level,
    conf_type = conf_type,
    time_unit_input = time_unit_input,
    time_unit_output = time_unit_output
  )
}
