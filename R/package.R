#' tern Package
#'
#' Package to create tables, listings and graphs to analyze clinical trials data.
#'
"_PACKAGE"

#' @import rtables ggplot2
#' @importFrom broom tidy
#' @importFrom formatters format_value propose_column_widths
#' @importFrom magrittr %>%
#' @importFrom methods new
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
#' @importFrom survival coxph strata Surv
#' @importFrom stats pchisq setNames complete.cases qnorm qt sd
NULL

# Resolve missing global definitions:
utils::globalVariables(c(
  ".",
  "x",
  "average",
  "difference",
  "control_coxph",
  "control_incidence_rate",
  "control_analyze_vars",
  "control_surv_time",
  "control_surv_timepoint"
))
