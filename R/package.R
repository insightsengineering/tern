#' tern Package
#'
#' tern is an analysis package to create tables, listings and graphs to analyze clinical trials data.
#'
"_PACKAGE"

#' @import rtables ggplot2
#' @importFrom broom tidy
#' @importFrom methods new
#' @importFrom magrittr %>%
#' @importFrom rlang .data := !!
#' @importFrom survival coxph strata Surv
#' @importFrom optimx optimx
#' @importFrom dfoptim nmk
NULL

# We need this to avoid R CMD check warning about missing global definitions.
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".",
    "x",
    "control_coxph",
    "control_incidence_rate",
    "control_summarize_vars",
    "control_surv_time",
    "control_surv_timepoint"
  ))
}
