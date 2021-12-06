#' tern Package
#'
#' tern is an analysis package to create tables, listings and graphs to analyze clinical trials data.
#'
"_PACKAGE"

#' @import rtables
#' @importFrom broom tidy
#' @importFrom survival strata coxph Surv
#' @importFrom grid gList
#' @importFrom rlang .data := !!
#' @importFrom magrittr %>%
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
