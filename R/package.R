#' tern Package
#'
#' tern is an analysis package to create tables, listings and graphs to analyze clinical trials data.
#'
"_PACKAGE"

#' @import rtables
#' @import utils.nest
#' @importFrom assertthat assert_that
#' @importFrom ggplot2 ggplot
#' @importFrom magrittr %>%
#' @importFrom methods is
# Packages that are not imported and also not prefixed
# survival
# stats
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
