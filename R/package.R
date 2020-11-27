#' tern Package
#'
#' tern is an analysis package to create tables, listings and graphs to analyze clinical trials data.
#'
"_PACKAGE"

#' @import assertthat
#' @import utils.nest
#' @import rtables
#' @import ggplot2
#' @import survival
#' @import grid
#' @importFrom rlang "%||%"
#' @importFrom methods is
#' @importFrom utils globalVariables
#' @importFrom stats qchisq
NULL

# We need this to avoid R CMD check warning about missing global definitions.
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".",
    "x",
    "control_coxph",
    "control_incidence_rate",
    "control_surv_time",
    "control_surv_timepoint"
  ))
}
