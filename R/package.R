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

# We need this to avoid R CMD check warning about the '.' in magrittr pipes.
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
