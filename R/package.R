#' TLG creation functions
#'
#' Functions to create TLG
#'
"_PACKAGE"

#' @import utils.nest
#' @importFrom rlang "%||%"
#' @importFrom methods is
#' @importFrom utils globalVariables
NULL

# We need this to avoid R CMD check warning about the '.' in magrittr pipes.
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
