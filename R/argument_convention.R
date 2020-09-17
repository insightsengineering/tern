#' Standard Arguments
#'
#' The documentation to this function lists all the arguments in \code{tern} that are used repeatedly to express an
#' analysis.
#'
#' @details
#' Although this function just returns \code{NULL} it has two uses, for the \code{tern} users it provides a
#' documentation of arguments that are commonly and consistently used in the framework. For the developer it adds a
#' single reference point to import the \code{roxygen} argument description with
#' \code{@inheritParams argument_convention}
#'
#' @md
#' @param col_by (\code{factor} value)\cr for defining column group
#' @param na_rm (\code{logical} value)\cr
#'   Indicating whether \code{NA} values should be
#'   removed from \code{x} prior to counting the  'aa_trees.R' 'argument_convention.R' unique elements per cell.
#' @param tte (\code{numeric} vector)\cr
#'   Contains time to event data, currently no \code{NA} allowed
#' @param is_event (\code{logical} value)\cr
#'   \code{TRUE} if event, \code{FALSE} if \code{tte}
#'   is censored
#' @param rsp (\code{logical} vector)\cr
#'   \code{TRUE} if subject is a responder \code{FALSE} otherwise
#' @param conf_level confidence level of the interval.
#' @param prune_zero_rows `logical` whether to prune all zero rows.
#' @param lyt layout object pre-data used for tabulation.
#' @param df data frame containing analysis variables.
#' @param vars variable name(s) for the primary analysis variable(s).
#' @param variables list of additional analysis variables.
#' @param .var single variable name that is passed by rtables when requested by statistics function.
#' @param ... additional arguments for the lower level functions.
#'
#' @name argument_convention
NULL
