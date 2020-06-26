#' Dummy Function that Documents all Standard Arguments
#'
#' The documentation to this function lists all the arguments in \code{tern} that are used repeatedly to express an
#' analysis
#'
#'
#' @details
#' Although this function just returns \code{NULL} it has two uses, for the \code{tern} users it provides a
#' documentation of arguments that are commonly and consistently used in the framework. For the developer it adds a
#' single reference point to import the \code{roxygen} argument description with
#' \code{@inheritParams argument_convention}
#'
#' @md
#' @param col_by (\code{logical}, \code{factor} or \code{data.frame})\cr
#'   Contains reference and comparison group information, the
#'   first \code{level} indicates the reference group. See the functions
#'   \code{\link[rtables]{col_by_to_matrix}} and \code{\link[rtables]{by_add_total}}.
#' @param col_N (\code{integer} vector)\cr
#'   Contains the reference population per level of \code{col_by}. This by default is often
#'   set to \code{get_N(col_by)}. Names of the list are ignored, they are added in the order they appear.
#'   The default may not appropriate and should be modified by the user when \code{col_by} contains
#'   more than one record per subject.
#' @param table_tree (\code{logical} value)\cr
#'   If \code{FALSE} then a single \code{rtable} gets returned, if \code{TRUE} a
#'   \code{\link{node}} object gets returned with the elementary \code{rtable} objects.
#' @param indent (non-negative \code{integer})\cr
#'   0 means that the row should not be indented
#' @param na_rm (\code{logical} value)\cr
#'   Indicating whether \code{NA} values should be
#'   removed from \code{x} prior to counting the unique elements per cell.
#' @param tte (\code{numeric} vector)\cr
#'   Contains time to event data, currently no \code{NA} allowed
#' @param is_event (\code{logical} value)\cr
#'   \code{TRUE} if event, \code{FALSE} if \code{tte}
#'   is censored
#' @param rsp (\code{logical} vector)\cr
#'   \code{TRUE} if subject is a responder \code{FALSE} otherwise
#' @param total (\code{character} value)\cr
#'   Denotes column name of an added total column using \code{\link[rtables]{by_add_total}} to
#'   \code{col_by} and \code{\link{col_N_add_total}} to \code{col_N}. If \code{NULL} no total column is added.
#' @param conf_level confidence level of the interval.
#' @param prune_zero_rows `logical` whether to prune all zero rows
#'
#' @name argument_convention
NULL
