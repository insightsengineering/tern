#' Dummy Function that Documents all Standard Arguments
#'
#' The documentation to this function lists all the arguments in \code{tern} that are used repeatedly to express an
#' analysis
#'
#'
#' @details
#' Although this function just returns \code{NULL} it has two uses, for the \code{tern} users it provides a
#' documentation of arguments that are commonly and consistently used in the framework. For the developer it adds a
#' single reference point to import the roxygen argument description with \code{@inheritParams argument_convention}
#'
#' @param col_by logical, factor or data.frame with reference and comparison group information, the
#'   first \code{level} indicates the reference group. See the functions \code{factor_to_matrix_by}
#'   and \code{add_total_by}.
#' @param col_N a vecor with the reference population per level of \code{col_by}. This by default is often
#'   set to \code{get_N(col_by)}. Names of the list are ignored, they are added in the order they appear.
#'   The default may not appropriate and should be modified by the user when \code{col_by} contains
#'   more than one record per subject.
#' @param table_tree logical, if \code{FALSE} then a single \code{rtable} gets returned, if \code{TRUE} a
#'   \code{\link{node}} object gets returned with the elementary \code{rtable} objects.
#' @param indent non-negative integer where 0 means that the row should not be indented
#' @param na_rm a logical value indicating whether \code{NA} values should be
#'   removed from \code{x} prior to counting the unique elements per cell.
#' @param tte vector with numeric time to event data, currently no \code{NA} allowed
#' @param is_event is boolean, \code{TRUE} if event, \code{FALSE} if \code{tte}
#'   is censored
#' @param rsp boolean vector, \code{TRUE} if subject is a responder \code{FALSE}
#'   otherwise
#' @param total string of column name of an added total column using \code{\link[rtables]{by_add_total}} to
#'   \code{col_by} and \code{\link{col_N_add_total}} to \code{col_N}. If \code{NULL} no total column is added.
#'
argument_convention <- function(col_by, col_N, table_tree, indent, na_rm, tte, is_event, rsp, total) { # nolintr
  NULL
}
