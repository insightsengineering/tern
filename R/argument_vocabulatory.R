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
#' @param col_by factor with reference and comparison group information, the
#'   first \code{level} indicates the reference group.
#' @param col_N a vecor with the reference population per level of \code{col_by}. This often by default
#'   \code{table(col_by)}.
#' @param total character string that will be used as a label for a column with total population. If the levels of
#'   \code{col_by} are the only columns of interest then total should be \code{NULL}.
#' @param table_tree logical, if \code{FALSE} then a single \code{rtable} gets returned, if \code{FALSE} a
#'   \code{table_tree} gets returned with is either a list or a nested list of elementary \code{rtable} objects.
#' @param indent non-negative integer where 0 means that the row should not be indented
#' @param na_rm a logical value indicating whether \code{NA} values should be
#'   removed from \code{x} prior to counting the unique elements per cell.
#' @param tte vector with numeric time to event data, currently no \code{NA} allowed
#' @param is_event is boolean, \code{TRUE} if event, \code{FALSE} if \code{tte}
#'   is censored
argument_convention <- function(col_by, col_N, total, table_tree, indent, na_rm, tte, is_event) { #nolintr
  NULL
}
