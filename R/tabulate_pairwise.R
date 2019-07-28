#' Calculate statistics Pairwise with reference level
#'
#' @inheritParams rtables::rrow
#' @param x a vector
#' @param col_by a factor first level is taken as reference level
#' @param FUN a function with two arguments, first argument is the subset of x
#'   and the second argument is a factor with two levels from col_by
#'
#' @importFrom stats relevel
#'
#' @template author_waddella
#'
#' @return an \code{rtable}
#'
#' @examples
#' tabulate_pairwise(
#'   x = iris$Sepal.Length,
#'   col_by = iris$Species,
#'   FUN = function(xi, col_by_i) diff(tapply(xi, col_by_i, mean)),
#'   row.name = "diff mean"
#' )
tabulate_pairwise <- function(x,
                              col_by,
                              FUN, # nolint
                              row.name, # nolint
                              format = NULL,
                              indent = 0) {
  stopifnot(
    is.atomic(x) || is.data.frame(x),
    is.factor(col_by),
    length(levels(col_by)) >= 2,
    !any(is.na(col_by))
  )

  ref_level <- levels(col_by)[1]

  row_data <- lapply(levels(col_by)[-1], function(comp_level) {

    sel <- col_by %in% c(ref_level, comp_level)

    x_sel <- subset(x, sel)
    tmp_col_by_sel <- subset(col_by, sel)
    col_by_sel <- relevel(droplevels(tmp_col_by_sel), ref_level, comp_level)

    FUN(x_sel, col_by_sel)
  })

  header <- rheader(rrowl("", levels(col_by)))

  rtable(
    header = header,
    rrowl(row.name = row.name, c(list(NULL), row_data), format = format, indent = indent)
  )
}

#todo: keep this??
#' #' Calculate statistics Pairwise with reference level
#' #'
#' #' @inheritParams rtables::rrow
#' #' @param x a vector
#' #' @param col_by a factor first level is taken as reference level
#' #' @param FUN a function with two arguments, first argument is the subset of x
#' #'   and the second argument is a factor with two levels from col_by
#' #'
#' #' @importFrom stats relevel
#' #'
#' #' @template author_waddella
#' #'
#' #' @return an \code{rtable}
#' #'
#' #' @examples
#' #' tabulate_pairwise(
#' #'   x = iris$Sepal.Length,
#' #'   col_by = iris$Species,
#' #'   FUN = function(x, col_by_i) {
#' #'     mean(subset(x, col_by_i[, 2])) - mean(subset(x, col_by_i[, 1]))
#' #'   },
#' #'   row.name = "diff mean"
#' #' )
#' #'
#' #' #todo: add ref_level argument to not select first column
#' #' # todo: should we keep old simplified version as well when col_by is a factor?
#' tabulate_pairwise <- function(x,
#'                               col_by,
#'                               FUN, # nolint
#'                               row.name, # nolint
#'                               format = NULL) {
#'   col_by <- col_by_to_matrix(col_by, x)
#'   stopifnot(
#'     is.atomic(x) || is.data.frame(x),
#'     ncol(col_by) >= 2
#'   )
#'
#'   ref_level <- colnames(col_by)[[1]]
#'
#'   row_data <- lapply(colnames(col_by)[-1], function(comp_level) {
#'     # rows <- do.call(`|`, col_by[, c(ref_level, comp_level)]) # any row-wise, todo: is there a nicer alternative with axis argument?
#'     # FUN(subset(x, rows), col_by[rows, c(ref_level, comp_level)])
#'     FUN(x, col_by[, c(ref_level, comp_level)])
#'   })
#'
#'   header <- rheader(rrowl("", colnames(col_by)))
#'   rtable(
#'     header = header,
#'     rrowl(row.name = row.name, c(list(NULL), row_data), format = format)
#'   )
#' }
