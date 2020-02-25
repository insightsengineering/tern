#' Calculate statistics Pairwise with reference level
#'
#' @inheritParams rtables::rrow
#' @param x a vector
#' @param col_by a factor first level is taken as reference level
#' @param FUN a function with two arguments, first argument is the subset of x
#'   and the second argument is a factor with two levels from col_by
#'
#' @importFrom stats relevel
#' @export
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
#'
#' tabulate_pairwise(
#'    x = data.frame(
#'         val = sample(c("X1", "X2"), 100, replace = TRUE),
#'         strata = sample(c("A", "B"), 100, replace = TRUE)
#'    ),
#'   col_by = factor(sample(c("BY1", "BY2", "BY3"), 100, replace = TRUE)),
#'   FUN = function(xi, col_by_i) {
#'             mhp <- mantelhaen.test(xi$val, col_by_i, xi$strata)
#'             mhp$p.value
#'             },
#'   row.name = "MH pvalue: stratified"
#' )
#'
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
