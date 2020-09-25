#' Difference Test for Two Proportions
#'
#' Various tests were implemented to test the difference between two
#' proportions.
#'
#' @template formatting_arguments
#'
#' @name prop_diff_test
#'
NULL


#' @describeIn prop_diff_test performs Chi-Squared test.
#'   Internally calls [stats::prop.test()].
#' @param tbl (`matrix`)\cr
#'   with two groups in rows and the binary response (`TRUE`/`FALSE`) in
#'   columns.
#' @importFrom stats prop.test
#' @export
#' @examples
#' A <- 20
#' B <- 20
#' set.seed(1)
#' rsp <- c(
#'   sample(c(TRUE, FALSE), size = A, prob = c(3/4, 1/4), replace = TRUE),
#'   sample(c(TRUE, FALSE), size = A, prob = c(1/2, 1/2), replace = TRUE)
#' )
#' grp <- c(rep("A", A), rep("B", B))
#' tbl <- table(grp, rsp)
#' prop_chisq(tbl)
#'
prop_chisq <- function(tbl) stats::prop.test(tbl, correct = FALSE)$p.value


#' @describeIn prop_diff_test performs stratified Cochran-Mantel-Haenszel test.
#'   Internally calls [stats::mantelhaen.test()].
#' @param tbl (`array`)\cr
#'   with two groups in rows, the binary response (`TRUE`/`FALSE`) in
#'   columns, the strata in third dimension.
#' @importFrom stats mantelhaen.test
#' @export
#' @examples
#'
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' grp <- factor(rep(c("A", "B"), each = 50))
#' strata <- factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
#' tbl <- table(grp, rsp, strata)
#' prop_cmh(tbl)
#'
prop_cmh <- function(tbl) {

  if (any(apply(tbl, MARGIN = 3, sum) < 5)) {
    warning("<5 data points in some strata. CMH test may be incorrect.")
  }

  stats::mantelhaen.test(tbl, correct = FALSE)$p.value
}
