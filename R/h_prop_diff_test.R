#' Helper Functions to Test Proportion Differences
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper functions to implement various tests on the difference between two
#' proportions.
#'
#' @param tbl (`matrix`)\cr
#'   with two groups in rows and the binary response (`TRUE`/`FALSE`) in
#'   columns.
#'
#' @seealso [prop_diff_test()] for implementation of these helper functions.
#'
#' @name h_prop_diff_test
NULL

#' @describeIn h_prop_diff_test performs Chi-Squared test.
#'   Internally calls [stats::prop.test()].
#'
#' @examples
#' # Non-stratified proportion difference test
#'
#' ## Data
#' A <- 20
#' B <- 20
#' set.seed(1)
#' rsp <- c(
#'   sample(c(TRUE, FALSE), size = A, prob = c(3 / 4, 1 / 4), replace = TRUE),
#'   sample(c(TRUE, FALSE), size = A, prob = c(1 / 2, 1 / 2), replace = TRUE)
#' )
#' grp <- c(rep("A", A), rep("B", B))
#' tbl <- table(grp, rsp)
#'
#' ## Chi-Squared test
#' tern:::prop_chisq(tbl)
#'
#' @keywords internal
prop_chisq <- function(tbl) {
  assertthat::assert_that(ncol(tbl) == 2, nrow(tbl) == 2)
  tbl <- tbl[, c("TRUE", "FALSE")]
  if (any(colSums(tbl) == 0)) {
    return(1)
  }
  stats::prop.test(tbl, correct = FALSE)$p.value
}

#' @describeIn h_prop_diff_test performs the Chi-Squared test with Schouten
#'   correction ([Schouten 1980](
#'   https://onlinelibrary.wiley.com/doi/abs/10.1002/bimj.4710220305)).
#'
#' @examples
#' ## Chi-Squared test + Schouten correction.
#' tern:::prop_schouten(tbl)
#'
#' @keywords internal
prop_schouten <- function(tbl) {
  assertthat::assert_that(ncol(tbl) == 2, nrow(tbl) == 2)

  tbl <- tbl[, c("TRUE", "FALSE")]
  if (any(colSums(tbl) == 0)) {
    return(1)
  }

  n <- sum(tbl)
  n1 <- sum(tbl[1, ])
  n2 <- sum(tbl[2, ])

  ad <- diag(tbl)
  bc <- diag(apply(tbl, 2, rev))
  ac <- tbl[, 1]
  bd <- tbl[, 2]

  t_schouten <- (n - 1) *
    (abs(prod(ad) - prod(bc)) - 0.5 * min(n1, n2))^2 /
    (n1 * n2 * sum(ac) * sum(bd))

  1 - stats::pchisq(t_schouten, df = 1)
}

#' @describeIn h_prop_diff_test performs the Fisher's exact test.
#'   Internally calls [stats::fisher.test()].
#'
#' @examples
#' ## Fisher's exact test
#' tern:::prop_fisher(tbl)
#'
#' @keywords internal
prop_fisher <- function(tbl) {
  assertthat::assert_that(ncol(tbl) == 2, nrow(tbl) == 2)
  tbl <- tbl[, c("TRUE", "FALSE")]
  stats::fisher.test(tbl)$p.value
}

#' @describeIn h_prop_diff_test performs stratified Cochran-Mantel-Haenszel test.
#'   Internally calls [stats::mantelhaen.test()]. Note that strata with less than two observations
#'   are automatically discarded.
#'
#' @param ary (`array`, 3 dimensions)\cr
#'   with two groups in rows, the binary response (`TRUE`/`FALSE`) in
#'   columns, the strata in the third dimension.
#'
#' @examples
#' # Stratified proportion difference test
#'
#' ## Data
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' grp <- factor(rep(c("A", "B"), each = 50))
#' strata <- factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
#' tbl <- table(grp, rsp, strata)
#'
#' ## Cochran-Mantel-Haenszel test
#' tern:::prop_cmh(tbl)
#'
#' @keywords internal
prop_cmh <- function(ary) {
  assertthat::assert_that(
    is.array(ary),
    ncol(ary) == 2, nrow(ary) == 2,
    length(dim(ary)) == 3
  )

  strata_sizes <- apply(ary, MARGIN = 3, sum)
  if (any(strata_sizes < 5)) {
    warning("<5 data points in some strata. CMH test may be incorrect.")
    ary <- ary[, , strata_sizes > 1]
  }

  stats::mantelhaen.test(ary, correct = FALSE)$p.value
}
