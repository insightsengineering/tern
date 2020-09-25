#' Difference Test for Two Proportions
#'
#' Various tests were implemented to test the difference between two
#' proportions.
#'
#' @param tbl (`matrix`)\cr
#'   with two groups in rows and the binary response (`TRUE`/`FALSE`) in
#'   columns.
#'
#' @template formatting_arguments
#' @order 1
#' @name prop_diff_test
#'
NULL


#' @describeIn prop_diff_test performs Chi-Squared test.
#'   Internally calls [stats::prop.test()].
#' @importFrom stats prop.test
#' @export
#' @order 2
#' @examples
#'
#' # Non-stratified proportion difference test
#'
#' ## Data
#' A <- 20
#' B <- 20
#' set.seed(1)
#' rsp <- c(
#'   sample(c(TRUE, FALSE), size = A, prob = c(3/4, 1/4), replace = TRUE),
#'   sample(c(TRUE, FALSE), size = A, prob = c(1/2, 1/2), replace = TRUE)
#' )
#' grp <- c(rep("A", A), rep("B", B))
#' tbl <- table(grp, rsp)
#'
#' ## Chi-Squared test
#' prop_chisq(tbl)
#'
prop_chisq <- function(tbl) {
  assert_that(ncol(tbl) == 2, nrow(tbl) == 2)
  stats::prop.test(tbl, correct = FALSE)$p.value
}


#' @describeIn prop_diff_test performs stratified Cochran-Mantel-Haenszel test.
#'   Internally calls [stats::mantelhaen.test()].
#' @param ary (`array`, 3 dimensions)\cr
#'   with two groups in rows, the binary response (`TRUE`/`FALSE`) in
#'   columns, the strata in the third dimension.
#' @importFrom stats mantelhaen.test
#' @export
#' @order 3
#' @examples
#'
#' # Stratified proportion difference test
#'
#' ## Data
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' grp <- factor(rep(c("A", "B"), each = 50))
#' strata <- factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
#' tbl <- table(grp, rsp, strata)
#'
#' ## Cochran-Mantel-Haenszel test
#' prop_cmh(tbl)
#'
prop_cmh <- function(ary) {

  assert_that(
    is.array(ary),
    ncol(ary) == 2, nrow(ary) == 2,
    length(dim(ary)) == 3
  )

  if (any(apply(ary, MARGIN = 3, sum) < 5)) {
    warning("<5 data points in some strata. CMH test may be incorrect.")
  }

  stats::mantelhaen.test(ary, correct = FALSE)$p.value
}


#' @describeIn prop_diff_test performs the Chi-Squared test with Schouten
#'   correction.
#' @export
#' @order 2
#' @examples
#'
#' ## Chi-Squared test + Schouten correction.
#' prop_schouten(tbl)
#'
prop_schouten <- function(tbl) {

  assert_that(ncol(tbl) == 2, nrow(tbl) == 2)

  # Source: STREAM v2
  #nolint start
  # https://github.roche.com/MDIS/stream2/blob/82c6c54ea6c61d11746af3b413ad6b1b213096cd/app/macro/str_tlg_method_resp.sas#L1623
  #nolint end
  count_1_1 <- tbl[1, "FALSE"]
  count_1_2 <- tbl[1, "TRUE"]
  count_2_1 <- tbl[2, "FALSE"]
  count_2_2 <- tbl[2, "TRUE"]

  t_schouten <- (count_1_1 + count_1_2 + count_2_1 + count_2_2 - 1) *
    (abs(count_2_2 * count_1_1 - count_1_2 * count_2_1) -
       0.5 * min(count_1_1 + count_1_2, count_2_1 + count_2_2))^2 /
    ((count_1_1 + count_1_2) * (count_2_1 + count_2_2) *
       (count_1_2 + count_2_2) * (count_1_1 + count_2_1))

  1 - stats::pchisq(t_schouten, df = 1)
}


#' @describeIn prop_diff_test performs the Fisher's exact test.
#'   Internally calls [stats::fisher.test()].
#' @importFrom stats fisher.test
#' @export
#' @order 2
#' @examples
#'
#' ## Fisher's exact test
#' prop_fisher(tbl)
#'
prop_fisher <- function(tbl) {
  assert_that(ncol(tbl) == 2, nrow(tbl) == 2)
  stats::fisher.test(tbl)$p.value
}
