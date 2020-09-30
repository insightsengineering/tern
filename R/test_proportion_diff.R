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
#' @references \insertRef{Schouten1980}{tern}
#' @importFrom Rdpack reprompt
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
    (
      abs(count_2_2 * count_1_1 - count_1_2 * count_2_1) -
        0.5 * min(count_1_1 + count_1_2, count_2_1 + count_2_2)
    )^2 /
    (
      (count_1_1 + count_1_2) * (count_2_1 + count_2_2) *
        (count_1_2 + count_2_2) * (count_1_1 + count_2_1)
    )

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


#' @describeIn prop_diff_test Statistics function which test the difference
#'  between two proportions.
#'
#' @inheritParams argument_convention
#' @param method (`string`)\cr
#'   one of (`chisq`, `cmh`, `fisher`, `schouten`; specifies the test used
#'   to calculate the p-value.
#'
#' @return Named `list` with a single item `pval` with an attribute `label`
#'   describing the method used. The p-value tests the null hypothesis that
#'   proportions in two groups are the same.
#' @export
#' @order 4
#' @examples
#'
#' # Statistics function
#' dta <- data.frame(
#'   rsp = sample(c(TRUE, FALSE), 100, TRUE),
#'   grp = factor(rep(c("A", "B"), each = 50)),
#'   strat = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
#' )
#'
#' s_test_proportion_diff(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   variables = list(strata = "strat"),
#'   method = "cmh"
#' )
#'
s_test_proportion_diff <- function(df,
                                   .var,
                                   .ref_group,
                                   .in_ref_col,
                                   variables = list(strata = NULL),
                                   method = c("chisq", "schouten", "fisher", "cmh")) {

  method <- match.arg(method)
  y <- list(pval = "")

  if (!.in_ref_col) {

    assert_that(
      is_df_with_variables(df, list(rsp = .var)),
      is_df_with_variables(.ref_group, list(rsp = .var))
    )

    rsp <- c(.ref_group[[.var]], df[[.var]])
    grp <- factor(
      rep(c("ref", "Not-ref"), c(nrow(.ref_group), nrow(df))),
      levels = c("ref", "Not-ref")
    )

    if (!is.null(variables$strata) || method == "cmh") {
      strata <- variables$strata
      assert_that(
        !is.null(strata),
        is_df_with_variables(df, list(rsp = strata)),
        is_df_with_variables(.ref_group, list(rsp = strata))
      )
      strata <- c(interaction(.ref_group[[strata]]), interaction(df[[strata]]))
    }

    tbl <- switch(
      method,
      cmh = table(grp, rsp, strata),
      table(grp, rsp)
    )

    y$pval <- switch(
      method,
      chisq = prop_chisq(tbl),
      cmh = prop_cmh(tbl),
      fisher = prop_fisher(tbl),
      schouten = prop_schouten(tbl)
    )
  }

  y$pval <- with_label(y$pval, d_test_proportion_diff(method))
  y
}


#' Description of the Difference Test Between Two Proportions
#'
#' This is an auxiliary function that describes the analysis in
#' `s_test_proportion_diff`.
#'
#' @inheritParams s_test_proportion_diff
#' @return `string` describing the test from which the p-value is derived.
#'
d_test_proportion_diff <- function(method) {
  assert_that(is.string(method))
  meth_part <- switch(
    method,
    "schouten" = "Chi-Squared Test with Schouten Correction",
    "chisq" = "Chi-Squared Test",
    "cmh" = "Cochran-Mantel-Haenszel Test",
    "fisher" = "Fisher's Exact Test",
    stop(paste(method, "does not have a description"))
  )
  paste0("p-value (", meth_part, ")")
}


#' @describeIn prop_diff_test Layout creating function which can be used for
#'   creating tables, which can take statistics function arguments and
#'   additional format arguments.
#' @param ... other arguments are passed to [s_test_proportion_diff()].
#' @export
#' @examples
#'
#' # With rtables pipelines.
#' l <- split_cols_by(lyt = NULL, var = "grp", ref_group = "B") %>%
#'   test_proportion_diff(
#'     vars = "rsp",
#'     method = "cmh", variables = list(strata = "strat")
#'   )
#'
#' build_table(l, df = dta)
#'
test_proportion_diff <- function(lyt,
                                 vars,
                                 show_labels = "hidden",
                                 ...) {
  afun <- format_wrap_df(
    sfun = s_test_proportion_diff,
    formats =  c(pval = "x.xxxx | (<0.0001)"),
    indent_mods = c(pval = 2L)
  )

  analyze(
    lyt,
    vars,
    afun = afun,
    extra_args = list(...),
    show_labels = show_labels
  )
}
