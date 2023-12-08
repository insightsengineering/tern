#' Difference Test for Two Proportions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Various tests were implemented to test the difference between two proportions.
#'
#' @inheritParams argument_convention
#' @param method (`string`)\cr one of `chisq`, `cmh`, `fisher`, or `schouten`; specifies the test used
#'   to calculate the p-value.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("test_proportion_diff")`
#'   to see available statistics for this function.
#'
#' @seealso [h_prop_diff_test]
#'
#' @name prop_diff_test
#' @order 1
NULL

#' @describeIn prop_diff_test Statistics function which tests the difference between two proportions.
#'
#' @return
#' * `s_test_proportion_diff()` returns a named `list` with a single item `pval` with an attribute `label`
#'   describing the method used. The p-value tests the null hypothesis that proportions in two groups are the same.
#'
#' @keywords internal
s_test_proportion_diff <- function(df,
                                   .var,
                                   .ref_group,
                                   .in_ref_col,
                                   variables = list(strata = NULL),
                                   method = c("chisq", "schouten", "fisher", "cmh")) {
  method <- match.arg(method)
  y <- list(pval = "")

  if (!.in_ref_col) {
    assert_df_with_variables(df, list(rsp = .var))
    assert_df_with_variables(.ref_group, list(rsp = .var))
    rsp <- factor(
      c(.ref_group[[.var]], df[[.var]]),
      levels = c("TRUE", "FALSE")
    )
    grp <- factor(
      rep(c("ref", "Not-ref"), c(nrow(.ref_group), nrow(df))),
      levels = c("ref", "Not-ref")
    )

    if (!is.null(variables$strata) || method == "cmh") {
      strata <- variables$strata
      checkmate::assert_false(is.null(strata))
      strata_vars <- stats::setNames(as.list(strata), strata)
      assert_df_with_variables(df, strata_vars)
      assert_df_with_variables(.ref_group, strata_vars)
      strata <- c(interaction(.ref_group[strata]), interaction(df[strata]))
    }

    tbl <- switch(method,
      cmh = table(grp, rsp, strata),
      table(grp, rsp)
    )

    y$pval <- switch(method,
      chisq = prop_chisq(tbl),
      cmh = prop_cmh(tbl),
      fisher = prop_fisher(tbl),
      schouten = prop_schouten(tbl)
    )
  }

  y$pval <- formatters::with_label(y$pval, d_test_proportion_diff(method))
  y
}

#' Description of the Difference Test Between Two Proportions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is an auxiliary function that describes the analysis in `s_test_proportion_diff`.
#'
#' @inheritParams s_test_proportion_diff
#'
#' @return `string` describing the test from which the p-value is derived.
#'
#' @export
d_test_proportion_diff <- function(method) {
  checkmate::assert_string(method)
  meth_part <- switch(method,
    "schouten" = "Chi-Squared Test with Schouten Correction",
    "chisq" = "Chi-Squared Test",
    "cmh" = "Cochran-Mantel-Haenszel Test",
    "fisher" = "Fisher's Exact Test",
    stop(paste(method, "does not have a description"))
  )
  paste0("p-value (", meth_part, ")")
}

#' @describeIn prop_diff_test Formatted analysis function which is used as `afun` in `test_proportion_diff()`.
#'
#' @return
#' * `a_test_proportion_diff()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_test_proportion_diff <- make_afun(
  s_test_proportion_diff,
  .formats = c(pval = "x.xxxx | (<0.0001)"),
  .indent_mods = c(pval = 1L)
)

#' @describeIn prop_diff_test Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `test_proportion_diff()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_test_proportion_diff()` to the table layout.
#'
#' @examples
#' dta <- data.frame(
#'   rsp = sample(c(TRUE, FALSE), 100, TRUE),
#'   grp = factor(rep(c("A", "B"), each = 50)),
#'   strat = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
#' )
#'
#' # With `rtables` pipelines.
#' l <- basic_table() %>%
#'   split_cols_by(var = "grp", ref_group = "B") %>%
#'   test_proportion_diff(
#'     vars = "rsp",
#'     method = "cmh", variables = list(strata = "strat")
#'   )
#'
#' build_table(l, df = dta)
#'
#' @export
#' @order 2
test_proportion_diff <- function(lyt,
                                 vars,
                                 variables = list(strata = NULL),
                                 method = c("chisq", "schouten", "fisher", "cmh"),
                                 na_str = default_na_str(),
                                 nested = TRUE,
                                 ...,
                                 var_labels = vars,
                                 show_labels = "hidden",
                                 table_names = vars,
                                 .stats = NULL,
                                 .formats = NULL,
                                 .labels = NULL,
                                 .indent_mods = NULL) {
  extra_args <- list(variables = variables, method = method, ...)

  afun <- make_afun(
    a_test_proportion_diff,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )
  analyze(
    lyt,
    vars,
    afun = afun,
    var_labels = var_labels,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    show_labels = show_labels,
    table_names = table_names
  )
}

#' Helper Functions to Test Proportion Differences
#'
#' Helper functions to implement various tests on the difference between two proportions.
#'
#' @param tbl (`matrix`)\cr matrix with two groups in rows and the binary response (`TRUE`/`FALSE`) in columns.
#'
#' @return A p-value.
#'
#' @seealso [prop_diff_test()] for implementation of these helper functions.
#'
#' @name h_prop_diff_test
NULL

#' @describeIn h_prop_diff_test performs Chi-Squared test. Internally calls [stats::prop.test()].
#'
#' @keywords internal
prop_chisq <- function(tbl) {
  checkmate::assert_integer(c(ncol(tbl), nrow(tbl)), lower = 2, upper = 2)
  tbl <- tbl[, c("TRUE", "FALSE")]
  if (any(colSums(tbl) == 0)) {
    return(1)
  }
  stats::prop.test(tbl, correct = FALSE)$p.value
}

#' @describeIn h_prop_diff_test performs stratified Cochran-Mantel-Haenszel test. Internally calls
#'   [stats::mantelhaen.test()]. Note that strata with less than two observations are automatically discarded.
#'
#' @param ary (`array`, 3 dimensions)\cr array with two groups in rows, the binary response
#'   (`TRUE`/`FALSE`) in columns, and the strata in the third dimension.
#'
#' @keywords internal
prop_cmh <- function(ary) {
  checkmate::assert_array(ary)
  checkmate::assert_integer(c(ncol(ary), nrow(ary)), lower = 2, upper = 2)
  checkmate::assert_integer(length(dim(ary)), lower = 3, upper = 3)
  strata_sizes <- apply(ary, MARGIN = 3, sum)
  if (any(strata_sizes < 5)) {
    warning("<5 data points in some strata. CMH test may be incorrect.")
    ary <- ary[, , strata_sizes > 1]
  }

  stats::mantelhaen.test(ary, correct = FALSE)$p.value
}

#' @describeIn h_prop_diff_test performs the Chi-Squared test with Schouten correction.
#'
#' @seealso Schouten correction is based upon \insertCite{Schouten1980-kd;textual}{tern}.
#'
#' @keywords internal
prop_schouten <- function(tbl) {
  checkmate::assert_integer(c(ncol(tbl), nrow(tbl)), lower = 2, upper = 2)
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

#' @describeIn h_prop_diff_test performs the Fisher's exact test. Internally calls [stats::fisher.test()].
#'
#' @keywords internal
prop_fisher <- function(tbl) {
  checkmate::assert_integer(c(ncol(tbl), nrow(tbl)), lower = 2, upper = 2)
  tbl <- tbl[, c("TRUE", "FALSE")]
  stats::fisher.test(tbl)$p.value
}
