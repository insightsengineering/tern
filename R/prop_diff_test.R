#' Difference Test for Two Proportions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Various tests were implemented to test the difference between two
#' proportions.
#'
#' @param tbl (`matrix`)\cr
#'   with two groups in rows and the binary response (`TRUE`/`FALSE`) in
#'   columns.
#'
#' @name prop_diff_test
NULL

#' @describeIn prop_diff_test Statistics function which tests the difference
#' between two proportions.
#'
#' @inheritParams argument_convention
#' @param method (`string`)\cr
#'   one of (`chisq`, `cmh`, `fisher`, `schouten`; specifies the test used
#'   to calculate the p-value.
#' @return Named `list` with a single item `pval` with an attribute `label`
#'   describing the method used. The p-value tests the null hypothesis that
#'   proportions in two groups are the same.
#'
#' @examples
#' # Statistics function
#' dta <- data.frame(
#'   rsp = sample(c(TRUE, FALSE), 100, TRUE),
#'   grp = factor(rep(c("A", "B"), each = 50)),
#'   strat = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
#' )
#'
#' tern:::s_test_proportion_diff(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   variables = list(strata = "strat"),
#'   method = "cmh"
#' )
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
      strata_vars <- stats::setNames(as.list(strata), strata)
      assertthat::assert_that(
        !is.null(strata)
      )
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
#' This is an auxiliary function that describes the analysis in
#' `s_test_proportion_diff`.
#'
#' @inheritParams s_test_proportion_diff
#' @return `string` describing the test from which the p-value is derived.
#'
#' @export
d_test_proportion_diff <- function(method) {
  assertthat::assert_that(assertthat::is.string(method))
  meth_part <- switch(method,
                      "schouten" = "Chi-Squared Test with Schouten Correction",
                      "chisq" = "Chi-Squared Test",
                      "cmh" = "Cochran-Mantel-Haenszel Test",
                      "fisher" = "Fisher's Exact Test",
                      stop(paste(method, "does not have a description"))
  )
  paste0("p-value (", meth_part, ")")
}

#' @describeIn prop_diff_test Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#'
#' @examples
#' tern:::a_test_proportion_diff(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   variables = list(strata = "strat"),
#'   method = "cmh"
#' )
#'
#' @keywords internal
a_test_proportion_diff <- make_afun(
  s_test_proportion_diff,
  .formats = c(pval = "x.xxxx | (<0.0001)"),
  .indent_mods = c(pval = 1L)
)

#' @describeIn prop_diff_test Layout creating function which can be used for
#'   creating tables, which can take statistics function arguments and
#'   additional format arguments.
#'
#' @param ... other arguments are passed to [s_test_proportion_diff()].
#' @inheritParams argument_convention
#' @export
#'
#' @examples
#' # With `rtables` pipelines.
#' l <- basic_table() %>%
#'   split_cols_by(var = "grp", ref_group = "B") %>%
#'   test_proportion_diff(
#'     vars = "rsp",
#'     method = "cmh", variables = list(strata = "strat")
#'   )
#'
#' build_table(l, df = dta)
test_proportion_diff <- function(lyt,
                                 vars,
                                 ...,
                                 show_labels = "hidden",
                                 table_names = vars,
                                 .stats = NULL,
                                 .formats = NULL,
                                 .labels = NULL,
                                 .indent_mods = NULL) {
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
    extra_args = list(...),
    show_labels = show_labels,
    table_names = table_names
  )
}
