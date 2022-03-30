#' Difference Test for Two Proportions
#'
#' Various tests were implemented to test the difference between two
#' proportions.
#'
#' @param tbl (`matrix`)\cr
#'   with two groups in rows and the binary response (`TRUE`/`FALSE`) in
#'   columns.
#'
#' @order 1
#' @name prop_diff_test
#'
NULL

#' @describeIn prop_diff_test performs Chi-Squared test.
#'   Internally calls [stats::prop.test()].
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
#'   sample(c(TRUE, FALSE), size = A, prob = c(3 / 4, 1 / 4), replace = TRUE),
#'   sample(c(TRUE, FALSE), size = A, prob = c(1 / 2, 1 / 2), replace = TRUE)
#' )
#' grp <- c(rep("A", A), rep("B", B))
#' tbl <- table(grp, rsp)
#'
#' ## Chi-Squared test
#' prop_chisq(tbl)
prop_chisq <- function(tbl) {
  assertthat::assert_that(ncol(tbl) == 2, nrow(tbl) == 2)
  tbl <- tbl[, c("TRUE", "FALSE")]
  if (any(colSums(tbl) == 0)) {
    return(1)
  }
  stats::prop.test(tbl, correct = FALSE)$p.value
}

#' @describeIn prop_diff_test performs stratified Cochran-Mantel-Haenszel test.
#'   Internally calls [stats::mantelhaen.test()]. Note that strata with less than two observations
#'   are automatically discarded.
#' @param ary (`array`, 3 dimensions)\cr
#'   with two groups in rows, the binary response (`TRUE`/`FALSE`) in
#'   columns, the strata in the third dimension.
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


#' @describeIn prop_diff_test performs the Chi-Squared test with Schouten
#'   correction ([Schouten 1980](
#'   https://onlinelibrary.wiley.com/doi/abs/10.1002/bimj.4710220305)).
#' @export
#' @order 2
#' @examples
#'
#' ## Chi-Squared test + Schouten correction.
#' prop_schouten(tbl)
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


#' @describeIn prop_diff_test performs the Fisher's exact test.
#'   Internally calls [stats::fisher.test()].
#' @export
#' @order 2
#' @examples
#'
#' ## Fisher's exact test
#' prop_fisher(tbl)
prop_fisher <- function(tbl) {
  assertthat::assert_that(ncol(tbl) == 2, nrow(tbl) == 2)
  tbl <- tbl[, c("TRUE", "FALSE")]
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
#'
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
s_test_proportion_diff <- function(df,
                                   .var,
                                   .ref_group,
                                   .in_ref_col,
                                   variables = list(strata = NULL),
                                   method = c("chisq", "schouten", "fisher", "cmh")) {
  method <- match.arg(method)
  y <- list(pval = "")

  if (!.in_ref_col) {
    assertthat::assert_that(
      is_df_with_variables(df, list(rsp = .var)),
      is_df_with_variables(.ref_group, list(rsp = .var))
    )
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
        !is.null(strata),
        is_df_with_variables(df, strata_vars),
        is_df_with_variables(.ref_group, strata_vars)
      )
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
#' This is an auxiliary function that describes the analysis in
#' `s_test_proportion_diff`.
#'
#' @inheritParams s_test_proportion_diff
#' @return `string` describing the test from which the p-value is derived.
#' @keywords internal
#'
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
#' @export
#'
#' @examples
#' a_test_proportion_diff(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   variables = list(strata = "strat"),
#'   method = "cmh"
#' )
a_test_proportion_diff <- make_afun(
  s_test_proportion_diff,
  .formats = c(pval = "x.xxxx | (<0.0001)"),
  .indent_mods = c(pval = 1L)
)

#' @describeIn prop_diff_test Layout creating function which can be used for
#'   creating tables, which can take statistics function arguments and
#'   additional format arguments.
#' @param ... other arguments are passed to [s_test_proportion_diff()].
#' @inheritParams argument_convention
#' @export
#' @examples
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
