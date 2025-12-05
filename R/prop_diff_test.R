#' Difference test for two proportions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [test_proportion_diff()] creates a layout element to test the difference between two
#' proportions. The primary analysis variable, `vars`, indicates whether a response has occurred for each record. See
#' the `method` parameter for options of methods to use to calculate the p-value. The argument `alternative`
#' specifies the direction of the alternative hypothesis. Additionally, a stratification variable can be
#' supplied via the `strata` element of the `variables` argument.
#'
#' @inheritParams argument_convention
#' @param method (`string`)\cr one of `chisq`, `cmh`, `cmh_wh`, `fisher`, or `schouten`;
#'   specifies the test used to calculate the p-value.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("test_proportion_diff"), type = "sh")``
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
#' @examples
#'
#' ## "Mid" case: 4/4 respond in group A, 1/2 respond in group B.
#' nex <- 100 # Number of example rows
#' dta <- data.frame(
#'   "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
#'   "grp" = sample(c("A", "B"), nex, TRUE),
#'   "f1" = sample(c("a1", "a2"), nex, TRUE),
#'   "f2" = sample(c("x", "y", "z"), nex, TRUE),
#'   stringsAsFactors = TRUE
#' )
#' s_test_proportion_diff(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   variables = NULL,
#'   method = "chisq"
#' )
#'
#' @export
s_test_proportion_diff <- function(df,
                                   .var,
                                   .ref_group,
                                   .in_ref_col,
                                   variables = list(strata = NULL),
                                   method = c("chisq", "schouten", "fisher", "cmh", "cmh_wh"),
                                   alternative = c("two.sided", "less", "greater"),
                                   ...) {
  method <- match.arg(method)
  y <- list(pval = numeric())

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

    if (!is.null(variables$strata) || method %in% c("cmh", "cmh_wh")) {
      strata <- variables$strata
      checkmate::assert_false(is.null(strata))
      strata_vars <- stats::setNames(as.list(strata), strata)
      assert_df_with_variables(df, strata_vars)
      assert_df_with_variables(.ref_group, strata_vars)
      strata <- c(interaction(.ref_group[strata]), interaction(df[strata]))
    }

    tbl <- switch(method,
      cmh = table(grp, rsp, strata),
      cmh_wh = table(grp, rsp, strata),
      table(grp, rsp)
    )

    y$pval <- switch(method,
      chisq = prop_chisq(tbl, alternative = alternative),
      cmh = prop_cmh(tbl, alternative = alternative),
      fisher = prop_fisher(tbl, alternative = alternative),
      schouten = prop_schouten(tbl, alternative = alternative),
      cmh_wh = prop_cmh(tbl, alternative = alternative, transform = "wilson_hilferty")
    )
  }

  y$pval <- formatters::with_label(y$pval, d_test_proportion_diff(method, alternative = alternative))
  y
}

#' Description of the difference test between two proportions
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is an auxiliary function that describes the analysis in `s_test_proportion_diff`.
#'
#' @inheritParams s_test_proportion_diff
#'
#' @return A `string` describing the test from which the p-value is derived.
#'
#' @export
d_test_proportion_diff <- function(method, alternative = c("two.sided", "less", "greater")) {
  checkmate::assert_string(method)
  alternative <- match.arg(alternative)

  meth_part <- switch(method,
    "schouten" = "Chi-Squared Test with Schouten Correction",
    "chisq" = "Chi-Squared Test",
    "cmh" = "Cochran-Mantel-Haenszel Test",
    "cmh_wh" = "Cochran-Mantel-Haenszel Test with Wilson-Hilferty Transformation",
    "fisher" = "Fisher's Exact Test",    
    stop(paste(method, "does not have a description"))
  )
  alt_part <- switch(alternative,
    two.sided = "",
    less = ", 1-sided, direction less",
    greater = ", 1-sided, direction greater"
  )
  paste0("p-value (", meth_part, alt_part, ")")
}

#' @describeIn prop_diff_test Formatted analysis function which is used as `afun` in `test_proportion_diff()`.
#'
#' @return
#' * `a_test_proportion_diff()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_test_proportion_diff <- function(df,
                                   ...,
                                   .stats = NULL,
                                   .stat_names = NULL,
                                   .formats = NULL,
                                   .labels = NULL,
                                   .indent_mods = NULL) {
  dots_extra_args <- list(...)

  # Check if there are user-defined functions
  default_and_custom_stats_list <- .split_std_from_custom_stats(.stats)
  .stats <- default_and_custom_stats_list$all_stats
  custom_stat_functions <- default_and_custom_stats_list$custom_stats

  # Adding automatically extra parameters to the statistic function (see ?rtables::additional_fun_params)
  extra_afun_params <- retrieve_extra_afun_params(
    names(dots_extra_args$.additional_fun_parameters)
  )
  dots_extra_args$.additional_fun_parameters <- NULL # After extraction we do not need them anymore

  # Main statistical functions application
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_test_proportion_diff,
    custom_stat_fnc_list = custom_stat_functions,
    args_list = c(
      df = list(df),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in with stats defaults if needed
  .stats <- get_stats("test_proportion_diff",
    stats_in = .stats,
    custom_stats_in = names(custom_stat_functions)
  )

  x_stats <- x_stats[.stats]

  # Fill in formats/indents/labels with custom input and defaults
  .formats <- get_formats_from_stats(.stats, .formats)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods)
  if (is.null(.labels)) {
    .labels <- sapply(x_stats, attr, "label")
    .labels <- .labels[nzchar(.labels) & !sapply(.labels, is.null) & !is.na(.labels)]
  }
  .labels <- get_labels_from_stats(.stats, .labels)

  # Auto format handling
  .formats <- apply_auto_formatting(
    .formats,
    x_stats,
    extra_afun_params$.df_row,
    extra_afun_params$.var
  )

  # Get and check statistical names from defaults
  .stat_names <- get_stat_names(x_stats, .stat_names) # note is x_stats

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = names(.labels),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

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
#'   strata = factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
#' )
#'
#' # With `rtables` pipelines.
#' l <- basic_table() %>%
#'   split_cols_by(var = "grp", ref_group = "B") %>%
#'   test_proportion_diff(
#'     vars = "rsp",
#'     method = "cmh", variables = list(strata = "strata")
#'   )
#'
#' build_table(l, df = dta)
#'
#' @export
#' @order 2
test_proportion_diff <- function(lyt,
                                 vars,
                                 variables = list(strata = NULL),
                                 method = c("chisq", "schouten", "fisher", "cmh", "cmh_wh"),
                                 alternative = c("two.sided", "less", "greater"),
                                 var_labels = vars,
                                 na_str = default_na_str(),
                                 nested = TRUE,
                                 show_labels = "hidden",
                                 table_names = vars,
                                 section_div = NA_character_,
                                 ...,
                                 na_rm = TRUE,
                                 .stats = c("pval"),
                                 .stat_names = NULL,
                                 .formats = c(pval = "x.xxxx | (<0.0001)"),
                                 .labels = NULL,
                                 .indent_mods = c(pval = 1L)) {
  # Depending on main functions
  extra_args <- list(
    "na_rm" = na_rm,
    "variables" = variables,
    "method" = method,
    "alternative" = alternative,
    ...
  )

  # Needed defaults
  if (!is.null(.stats)) extra_args[[".stats"]] <- .stats
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Adding all additional information from layout to analysis functions (see ?rtables::additional_fun_params)
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_test_proportion_diff) <- c(
    formals(a_test_proportion_diff),
    extra_args[[".additional_fun_parameters"]]
  )

  # Main {rtables} structural call
  analyze(
    lyt = lyt,
    vars = vars,
    var_labels = var_labels,
    afun = a_test_proportion_diff,
    na_str = na_str,
    inclNAs = !na_rm,
    nested = nested,
    extra_args = extra_args,
    show_labels = show_labels,
    table_names = table_names,
    section_div = section_div
  )
}

#' Helper functions to test proportion differences
#'
#' Helper functions to implement various tests on the difference between two proportions.
#'
#' @param tbl (`matrix`)\cr matrix with two groups in rows and the binary response (`TRUE`/`FALSE`) in columns.
#' @inheritParams argument_convention
#'
#' @return A p-value.
#'
#' @seealso [prop_diff_test()] for implementation of these helper functions.
#'
#' @name h_prop_diff_test
NULL

#' @describeIn h_prop_diff_test Performs Chi-Squared test. Internally calls [stats::prop.test()].
#'
#' @keywords internal
prop_chisq <- function(tbl, alternative = c("two.sided", "less", "greater")) {
  checkmate::assert_integer(c(ncol(tbl), nrow(tbl)), lower = 2, upper = 2)
  tbl <- tbl[, c("TRUE", "FALSE")]
  if (any(colSums(tbl) == 0)) {
    return(1)
  }
  stats::prop.test(tbl, correct = FALSE, alternative = alternative)$p.value
}

#' @describeIn h_prop_diff_test Performs stratified Cochran-Mantel-Haenszel test,
#'   using [stats::mantelhaen.test()] internally.
#'   Note that strata with less than two observations are automatically discarded.
#' 
#' @param ary (`array`, 3 dimensions)\cr array with two groups in rows, the binary response
#'   (`TRUE`/`FALSE`) in columns, and the strata in the third dimension.
#' @param transform (`string`)\cr either `none` or `wilson_hilferty`; specifies whether to apply
#'   the Wilson-Hilferty transformation of the chi-squared statistic.
#'
#' @keywords internal
prop_cmh <- function(ary, 
                     alternative = c("two.sided", "less", "greater"), 
                     transform = c("none", "wilson_hilferty")) {
  checkmate::assert_array(ary)
  checkmate::assert_integer(c(ncol(ary), nrow(ary)), lower = 2, upper = 2)
  checkmate::assert_integer(length(dim(ary)), lower = 3, upper = 3)
  alternative <- match.arg(alternative)
  transform <- match.arg(transform)

  strata_sizes <- apply(ary, MARGIN = 3, sum)
  if (any(strata_sizes < 5)) {
    warning("<5 data points in some strata. CMH test may be incorrect.")
    ary <- ary[, , strata_sizes > 1]
  }

  cmh_res <- stats::mantelhaen.test(ary, correct = FALSE, alternative = alternative)

  if (transform == "none") {
    cmh_res$p.value
  } else {
    chisq_stat <- unname(cmh_res$statistic)
    df <- unname(cmh_res$parameter)
    num <- (chisq_stat / df)^(1 / 3) - (1 - 2 / (9 * df))
    denom <- sqrt(2 / (9 * df))
    wh_stat <- num / denom

    if (alternative == "two.sided") {
      2 * stats::pnorm(-abs(wh_stat))
    } else {
      stats::pnorm(wh_stat, lower.tail = (alternative == "greater"))
    }
  }  
}

#' @describeIn h_prop_diff_test Performs the Chi-Squared test with Schouten correction.
#'
#' @seealso Schouten correction is based upon \insertCite{Schouten1980-kd;textual}{tern}.
#'
#' @keywords internal
prop_schouten <- function(tbl, alternative = c("two.sided", "less", "greater")) {
  checkmate::assert_integer(c(ncol(tbl), nrow(tbl)), lower = 2, upper = 2)
  alternative <- match.arg(alternative)
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

  if (alternative == "two.sided") {
    stats::pchisq(t_schouten, df = 1, lower.tail = FALSE)
  } else {
    # This follows the logic in stats::prop.test for one-sided p-values.
    x1 <- tbl[1, 1]
    x2 <- tbl[2, 1]
    delta <- (x1 / n1) - (x2 / n2)
    z <- sign(delta) * sqrt(t_schouten)
    stats::pnorm(z, lower.tail = (alternative == "less"))
  }
}

#' @describeIn h_prop_diff_test Performs the Fisher's exact test. Internally calls [stats::fisher.test()].
#'
#' @keywords internal
prop_fisher <- function(tbl, alternative = c("two.sided", "less", "greater")) {
  checkmate::assert_integer(c(ncol(tbl), nrow(tbl)), lower = 2, upper = 2)
  alternative <- match.arg(alternative) # Is needed here, because stats::fisher.test does not handle defaults.
  tbl <- tbl[, c("TRUE", "FALSE")]
  stats::fisher.test(tbl, alternative = alternative)$p.value
}
