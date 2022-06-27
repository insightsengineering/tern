#' Proportion Difference
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams argument_convention
#' @param grp (`factor`)\cr
#'   vector assigning observations to one out of two groups
#'   (e.g. reference and treatment group).
#'
#' @name prop_diff
NULL

#' Check: Proportion Difference Arguments
#'
#' Verifies that and/or convert arguments into valid values to be used in the
#' estimation of difference in responder proportions.
#'
#' @inheritParams prop_diff
#' @inheritParams prop_diff_wald
#'
#' @keywords internal
check_diff_prop_ci <- function(rsp,
                               grp,
                               strata = NULL,
                               conf_level,
                               correct = NULL) {
  assertthat::assert_that(
    is.logical(rsp),
    !anyNA(c(rsp, grp)),
    is_equal_length(rsp, grp),
    nlevels(grp) == 2,
    conf_level >= 0,
    conf_level <= 1
  )

  if (!is.null(correct)) assertthat::assert_that(assertthat::is.flag(correct))

  if (!is.null(strata)) assertthat::assert_that(is_equal_length(rsp, strata))

  invisible()
}

#' Description of Method Used for Proportion Comparison
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is an auxiliary function that describes the analysis in [s_proportion_diff()].
#'
#' @inheritParams s_proportion_diff
#' @param long (`logical`)\cr
#'   Whether a long or a short (default) description is required.
#' @return String describing the analysis.
#'
#' @export
d_proportion_diff <- function(conf_level,
                              method,
                              long = FALSE) {
  label <- paste0(conf_level * 100, "% CI")
  if (long) {
    label <- paste(
      label,
      ifelse(
        method == "cmh",
        "for adjusted difference",
        "for difference"
      )
    )
  }

  method_part <- switch(method,
                        "cmh" = "CMH, without correction",
                        "waldcc" = "Wald, with correction",
                        "wald" = "Wald, without correction",
                        "ha" = "Anderson-Hauck",
                        "newcombe" = "Newcombe, without correction",
                        "newcombecc" = "Newcombe, with correction",
                        stop(paste(method, "does not have a description"))
  )
  paste0(label, " (", method_part, ")")
}

#' @describeIn prop_diff Statistics function estimating the difference
#'   in terms of responder proportion.
#'
#' @param method (`string`)\cr
#'   the method used for the confidence interval estimation.
#'
#' @examples
#' # Summary
#'
#' ## "Mid" case: 4/4 respond in group A, 1/2 respond in group B.
#' dta <- data.frame(
#'   rsp = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE),
#'   grp = factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))
#' )
#'
#' s_proportion_diff(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   conf_level = 0.90,
#'   method = "ha"
#' )
#'
#' @export
s_proportion_diff <- function(df,
                              .var,
                              .ref_group,
                              .in_ref_col,
                              variables = list(strata = NULL),
                              conf_level = 0.95,
                              method = c(
                                "waldcc", "wald", "cmh",
                                "ha", "newcombe", "newcombecc"
                              )) {
  method <- match.arg(method)
  y <- list(diff = "", diff_ci = "")

  if (!.in_ref_col) {
    rsp <- c(.ref_group[[.var]], df[[.var]])
    grp <- factor(
      rep(
        c("ref", "Not-ref"),
        c(nrow(.ref_group), nrow(df))
      ),
      levels = c("ref", "Not-ref")
    )

    if (!is.null(variables$strata)) {
      strata <- variables$strata
      strata_vars <- stats::setNames(as.list(strata), strata)
      assertthat::assert_that(
        !is.null(strata)
      )
      assert_df_with_variables(df, strata_vars)
      assert_df_with_variables(.ref_group, strata_vars)
      strata <- factor(c(interaction(.ref_group[strata]), interaction(df[strata])))
    }

    y <- switch(method,
      wald = prop_diff_wald(rsp, grp, conf_level, correct = FALSE),
      waldcc = prop_diff_wald(rsp, grp, conf_level, correct = TRUE),
      ha = prop_diff_ha(rsp, grp, conf_level),
      newcombe = prop_diff_nc(rsp, grp, conf_level, correct = FALSE),
      newcombecc = prop_diff_nc(rsp, grp, conf_level, correct = TRUE),
      cmh = prop_diff_cmh(rsp, grp, strata, conf_level)[c("diff", "diff_ci")]
    )

    y$diff <- y$diff * 100
    y$diff_ci <- y$diff_ci * 100
  }

  attr(y$diff, "label") <- "Difference in Response rate (%)"
  attr(y$diff_ci, "label") <- d_proportion_diff(
    conf_level, method,
    long = FALSE
  )

  y
}

#' @describeIn prop_diff Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#'
#' @examples
#' a_proportion_diff(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   conf_level = 0.90,
#'   method = "ha"
#' )
#'
#' @export
a_proportion_diff <- make_afun(
  s_proportion_diff,
  .formats =  c(diff = "xx.x", diff_ci = "(xx.x, xx.x)"),
  .indent_mods = c(diff = 0L, diff_ci = 1L)
)

#' @describeIn prop_diff Adds a descriptive analyze layer to `rtables`
#'   pipelines. The analysis is applied to a `dataframe` and return the
#'   estimations, in `rcells`. The ellipsis (`...`) conveys arguments to
#'   `s_proportion_diff()`, for instance `na.rm = FALSE` if missing data
#'   should be accounted for.
#'
#' @inheritParams rtables::analyze
#' @param ... arguments passed to `s_proportion_diff()`.
#'
#' @examples
#' l <- basic_table() %>%
#'   split_cols_by(var = "grp", ref_group = "B") %>%
#'   estimate_proportion_diff(
#'     vars = "rsp",
#'     conf_level = 0.90,
#'     method = "ha"
#'   )
#'
#' build_table(l, df = dta)
#'
#' @export
estimate_proportion_diff <- function(lyt,
                                     vars,
                                     ...,
                                     var_labels = vars,
                                     show_labels = "hidden",
                                     table_names = vars,
                                     .stats = NULL,
                                     .formats = NULL,
                                     .labels = NULL,
                                     .indent_mods = NULL) {
  afun <- make_afun(
    a_proportion_diff,
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
    extra_args = list(...),
    show_labels = show_labels,
    table_names = table_names
  )
}
