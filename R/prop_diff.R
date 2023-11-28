#' Proportion Difference
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams prop_diff_strat_nc
#' @inheritParams argument_convention
#' @param method (`string`)\cr the method used for the confidence interval estimation.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("estimate_proportion_diff")`
#'   to see available statistics for this function.
#'
#' @seealso [d_proportion_diff()]
#'
#' @name prop_diff
#' @order 1
NULL

#' @describeIn prop_diff Statistics function estimating the difference
#'   in terms of responder proportion.
#'
#' @return
#' * `s_proportion_diff()` returns a named list of elements `diff` and `diff_ci`.
#'
#' @note When performing an unstratified analysis, methods `"cmh"`, `"strat_newcombe"`, and `"strat_newcombecc"` are
#'   not permitted.
#'
#' @examples
#' s_proportion_diff(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   conf_level = 0.90,
#'   method = "ha"
#' )
#'
#' # CMH example with strata
#' s_proportion_diff(
#'   df = subset(dta, grp == "A"),
#'   .var = "rsp",
#'   .ref_group = subset(dta, grp == "B"),
#'   .in_ref_col = FALSE,
#'   variables = list(strata = c("f1", "f2")),
#'   conf_level = 0.90,
#'   method = "cmh"
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
                                "ha", "newcombe", "newcombecc",
                                "strat_newcombe", "strat_newcombecc"
                              ),
                              weights_method = "cmh") {
  method <- match.arg(method)
  if (is.null(variables$strata) && checkmate::test_subset(method, c("cmh", "strat_newcombe", "strat_newcombecc"))) {
    stop(paste(
      "When performing an unstratified analysis, methods 'cmh', 'strat_newcombe', and 'strat_newcombecc' are not",
      "permitted. Please choose a different method."
    ))
  }
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
      strata_colnames <- variables$strata
      checkmate::assert_character(strata_colnames, null.ok = FALSE)
      strata_vars <- stats::setNames(as.list(strata_colnames), strata_colnames)

      assert_df_with_variables(df, strata_vars)
      assert_df_with_variables(.ref_group, strata_vars)

      # Merging interaction strata for reference group rows data and remaining
      strata <- c(
        interaction(.ref_group[strata_colnames]),
        interaction(df[strata_colnames])
      )
      strata <- as.factor(strata)
    }

    # Defining the std way to calculate weights for strat_newcombe
    if (!is.null(variables$weights_method)) {
      weights_method <- variables$weights_method
    } else {
      weights_method <- "cmh"
    }

    y <- switch(method,
      "wald" = prop_diff_wald(rsp, grp, conf_level, correct = FALSE),
      "waldcc" = prop_diff_wald(rsp, grp, conf_level, correct = TRUE),
      "ha" = prop_diff_ha(rsp, grp, conf_level),
      "newcombe" = prop_diff_nc(rsp, grp, conf_level, correct = FALSE),
      "newcombecc" = prop_diff_nc(rsp, grp, conf_level, correct = TRUE),
      "strat_newcombe" = prop_diff_strat_nc(rsp,
        grp,
        strata,
        weights_method,
        conf_level,
        correct = FALSE
      ),
      "strat_newcombecc" = prop_diff_strat_nc(rsp,
        grp,
        strata,
        weights_method,
        conf_level,
        correct = TRUE
      ),
      "cmh" = prop_diff_cmh(rsp, grp, strata, conf_level)[c("diff", "diff_ci")]
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

#' @describeIn prop_diff Formatted analysis function which is used as `afun` in `estimate_proportion_diff()`.
#'
#' @return
#' * `a_proportion_diff()` returns the corresponding list with formatted [rtables::CellValue()].
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
  .formats = c(diff = "xx.x", diff_ci = "(xx.x, xx.x)"),
  .indent_mods = c(diff = 0L, diff_ci = 1L)
)

#' @describeIn prop_diff Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `estimate_proportion_diff()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_proportion_diff()` to the table layout.
#'
#' @examples
#' ## "Mid" case: 4/4 respond in group A, 1/2 respond in group B.
#' nex <- 100 # Number of example rows
#' dta <- data.frame(
#'   "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
#'   "grp" = sample(c("A", "B"), nex, TRUE),
#'   "f1" = sample(c("a1", "a2"), nex, TRUE),
#'   "f2" = sample(c("x", "y", "z"), nex, TRUE),
#'   stringsAsFactors = TRUE
#' )
#'
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
#' @order 2
estimate_proportion_diff <- function(lyt,
                                     vars,
                                     variables = list(strata = NULL),
                                     conf_level = 0.95,
                                     method = c(
                                       "waldcc", "wald", "cmh",
                                       "ha", "newcombe", "newcombecc",
                                       "strat_newcombe", "strat_newcombecc"
                                     ),
                                     weights_method = "cmh",
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
  extra_args <- list(
    variables = variables, conf_level = conf_level, method = method, weights_method = weights_method, ...
  )

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
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    show_labels = show_labels,
    table_names = table_names
  )
}

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
  checkmate::assert_logical(rsp, any.missing = FALSE)
  checkmate::assert_factor(grp, len = length(rsp), any.missing = FALSE, n.levels = 2)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  checkmate::assert_flag(correct, null.ok = TRUE)

  if (!is.null(strata)) {
    checkmate::assert_factor(strata, len = length(rsp))
  }

  invisible()
}

#' Description of Method Used for Proportion Comparison
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is an auxiliary function that describes the analysis in
#' `s_proportion_diff`.
#'
#' @inheritParams s_proportion_diff
#' @param long (`logical`)\cr Whether a long or a short (default) description is required.
#'
#' @return A `string` describing the analysis.
#'
#' @seealso [prop_diff]
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
    "strat_newcombe" = "Stratified Newcombe, without correction",
    "strat_newcombecc" = "Stratified Newcombe, with correction",
    stop(paste(method, "does not have a description"))
  )
  paste0(label, " (", method_part, ")")
}

#' Helper Functions to Calculate Proportion Difference
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams argument_convention
#' @inheritParams prop_diff
#' @param grp (`factor`)\cr vector assigning observations to one out of two groups
#'   (e.g. reference and treatment group).
#'
#' @return A named `list` of elements `diff` (proportion difference) and `diff_ci`
#'   (proportion difference confidence interval).
#'
#' @seealso [prop_diff()] for implementation of these helper functions.
#'
#' @name h_prop_diff
NULL

#' @describeIn h_prop_diff The Wald interval follows the usual textbook
#'   definition for a single proportion confidence interval using the normal
#'   approximation. It is possible to include a continuity correction for Wald's
#'   interval.
#'
#' @param correct (`logical`)\cr whether to include the continuity correction. For further
#'   information, see [stats::prop.test()].
#'
#' @examples
#' # Wald confidence interval
#' set.seed(2)
#' rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)
#' grp <- factor(c(rep("A", 10), rep("B", 10)))
#'
#' prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.95, correct = FALSE)
#'
#' @export
prop_diff_wald <- function(rsp,
                           grp,
                           conf_level = 0.95,
                           correct = FALSE) {
  if (isTRUE(correct)) {
    mthd <- "waldcc"
  } else {
    mthd <- "wald"
  }
  grp <- as_factor_keep_attributes(grp)
  check_diff_prop_ci(
    rsp = rsp, grp = grp, conf_level = conf_level, correct = correct
  )

  # check if binary response is coded as logical
  checkmate::assert_logical(rsp, any.missing = FALSE)
  checkmate::assert_factor(grp, len = length(rsp), any.missing = FALSE, n.levels = 2)

  tbl <- table(grp, factor(rsp, levels = c(TRUE, FALSE)))
  # x1 and n1 are non-reference groups.
  diff_ci <- desctools_binom(
    x1 = tbl[2], n1 = sum(tbl[2], tbl[4]),
    x2 = tbl[1], n2 = sum(tbl[1], tbl[3]),
    conf.level = conf_level,
    method = mthd
  )

  list(
    "diff" = unname(diff_ci[, "est"]),
    "diff_ci" = unname(diff_ci[, c("lwr.ci", "upr.ci")])
  )
}

#' @describeIn h_prop_diff Anderson-Hauck confidence interval.
#'
#' @examples
#' # Anderson-Hauck confidence interval
#' ## "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
#' rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
#' grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))
#'
#' prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.90)
#'
#' ## Edge case: Same proportion of response in A and B.
#' rsp <- c(TRUE, FALSE, TRUE, FALSE)
#' grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
#'
#' prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.6)
#'
#' @export
prop_diff_ha <- function(rsp,
                         grp,
                         conf_level) {
  grp <- as_factor_keep_attributes(grp)
  check_diff_prop_ci(rsp = rsp, grp = grp, conf_level = conf_level)

  tbl <- table(grp, factor(rsp, levels = c(TRUE, FALSE)))
  # x1 and n1 are non-reference groups.
  ci <- desctools_binom(
    x1 = tbl[2], n1 = sum(tbl[2], tbl[4]),
    x2 = tbl[1], n2 = sum(tbl[1], tbl[3]),
    conf.level = conf_level,
    method = "ha"
  )
  list(
    "diff" = unname(ci[, "est"]),
    "diff_ci" = unname(ci[, c("lwr.ci", "upr.ci")])
  )
}

#' @describeIn h_prop_diff `Newcombe` confidence interval. It is based on
#'   the Wilson score confidence interval for a single binomial proportion.
#'
#' @examples
#' # `Newcombe` confidence interval
#'
#' set.seed(1)
#' rsp <- c(
#'   sample(c(TRUE, FALSE), size = 40, prob = c(3 / 4, 1 / 4), replace = TRUE),
#'   sample(c(TRUE, FALSE), size = 40, prob = c(1 / 2, 1 / 2), replace = TRUE)
#' )
#' grp <- factor(rep(c("A", "B"), each = 40), levels = c("B", "A"))
#' table(rsp, grp)
#'
#' prop_diff_nc(rsp = rsp, grp = grp, conf_level = 0.9)
#'
#' @export
prop_diff_nc <- function(rsp,
                         grp,
                         conf_level,
                         correct = FALSE) {
  if (isTRUE(correct)) {
    mthd <- "scorecc"
  } else {
    mthd <- "score"
  }
  grp <- as_factor_keep_attributes(grp)
  check_diff_prop_ci(rsp = rsp, grp = grp, conf_level = conf_level)

  p_grp <- tapply(rsp, grp, mean)
  diff_p <- unname(diff(p_grp))
  tbl <- table(grp, factor(rsp, levels = c(TRUE, FALSE)))
  ci <- desctools_binom(
    # x1 and n1 are non-reference groups.
    x1 = tbl[2], n1 = sum(tbl[2], tbl[4]),
    x2 = tbl[1], n2 = sum(tbl[1], tbl[3]),
    conf.level = conf_level,
    method = mthd
  )
  list(
    "diff" = unname(ci[, "est"]),
    "diff_ci" = unname(ci[, c("lwr.ci", "upr.ci")])
  )
}

#' @describeIn h_prop_diff Calculates the weighted difference. This is defined as the difference in
#'   response rates between the experimental treatment group and the control treatment group, adjusted
#'   for stratification factors by applying `Cochran-Mantel-Haenszel` (`CMH`) weights. For the `CMH` chi-squared
#'   test, use [stats::mantelhaen.test()].
#'
#' @param strata (`factor`)\cr variable with one level per stratum and same length as `rsp`.
#'
#' @examples
#' # Cochran-Mantel-Haenszel confidence interval
#'
#' set.seed(2)
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' grp <- sample(c("Placebo", "Treatment"), 100, TRUE)
#' grp <- factor(grp, levels = c("Placebo", "Treatment"))
#' strata_data <- data.frame(
#'   "f1" = sample(c("a", "b"), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE
#' )
#'
#' prop_diff_cmh(
#'   rsp = rsp, grp = grp, strata = interaction(strata_data),
#'   conf_level = 0.90
#' )
#'
#' @export
prop_diff_cmh <- function(rsp,
                          grp,
                          strata,
                          conf_level = 0.95) {
  grp <- as_factor_keep_attributes(grp)
  strata <- as_factor_keep_attributes(strata)
  check_diff_prop_ci(
    rsp = rsp, grp = grp, conf_level = conf_level, strata = strata
  )

  if (any(tapply(rsp, strata, length) < 5)) {
    warning("Less than 5 observations in some strata.")
  }

  # first dimension: FALSE, TRUE
  # 2nd dimension: CONTROL, TX
  # 3rd dimension: levels of strat
  # rsp as factor rsp to handle edge case of no FALSE (or TRUE) rsp records
  t_tbl <- table(
    factor(rsp, levels = c("FALSE", "TRUE")),
    grp,
    strata
  )
  n1 <- colSums(t_tbl[1:2, 1, ])
  n2 <- colSums(t_tbl[1:2, 2, ])
  p1 <- t_tbl[2, 1, ] / n1
  p2 <- t_tbl[2, 2, ] / n2
  # CMH weights
  use_stratum <- (n1 > 0) & (n2 > 0)
  n1 <- n1[use_stratum]
  n2 <- n2[use_stratum]
  p1 <- p1[use_stratum]
  p2 <- p2[use_stratum]
  wt <- (n1 * n2 / (n1 + n2))
  wt_normalized <- wt / sum(wt)
  est1 <- sum(wt_normalized * p1)
  est2 <- sum(wt_normalized * p2)
  estimate <- c(est1, est2)
  names(estimate) <- levels(grp)
  se1 <- sqrt(sum(wt_normalized^2 * p1 * (1 - p1) / n1))
  se2 <- sqrt(sum(wt_normalized^2 * p2 * (1 - p2) / n2))
  z <- stats::qnorm((1 + conf_level) / 2)
  err1 <- z * se1
  err2 <- z * se2
  ci1 <- c((est1 - err1), (est1 + err1))
  ci2 <- c((est2 - err2), (est2 + err2))
  estimate_ci <- list(ci1, ci2)
  names(estimate_ci) <- levels(grp)
  diff_est <- est2 - est1
  se_diff <- sqrt(sum(((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2)) * wt_normalized^2))
  diff_ci <- c(diff_est - z * se_diff, diff_est + z * se_diff)

  list(
    prop = estimate,
    prop_ci = estimate_ci,
    diff = diff_est,
    diff_ci = diff_ci,
    weights = wt_normalized,
    n1 = n1,
    n2 = n2
  )
}

#' @describeIn h_prop_diff Calculates the stratified `Newcombe` confidence interval and difference in response
#'   rates between the experimental treatment group and the control treatment group, adjusted for stratification
#'   factors. This implementation follows closely the one proposed by \insertCite{Yan2010-jt;textual}{tern}.
#'   Weights can be estimated from the heuristic proposed in [prop_strat_wilson()] or from `CMH`-derived weights
#'   (see [prop_diff_cmh()]).
#'
#' @param strata (`factor`)\cr variable with one level per stratum and same length as `rsp`.
#' @param weights_method (`string`)\cr weights method. Can be either `"cmh"` or `"heuristic"`
#'   and directs the way weights are estimated.
#'
#' @references
#' \insertRef{Yan2010-jt}{tern}
#'
#' @examples
#' # Stratified `Newcombe` confidence interval
#'
#' set.seed(2)
#' data_set <- data.frame(
#'   "rsp" = sample(c(TRUE, FALSE), 100, TRUE),
#'   "f1" = sample(c("a", "b"), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   "grp" = sample(c("Placebo", "Treatment"), 100, TRUE),
#'   stringsAsFactors = TRUE
#' )
#'
#' prop_diff_strat_nc(
#'   rsp = data_set$rsp, grp = data_set$grp, strata = interaction(data_set[2:3]),
#'   weights_method = "cmh",
#'   conf_level = 0.90
#' )
#'
#' prop_diff_strat_nc(
#'   rsp = data_set$rsp, grp = data_set$grp, strata = interaction(data_set[2:3]),
#'   weights_method = "wilson_h",
#'   conf_level = 0.90
#' )
#'
#' @export
prop_diff_strat_nc <- function(rsp,
                               grp,
                               strata,
                               weights_method = c("cmh", "wilson_h"),
                               conf_level = 0.95,
                               correct = FALSE) {
  weights_method <- match.arg(weights_method)
  grp <- as_factor_keep_attributes(grp)
  strata <- as_factor_keep_attributes(strata)
  check_diff_prop_ci(
    rsp = rsp, grp = grp, conf_level = conf_level, strata = strata
  )
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  checkmate::assert_flag(correct)
  if (any(tapply(rsp, strata, length) < 5)) {
    warning("Less than 5 observations in some strata.")
  }

  rsp_by_grp <- split(rsp, f = grp)
  strata_by_grp <- split(strata, f = grp)

  # Finding the weights
  weights <- if (identical(weights_method, "cmh")) {
    prop_diff_cmh(rsp = rsp, grp = grp, strata = strata)$weights
  } else if (identical(weights_method, "wilson_h")) {
    prop_strat_wilson(rsp, strata, conf_level = conf_level, correct = correct)$weights
  }
  weights[levels(strata)[!levels(strata) %in% names(weights)]] <- 0

  # Calculating lower (`l`) and upper (`u`) confidence bounds per group.
  strat_wilson_by_grp <- Map(
    prop_strat_wilson,
    rsp = rsp_by_grp,
    strata = strata_by_grp,
    weights = list(weights, weights),
    conf_level = conf_level,
    correct = correct
  )

  ci_ref <- strat_wilson_by_grp[[1]]
  ci_trt <- strat_wilson_by_grp[[2]]
  l_ref <- as.numeric(ci_ref$conf_int[1])
  u_ref <- as.numeric(ci_ref$conf_int[2])
  l_trt <- as.numeric(ci_trt$conf_int[1])
  u_trt <- as.numeric(ci_trt$conf_int[2])

  # Estimating the diff and n_ref, n_trt (it allows different weights to be used)
  t_tbl <- table(
    factor(rsp, levels = c("FALSE", "TRUE")),
    grp,
    strata
  )
  n_ref <- colSums(t_tbl[1:2, 1, ])
  n_trt <- colSums(t_tbl[1:2, 2, ])
  use_stratum <- (n_ref > 0) & (n_trt > 0)
  n_ref <- n_ref[use_stratum]
  n_trt <- n_trt[use_stratum]
  p_ref <- t_tbl[2, 1, use_stratum] / n_ref
  p_trt <- t_tbl[2, 2, use_stratum] / n_trt
  est1 <- sum(weights * p_ref)
  est2 <- sum(weights * p_trt)
  diff_est <- est2 - est1

  lambda1 <- sum(weights^2 / n_ref)
  lambda2 <- sum(weights^2 / n_trt)
  z <- stats::qnorm((1 + conf_level) / 2)

  lower <- diff_est - z * sqrt(lambda2 * l_trt * (1 - l_trt) + lambda1 * u_ref * (1 - u_ref))
  upper <- diff_est + z * sqrt(lambda1 * l_ref * (1 - l_ref) + lambda2 * u_trt * (1 - u_trt))

  list(
    "diff" = diff_est,
    "diff_ci" = c("lower" = lower, "upper" = upper)
  )
}
