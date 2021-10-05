#' Proportion Difference
#'
#' @inheritParams argument_convention
#' @param grp (`factor`)\cr
#'   vector assigning observations to one out of two groups
#'   (e.g. reference and treatment group).
#'
#' @name prop_difference
#'
NULL

#' Check: Proportion Difference Arguments
#'
#' Verifies that and/or convert arguments into valid values to be used in the
#' estimation of difference in responder proportions.
#'
#' @inheritParams prop_difference
#' @inheritParams prop_diff_wald
check_diff_prop_ci <- function(rsp,
                               grp,
                               strata = NULL,
                               conf_level,
                               correct = NULL) {
  assert_that(
    is.logical(rsp),
    !anyNA(c(rsp, grp)),
    is_equal_length(rsp, grp),
    nlevels(grp) == 2,
    conf_level >= 0,
    conf_level <= 1
  )

  if (!is.null(correct)) assert_that(is.flag(correct))

  if (!is.null(strata)) assert_that(is_equal_length(rsp, strata))

  invisible()
}


#' Description of Method Used for Proportion Comparison
#'
#' This is an auxiliary function that describes the analysis in
#' `s_proportion_diff`.
#'
#' @inheritParams s_proportion_diff
#' @param long (`logical`)\cr
#'   Whether a long or a short (default) description is required.
#' @return String describing the analysis.
#'
d_proportion_diff <- function(conf_level,
                              method,
                              long = FALSE) {
  label <- paste0(conf_level * 100, "% CI")
  if (long) label <- paste(
    label,
    ifelse(
      method == "cmh",
      "for adjusted difference",
      "for difference"
    )
  )

  method_part <- switch(
    method,
    "cmh" = "CMH, without correction",
    "waldcc" = "Wald, with correction",
    "wald" = "Wald, without correction",
    "ha" = "Anderson-Hauck",
    "newcombe" = "Newcombe",
    stop(paste(method, "does not have a description"))
  )
  paste0(label, " (", method_part, ")")
}


#' @describeIn prop_difference The Wald interval follows the usual textbook
#'   definition for a single proportion confidence interval using the normal
#'   approximation. It is possible to include a continuity correction for Wald's
#'   interval.
#'
#' @param correct `logical`\cr
#'   include the continuity correction.
#' @importFrom stats prop.test
#' @export
#' @examples
#'
#' # Wald confidence interval
#'
#' set.seed(2)
#' rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)
#' grp <- c(rep("A", 10), rep("B", 10))
#' prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.90, correct = FALSE)
#'
prop_diff_wald <- function(rsp,
                           grp,
                           conf_level,
                           correct) {
  grp <- as_factor_keep_attributes(grp)
  check_diff_prop_ci(
    rsp = rsp, grp = grp, conf_level = conf_level, correct = correct
  )

  p_grp <- tapply(rsp, grp, mean)
  diff_ci <- if (all(rsp == rsp[1])) {
    c(NA, NA)
  } else {
    stats::prop.test(
      table(grp, rsp),
      correct = correct,
      conf.level = conf_level
    )$conf.int[1:2]
  }

  list(
    diff = unname(diff(p_grp)),
    diff_ci = diff_ci
  )
}


#' @describeIn prop_difference Anderson-Hauck confidence interval.
#' @importFrom stats qnorm
#' @export
#' @examples
#' # Anderson-Hauck confidence interval
#'
#' ## "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
#' rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
#' grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))
#' prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.90)
#'
#' ## Edge case: Same proportion of response in A and B.
#' rsp <- c(TRUE, FALSE, TRUE, FALSE)
#' grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
#' prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.6)
#'
prop_diff_ha <- function(rsp,
                         grp,
                         conf_level) {
  grp <- as_factor_keep_attributes(grp)
  check_diff_prop_ci(rsp = rsp, grp = grp, conf_level = conf_level)

  n_grp <- tapply(rsp, grp, length)
  p_grp <- tapply(rsp, grp, mean)
  diff_p <- unname(diff(p_grp))
  z <- qnorm((1 + conf_level) / 2)
  err <- 1 /
    (2 * min(n_grp)) + z * sqrt(sum(p_grp * (1 - p_grp) / (n_grp - 1)))
  l_ci <- max(-1, diff_p - err)
  u_ci <- min(1, diff_p + err)
  list(
    "diff" = diff_p,
    "diff_ci" = c(l_ci, u_ci)
  )
}


#' @describeIn prop_difference Newcombe confidence interval. It is based on
#'   the Wilson score confidence interval for a single binomial proportion.
#' @export
#' @examples
#'
#' # Newcombe confidence interval
#'
#' set.seed(1)
#' rsp <- c(
#'   sample(c(TRUE, FALSE), size = 40, prob = c(3/4, 1/4), replace = TRUE),
#'   sample(c(TRUE, FALSE), size = 40, prob = c(1/2, 1/2), replace = TRUE)
#' )
#' grp <- factor(rep(c("A", "B"), each = 40), levels = c("B", "A"))
#' table(rsp, grp)
#' prop_diff_nc(rsp = rsp, grp = grp, conf_level = 0.9)
#'
prop_diff_nc <- function(rsp,
                         grp,
                         conf_level) {
  grp <- as_factor_keep_attributes(grp)
  check_diff_prop_ci(rsp = rsp, grp = grp, conf_level = conf_level)

  # Source:
  # https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf
  p_grp <- tapply(rsp, grp, mean)
  diff_p <- unname(diff(p_grp))
  x_grp <- split(rsp, f = grp)
  ci_grp <- lapply(x_grp, FUN = prop_wilson, conf_level = conf_level)
  l1 <- ci_grp[[1]][1]
  u1 <- ci_grp[[1]][2]
  l2 <- ci_grp[[2]][1]
  u2 <- ci_grp[[2]][2]
  l_ci <- max(-1, diff_p - sqrt((u1 - p_grp[1])^2 + (p_grp[2] - l2)^2))
  u_ci <- min(1, diff_p + sqrt((p_grp[1] - l1)^2 + (u2 - p_grp[2])^2))
  list(
    "diff" = diff_p,
    "diff_ci" = c(l_ci, u_ci)
  )
}


#' @describeIn prop_difference Calculates the weighted difference.
#'     This is defined as the difference in response rates between the
#'     experimental treatment group and the control treatment group, adjusted
#'     for stratification factors by applying Cochran-Mantel-Haenszel (CMH)
#'     weights. For the CMH chi-squared test, use [stats::mantelhaen.test()].
#'
#' @param strata (`factor`)\cr
#'   with one level per stratum and same length as `rsp`.
#' @importFrom stats qnorm
#' @export
#' @examples
#'
#' # Cochran-Mantel-Haenszel confidence interval
#'
#' set.seed(2)
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' grp <- sample(c("Placebo", "Treatment"), 100, TRUE)
#' grp <- factor(grp, levels = c("Placebo", "Treatment"))
#' strata_data <- data.frame(
#'   "f1" = sample(c("a", "b"), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE)
#'
#' prop_diff_cmh(
#'   rsp = rsp, grp = grp, strata = interaction(strata_data),
#'   conf_level = 0.90
#' )
#'
prop_diff_cmh <- function(rsp,
                          grp,
                          strata,
                          conf_level = 0.95) {
  grp <- as_factor_keep_attributes(grp)
  strata <- as_factor_keep_attributes(strata)
  check_diff_prop_ci(
    rsp = rsp, grp = grp, conf_level = conf_level, strata = strata
  )

  if (any(tapply(rsp, strata, length) < 5))
    warning("Less than 5 observations in some strata.")

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
  wt <- (n1 * n2 / (n1 + n2)) / sum(n1 * n2 / (n1 + n2))
  use_stratum <- wt > 0
  wt <- wt[use_stratum]
  p1 <- p1[use_stratum]
  p2 <- p2[use_stratum]
  n1 <- n1[use_stratum]
  n2 <- n2[use_stratum]
  est1 <- sum(wt * p1)
  est2 <- sum(wt * p2)
  estimate <- c(est1, est2)
  names(estimate) <- levels(grp)
  se1 <- sqrt(sum(wt^2 * p1 * (1 - p1) / n1))
  se2 <- sqrt(sum(wt^2 * p2 * (1 - p2) / n2))
  z <- qnorm((1 + conf_level) / 2)
  err1 <- z * se1
  err2 <- z * se2
  ci1 <- c((est1 - err1), (est1 + err1))
  ci2 <- c((est2 - err2), (est2 + err2))
  estimate_ci <- list(ci1, ci2)
  names(estimate_ci) <- levels(grp)
  diff_est <- est2 - est1
  se_diff <- sqrt(sum(((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2)) * wt^2))
  diff_ci <- c(diff_est - z * se_diff, diff_est + z * se_diff)

  list(
    prop = estimate,
    prop_ci = estimate_ci,
    diff = diff_est,
    diff_ci = diff_ci
  )
}


#' @describeIn prop_difference Statistics function estimating the difference
#'   in terms of responder proportion.
#' @param method (`string`)\cr
#'   the method used for the confidence interval estimation.
#' @importFrom stats setNames
#' @export
#' @examples
#'
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
s_proportion_diff <- function(df,
                              .var,
                              .ref_group,
                              .in_ref_col,
                              variables = list(strata = NULL),
                              conf_level = 0.95,
                              method = c(
                                "waldcc", "wald", "cmh",
                                "ha", "newcombe"
                              )
) {
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
      strata_vars <- setNames(as.list(strata), strata)
      assert_that(
        !is.null(strata),
        is_df_with_variables(df, strata_vars),
        is_df_with_variables(.ref_group, strata_vars)
      )
      strata <- factor(c(interaction(.ref_group[strata]), interaction(df[strata])))
    }

    y <- switch(
      method,
      wald = prop_diff_wald(rsp, grp, conf_level, correct = FALSE),
      waldcc = prop_diff_wald(rsp, grp, conf_level, correct = TRUE),
      ha = prop_diff_ha(rsp, grp, conf_level),
      newcombe = prop_diff_nc(rsp, grp, conf_level),
      cmh = prop_diff_cmh(rsp, grp, strata, conf_level)[c("diff", "diff_ci")]
    )

    y$diff <- y$diff * 100
    y$diff_ci <- y$diff_ci * 100

  }

  attr(y$diff, "label") <- "Difference in Response rate (%)"
  attr(y$diff_ci, "label") <- d_proportion_diff(
    conf_level, method, long = FALSE
  )

  y
}

#' @describeIn prop_difference Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
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
a_proportion_diff <- make_afun(
  s_proportion_diff,
  .formats =  c(diff = "xx.x", diff_ci = "(xx.x, xx.x)"),
  .indent_mods = c(diff = 0L, diff_ci = 1L)
)

#' @describeIn prop_difference Adds a descriptive analyze layer to `rtables`
#'   pipelines. The analysis is applied to a `dataframe` and return the
#'   estimations, in `rcells`. The ellipsis (`...`) conveys arguments to
#'   `s_proportion_diff()`, for instance `na.rm = FALSE` if missing data
#'   should be accounted for.
#' @inheritParams rtables::analyze
#' @param ... arguments passed to `s_proportion_diff()`.
#' @export
#' @examples
#'
#' l <- basic_table() %>%
#' split_cols_by(var = "grp", ref_group = "B") %>%
#'   estimate_proportion_diff(
#'     vars = "rsp",
#'     conf_level = 0.90,
#'     method = "ha"
#'   )
#'
#' build_table(l, df = dta)
#'
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
