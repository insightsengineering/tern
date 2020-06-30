#' Summary for proportion of successful outcomes
#'
#' @inheritParams argument_convention
#' @param x \code{logical} vector. \code{TRUE} represents a successful outcome.
#' @param prop_ci_method one of (\code{"waldcc"}, \code{"wald"}, \code{"clopper-pearson"},
#'   \code{"wilson"}, \code{agresti-coull} or \code{jeffreys}).\cr
#'   Specifies the method used to construct the confidence interval for proportion
#'   of successful outcomes.
#'
#' @details
#' The following options are supported for confidence interval for a proportion:
#'   * The \code{wald} interval follows the usual textbook definition for a single proportion
#'     confidence interval using the normal approximation.
#'   * The \code{waldcc} interval is similar to \code{wald} and in addition uses the continuity
#'     correction \code{1/2n}.
#'   * The \code{clopper-pearson} interval calls \code{\link[stats]{binom.test}}. Also referred to
#'     as the 'exact' method.
#'   * The \code{wilson} interval calls \code{\link[stats]{prop.test}} with option \code{correct=FALSE}.
#'     Also referred to as Wilson score interval.
#'   * The \code{agresti-coull} interval was created by Alan Agresti and Brent Coull and can be understood (for 95% CI)
#'     as adding two successes and two failures to the data, and then using the Wald formula to construct a CI.
#'   * The \code{jeffreys} interval is an equal-tailed interval based on the non-informative Jeffreys prior for a
#'     binomial proportion.
#'
#' @md
#'
#' @return Named list with analysis summary:
#' \describe{
#'   \item{prop}{Proportion of successful outcomes in sample.}
#'   \item{prop_ci}{Confidence interval for \code{prop}.}
#'   \item{label_ci}{Label for confidence interval in \code{prop_ci}.}
#' }
#'
#' @export
#'
#' @note Note that the \code{prop} element of the analysis summary retains the maximum likelihood estimator, also
#'   when the \code{agresti-coull} method is used, while e.g. other packages might return instead the modified
#'   proportion estimator (obtained by adding artificial success and failures to the data). This behavior is consistent
#'   with the software SAS.
#'
#' @seealso \code{\link{t_el_proportion}},
#'   \code{\link{s_proportion_diff}}, \code{\link{s_test_proportion_diff}}
#'
#' @importFrom stats binom.test prop.test qnorm qbeta
#'
#' @examples
#' set.seed(1)
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' s_proportion(rsp)
#'
#' s_proportion(rsp, conf_level = 0.9, prop_ci_method = "clopper-pearson")
s_proportion <- function(x, conf_level = 0.95,
                         prop_ci_method = c("waldcc", "wald", "clopper-pearson", "wilson", "agresti-coull",
                                            "jeffreys")) {

  check_is_event(x)
  prop_ci_method <- match.arg(prop_ci_method)
  check_conf_level(conf_level)

  # Common variables used below.
  n <- length(x)
  x_sum <- sum(x)
  p_hat <- mean(x)
  z <- qnorm((1 + conf_level) / 2)
  label_ci_start <- paste0(conf_level * 100, "% CI for Response Rates")

  results <- if (prop_ci_method == "clopper-pearson") {
    list(
      "prop_ci" = as.numeric(binom.test(x_sum, n, conf.level = conf_level)$conf.int),
      "label_ci" =  paste(label_ci_start, "(Clopper-Pearson)")
    )
  } else if (prop_ci_method == "wilson") {
    list(
      "prop_ci" = as.numeric(prop.test(x_sum, n, correct = FALSE, conf.level = conf_level)$conf.int),
      "label_ci" =  paste(label_ci_start, "(Wilson)")
    )
  } else if (prop_ci_method %in% c("wald", "waldcc")) {
    q_hat <- 1 - p_hat
    correct <- ifelse(
      prop_ci_method == "waldcc",
      1 / (2 * n),
      0
    )
    err <- z * sqrt(p_hat * q_hat) / sqrt(n) + correct
    l_ci <- max(0, p_hat - err)
    u_ci <- min(1, p_hat + err)

    list(
      "prop_ci" = c(l_ci, u_ci),
      "label_ci" = ifelse(
        prop_ci_method == "wald",
        paste0(label_ci_start, "(Wald, without correction)"),
        paste0(label_ci_start, "(Wald, with correction)")
      )
    )
  } else if (prop_ci_method == "agresti-coull") {
    # Add here both z^2 / 2 successes and failures.
    x_sum_tilde <- x_sum + z^2 / 2
    n_tilde <- n + z^2
    # Then proceed as with the Wald interval.
    p_tilde <- x_sum_tilde / n_tilde
    q_tilde <- 1 - p_tilde
    err <- z * sqrt(p_tilde * q_tilde) / sqrt(n_tilde)
    l_ci <- max(0, p_tilde - err)
    u_ci <- min(1, p_tilde + err)
    label_ci <- paste(label_ci_start, "(Agresti-Coull)")

    list(
      "prop_ci" = c(l_ci, u_ci),
      "label_ci" = label_ci
    )
  } else if (prop_ci_method == "jeffreys") {
    alpha <- 1 - conf_level
    l_ci <- ifelse(
      x_sum == 0,
      0,
      stats::qbeta(alpha / 2, x_sum + 0.5, n - x_sum + 0.5)
    )
    u_ci <- ifelse(
      x_sum == n,
      1,
      stats::qbeta(1 - alpha / 2, x_sum + 0.5, n - x_sum + 0.5)
    )
    label_ci <- paste(label_ci_start, "(Jeffreys)")

    list(
      "prop_ci" = c(l_ci, u_ci),
      "label_ci" = label_ci
    )
  }

  # We always report the same estimate for the binomial proportion.
  results <- c(
    list("prop" = p_hat),
    results
  )
  return(results)
}

#' Adjusted difference in proportions by CMH weights
#'
#' This function calculates a weighted difference in proportions for binary outcomes data.
#' This is defined as the difference in response rates between the experimental treatment
#' group and the control treatment group, adjusted for stratification factors by applying
#' Cochran-Mantel-Haenszel (CMH) weights. For the CMH chi-squared test,  use
#' \code{\link[stats]{mantelhaen.test}}.
#'
#' @inheritParams argument_convention
#' @param x \code{logical} vector. \code{TRUE} represents a successful outcome.
#' @param grp a \code{factor} vector with 2 levels. Must be the same length as \code{x}.
#' @param strat factor with one level per stratum and same length as \code{x}.
#'
#' @return Named list with analysis summary:
#' \describe{
#'   \item{estimate}{CMH-weight adjusted proportion of successful outcomes in each group.}
#'   \item{estimate_ci}{Confidence interval for adjusted proportions in \code{estimate}.}
#'   \item{diff_est}{CMH-weight adjusted difference in proportion between groups.}
#'   \item{diff_ci}{Confidence interval for adjusted difference in \code{diff_est}.}
#'   \item{conf_level}{Confidence level used for \code{diff_ci} and \code{estimate_ci}.}
#' }
#'
#' @export
#'
#' @seealso \code{\link{t_el_proportion_diff}},
#'   \code{\link{s_proportion_diff}}, \code{\link{s_test_proportion_diff}}
#'
#' @references
#'   Kim Y, Won S. Adjusted proportion difference and confidence interval in stratified randomized trials.
#'   [Internet]. PharmaSUG; 2013 [cited 14 May 2020]. (PharmaSUG 2013 Conference Proceedings). Report No.: SP04.
#'   Available from: https://www.pharmasug.org/proceedings/2013/SP/PharmaSUG-2013-SP04.pdf
#'
#' @examples
#' set.seed(1)
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' trt <- sample(c("Placebo", "Treatment"), 100, TRUE)
#' trt <- factor(trt, levels = c("Placebo", "Treatment"))
#' strata_data <- data.frame(
#'   "f1" = sample(c("a", "b"), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE)
#' s_adj_proportion_diff(rsp, trt, interaction(strata_data))
#'
s_adj_proportion_diff <- function(x, grp, strat, conf_level = 0.95) {

  check_is_event(x)
  check_is_factor(grp, allow_na = FALSE)
  check_is_factor(strat, allow_na = FALSE)
  check_same_n(x = x, grp = grp, strat = strat)
  check_strata_levels(strat)
  check_numeric_range(conf_level)

  stop_if_not(
    list(length(unique(x)) == 2, "x must include TRUE and FALSE"),
    list(length(levels(grp)) == 2, "grp must have 2 levels")
  )

  if (any(tapply(x, strat, length) < 5)) {
    warning("Less than 5 observations in some strata.")
  }

  # first dimension: FALSE, TRUE
  # 2nd dimension: CONTROL, TX
  # 3rd dimension: levels of strat
  t_tbl <- table(x, grp, strat)

  n1 <- colSums(t_tbl[1:2, 1, ])
  n2 <- colSums(t_tbl[1:2, 2, ])

  p1 <- t_tbl[2, 1, ] / n1
  p2 <- t_tbl[2, 2, ] / n2

  # CMH weights
  wt <- (n1 * n2 / (n1 + n2)) / sum(n1 * n2 / (n1 + n2))

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

  result <- list(
    estimate = estimate,
    estimate_ci = estimate_ci,
    diff_est = diff_est,
    diff_ci = diff_ci,
    conf_level = conf_level
  )

  result
}

#' Summary of difference between two proportions
#'
#' This function is a wrapper to calculate the difference between
#' two proportions or weighed difference if stratification variables are
#' used. In addition provides various options for types of confidence intervals.
#'
#' @inheritParams argument_convention
#' @param x \code{logical} vector. \code{TRUE} represents a successful outcome.
#' @param grp a \code{factor} vector with 2 levels. Must be the same length as \code{x}.
#' @param strat factor with one level per stratum and same length as \code{x}. Only required
#'   for stratified analysis when method is "cmh".
#' @param diff_ci_method one of (\code{"wald"}, \code{"waldcc"}, \code{"cmh"})\cr
#'   Specifies the method used to construct the confidence interval for the difference
#'   in proportion of outcomes.
#'
#' @details
#' The following options are supported for difference of proportions confidence intervals:
#' \itemize{
#'   \item{Option \code{wald} calls \code{\link[stats]{prop.test}} with \code{correct=FALSE}}
#'   \item{Option \code{waldcc} includes continuity correction for Wald interval and
#'   calls \code{\link[stats]{prop.test}} with \code{correct=TRUE}}
#'   \item{Option \code{cmh} derives a CI for CMH-weighted difference of proportions and
#'    calls \code{\link{s_adj_proportion_diff}}.}
#' }
#'
#' @return Named list with analysis summary.
#' \describe{
#'   \item{diff}{Difference in proportion of successful outcomes between groups.}
#'   \item{diff_ci}{Confidence interval for \code{diff}.}
#'   \item{label_ci}{Label for confidence interval in \code{diff_ci}.}
#' }
#'
#' @export
#'
#' @seealso \code{\link{t_el_proportion_diff}},
#'   \code{\link{s_adj_proportion_diff}}, \code{\link{s_proportion}},
#'   \code{\link{s_test_proportion_diff}}
#'
#' @importFrom stats prop.test
#' @importFrom utils.nest stop_if_not
#'
#' @examples
#' set.seed(1)
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' trt <- sample(c("Placebo", "Treatment"), 100, TRUE)
#' trt <- factor(trt, levels = c("Placebo", "Treatment"))
#' strata_data <- data.frame(
#'   "f1" = sample(c("a", "b"), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE)
#'
#' s_proportion_diff(rsp, trt, diff_ci_method = "wald")
#' s_proportion_diff(rsp, trt, diff_ci_method = "waldcc")
#' s_proportion_diff(rsp, trt, strat = interaction(strata_data), diff_ci_method = "cmh")
#'
#' # case without responders
#' s_proportion_diff(rep(FALSE, length(trt)), trt, diff_ci_method = "wald")
s_proportion_diff <- function(x, grp, strat = NULL, conf_level = 0.95,
                              diff_ci_method = c("wald", "waldcc", "cmh")) {

  check_is_event(x)
  check_is_factor(grp, allow_na = FALSE)
  check_same_n(x = x, grp = grp)
  diff_ci_method <- match.arg(diff_ci_method)

  if (!is.null(strat)) {
    check_is_factor(strat, allow_na = FALSE)
    check_strata_levels(strat)
    check_same_n(x = x, strat = strat)
    if (diff_ci_method != "cmh") {
      warning(paste0(diff_ci_method,
        " method is not applicable with stratification factors. Stratification factors are ignored."))
    }

  } else {
    stop_if_not(list(diff_ci_method != "cmh",
                     paste0(diff_ci_method, " method is not applicable when strat is NULL")))
  }

  stop_if_not(
    list(length(levels(grp)) == 2, "grp must have 2 levels")
  )

  # case when only responder / non-reponders
  if (length(unique(x)) == 1) {
    return(
      list(
        "diff" = 0,
        "diff_ci" = c(NA, NA),
        "label_ci" = NA_character_
      )
    )
  }

  result <- if (diff_ci_method == "wald") {
    list(
      "diff" = unname(diff(tapply(x, grp, mean))),
      "diff_ci" = prop.test(table(grp, x), correct = FALSE, conf.level = conf_level)$conf.int[1:2],
      "label_ci" =  paste0(conf_level * 100, "% CI for difference (Wald without correction)")
    )
  } else if (diff_ci_method == "waldcc") {
    list(
      "diff" = unname(diff(tapply(x, grp, mean))),
      "diff_ci" = prop.test(table(grp, x), correct = TRUE, conf.level = conf_level)$conf.int[1:2],
      "label_ci" = paste0(conf_level * 100, "% CI for difference (Wald with correction)")
    )
  } else if (diff_ci_method == "cmh") {

    est <- s_adj_proportion_diff(x, grp, strat, conf_level = conf_level)

    list(
      "diff" = est$diff_est,
      "diff_ci" = est$diff_ci,
      "label_ci" = paste0(conf_level * 100, "% CI for adjusted difference (CMH, without correction)")
    )
  }

  result
}


#' Test for difference between two proportions
#'
#' This function is a wrapper to extract the p-value from various tests for
#' difference of two proportions.
#'
#' @param x \code{logical} vector. \code{TRUE} represents a successful outcome.
#' @param grp a \code{factor} vector with 2 levels. Must be the same length as \code{x}.
#' @param strat a \code{factor} vector with at least 2 levels identifying the strata. Only required if
#' test is \code{"cmh"}.
#' @param test one of (\code{"chisq", "cmh"})\cr
#'   Specifies the test used to calculate the p-value.
#'
#' @details
#' The following options are supported for testing difference of proportions:
#' \itemize{
#'   \item{Option \code{chisq} performs Chi-Squared test. Internally calls \code{\link[stats]{prop.test}}.}
#'   \item{Option \code{cmh} performs stratified Cochran-Mantel-Haenszel test.
#'   Internally calls \code{\link[stats]{mantelhaen.test}}.}
#' }
#'
#' @return Named list with analysis summary.
#' \describe{
#'   \item{p_value}{P-value for testing null hypothesis that proportions in two groups are the same.}
#'   \item{test_name}{Label for test used to calculate \code{p_value}.}
#' }
#'
#' @export
#'
#' @importFrom stats prop.test mantelhaen.test
#' @importFrom utils.nest stop_if_not
#'
#' @seealso \code{\link{t_el_test_proportion_diff}}
#'   \code{\link{s_proportion}}, \code{\link{s_proportion_diff}}
#'
#' @examples
#' set.seed(1)
#' x <- sample(c(TRUE, FALSE), 100, TRUE)
#' grp <- factor(rep(c("A", "B"), each = 50))
#' strat <- factor(rep(c("V", "W", "X", "Y", "Z"), each = 20))
#' s_test_proportion_diff(x, grp, test = "chisq")
#' s_test_proportion_diff(x, grp, strat, test = "cmh")
#'
#' \dontrun{
#' # warning occurs because small size in strata
#' s_test_proportion_diff(
#'   x = c(TRUE, FALSE, FALSE, TRUE),
#'   grp = factor(c("A", "B", "A", "B")),
#'   strat = factor(c("X", "Y", "Y", "X")),
#'   test = "cmh"
#' )
#' }
s_test_proportion_diff <- function(x, grp, strat = NULL, test = c("chisq", "cmh")) {

  check_is_event(x)
  check_is_factor(grp, allow_na = FALSE)
  check_same_n(x = x, grp = grp)
  test <- match.arg(test)

  if (!is.null(strat)) {
    check_is_factor(strat, allow_na = FALSE)
    check_strata_levels(strat)
    check_same_n(x = x, strat = strat)
    stop_if_not(list(test == "cmh", paste0(test, " test is not applicable when strata_data is not NULL.")))
  } else {
    stop_if_not(list(test != "cmh", paste0(test, " test is not applicable when strat is NULL")))
  }

  stop_if_not(
    list(length(unique(x)) == 2, "x must include TRUE and FALSE"),
    list(length(levels(grp)) == 2, "grp must have 2 levels")
  )

  t_tbl <- if (is.null(strat)) {
    table(grp, x)
  } else {
    table(grp, x, strat)
  }

  result <- if (test == "chisq") {
    list(
      "p_value" = prop.test(t_tbl, correct = FALSE)$p.value,
      "test_name" = "Chi-squared Test"
    )
  } else if (test == "cmh") {

    if (any(tapply(x, strat, length) < 5)) {
      note <- "<5 data points in some strata. CMH test may be incorrect."
      warning(note)
    }

    list(
      "p_value" =  mantelhaen.test(t_tbl, correct = FALSE)$p.value,
      "test_name" = "Cochran-Mantel-Haenszel Test"
    )
  }

  result
}


#' Table for proportion of successful outcomes
#'
#' @inheritParams argument_convention
#' @inheritParams s_proportion
#'
#' @inherit s_proportion details
#'
#' @template return_rtable
#'
#' @export
#'
#' @seealso \code{\link{s_proportion}},
#'   \code{\link{t_el_proportion_diff}}, \code{\link{t_el_test_proportion_diff}},
#'   \code{\link{t_binary_outcome}}
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADRS <- radrs(cached = TRUE) %>%
#'   dplyr::filter(PARAMCD == "BESRSPI")
#'
#' t_el_proportion(
#'   ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD
#' )
#'
#' t_el_proportion(
#'   ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD,
#'   prop_ci_method = "wilson",
#'   conf_level = 0.90
#' )
#'
#' # single arm
#' t_el_proportion(
#'   ADRS$AVALC == "CR",
#'   col_by = by_all("All"),
#'   prop_ci_method = "clopper-pearson"
#' )
#'
#' # Add total
#' t_el_proportion(
#'   ADRS$AVALC == "CR",
#'   col_by = by_add_total(ADRS$ARMCD, "All")
#' )

t_el_proportion <- function(rsp, col_by, conf_level = 0.95,
                            prop_ci_method = c("waldcc", "wald", "clopper-pearson", "wilson")) {

  check_is_event(rsp)
  col_by <- col_by_to_matrix(col_by, rsp)
  col_by <- droplevels(col_by)
  check_same_n(rsp = rsp, col_by = col_by)

  prop_ci_method <- match.arg(prop_ci_method)
  prop_ci_method_label <- switch(
    prop_ci_method,
    "waldcc" = "% CI for Response Rates (Wald, with correction)",
    "wald" = "% CI for Response Rates (Wald, without correction)",
    "clopper-pearson" = "% CI for Response Rates (Clopper-Pearson)",
    "wilson" = "% CI for Response Rates (Wilson)"
  )

  # Table of responders
  tbl_rates <- t_count_true(rsp, col_by, row_name = "Responders")

  tbl_rate_ci <- rtabulate(
    x = rsp,
    col_by = col_by,
    function(x) {
      100 * s_proportion(x, conf_level = conf_level, prop_ci_method = prop_ci_method)$prop_ci
    },
    format = "(xx.xx, xx.xx)",
    row.name = paste0(conf_level * 100, prop_ci_method_label)
  )

  rbind(
    tbl_rates,
    tbl_rate_ci
  )

}


#' Table for testing difference in proportions
#'
#' This function will tabulate the p-value for testing the null hypothesis of no
#' difference between the proportion of events in the reference group as compared to
#' the proportion of events in each of the remaining group levels in \code{col_by}.
#'
#' @inheritParams argument_convention
#' @inheritParams s_test_proportion_diff
#' @param strata_data (\code{data.frame})\cr
#'   Used for stratification factors. If \code{NULL}, no stratified
#'   analysis is performed.
#'
#' @inherit s_test_proportion_diff details
#'
#' @template return_rtable
#'
#' @export
#'
#' @seealso \code{\link{s_test_proportion_diff}}
#'   \code{\link{t_el_proportion}}, \code{\link{t_el_proportion_diff}},
#'   \code{\link{t_binary_outcome}}
#'
#' @importFrom rtables col_by_to_factor
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADRS <- radrs(cached = TRUE) %>%
#'   dplyr::filter(PARAMCD == "BESRSPI")
#'
#' t_el_test_proportion_diff(
#'   ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD,
#'   test = "chisq"
#' )
#'
#' t_el_test_proportion_diff(
#'   ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD,
#'   strata_data = ADRS[, c("STRATA1", "STRATA2")],
#'   test = "cmh"
#' )
#'
#' # all observations as non-responders
#' t_el_test_proportion_diff(
#'   rep(FALSE, nrow(ADRS)),
#'   col_by = ADRS$ARMCD,
#'   test = "chisq"
#' )
#'
t_el_test_proportion_diff <- function(rsp, col_by, strata_data = NULL, test = c("chisq", "cmh")) {

  col_by <- col_by_to_factor(col_by)
  col_by <- droplevels(col_by)
  check_binary_endpoint_args(rsp, col_by, strata_data)

  test <- match.arg(test)
  test_label <- switch(
    test,
    chisq = "p-value (Chi-squared Test)",
    cmh = "p-value (CMH Test)"
  )

  # Table of responders
  tbl_rates <- t_count_true(rsp, col_by, row_name = "Responders")

  # format for p-values
  format_pval_na <- function(x, output) {
    if (is.na(x)) {
      "-"
    } else {
      if (x < 0.0001) {
        "<.0001"
      } else {
        paste(round(x, 4))
      }
    }
  }

  if (all(rsp) || all(!rsp)) {

    # create empty table
    tbl_empty_header <- rheader(rrowl("", levels(col_by)))

    tbl_empty <- rtable(
      header = tbl_empty_header,
      rrowl(row.name = test_label, c(list(NULL), rep(NA, length(levels(col_by)[-1]))), format = format_pval_na)
    )

    return(rbind(tbl_rates, tbl_empty))
  }

  if (!is.null(strata_data)) {
    rsp <- data.frame(
      rsp = rsp,
      strat = interaction(strata_data, drop = TRUE)
    )
  }

  tbl_pval <- tabulate_pairwise(
    rsp,
    col_by,
    function(x, by) {
      if (is.data.frame(x)) {
        s_test_proportion_diff(x[[1]], by, strat = x[[2]], test = test)$p_value
      } else {
        s_test_proportion_diff(x, by, strat = NULL, test = test)$p_value
      }
    },
    format = format_pval_na,
    row.name = ""
  )

  row.names(tbl_pval) <- test_label

  rbind(
    tbl_rates,
    tbl_pval
  )

}


#' Table for difference in proportions
#'
#' This function will calculate the difference in proportion of events between each
#' comparison group versus the reference group. The first level of \code{col_by} is
#' used as a reference group.
#'
#' @inheritParams argument_convention
#' @inheritParams s_proportion_diff
#' @param strata_data (\code{data.frame})\cr
#'   Used for stratification factors. If \code{NULL}, no stratified
#'   analysis is performed.
#'
#' @inherit s_proportion_diff details
#'
#' @template return_rtable
#'
#' @export
#'
#' @seealso \code{\link{s_proportion_diff}},
#'   \code{\link{t_el_proportion}}, \code{\link{t_el_test_proportion_diff}},
#'   \code{\link{t_binary_outcome}}
#'
#' @importFrom rtables col_by_to_factor
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADRS <- radrs(cached = TRUE) %>%
#'   dplyr::filter(PARAMCD == "BESRSPI")
#'
#' t_el_proportion_diff(
#'   ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD,
#'   diff_ci_method = "wald"
#' )
#'
#' t_el_proportion_diff(
#'   ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD,
#'   strata_data = ADRS[, c("STRATA1", "STRATA2")],
#'   diff_ci_method = "cmh",
#'   conf_level = 0.99
#' )
#'
#' # all observations as non-responders
#' t_el_proportion_diff(
#'   rep(FALSE, nrow(ADRS)),
#'   col_by = ADRS$ARMCD,
#'   diff_ci_method = "wald"
#' )
t_el_proportion_diff <- function(rsp, col_by, strata_data = NULL, conf_level = 0.95,
                                 diff_ci_method = c("wald", "waldcc", "cmh")) {

  col_by <- col_by_to_factor(col_by)
  col_by <- droplevels(col_by)
  check_binary_endpoint_args(rsp, col_by, strata_data)

  diff_ci_method <- match.arg(diff_ci_method)
  method_label <- switch(
    diff_ci_method,
    wald = "CI (Wald without correction)",
    waldcc = "CI (Wald with correction)",
    cmh = "CI (CMH, without correction)"
  )
  method_label <- paste0(100 * conf_level, "% ", method_label)

  # Table of responders
  tbl_rates <- t_count_true(rsp, col_by, row_name = "Responders")

  if (!is.null(strata_data)) {
    rsp <- data.frame(
      rsp = rsp,
      strat = interaction(strata_data, drop = TRUE)
    )
  }

  format_ci_na <- function(x, output) {
    if (all(is.na(x))) {
      "-"
    } else {
      paste0("(", round(x, 2)[1], ", ", round(x, 2)[2], ")")
    }
  }

  tbl_prop_diff <- tabulate_pairwise(
    rsp,
    col_by,
    function(x, by) {
      if (is.data.frame(x)) {
        100 * s_proportion_diff(x[[1]], by, strat = x[[2]],
                                diff_ci_method = diff_ci_method, conf_level = conf_level)$diff
      } else {
        100 * s_proportion_diff(x, by, strat = NULL,
                                diff_ci_method = diff_ci_method, conf_level = conf_level)$diff
      }
    },
    format = "xx.xx",
    row.name = ifelse(
      diff_ci_method == "cmh",
      "Weighted Difference in Response Rates (%)",
      "Difference in Response Rates (%)"
    )
  )

  tbl_prop_diff_ci <- tabulate_pairwise(
    rsp,
    col_by,
    function(x, by) {
      if (is.data.frame(x)) {
        100 * s_proportion_diff(x[[1]], by, strat = x[[2]],
                                diff_ci_method = diff_ci_method, conf_level = conf_level)$diff_ci
      } else {
        100 * s_proportion_diff(x, by, strat = NULL,
                                diff_ci_method = diff_ci_method, conf_level = conf_level)$diff_ci
      }
    },
    format = format_ci_na,
    row.name = method_label,
    indent = 1
  )

  rbind(
    tbl_rates,
    tbl_prop_diff,
    tbl_prop_diff_ci
  )
}

#' Summary function for odds ratio estimation.
#'
#' @inheritParams argument_convention
#' @param strat Optional factor with one level per stratum and same length as \code{rsp}. If `NULL`,
#'   then the unstratified analysis is computed.
#' @return A \code{data.frame} with columns:
#' \describe{
#'   \item{level}{The level of the `arm` variable which is compared with the reference level.}
#'   \item{odds_ratio}{The odds ratio estimate.}
#'   \item{ci_lower}{The lower bound of the confidence interval.}
#'   \item{ci_upper}{The upper bound of the confidence interval.}
#' }
#' The attribute `conf_level` saves the used confidence level.
#'
#' @details This function uses either logistic regression for unstratified analyses, or conditional logistic
#'   regression for stratified analyses.
#'   The Wald confidence interval with the specified confidence level is calculated. Note that
#'   for stratified analyses, there is currently no implementation for conditional likelihood confidence intervals,
#'   therefore we don't offer the option of a likelihood confidence interval.
#'   Note that when `rsp` contains only responders or non-responders, then the result values will be `NA`,
#'   because no odds ratio estimation is possible.
#'
#' @importFrom stats coef glm as.formula confint confint.default
#' @importFrom survival clogit
#' @export
#'
#' @examples
#' set.seed(1)
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' trt <- sample(c("Placebo", "Treatment", "Combination"), 100, TRUE)
#' trt <- factor(trt, levels = c("Placebo", "Treatment", "Combination"))
#' strata_data <- data.frame(
#'   "f1" = sample(c("a", "b"), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE
#' )
#'
#' s_odds_ratio(rsp, trt)
#' s_odds_ratio(rsp, trt, strat = interaction(strata_data))
s_odds_ratio <- function(rsp,
                         col_by,
                         conf_level = 0.95,
                         strat = NULL) {
  # Check inputs.
  check_is_event(rsp)
  check_is_factor(col_by, allow_na = FALSE)
  check_same_n(rsp = rsp, col_by = col_by)
  check_conf_level(conf_level)
  use_strata <- !is.null(strat)
  if (use_strata) {
    check_is_factor(strat, allow_na = FALSE)
    check_strata_levels(strat)
    check_same_n(rsp = rsp, strat = strat)
  }

  if (all(rsp) || all(!rsp)) {
    result <- data.frame(
      level = levels(col_by)[-1],
      odds_ratio = NA,
      ci_lower = NA,
      ci_upper = NA,
      row.names = NULL
    )
    attr(result, "conf_level") <- conf_level
    return(result)
  }

  # Obtain odds ratio estimates with CIs for the non-reference
  # levels of `col_by`.
  raw_results <-
    if (use_strata) {
      formula <- stats::as.formula("rsp ~ col_by + strata(strat)")
      data <- data.frame(
        rsp = rsp,
        col_by = col_by,
        strat = strat
      )
      model_fit <- survival::clogit(
        formula = formula,
        data = data
      )
      list(
        or = exp(stats::coef(model_fit)),
        ci = exp(stats::confint(model_fit, level = conf_level))
      )
    } else {
      formula <- as.formula("rsp ~ col_by")
      data <- data.frame(
        rsp = rsp,
        col_by = col_by
      )
      model_fit <- stats::glm(
        formula = formula,
        data = data,
        family = binomial(link = "logit")
      )
      # Note that here we need to discard the intercept.
      list(
        or = exp(stats::coef(model_fit)[-1]),
        # We use `confint.default` to choose the Wald confidence interval.
        ci = exp(stats::confint.default(model_fit, level = conf_level)[-1, , drop = FALSE])
      )
    }

  # Combine into a data frame for return.
  result <- data.frame(
    level = levels(col_by)[-1],
    odds_ratio = raw_results$or,
    ci_lower = raw_results$ci[, 1],
    ci_upper = raw_results$ci[, 2],
    row.names = NULL
  )
  attr(result, "conf_level") <- conf_level
  return(result)
}

#' Elementary summary table for odds ratio estimation.
#'
#' @inheritParams argument_convention
#' @param strata_data (\code{data.frame}) Optional stratification factors. If \code{NULL}, the unstratified analysis
#'   will be performed.
#' @inherit s_odds_ratio details
#' @template return_rtable
#'
#' @importFrom rtables rtabulate header_add_N rrowl
#' @export
#'
#' @seealso \code{\link{s_odds_ratio}}, \code{\link{t_el_proportion}},
#'   \code{\link{t_binary_outcome}}
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADRS <- radrs(cached = TRUE) %>%
#'   dplyr::filter(PARAMCD == "BESRSPI")
#'
#' t_el_odds_ratio(
#'   rsp = ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD,
#'   conf_level = 0.9
#' )
#'
#' t_el_odds_ratio(
#'   rsp = ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD,
#'   strata_data = ADRS[, c("STRATA1", "STRATA2")]
#' )
t_el_odds_ratio <- function(rsp,
                            col_by,
                            conf_level = 0.95,
                            strata_data = NULL) {
  col_by <- col_by_to_factor(col_by)
  col_by <- droplevels(col_by)
  check_binary_endpoint_args(rsp, col_by, strata_data)
  strat <- if (!is.null(strata_data)) interaction(strata_data, drop = TRUE) else NULL

  # Obtain Odds Ratio analysis results and combine in table.
  results_or <- s_odds_ratio(
    rsp = rsp,
    col_by = col_by,
    conf_level = conf_level,
    strat = strat
  )
  results_or <- Map(
    function(x, y, z) c(x, y, z),
    results_or$odds_ratio,
    results_or$ci_lower,
    results_or$ci_upper
  )
  tbl_or <- rtables::rtable(
    header = levels(col_by),
    rtables::rrowl(
      paste0("Odds Ratio", ifelse(is.null(strat), "", "*"), " (", conf_level * 100, "% CI)"),
      c(list(NULL),
        results_or),
      format = "xx.xx (xx.xx - xx.xx)"
    )
  )

  # Obtain table with number and % of responders.
  tbl_rsp <- rtables::rtabulate(
    rsp,
    col_by = col_by,
    positives_and_proportion,
    format = "xx.xx (xx.xx%)",
    row.name = "Responders"
  )

  # Combine in table, add Ns. (Footer is going to be handled by t_binary_outcome).
  tbl <- rbind(
    tbl_rsp,
    tbl_or
  )
  tbl <- rtables::header_add_N(tbl, N = tabulate(col_by))
  return(tbl)
}

#' Summary table for binary outcome
#'
#' This is a convenience wrapper function for creating a summary of common analyses
#' for binary outcome variables. Options include reporting unstratified
#' analysis, stratified analysis or both.
#'
#' @inheritParams argument_convention
#' @inheritParams t_el_proportion
#' @inheritParams t_el_proportion_diff
#' @inheritParams t_el_test_proportion_diff
#' @param unstrat_analysis named character vector specifying sections to include
#' under the heading "Unstratified Analysis". If \code{NULL}, the table section is excluded.
#' The named slot options are:
#'   * \code{diff_ci_method} The method used for the confidence interval for difference of
#' proportions. Default method is "waldcc", Wald with continuity correction.
#'   * \code{diff_test} Test for a difference between two proportions. Default
#'     method is "chisq", Chi-squared test.
#' @param strat_analysis named character vector specifying sections to include
#' under the heading "Stratified Analysis". Only applicable if stratification factors
#' from \code{strata_data} are provided.
#' The named slot options are:
#'   * \code{diff_ci_method} The method used for the confidence interval for difference of
#'     proportions. Default method is "waldcc", Wald with continuity correction.
#'   * \code{diff_test} Test for a difference between two proportions. Default
#'     method is "cmh", Cochran-Mantel-Haenszel test.
#'
#' @details
#' Table sections are matched by name, so to remove a section specify a subset of the named slots
#'   in \code{strat_analysis} or \code{strat_analysis}.
#'
#' The following options are supported for confidence interval for a proportion:
#'   * The \code{wald} interval follows the usual textbook definition for a single proportion
#'     confidence interval using the normal approximation.
#'   * The \code{waldcc} interval is similar to \code{wald} and in addition uses the continuity
#'     correction \code{1/2n}.
#'   * The \code{clopper-pearson} interval calls \code{\link[stats]{binom.test}}. Also referred to
#'     as the 'exact' method.
#'   * The \code{wilson} interval calls \code{\link[stats]{prop.test}} with option \code{correct=FALSE}.
#'     Also referred to as Wilson score interval.
#'
#' The following options are supported for difference of proportions confidence intervals:
#'   * Option \code{"wald"} calls \code{\link[stats]{prop.test}} with \code{correct=FALSE}
#'   * Option \code{"waldcc"} includes continuity correction for Wald interval and
#'   calls \code{\link[stats]{prop.test}} with \code{correct=TRUE}
#'   * Option \code{"cmh"} derives a CI for CMH-weighted difference of proportions and
#'    calls \code{\link{s_adj_proportion_diff}}
#'
#' The following options are supported for testing difference of proportions:
#'   * Option \code{chisq} performs Chi-Squared test. Internally calls \code{\link[stats]{prop.test}}.
#'   * Option \code{cmh} performs stratified Cochran-Mantel-Haenszel test.
#'   Internally calls \code{\link[stats]{mantelhaen.test}}.
#'
#' @md
#'
#' @template return_rtable
#'
#' @export
#'
#' @seealso \code{\link{t_el_proportion}},
#'   \code{\link{t_el_proportion_diff}}, \code{\link{t_el_test_proportion_diff}}
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' ADRS <- radrs(cached = TRUE) %>%
#'   dplyr::filter(PARAMCD == "BESRSPI")
#'
#' # unstratified analysis only with default options for unstrat_analysis
#' t_binary_outcome(
#'   ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD
#' )
#'
#' # unstratified + stratified analysis with default options for
#' # unstrat_analysis and strat_analysis
#' t_binary_outcome(
#'   ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD,
#'   strata_data = ADRS[, c("STRATA1", "STRATA2")]
#' )
#'
#' # stratified analysis only
#' # Show difference with CMH weights and p-value from CMH test
#' t_binary_outcome(
#'   ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD,
#'   strata_data = ADRS[, c("STRATA1", "STRATA2")],
#'   unstrat_analysis = NULL,
#'   strat_analysis = c("diff_ci_method" = "cmh", "diff_test" = "cmh")
#'  )
#'
#' # exclude comparison between arms
#' # specify CI method for proportions and change CI level
#' t_binary_outcome(
#'   ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD,
#'   prop_ci_method = "clopper-pearson",
#'   unstrat_analysis = NULL,
#'   conf_level = 0.90
#'  )

t_binary_outcome <- function(rsp, col_by, strata_data = NULL, conf_level = 0.95,
                             prop_ci_method = c("waldcc", "wald", "clopper-pearson", "wilson"),
                             unstrat_analysis = c("diff_ci_method" = "waldcc", "diff_test" = "chisq"),
                             strat_analysis = c("diff_ci_method" = "waldcc", "diff_test" = "cmh")
) {

  # NOTE: argument checks are done inside each elementary function, so not checked here

  # Table of responders + optional CI
  tbl_response <- if (!is.null(prop_ci_method)) {
    t_el_proportion(rsp, col_by, conf_level = conf_level, prop_ci_method = prop_ci_method)
  } else {
    t_count_true(rsp, col_by, row_name = "Responders")
  }

  # Unstratified analysis table
  tbl_u <- if (!is.null(unstrat_analysis)) {

    if (!all(names(unstrat_analysis) %in% c("diff_ci_method", "diff_test"))) {
      stop("unstrat_analysis elements must be any from this list: diff_ci_method, diff_test")
    }

    tbl_u_difference <- if ("diff_ci_method" %in% names(unstrat_analysis)) {
      t_el_proportion_diff(rsp, col_by, strata_data = NULL,
                           conf_level = conf_level,
                           diff_ci_method = unstrat_analysis["diff_ci_method"])[-1, ]
    } else {
      NULL
    }

    tbl_u_test <- if ("diff_test" %in% names(unstrat_analysis)) {
      t_el_test_proportion_diff(rsp, col_by, strata_data = NULL,
                                test = unstrat_analysis["diff_test"])[-1, ]
    } else {
      NULL
    }

    insert_rrow(
      indent(rbind(tbl_u_difference, tbl_u_test), 1),
      rrow("Unstratified Analysis")
    )

  } else {
    NULL
  }

  tbl_footer <- NULL

  # Stratified analysis table
  tbl_s <- if (!is.null(strat_analysis) && !is.null(strata_data)) {

    if (!all(names(strat_analysis) %in% c("diff_ci_method", "diff_test"))) {
      stop("strat_analysis elements must be any from this list: diff_ci_method, diff_test")
    }

    tbl_s_difference <- if ("diff_ci_method" %in% names(strat_analysis)) {

      # strata_data is set to null to fulfill GDSR RSPT01 template criteria:
      # difference in response section (any of the unstratified options) may be repeated between
      # stratified and unstratified analysis table sections
      tmp_strata_data <- if (strat_analysis["diff_ci_method"] == "cmh") {
        strata_data
      } else {
        NULL
      }

      t_el_proportion_diff(rsp, col_by,
                           strata_data = tmp_strata_data,
                           conf_level = conf_level,
                           diff_ci_method = strat_analysis["diff_ci_method"])[-1, ]
    } else {
      NULL
    }

    tbl_s_test <- if ("diff_test" %in% names(strat_analysis)) {
      t_el_test_proportion_diff(rsp, col_by, strata_data = strata_data,
                                test = strat_analysis["diff_test"])[-1, ]
    } else {
      NULL
    }

    n_strata <- length(strata_data)
    tbl_footer <- rtable(
      header = header(tbl_response),
      rrow(
        paste(
          "* Model stratified by",
          ifelse(
            n_strata < 2,
            names(strata_data),
            paste(paste(names(strata_data)[-n_strata], collapse = ", "), "and", names(strata_data)[(n_strata)])
          )
        )
      )
    )

    insert_rrow(
      indent(rbind(tbl_s_difference, tbl_s_test), 1),
      rrow("Stratified Analysis*")
    )

  } else {
    NULL
  }

  rbind(
    tbl_response,
    tbl_u,
    tbl_s,
    tbl_footer,
    gap = 1
  )

}
