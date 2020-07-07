#' Describe the proportion summary
#'
#' This is an auxiliary function that describes the analysis in `s_proportion`.
#'
#' @inheritParams s_proportion
#' @param long Whether a long or a short (default) description is required.
#' @return String describing the analysis.
d_proportion <- function(conf_level,
                         prop_ci_method,
                         long = FALSE) {
  label <- paste0(conf_level * 100, "% CI")
  if (long) label <- paste(label, "for Response Rates")
  method_part <- switch(
    prop_ci_method,
    "clopper-pearson" = "Clopper-Pearson",
    "waldcc" = "Wald, with correction",
    "wald" = "Wald, without correction",
    "wilson" = "Wilson",
    "agresti-coull" = "Agresti-Coull",
    "jeffreys" = "Jeffreys",
    stop(paste(prop_ci_method, "does not have a description"))
  )
  label <- paste0(label, " (", method_part, ")")
  return(label)
}

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

  # Calculate CI.
  prop_ci <- if (prop_ci_method == "clopper-pearson") {

    as.numeric(binom.test(x_sum, n, conf.level = conf_level)$conf.int)

  } else if (prop_ci_method == "wilson") {

    as.numeric(prop.test(x_sum, n, correct = FALSE, conf.level = conf_level)$conf.int)

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

    c(l_ci, u_ci)

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

    c(l_ci, u_ci)

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

    c(l_ci, u_ci)
  }

  results <- list(
    "prop" = p_hat,  # Note: We always return the same response proportion estimate.
    "prop_ci" = prop_ci,
    "label_ci" = d_proportion(
      conf_level = conf_level,
      prop_ci_method = prop_ci_method,
      long = TRUE
    )
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

#' Describe the difference between two proportions summary
#'
#' This is an auxiliary function that describes the analysis in `s_proportion_diff`.
#'
#' @inheritParams s_proportion_diff
#' @param long Whether a long or a short (default) description is required.
#' @return String describing the analysis.
d_proportion_diff <- function(conf_level,
                              diff_ci_method,
                              long = FALSE) {
  label <- paste0(conf_level * 100, "% CI")
  if (long) label <- paste(
    label,
    ifelse(
      diff_ci_method == "cmh",
      "for adjusted difference",
      "for difference"
    )
  )

  method_part <- switch(
    diff_ci_method,
    "cmh" = "CMH, without correction",
    "waldcc" = "Wald, with correction",
    "wald" = "Wald, without correction",
    "anderson-hauck" = "Anderson-Hauck",
    "ha" = "Anderson-Hauck",
    "newcombe" = "Newcombe",
    stop(paste(diff_ci_method, "does not have a description"))
  )
  label <- paste0(label, " (", method_part, ")")
  return(label)
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
#' @param diff_ci_method one of (\code{"wald"}, \code{"waldcc"}, \code{"cmh"}, \code{"anderson-hauck"} or \code{"ha"},
#'   \code{"newcombe"})\cr
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
#'   \item{Options \code{anderson-hauck} or \code{ha} derives the Anderson-Hauck confidence interval.}
#'   \item{Option \code{newcombe} derives the Newcombe confidence interval, which is based on the Wilson score
#'     confidence interval for a single binomial proportion.}
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
s_proportion_diff <- function(x,
                              grp,
                              strat = NULL,
                              conf_level = 0.95,
                              diff_ci_method = c("wald", "waldcc", "cmh", "anderson-hauck", "ha", "newcombe")) {

  check_is_event(x)
  check_is_factor(grp, allow_na = FALSE)
  check_same_n(x = x, grp = grp)
  check_conf_level(conf_level)
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

  # Common variables used below.
  p_grp <- tapply(x, grp, mean)
  diff_p <- unname(diff(p_grp))
  n_grp <- tapply(x, grp, length)
  z <- qnorm((1 + conf_level) / 2)

  results <- if (diff_ci_method == "wald") {
    diff_ci <- stats::prop.test(
      table(grp, x),
      correct = FALSE,
      conf.level = conf_level
    )$conf.int[1:2]
    list(
      "diff" = diff_p,
      "diff_ci" = diff_ci
    )
  } else if (diff_ci_method == "waldcc") {
    diff_ci <- stats::prop.test(
      table(grp, x),
      correct = TRUE,
      conf.level = conf_level
    )$conf.int[1:2]
    list(
      "diff" = diff_p,
      "diff_ci" = diff_ci
    )
  } else if (diff_ci_method == "cmh") {
    est <- s_adj_proportion_diff(
      x = x,
      grp = grp,
      strat = strat,
      conf_level = conf_level
    )
    list(
      "diff" = est$diff_est,
      "diff_ci" = est$diff_ci
    )
  } else if (diff_ci_method %in% c("anderson-hauck", "ha")) {
    err <- 1 / (2 * min(n_grp)) + z * sqrt(sum(p_grp * (1 - p_grp) / (n_grp - 1)))
    l_ci <- max(-1, diff_p - err)
    u_ci <- min(1, diff_p + err)
    list(
      "diff" = diff_p,
      "diff_ci" = c(l_ci, u_ci)
    )
  } else if (diff_ci_method == "newcombe") {
    # Source:
    # https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf
    x_grp <- split(x, f = grp)
    ci_grp <- lapply(
      x_grp,
      FUN = function(x) {
        s_proportion(
          x = x,
          conf_level = conf_level,
          prop_ci_method = "wilson"
        )$prop_ci
      }
    )
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
  results$label_ci <- d_proportion_diff(
    conf_level = conf_level,
    diff_ci_method = diff_ci_method,
    long = TRUE
  )

  return(results)
}

#' Describe the test between two proportions
#'
#' This is an auxiliary function that describes the analysis in `s_test_proportion_diff`.
#'
#' @inheritParams s_test_proportion_diff
#' @return String describing the test from which the p-value is derived.
d_test_proportion_diff <- function(test) {
  test_part <- switch(
    test,
    "schouten" = "Chi-Squared Test with Schouten Correction",
    "chisq" = "Chi-Squared Test",
    "cmh" = "Cochran-Mantel-Haenszel Test",
    "fisher" = "Fisher's Exact Test",
    stop(paste(test, "does not have a description"))
  )
  label <- paste0("p-value (", test_part, ")")
  return(label)
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
#' @param test one of (\code{"chisq", "cmh", "fisher", "schouten"})\cr
#'   Specifies the test used to calculate the p-value.
#'
#' @details
#' The following options are supported for testing difference of proportions:
#' \itemize{
#'   \item{Option \code{schouten} performs the Chi-Squared test with Schouten correction.}
#'   \item{Option \code{chisq} performs Chi-Squared test. Internally calls \code{\link[stats]{prop.test}}.}
#'   \item{Option \code{cmh} performs stratified Cochran-Mantel-Haenszel test.
#'   Internally calls \code{\link[stats]{mantelhaen.test}}.}
#'   \item{Option \code{fisher} performs the Fisher's exact test.}
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
#' @importFrom stats prop.test mantelhaen.test fisher.test pchisq
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
s_test_proportion_diff <- function(x,
                                   grp,
                                   strat = NULL,
                                   test = c("schouten", "chisq", "cmh", "fisher")) {

  check_is_event(x)
  check_is_factor(grp, allow_na = FALSE)
  check_same_n(x = x, grp = grp)
  test <- match.arg(test)

  if (!is.null(strat)) {
    check_is_factor(strat, allow_na = FALSE)
    check_strata_levels(strat)
    check_same_n(x = x, strat = strat)
    stop_if_not(list(
      test == "cmh",
      paste(test, "test is not applicable when strata_data is not NULL.")
    ))
  } else {
    stop_if_not(list(
      test != "cmh",
      paste(test, "test is not applicable when strat is NULL"))
    )
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

  pval <-
    if (test == "chisq") {

      stats::prop.test(t_tbl, correct = FALSE)$p.value

    } else if (test == "cmh") {

      if (any(tapply(x, strat, length) < 5)) {
        note <- "<5 data points in some strata. CMH test may be incorrect."
        warning(note)
      }

      stats::mantelhaen.test(t_tbl, correct = FALSE)$p.value

    } else if (test == "fisher") {

      stats::fisher.test(t_tbl)$p.value

    } else if (test == "schouten") {
      # Source: STREAM v2
      #nolint start
      # https://github.roche.com/MDIS/stream2/blob/82c6c54ea6c61d11746af3b413ad6b1b213096cd/app/macro/str_tlg_method_resp.sas#L1623
      #nolint end
      count_1_1 <- t_tbl[1, "FALSE"]
      count_1_2 <- t_tbl[1, "TRUE"]
      count_2_1 <- t_tbl[2, "FALSE"]
      count_2_2 <- t_tbl[2, "TRUE"]
      t_schouten <- (count_1_1 + count_1_2 + count_2_1 + count_2_2 - 1) *
        (abs(count_2_2 * count_1_1 - count_1_2 * count_2_1) -
        0.5 * min(count_1_1 + count_1_2, count_2_1 + count_2_2))^2 /
        ((count_1_1 + count_1_2) * (count_2_1 + count_2_2) *
        (count_1_2 + count_2_2) * (count_1_1 + count_2_1))
      p_value <- 1 - stats::pchisq(t_schouten, df = 1)
      p_value
    }

  results <- list(
    "p_value" = pval,
    "test_name" = d_test_proportion_diff(test)
  )
  return(results)
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
#' @seealso \code{\link{s_proportion}}, \code{\link{t_el_multinomial_proportion}},
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
t_el_proportion <- function(rsp,
                            col_by,
                            conf_level = 0.95,
                            prop_ci_method = "waldcc") {

  check_is_event(rsp)
  col_by <- col_by_to_matrix(col_by, rsp)
  col_by <- droplevels(col_by)
  check_same_n(rsp = rsp, col_by = col_by)
  # Note: `conf_level` and `prop_ci_method` are checked downstream in `s_proportion`, so not again here.

  # Table of responders
  tbl_rates <- t_count_true(rsp, col_by, row_name = "Responders")

  label_ci <- d_proportion(
    conf_level = conf_level,
    prop_ci_method = prop_ci_method,
    long = FALSE
  )
  tbl_rate_ci <- rtabulate(
    x = rsp,
    col_by = col_by,
    function(x) {
      100 * s_proportion(x, conf_level = conf_level, prop_ci_method = prop_ci_method)$prop_ci
    },
    format = "(xx.xx, xx.xx)",
    row.name = label_ci
  )

  rbind(
    tbl_rates,
    tbl_rate_ci
  )

}

#' Table for proportion of outcomes per factor level
#'
#' @inheritParams argument_convention
#' @inheritParams s_proportion
#' @param rsp_multinomial (\code{factor} vector) The levels represent ordered response categories.
#'   \code{NA} values or empty strings are not allowed.
#' @param levels_without_ci (\code{character} vector) If any levels of \code{rsp_multinomial}
#'  match, the corresponding rates are reported without a confidence interval.
#'
#' @return return_rtable
#'
#' @export
#'
#' @seealso \code{\link{s_proportion}}, \code{\link{t_el_proportion}},
#'   \code{\link{t_binary_outcome}}
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADRS <- radrs(cached = TRUE) %>%
#'   dplyr::filter(PARAMCD == "BESRSPI")
#'
#' t_el_multinomial_proportion(ADRS$AVALC, col_by = ADRS$ARMCD)
#'
#' t_el_multinomial_proportion(ADRS$AVALC, col_by = ADRS$ARMCD, levels_without_ci = "NE")
#'
#' # apply standard oncology labels, given factor input.
#' m_rsp <- ADRS$AVALC
#' levels(m_rsp) <- get_onco_rsp_label()[match(levels(m_rsp), names(get_onco_rsp_label()))]
#' t_el_multinomial_proportion(m_rsp, col_by = ADRS$ARMCD, levels_without_ci = "Not Evaluable (NE)")
#'
#' t_el_multinomial_proportion(
#'   ADRS$AVALC,
#'   col_by = ADRS$ARMCD,
#'   prop_ci_method = "wilson",
#'   conf_level = 0.90
#' )
#'
#' # single arm
#' t_el_multinomial_proportion(
#'   ADRS$AVALC,
#'   col_by = by_all("All"),
#'   prop_ci_method = "clopper-pearson"
#' )
#'
#' # Add total
#' t_el_multinomial_proportion(
#'   ADRS$AVALC,
#'   col_by = by_add_total(ADRS$ARMCD, "All")
#' )

t_el_multinomial_proportion <- function(rsp_multinomial, col_by, conf_level = 0.95,
                                        prop_ci_method = "waldcc", levels_without_ci = NULL) {

  check_is_factor(sas_na(rsp_multinomial, empty = TRUE, whitespaces = TRUE), allow_na = FALSE)
  stopifnot(is.character(levels_without_ci) || is.null(levels_without_ci))

  l_tbls <- lapply(levels(rsp_multinomial), function(level_i){

    if (level_i %in% unique(levels_without_ci)) {

      tbl_i <- t_count_true(rsp_multinomial == level_i, col_by, row_name = level_i)

    } else {

      tbl_i <- t_el_proportion(
        rsp_multinomial == level_i,
        col_by,
        conf_level,
        prop_ci_method
      )
      row.names(tbl_i)[1] <- level_i
    }

    tbl_i
  })

  tbl <- rbindl_rtables(l_tbls, gap = 1)
  tbl
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
#' @seealso \code{\link{s_test_proportion_diff}},
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
t_el_test_proportion_diff <- function(rsp, col_by, strata_data = NULL, test = "schouten") {

  col_by <- col_by_to_factor(col_by)
  col_by <- droplevels(col_by)
  check_binary_endpoint_args(rsp, col_by, strata_data)
  # Note: `test` is checked downstream in `s_test_proportion_diff`, so not again here.

  # Obtain label (from arbitrary data).
  test_label <- d_test_proportion_diff(test)

  # Table of responders
  tbl_rates <- t_count_true(rsp, col_by, row_name = "Responders")

  if (all(rsp) || all(!rsp)) {

    # create empty table
    tbl_empty_header <- rheader(rrowl("", levels(col_by)))

    tbl_empty <- rtable(
      header = tbl_empty_header,
      rrowl(row.name = test_label, c(list(NULL), rep(NA, length(levels(col_by)[-1]))), format = "xx.xx")
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
    format = "x.xxxx | (<0.0001)",
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
                                 diff_ci_method = "wald") {

  col_by <- col_by_to_factor(col_by)
  col_by <- droplevels(col_by)
  check_binary_endpoint_args(rsp, col_by, strata_data)
  # Note: `conf_level` and `diff_ci_method` are checked downstream in `s_proportion_diff`, so not again here.

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

  label_ci <- d_proportion_diff(
    conf_level = conf_level,
    diff_ci_method = diff_ci_method,
    long = FALSE
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
    row.name = label_ci,
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
      paste0("Odds Ratio (", conf_level * 100, "% CI)"),
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

#' Control function for binary comparison analyses
#'
#' This is an auxiliary function for controlling the arguments for \code{\link{t_binary_outcome}} specifying
#' how the response proportions should be compared between the arms.
#'
#' @param diff_ci The method used for the confidence interval for difference of
#'   proportions (see \code{\link{s_proportion_diff}} for details). Default method is here "waldcc",
#'   i.e. Wald with continuity correction. If \code{NULL}, then no confidence intervals will be calculated.
#' @param diff_test Test for a difference between two proportions
#'   (see \code{\link{s_test_proportion_diff}} for details). Default method is
#'   here `schouten`, i.e. Chi-Squared Test with Schouten Correction.
#'   If `NULL`, then no test will be performed and no p-values reported.
#' @param odds_ratio (\code{logical}) Whether the odds ratio estimates and confidence intervals should be
#'   reported (default) or not.
#' @return a list of components named as the arguments
#'
#' @md
#' @export
control_binary_comparison <- function(diff_ci = "waldcc",
                                      diff_test = "schouten",
                                      odds_ratio = TRUE) {

  # Note: `diff_ci` and `diff_test` are checked downstream if they are strings (by the `s_*` functions.)
  # To avoid duplicate checks, we don't check these details here.

  if (!is.null(diff_ci)) {
    stopifnot(is.character(diff_ci), identical(length(diff_ci), 1L))
  }
  if (!is.null(diff_test)) {
    stopifnot(is.character(diff_test), identical(length(diff_test), 1L))
  }
  stopifnot(is.logical(odds_ratio), identical(length(odds_ratio), 1L))

  list(diff_ci = diff_ci, diff_test = diff_test, odds_ratio = odds_ratio)
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
#' @param unstrat_analysis list specified by calling \code{\link{control_binary_comparison}} for controlling the
#'  "Unstratified Analysis" section. If \code{NULL}, this table section is excluded.
#' @param strat_analysis list specified by calling \code{\link{control_binary_comparison}} for controlling the
#'  "Stratified Analysis" section. Only applicable if stratification factors from \code{strata_data} are provided.
#'  If \code{NULL}, this table section is excluded.
#' @param rsp_multinomial (\code{factor} vector) The levels represent ordered response categories.
#'   \code{NA} values or empty strings are not allowed. If \code{NULL}, tabulation by each
#'   response category will not be performed.
#'
#' @details
#' Table subsections are matched by name, so to remove a subsection set any the corresponding arguments of
#' \code{unstrat_analysis}, \code{strat_analysis} or \code{rsp_multinomial} to \code{NULL}.
#'
#' The display order of response categories in partitioned statistics section
#' inherits the factor level order of \code{partition_analysis}. Use
#' \code{\link[base]{factor}} and its \code{levels} argument to include or
#' exclude response categories and arrange display order. If response values
#' contain "NE", "Missing", "NE/Missing", "Not Evaluable (NE)" or "Missing or unevaluable",
#' the confidence interval will not be calculated.
#'
#' @md
#'
#' @template return_rtable
#'
#' @export
#'
#' @seealso \code{\link{control_binary_comparison}}, \code{\link{t_el_proportion}},
#'   \code{\link{t_el_proportion_diff}}, \code{\link{t_el_test_proportion_diff}},
#'   \code{\link{t_el_multinomial_proportion}}
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' ADRS <- radrs(cached = TRUE) %>%
#'   dplyr::filter(PARAMCD == "BESRSPI")
#'
#' # Unstratified analysis only with default options for `unstrat_analysis`.
#' t_binary_outcome(
#'   ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD
#' )
#'
#' # Unstratified + stratified analysis with default options for
#' # `unstrat_analysis` and `strat_analysis`.
#' t_binary_outcome(
#'   ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD,
#'   strata_data = ADRS[, c("STRATA1", "STRATA2")]
#' )
#'
#' # Stratified analysis only, using CMH weights and corresponding test statistic.
#' # Exclude Odds Ratio estimates.
#' t_binary_outcome(
#'   ADRS$AVALC == "CR",
#'   col_by = ADRS$ARMCD,
#'   strata_data = ADRS[, c("STRATA1", "STRATA2")],
#'   unstrat_analysis = NULL,
#'   strat_analysis = control_binary_comparison(diff_ci = "cmh", diff_test = "cmh", odds_ratio = FALSE)
#'  )
#'
#' # Exclude any comparisons between arms,
#' # specify non-default CI method for proportions and CI level,
#' # and include individual response categories.
#'
#' # First recode multinomial response with standard labels.
#' m_rsp <- ADRS$AVALC
#' levels(m_rsp) <- get_onco_rsp_label()[match(levels(m_rsp), names(get_onco_rsp_label()))]
#'
#' t_binary_outcome(
#'   ADRS$AVALC %in% c("CR", "PR"),
#'   col_by = ADRS$ARMCD,
#'   prop_ci_method = "clopper-pearson",
#'   unstrat_analysis = NULL,
#'   conf_level = 0.90,
#'   rsp_multinomial = m_rsp
#'  )
t_binary_outcome <- function(rsp,
                             col_by,
                             strata_data = NULL,
                             conf_level = 0.95,
                             prop_ci_method = "waldcc",
                             unstrat_analysis = control_binary_comparison(),
                             strat_analysis = control_binary_comparison(diff_test = "cmh"),
                             rsp_multinomial = NULL) {

  # Note: Argument checks are done inside each elementary function, so not checked here.

  if (is.null(rsp) & !is.null(rsp_multinomial)) {
    stop("For only the multinomial response summary use t_el_multinomial_proportion.")
  }

  # Table of responders + optional CI.
  tbl_response <- if (!is.null(prop_ci_method)) {
    t_el_proportion(rsp, col_by, conf_level = conf_level, prop_ci_method = prop_ci_method)
  } else {
    t_count_true(rsp, col_by, row_name = "Responders")
  }

  # Unstratified analysis table.
  tbl_u <- if (!is.null(unstrat_analysis)) {

    # Differences of proportions estimates and CIs.
    tbl_u_difference <- if (!is.null(unstrat_analysis$diff_ci)) {

      t_el_proportion_diff(
        rsp = rsp,
        col_by = col_by,
        strata_data = NULL,
        conf_level = conf_level,
        diff_ci_method = unstrat_analysis$diff_ci
      )[-1, ]  # Remove the "Responders" row.

    } else {
      NULL
    }

    # Test for different proportions.
    tbl_u_test <- if (!is.null(unstrat_analysis$diff_test)) {

      t_el_test_proportion_diff(
        rsp = rsp,
        col_by = col_by,
        strata_data = NULL,
        test = unstrat_analysis$diff_test
      )[-1, ]  # Remove the "Responders" row.

    } else {
      NULL
    }

    # Odds Ratio estimates and CIs.
    tbl_u_odds_ratio <- if (unstrat_analysis$odds_ratio) {

      t_el_odds_ratio(
        rsp = rsp,
        col_by = col_by,
        conf_level = conf_level,
        strata_data = NULL
      )[-1, ]  # Remove the "Responders" row.

    } else {
      NULL
    }

    insert_rrow(
      indent(
        rbind(
          tbl_u_difference,
          tbl_u_test,
          tbl_u_odds_ratio
        ),
      1),
      rrow("Unstratified Analysis")
    )

  } else {
    NULL
  }

  tbl_footer <- NULL

  # Stratified analysis table.
  tbl_s <- if (!is.null(strat_analysis) && !is.null(strata_data)) {

    # Differences of proportions estimates and CIs.
    tbl_s_difference <- if (!is.null(strat_analysis$diff_ci)) {

      # If not Cox-Mantel Haenszel method is chosen, then `strata_data`` is set to `NULL`` to fulfill
      # GDSR RSPT01 template criterion:
      # The difference in proportions section (any of the unstratified options) may be repeated between
      # stratified and unstratified analysis table sections.
      tmp_strata_data <- if (strat_analysis$diff_ci == "cmh") {
        strata_data
      } else {
        NULL
      }

      t_el_proportion_diff(
        rsp = rsp,
        col_by = col_by,
        strata_data = tmp_strata_data,
        conf_level = conf_level,
        diff_ci_method = strat_analysis$diff_ci
      )[-1, ]  # Remove the "Responders" row.

    } else {
      NULL
    }

    # Test for different proportions.
    tbl_s_test <- if (!is.null(strat_analysis$diff_test)) {

      t_el_test_proportion_diff(
        rsp = rsp,
        col_by = col_by,
        strata_data = strata_data,
        test = strat_analysis$diff_test
      )[-1, ]  # Remove the "Responders" row.

    } else {
      NULL
    }

    # Odds Ratio estimates and CIs.
    tbl_s_odds_ratio <- if (strat_analysis$odds_ratio) {

      t_el_odds_ratio(
        rsp = rsp,
        col_by = col_by,
        conf_level = conf_level,
        strata_data = strata_data
      )[-1, ]  # Remove the "Responders" row.

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
      indent(
        rbind(
          tbl_s_difference,
          tbl_s_test,
          tbl_s_odds_ratio),
        1
      ),
      rrow("Stratified Analysis*")
    )

  } else {
    NULL
  }

  tbl_multinomial <- if (!is.null(rsp_multinomial)) {

    levels_without_ci <- c("NE", "Missing", "NE/Missing", "Not Evaluable (NE)", "Missing or unevaluable")

    t_el_multinomial_proportion(
      rsp_multinomial,
      col_by,
      conf_level = conf_level,
      prop_ci_method = prop_ci_method,
      levels_without_ci = levels_without_ci
    )

  } else {
    NULL
  }

  tbl <- rbind(
    tbl_response,
    tbl_u,
    tbl_s,
    tbl_multinomial,
    tbl_footer,
    gap = 1
  )
  return(tbl)
}


#' Get label for standard oncology response categories
#'
#' This is a convenience function for labeling the partition section in `t_binary_outcome`.
#'
#' @return Named \code{character} vector.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' rsp <- c("CR", "NE", "PR")
#' # Recode values with standard labels.
#' rsp <- get_onco_rsp_label()[ match(rsp, names(get_onco_rsp_label()))  ]
#' # Create factor.
#' rsp <- factor(rsp, levels = get_onco_rsp_label())

get_onco_rsp_label <- function() {

  rsp_full_label <- c(
    CR          = "Complete Response (CR)",
    PR          = "Partial Response (PR)",
    SD          = "Stable Disease (SD)",
    `NON CR/PD` = "Non-CR or Non-PD (NON CR/PD)",
    PD          = "Progressive Disease (PD)",
    NE          = "Not Evaluable (NE)",
    Missing     = "Missing",
    `NE/Missing` = "Missing or unevaluable"
  )
  rsp_full_label
}
