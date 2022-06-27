#' Helper Functions to Calculate Proportion Difference
#'
#' @inheritParams argument_convention
#' @inheritParams prop_diff
#' @param grp (`factor`)\cr
#'   vector assigning observations to one out of two groups
#'   (e.g. reference and treatment group).
#'
#' @name h_prop_diff
NULL

#' @describeIn h_prop_diff The Wald interval follows the usual textbook
#'   definition for a single proportion confidence interval using the normal
#'   approximation. It is possible to include a continuity correction for Wald's
#'   interval.
#'
#' @param correct `logical`\cr
#'   include the continuity correction.
#'
#' @examples
#' # Wald confidence interval
#' set.seed(2)
#' rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)
#' grp <- c(rep("A", 10), rep("B", 10))
#' tern:::prop_diff_wald(rsp = rsp, grp = grp, conf_level = 0.90, correct = FALSE)
#'
#' @keywords internal
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


#' @describeIn h_prop_diff Anderson-Hauck confidence interval.
#'
#' @examples
#' # Anderson-Hauck confidence interval
#' ## "Mid" case: 3/4 respond in group A, 1/2 respond in group B.
#' rsp <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
#' grp <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))
#' tern:::prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.90)
#'
#' ## Edge case: Same proportion of response in A and B.
#' rsp <- c(TRUE, FALSE, TRUE, FALSE)
#' grp <- factor(c("A", "A", "B", "B"), levels = c("A", "B"))
#' tern:::prop_diff_ha(rsp = rsp, grp = grp, conf_level = 0.6)
#'
#' @keywords internal
prop_diff_ha <- function(rsp,
                         grp,
                         conf_level) {
  grp <- as_factor_keep_attributes(grp)
  check_diff_prop_ci(rsp = rsp, grp = grp, conf_level = conf_level)

  n_grp <- tapply(rsp, grp, length)
  p_grp <- tapply(rsp, grp, mean)
  diff_p <- unname(diff(p_grp))
  z <- stats::qnorm((1 + conf_level) / 2)
  err <- 1 /
    (2 * min(n_grp)) + z * sqrt(sum(p_grp * (1 - p_grp) / (n_grp - 1)))
  l_ci <- max(-1, diff_p - err)
  u_ci <- min(1, diff_p + err)
  list(
    "diff" = diff_p,
    "diff_ci" = c(l_ci, u_ci)
  )
}


#' @describeIn h_prop_diff Newcombe confidence interval. It is based on
#'   the Wilson score confidence interval for a single binomial proportion.
#'
#' @examples
#' # Newcombe confidence interval
#'
#' set.seed(1)
#' rsp <- c(
#'   sample(c(TRUE, FALSE), size = 40, prob = c(3 / 4, 1 / 4), replace = TRUE),
#'   sample(c(TRUE, FALSE), size = 40, prob = c(1 / 2, 1 / 2), replace = TRUE)
#' )
#' grp <- factor(rep(c("A", "B"), each = 40), levels = c("B", "A"))
#' table(rsp, grp)
#' tern:::prop_diff_nc(rsp = rsp, grp = grp, conf_level = 0.9)
#'
#' @keywords internal
prop_diff_nc <- function(rsp,
                         grp,
                         conf_level,
                         correct = FALSE) {
  grp <- as_factor_keep_attributes(grp)
  check_diff_prop_ci(rsp = rsp, grp = grp, conf_level = conf_level)

  # Source:
  # https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf
  p_grp <- tapply(rsp, grp, mean)
  diff_p <- unname(diff(p_grp))
  x_grp <- split(rsp, f = grp)
  ci_grp <- lapply(x_grp, FUN = prop_wilson, correct = correct, conf_level = conf_level)
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


#' @describeIn h_prop_diff Calculates the weighted difference.
#'     This is defined as the difference in response rates between the
#'     experimental treatment group and the control treatment group, adjusted
#'     for stratification factors by applying Cochran-Mantel-Haenszel (CMH)
#'     weights. For the CMH chi-squared test, use [stats::mantelhaen.test()].
#'
#' @param strata (`factor`)\cr
#'   with one level per stratum and same length as `rsp`.
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
#' tern:::prop_diff_cmh(
#'   rsp = rsp, grp = grp, strata = interaction(strata_data),
#'   conf_level = 0.90
#' )
#'
#' @keywords internal
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
  z <- stats::qnorm((1 + conf_level) / 2)
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
