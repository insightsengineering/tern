#' Helper Functions for Calculating Proportion Confidence Intervals
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Functions to calculate different proportion confidence intervals for use in [estimate_proportion()].
#'
#' @inheritParams estimate_proportions
#'
#' @name h_prop
NULL


#' @describeIn h_prop the Wilson interval calls [stats::prop.test()].
#'  Also referred to as Wilson score interval.
#'
#' @examples
#' rsp <- c(
#'   TRUE, TRUE, TRUE, TRUE, TRUE,
#'   FALSE, FALSE, FALSE, FALSE, FALSE
#' )
#' prop_wilson(rsp, conf_level = 0.9)
#'
#' @export
prop_wilson <- function(rsp, conf_level, correct = FALSE) {
  y <- stats::prop.test(
    sum(rsp),
    length(rsp),
    correct = correct,
    conf.level = conf_level
  )

  as.numeric(y$conf.int)
}


#' @describeIn h_prop the Clopper-Pearson interval calls
#'   [stats::binom.test()]. Also referred to as the `exact` method.
#'
#' @inheritParams argument_convention
#'
#' @examples
#' prop_clopper_pearson(rsp, conf_level = .95)
#'
#' @export
prop_clopper_pearson <- function(rsp,
                                 conf_level) {
  y <- stats::binom.test(
    x = sum(rsp),
    n = length(rsp),
    conf.level = conf_level
  )
  as.numeric(y$conf.int)
}

#' @describeIn h_prop the Wald interval follows the usual
#'   textbook definition for a single proportion confidence interval using the
#'   normal approximation.
#'
#' @inheritParams argument_convention
#' @param correct (`flag`)\cr apply continuity correction.
#'
#' @examples
#' prop_wald(rsp, conf_level = 0.95)
#' prop_wald(rsp, conf_level = 0.95, correct = TRUE)
#'
#' @export
prop_wald <- function(rsp, conf_level, correct = FALSE) {
  n <- length(rsp)
  p_hat <- mean(rsp)
  z <- stats::qnorm((1 + conf_level) / 2)
  q_hat <- 1 - p_hat
  correct <- if (correct) 1 / (2 * n) else 0

  err <- z * sqrt(p_hat * q_hat) / sqrt(n) + correct
  l_ci <- max(0, p_hat - err)
  u_ci <- min(1, p_hat + err)

  c(l_ci, u_ci)
}


#' @describeIn h_prop the Agresti-Coull interval was created by
#'   Alan Agresti and Brent Coull and can be understood (for 95% CI) as adding
#'   two successes and two failures to the data, and then using the Wald
#'   formula to construct a CI.
#'
#' @inheritParams argument_convention
#'
#' @examples
#' prop_agresti_coull(rsp, conf_level = 0.95)
#'
#' @export
prop_agresti_coull <- function(rsp, conf_level) {
  n <- length(rsp)
  x_sum <- sum(rsp)
  z <- stats::qnorm((1 + conf_level) / 2)

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
}


#' @describeIn h_prop the Jeffreys interval is an equal-tailed
#'   interval based on the non-informative Jeffreys prior for a binomial
#'   proportion.
#'
#' @inheritParams argument_convention
#'
#' @examples
#' prop_jeffreys(rsp, conf_level = 0.95)
#'
#' @export
prop_jeffreys <- function(rsp,
                          conf_level) {
  n <- length(rsp)
  x_sum <- sum(rsp)

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
