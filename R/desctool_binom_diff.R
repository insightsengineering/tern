#' Confidence Intervals for a Difference of Binomials
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Several confidence intervals for the difference between proportions.
#'
#' @inheritParams argument_convention
#' @param grp (`factor`)\cr
#'   vector assigning observations to one out of two groups
#'   (e.g. reference and treatment group).
#' @name desctool_binom

NULL

#' Recycle list of parameters
#'
#' This function recycles all supplied elments to the maximal dimension.
#' Author: Andri Signorell
#'
#' @keywords internal
h_recycle <- function (...)
{
  lst <- list(...)
  maxdim <- max(lengths(lst))
  res <- lapply(lst, rep, length.out = maxdim)
  attr(res, "maxdim") <- maxdim
  return(res)
}

#' @describeIn prop_diff The Wald interval follows the usual textbook
#'   definition for a single proportion confidence interval using the normal
#'   approximation. It is possible to include a continuity correction for Wald's
#'   interval.
#'
#' @param correct `logical`\cr
#'   include the continuity correction.
#'
#' @examples
#' # Internal function - s_count_abnormal
#' \dontrun{
#' # Wald confidence interval
#' set.seed(2)
#' rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)
#' grp <- factor(c(rep("A", 10), rep("B", 10)))
#' tbl <- table(grp, factor(rsp, levels = c(TRUE, FALSE)))
#' DescTools_Binom_Wald(tbl[1], sum(tbl[1], tbl[3]), tbl[2], sum(tbl[2], tbl[4]), conf.level = 0.90, method = "waldcc")
#'}
#'
#' @keywords internal
DescTools_Binom_Wald <- function (x1, n1, x2, n2, conf.level = conf_level, method = method)
{
  iBinomDiffCI <- function(x1, n1, x2, n2, conf.level, sides,
                           method) {
    conf.level <- 1 - 2 * (1 - conf.level)
    alpha <- 1 - conf.level
    kappa <- qnorm(1 - alpha/2)
    p1.hat <- x1/n1
    p2.hat <- x2/n2
    est <- p1.hat - p2.hat
    switch(method, wald = {
      vd <- p1.hat * (1 - p1.hat)/n1 + p2.hat * (1 - p2.hat)/n2
      term2 <- kappa * sqrt(vd)
      CI.lower <- max(-1, est - term2)
      CI.upper <- min(1, est + term2)
    }, waldcc = {
      vd <- p1.hat * (1 - p1.hat)/n1 + p2.hat * (1 - p2.hat)/n2
      term2 <- kappa * sqrt(vd)
      term2 <- term2 + 0.5 * (1/n1 + 1/n2)
      CI.lower <- max(-1, est - term2)
      CI.upper <- min(1, est + term2)
    })
    ci <- c(est = est, lwr.ci = min(CI.lower, CI.upper),
            upr.ci = max(CI.lower, CI.upper))
    return(ci)
  }
  method <- match.arg(arg = method, several.ok = TRUE)
  lst <- h_recycle(x1 = x1, n1 = n1, x2 = x2, n2 = n2, conf.level = conf.level,
                 sides = "two.sided", method = method)
  res <- t(sapply(1:attr(lst, "maxdim"), function(i) iBinomDiffCI(x1 = lst$x1[i],
                                                                  n1 = lst$n1[i], x2 = lst$x2[i], n2 = lst$n2[i], conf.level = lst$conf.level[i],
                                                                  sides = lst$sides[i], method = lst$method[i])))
  lgn <- h_recycle(x1 = if (is.null(names(x1)))
    paste("x1", seq_along(x1), sep = ".")
    else names(x1), n1 = if (is.null(names(n1)))
      paste("n1", seq_along(n1), sep = ".")
    else names(n1), x2 = if (is.null(names(x2)))
      paste("x2", seq_along(x2), sep = ".")
    else names(x2), n2 = if (is.null(names(n2)))
      paste("n2", seq_along(n2), sep = ".")
    else names(n2), conf.level = conf.level, sides = "two.sided", method = method)
  xn <- apply(as.data.frame(lgn[sapply(lgn, function(x) length(unique(x)) !=
                                         1)]), 1, paste, collapse = ":")
  rownames(res) <- xn
  return(res)
}


