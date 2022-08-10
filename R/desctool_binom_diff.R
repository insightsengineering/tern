#' Confidence Intervals for a Difference of Binomials
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Several confidence intervals for the difference between proportions.
#'
#' @param grp (`factor`)\cr
#'   vector assigning observations to one out of two groups
#'   (e.g. reference and treatment group).
#' @name desctool_binom

NULL

#' Recycle list of parameters
#'
#' @describeIn DescTools_Binom This function recycles all supplied elements to the maximal dimension.
#'
#' @keywords internal
h_recycle <- function(...) {
  lst <- list(...)
  maxdim <- max(lengths(lst))
  res <- lapply(lst, rep, length.out = maxdim)
  attr(res, "maxdim") <- maxdim
  return(res)
}

#' @describeIn DescTools_Binom Several Confidence Intervals for the difference between proportions.
#'
#'
#' @return A named list of 3 values:
#'   - `est`: estimate of proportion difference.
#'   - `lwrci`: estimate of lower end of the confidence interval
#'   - `upci`: estiamte of upper end of the confidence interval.
#' @examples
#' # Internal function -
#' \dontrun{
#'
#' set.seed(2)
#' rsp <- sample(c(TRUE, FALSE), replace = TRUE, size = 20)
#' grp <- factor(c(rep("A", 10), rep("B", 10)))
#' tbl <- table(grp, factor(rsp, levels = c(TRUE, FALSE)))
#' DescTools_Binom(tbl[1], sum(tbl[1], tbl[3]), tbl[2], sum(tbl[2], tbl[4]), conf.level = 0.90, method = "waldcc")
#' }
#'
#' @keywords internal
DescTools_Binom <- function(x1, n1, x2, n2, conf.level = 0.95, sides = c(
                              "two.sided",
                              "left", "right"
                            ), method = c(
                              "ac", "wald", "waldcc", "score",
                              "scorecc", "mn", "mee", "blj", "ha", "hal", "jp"
                            )) {
  if (missing(sides)) {
    sides <- match.arg(sides)
  }
  if (missing(method)) {
    method <- match.arg(method)
  }
  iBinomDiffCI <- function(x1, n1, x2, n2, conf.level, sides,
                           method) {
    if (sides != "two.sided") {
      conf.level <- 1 - 2 * (1 - conf.level)
    }
    alpha <- 1 - conf.level
    kappa <- stats::qnorm(1 - alpha / 2)
    p1.hat <- x1 / n1
    p2.hat <- x2 / n2
    est <- p1.hat - p2.hat
    switch(method,
      wald = {
        vd <- p1.hat * (1 - p1.hat) / n1 + p2.hat * (1 - p2.hat) / n2
        term2 <- kappa * sqrt(vd)
        CI.lower <- max(-1, est - term2)
        CI.upper <- min(1, est + term2)
      },
      waldcc = {
        vd <- p1.hat * (1 - p1.hat) / n1 + p2.hat * (1 - p2.hat) / n2
        term2 <- kappa * sqrt(vd)
        term2 <- term2 + 0.5 * (1 / n1 + 1 / n2)
        CI.lower <- max(-1, est - term2)
        CI.upper <- min(1, est + term2)
      },
      ac = {
        n1 <- n1 + 2
        n2 <- n2 + 2
        x1 <- x1 + 1
        x2 <- x2 + 1
        p1.hat <- x1 / n1
        p2.hat <- x2 / n2
        est1 <- p1.hat - p2.hat
        vd <- p1.hat * (1 - p1.hat) / n1 + p2.hat * (1 - p2.hat) / n2
        term2 <- kappa * sqrt(vd)
        CI.lower <- max(-1, est1 - term2)
        CI.upper <- min(1, est1 + term2)
      },
      exact = {
        CI.lower <- NA
        CI.upper <- NA
      },
      score = {
        w1 <- DescTools_BinomCI(
          x = x1, n = n1, conf.level = conf.level,
          method = "wilson"
        )
        w2 <- DescTools_BinomCI(
          x = x2, n = n2, conf.level = conf.level,
          method = "wilson"
        )
        l1 <- w1[2]
        u1 <- w1[3]
        l2 <- w2[2]
        u2 <- w2[3]
        CI.lower <- est - kappa * sqrt(l1 * (1 - l1) / n1 +
          u2 * (1 - u2) / n2)
        CI.upper <- est + kappa * sqrt(u1 * (1 - u1) / n1 +
          l2 * (1 - l2) / n2)
      },
      scorecc = {
        w1 <- DescTools_BinomCI(
          x = x1, n = n1, conf.level = conf.level,
          method = "wilsoncc"
        )
        w2 <- DescTools_BinomCI(
          x = x2, n = n2, conf.level = conf.level,
          method = "wilsoncc"
        )
        l1 <- w1[2]
        u1 <- w1[3]
        l2 <- w2[2]
        u2 <- w2[3]
        CI.lower <- max(-1, est - sqrt((p1.hat - l1)^2 +
          (u2 - p2.hat)^2))
        CI.upper <- min(1, est + sqrt((u1 - p1.hat)^2 + (p2.hat -
          l2)^2))
      },
      mee = {
        .score <- function(p1, n1, p2, n2, dif) {
          if (dif > 1) dif <- 1
          if (dif < -1) dif <- -1
          diff <- p1 - p2 - dif
          if (abs(diff) == 0) {
            res <- 0
          } else {
            t <- n2 / n1
            a <- 1 + t
            b <- -(1 + t + p1 + t * p2 + dif * (t + 2))
            c <- dif * dif + dif * (2 * p1 + t + 1) + p1 +
              t * p2
            d <- -p1 * dif * (1 + dif)
            v <- (b / a / 3)^3 - b * c / (6 * a * a) + d / a / 2
            if (abs(v) < .Machine$double.eps) v <- 0
            s <- sqrt((b / a / 3)^2 - c / a / 3)
            u <- ifelse(v > 0, 1, -1) * s
            w <- (3.141592654 + acos(v / u^3)) / 3
            p1d <- 2 * u * cos(w) - b / a / 3
            p2d <- p1d - dif
            n <- n1 + n2
            res <- (p1d * (1 - p1d) / n1 + p2d * (1 - p2d) / n2)
          }
          return(sqrt(res))
        }
        pval <- function(delta) {
          z <- (est - delta) / .score(
            p1.hat, n1, p2.hat,
            n2, delta
          )
          2 * min(stats::pnorm(z), 1 - stats::pnorm(z))
        }
        CI.lower <- max(-1, stats::uniroot(function(delta) {
          pval(delta) -
            alpha
        }, interval = c(-1 + 1e-06, est - 1e-06))$root)
        CI.upper <- min(1, stats::uniroot(function(delta) {
          pval(delta) -
            alpha
        }, interval = c(est + 1e-06, 1 - 1e-06))$root)
      },
      blj = {
        p1.dash <- (x1 + 0.5) / (n1 + 1)
        p2.dash <- (x2 + 0.5) / (n2 + 1)
        vd <- p1.dash * (1 - p1.dash) / n1 + p2.dash * (1 -
          p2.dash) / n2
        term2 <- kappa * sqrt(vd)
        est.dash <- p1.dash - p2.dash
        CI.lower <- max(-1, est.dash - term2)
        CI.upper <- min(1, est.dash + term2)
      },
      ha = {
        term2 <- 1 / (2 * min(n1, n2)) + kappa * sqrt(p1.hat *
          (1 - p1.hat) / (n1 - 1) + p2.hat * (1 - p2.hat) / (n2 -
            1))
        CI.lower <- max(-1, est - term2)
        CI.upper <- min(1, est + term2)
      },
      mn = {
        .conf <- function(x1, n1, x2, n2, z, lower = FALSE) {
          p1 <- x1 / n1
          p2 <- x2 / n2
          p.hat <- p1 - p2
          dp <- 1 + ifelse(lower, 1, -1) * p.hat
          i <- 1
          while (i <= 50) {
            dp <- 0.5 * dp
            y <- p.hat + ifelse(lower, -1, 1) * dp
            score <- .score(p1, n1, p2, n2, y)
            if (score < z) {
              p.hat <- y
            }
            if ((dp < 1e-07) || (abs(z - score) < 1e-06)) {
              (break)()
            } else {
              i <- i +
                1
            }
          }
          return(y)
        }
        .score <- function(p1, n1, p2, n2, dif) {
          diff <- p1 - p2 - dif
          if (abs(diff) == 0) {
            res <- 0
          } else {
            t <- n2 / n1
            a <- 1 + t
            b <- -(1 + t + p1 + t * p2 + dif * (t + 2))
            c <- dif * dif + dif * (2 * p1 + t + 1) + p1 +
              t * p2
            d <- -p1 * dif * (1 + dif)
            v <- (b / a / 3)^3 - b * c / (6 * a * a) + d / a / 2
            s <- sqrt((b / a / 3)^2 - c / a / 3)
            u <- ifelse(v > 0, 1, -1) * s
            w <- (3.141592654 + acos(v / u^3)) / 3
            p1d <- 2 * u * cos(w) - b / a / 3
            p2d <- p1d - dif
            n <- n1 + n2
            var <- (p1d * (1 - p1d) / n1 + p2d * (1 - p2d) / n2) *
              n / (n - 1)
            res <- diff^2 / var
          }
          return(res)
        }
        z <- stats::qchisq(conf.level, 1)
        CI.lower <- max(-1, .conf(x1, n1, x2, n2, z, TRUE))
        CI.upper <- min(1, .conf(x1, n1, x2, n2, z, FALSE))
      },
      beal = {
        a <- p1.hat + p2.hat
        b <- p1.hat - p2.hat
        u <- ((1 / n1) + (1 / n2)) / 4
        v <- ((1 / n1) - (1 / n2)) / 4
        V <- u * ((2 - a) * a - b^2) + 2 * v * (1 - a) *
          b
        z <- stats::qchisq(p = 1 - alpha / 2, df = 1)
        A <- sqrt(z * (V + z * u^2 * (2 - a) * a + z * v^2 *
          (1 - a)^2))
        B <- (b + z * v * (1 - a)) / (1 + z * u)
        CI.lower <- max(-1, B - A / (1 + z * u))
        CI.upper <- min(1, B + A / (1 + z * u))
      },
      hal = {
        psi <- (p1.hat + p2.hat) / 2
        u <- (1 / n1 + 1 / n2) / 4
        v <- (1 / n1 - 1 / n2) / 4
        z <- kappa
        theta <- ((p1.hat - p2.hat) + z^2 * v * (1 - 2 *
          psi)) / (1 + z^2 * u)
        w <- z / (1 + z^2 * u) * sqrt(u * (4 * psi * (1 - psi) -
          (p1.hat - p2.hat)^2) + 2 * v * (1 - 2 * psi) *
          (p1.hat - p2.hat) + 4 * z^2 * u^2 * (1 - psi) *
          psi + z^2 * v^2 * (1 - 2 * psi)^2)
        c(theta + w, theta - w)
        CI.lower <- max(-1, theta - w)
        CI.upper <- min(1, theta + w)
      },
      jp = {
        psi <- 0.5 * ((x1 + 0.5) / (n1 + 1) + (x2 + 0.5) / (n2 +
          1))
        u <- (1 / n1 + 1 / n2) / 4
        v <- (1 / n1 - 1 / n2) / 4
        z <- kappa
        theta <- ((p1.hat - p2.hat) + z^2 * v * (1 - 2 *
          psi)) / (1 + z^2 * u)
        w <- z / (1 + z^2 * u) * sqrt(u * (4 * psi * (1 - psi) -
          (p1.hat - p2.hat)^2) + 2 * v * (1 - 2 * psi) *
          (p1.hat - p2.hat) + 4 * z^2 * u^2 * (1 - psi) *
          psi + z^2 * v^2 * (1 - 2 * psi)^2)
        c(theta + w, theta - w)
        CI.lower <- max(-1, theta - w)
        CI.upper <- min(1, theta + w)
      },
    )
    ci <- c(
      est = est, lwr.ci = min(CI.lower, CI.upper),
      upr.ci = max(CI.lower, CI.upper)
    )
    if (sides == "left") {
      ci[3] <- 1
    } else if (sides == "right") {
      ci[2] <- -1
    }
    return(ci)
  }
  method <- match.arg(arg = method, several.ok = TRUE)
  sides <- match.arg(arg = sides, several.ok = TRUE)
  lst <- h_recycle(
    x1 = x1, n1 = n1, x2 = x2, n2 = n2, conf.level = conf.level,
    sides = sides, method = method
  )
  res <- t(sapply(1:attr(lst, "maxdim"), function(i) {
    iBinomDiffCI(
      x1 = lst$x1[i],
      n1 = lst$n1[i], x2 = lst$x2[i], n2 = lst$n2[i], conf.level = lst$conf.level[i],
      sides = lst$sides[i], method = lst$method[i]
    )
  }))
  lgn <- h_recycle(x1 = if (is.null(names(x1))) {
    paste("x1", seq_along(x1), sep = ".")
  } else {
    names(x1)
  }, n1 = if (is.null(names(n1))) {
    paste("n1", seq_along(n1), sep = ".")
  } else {
    names(n1)
  }, x2 = if (is.null(names(x2))) {
    paste("x2", seq_along(x2), sep = ".")
  } else {
    names(x2)
  }, n2 = if (is.null(names(n2))) {
    paste("n2", seq_along(n2), sep = ".")
  } else {
    names(n2)
  }, conf.level = conf.level, sides = sides, method = method)
  xn <- apply(as.data.frame(lgn[sapply(lgn, function(x) {
    length(unique(x)) !=
      1
  })]), 1, paste, collapse = ":")
  rownames(res) <- xn
  return(res)
}

#' @describeIn DescTools_Binom Compute confidence intervals for binomial proportions.
#'
#' @param x \cr number of successes
#' @param n \cr number of trials
#' @param conf.level \cr confidence level, defaults to 0.95
#' @param sides \cr a character string specifying the side of the confidence interval. Must be one of "two-sided" (default), "left" or "right".
#' @param method \cr character string specifying which method to use. Can be one out of: "wald", "wilson", "wilsoncc", "agresti-coull", "jeffreys", "modified wilson", "modified jeffreys", "clopper-pearson", "arcsine.
#' ", "logit", "witting", "pratt", "midp", "lik" and "blaker"
#'
#'
#'  @return A matric with 3 columns containing:
#'   - `est`: estimate of proportion difference.
#'   - `lwrci`: lower end of the confidence interval
#'   - `upci`:  upper end of the confidence interval.
#'
#' @keywords internal
DescTools_BinomCI <- function(x, n, conf.level = 0.95, sides = c(
                                "two.sided", "left",
                                "right"
                              ), method = c(
                                "wilson", "wald", "waldcc", "agresti-coull",
                                "jeffreys", "modified wilson", "wilsoncc", "modified jeffreys",
                                "clopper-pearson", "arcsine", "logit", "witting", "pratt",
                                "midp", "lik", "blaker"
                              ), rand = 123, tol = 1e-05) {
  if (missing(method)) {
    method <- "wilson"
  }
  if (missing(sides)) {
    sides <- "two.sided"
  }
  iBinomCI <- function(x, n, conf.level = 0.95, sides = c(
                         "two.sided",
                         "left", "right"
                       ), method = c(
                         "wilson", "wilsoncc", "wald",
                         "waldcc", "agresti-coull", "jeffreys", "modified wilson",
                         "modified jeffreys", "clopper-pearson", "arcsine", "logit",
                         "witting", "pratt", "midp", "lik", "blaker"
                       ), rand = 123,
                       tol = 1e-05) {
    if (length(x) != 1) {
      stop("'x' has to be of length 1 (number of successes)")
    }
    if (length(n) != 1) {
      stop("'n' has to be of length 1 (number of trials)")
    }
    if (length(conf.level) != 1) {
      stop("'conf.level' has to be of length 1 (confidence level)")
    }
    if (conf.level < 0.5 | conf.level > 1) {
      stop("'conf.level' has to be in [0.5, 1]")
    }
    sides <- match.arg(sides, choices = c(
      "two.sided", "left",
      "right"
    ), several.ok = FALSE)
    if (sides != "two.sided") {
      conf.level <- 1 - 2 * (1 - conf.level)
    }
    alpha <- 1 - conf.level
    kappa <- stats::qnorm(1 - alpha / 2)
    p.hat <- x / n
    q.hat <- 1 - p.hat
    est <- p.hat
    switch(match.arg(arg = method, choices = c(
      "wilson",
      "wald", "waldcc", "wilsoncc", "agresti-coull", "jeffreys",
      "modified wilson", "modified jeffreys", "clopper-pearson",
      "arcsine", "logit", "witting", "pratt", "midp", "lik",
      "blaker"
    )),
    wald = {
      term2 <- kappa * sqrt(p.hat * q.hat) / sqrt(n)
      CI.lower <- max(0, p.hat - term2)
      CI.upper <- min(1, p.hat + term2)
    },
    waldcc = {
      term2 <- kappa * sqrt(p.hat * q.hat) / sqrt(n)
      term2 <- term2 + 1 / (2 * n)
      CI.lower <- max(0, p.hat - term2)
      CI.upper <- min(1, p.hat + term2)
    },
    wilson = {
      term1 <- (x + kappa^2 / 2) / (n + kappa^2)
      term2 <- kappa * sqrt(n) / (n + kappa^2) * sqrt(p.hat *
        q.hat + kappa^2 / (4 * n))
      CI.lower <- max(0, term1 - term2)
      CI.upper <- min(1, term1 + term2)
    },
    wilsoncc = {
      lci <- (2 * x + kappa^2 - 1 - kappa * sqrt(kappa^2 -
        2 - 1 / n + 4 * p.hat * (n * q.hat + 1))) / (2 *
        (n + kappa^2))
      uci <- (2 * x + kappa^2 + 1 + kappa * sqrt(kappa^2 +
        2 - 1 / n + 4 * p.hat * (n * q.hat - 1))) / (2 *
        (n + kappa^2))
      CI.lower <- max(0, ifelse(p.hat == 0, 0, lci))
      CI.upper <- min(1, ifelse(p.hat == 1, 1, uci))
    },
    `agresti-coull` = {
      x.tilde <- x + kappa^2 / 2
      n.tilde <- n + kappa^2
      p.tilde <- x.tilde / n.tilde
      q.tilde <- 1 - p.tilde
      est <- p.tilde
      term2 <- kappa * sqrt(p.tilde * q.tilde) / sqrt(n.tilde)
      CI.lower <- max(0, p.tilde - term2)
      CI.upper <- min(1, p.tilde + term2)
    },
    jeffreys = {
      if (x == 0) {
        CI.lower <- 0
      } else {
        CI.lower <- stats::qbeta(
          alpha / 2,
          x + 0.5, n - x + 0.5
        )
      }
      if (x == n) {
        CI.upper <- 1
      } else {
        CI.upper <- stats::qbeta(1 -
          alpha / 2, x + 0.5, n - x + 0.5)
      }
    },
    `modified wilson` = {
      term1 <- (x + kappa^2 / 2) / (n + kappa^2)
      term2 <- kappa * sqrt(n) / (n + kappa^2) * sqrt(p.hat *
        q.hat + kappa^2 / (4 * n))
      if ((n <= 50 & x %in% c(1, 2)) | (n >= 51 & x %in%
        c(1:3))) {
        CI.lower <- 0.5 * stats::qchisq(alpha, 2 *
          x) / n
      } else {
        CI.lower <- max(0, term1 - term2)
      }
      if ((n <= 50 & x %in% c(n - 1, n - 2)) | (n >= 51 &
        x %in% c(n - (1:3)))) {
        CI.upper <- 1 - 0.5 * stats::qchisq(
          alpha,
          2 * (n - x)
        ) / n
      } else {
        CI.upper <- min(1, term1 +
          term2)
      }
    },
    `modified jeffreys` = {
      if (x == n) {
        CI.lower <- (alpha / 2)^(1 / n)
      } else {
        if (x <= 1) {
          CI.lower <- 0
        } else {
          CI.lower <- stats::qbeta(
            alpha / 2,
            x + 0.5, n - x + 0.5
          )
        }
      }
      if (x == 0) {
        CI.upper <- 1 - (alpha / 2)^(1 / n)
      } else {
        if (x >= n - 1) {
          CI.upper <- 1
        } else {
          CI.upper <- stats::qbeta(1 -
            alpha / 2, x + 0.5, n - x + 0.5)
        }
      }
    },
    `clopper-pearson` = {
      CI.lower <- stats::qbeta(alpha / 2, x, n - x + 1)
      CI.upper <- stats::qbeta(1 - alpha / 2, x + 1, n - x)
    },
    arcsine = {
      p.tilde <- (x + 0.375) / (n + 0.75)
      est <- p.tilde
      CI.lower <- sin(asin(sqrt(p.tilde)) - 0.5 * kappa / sqrt(n))^2
      CI.upper <- sin(asin(sqrt(p.tilde)) + 0.5 * kappa / sqrt(n))^2
    },
    logit = {
      lambda.hat <- log(x / (n - x))
      V.hat <- n / (x * (n - x))
      lambda.lower <- lambda.hat - kappa * sqrt(V.hat)
      lambda.upper <- lambda.hat + kappa * sqrt(V.hat)
      CI.lower <- exp(lambda.lower) / (1 + exp(lambda.lower))
      CI.upper <- exp(lambda.upper) / (1 + exp(lambda.upper))
    },
    witting = {
      set.seed(rand)
      x.tilde <- x + stats::runif(1, min = 0, max = 1)
      pbinom.abscont <- function(q, size, prob) {
        v <- trunc(q)
        term1 <- stats::pbinom(v - 1, size = size, prob = prob)
        term2 <- (q - v) * stats::dbinom(v, size = size, prob = prob)
        return(term1 + term2)
      }
      qbinom.abscont <- function(p, size, x) {
        fun <- function(prob, size, x, p) {
          pbinom.abscont(x, size, prob) - p
        }
        stats::uniroot(fun,
          interval = c(0, 1), size = size,
          x = x, p = p
        )$root
      }
      CI.lower <- qbinom.abscont(1 - alpha, size = n, x = x.tilde)
      CI.upper <- qbinom.abscont(alpha, size = n, x = x.tilde)
    },
    pratt = {
      if (x == 0) {
        CI.lower <- 0
        CI.upper <- 1 - alpha^(1 / n)
      } else if (x == 1) {
        CI.lower <- 1 - (1 - alpha / 2)^(1 / n)
        CI.upper <- 1 - (alpha / 2)^(1 / n)
      } else if (x == (n - 1)) {
        CI.lower <- (alpha / 2)^(1 / n)
        CI.upper <- (1 - alpha / 2)^(1 / n)
      } else if (x == n) {
        CI.lower <- alpha^(1 / n)
        CI.upper <- 1
      } else {
        z <- stats::qnorm(1 - alpha / 2)
        A <- ((x + 1) / (n - x))^2
        B <- 81 * (x + 1) * (n - x) - 9 * n - 8
        C <- (0 - 3) * z * sqrt(9 * (x + 1) * (n - x) *
          (9 * n + 5 - z^2) + n + 1)
        D <- 81 * (x + 1)^2 - 9 * (x + 1) * (2 + z^2) +
          1
        E <- 1 + A * ((B + C) / D)^3
        CI.upper <- 1 / E
        A <- (x / (n - x - 1))^2
        B <- 81 * x * (n - x - 1) - 9 * n - 8
        C <- 3 * z * sqrt(9 * x * (n - x - 1) * (9 *
          n + 5 - z^2) + n + 1)
        D <- 81 * x^2 - 9 * x * (2 + z^2) + 1
        E <- 1 + A * ((B + C) / D)^3
        CI.lower <- 1 / E
      }
    },
    midp = {
      f.low <- function(pi, x, n) {
        1 / 2 * stats::dbinom(x, size = n, prob = pi) + stats::pbinom(x,
          size = n, prob = pi, lower.tail = FALSE
        ) -
          (1 - conf.level) / 2
      }
      f.up <- function(pi, x, n) {
        1 / 2 * stats::dbinom(x, size = n, prob = pi) + stats::pbinom(x -
          1, size = n, prob = pi) - (1 - conf.level) / 2
      }
      CI.lower <- 0
      CI.upper <- 1
      if (x != 0) {
        CI.lower <- stats::uniroot(f.low,
          interval = c(0, p.hat),
          x = x, n = n
        )$root
      }
      if (x != n) {
        CI.upper <- stats::uniroot(f.up, interval = c(
          p.hat,
          1
        ), x = x, n = n)$root
      }
    },
    lik = {
      CI.lower <- 0
      CI.upper <- 1
      z <- stats::qnorm(1 - alpha * 0.5)
      tol <- .Machine$double.eps^0.5
      BinDev <- function(y, x, mu, wt, bound = 0, tol = .Machine$double.eps^0.5,
                         ...) {
        ll.y <- ifelse(y %in% c(0, 1), 0, stats::dbinom(x, wt,
          y,
          log = TRUE
        ))
        ll.mu <- ifelse(mu %in% c(0, 1), 0, stats::dbinom(x,
          wt, mu,
          log = TRUE
        ))
        res <- ifelse(abs(y - mu) < tol, 0, sign(y -
          mu) * sqrt(-2 * (ll.y - ll.mu)))
        return(res - bound)
      }
      if (x != 0 && tol < p.hat) {
        CI.lower <- if (BinDev(
          tol, x, p.hat, n, -z,
          tol
        ) <= 0) {
          stats::uniroot(
            f = BinDev, interval = c(tol, if (p.hat <
              tol || p.hat == 1) {
              1 - tol
            } else {
              p.hat
            }), bound = -z,
            x = x, mu = p.hat, wt = n
          )$root
        }
      }
      if (x != n && p.hat < (1 - tol)) {
        CI.upper <- if (BinDev(y = 1 - tol, x = x, mu = ifelse(p.hat >
          1 - tol, tol, p.hat), wt = n, bound = z, tol = tol) <
          0) {
          CI.lower <- if (BinDev(
            tol, x, if (p.hat <
              tol || p.hat == 1) {
              1 - tol
            } else {
              p.hat
            }, n,
            -z, tol
          ) <= 0) {
            stats::uniroot(
              f = BinDev, interval = c(tol, p.hat),
              bound = -z, x = x, mu = p.hat, wt = n
            )$root
          }
        } else {
          stats::uniroot(
            f = BinDev, interval = c(if (p.hat >
              1 - tol) {
              tol
            } else {
              p.hat
            }, 1 - tol), bound = z,
            x = x, mu = p.hat, wt = n
          )$root
        }
      }
    },
    blaker = {
      acceptbin <- function(x, n, p) {
        p1 <- 1 - stats::pbinom(x - 1, n, p)
        p2 <- stats::pbinom(x, n, p)
        a1 <- p1 + stats::pbinom(stats::qbinom(p1, n, p) - 1, n, p)
        a2 <- p2 + 1 - stats::pbinom(
          stats::qbinom(1 - p2, n, p), n,
          p
        )
        return(min(a1, a2))
      }
      CI.lower <- 0
      CI.upper <- 1
      if (x != 0) {
        CI.lower <- stats::qbeta((1 - conf.level) / 2, x, n -
          x + 1)
        while (acceptbin(x, n, CI.lower + tol) < (1 -
          conf.level)) {
          CI.lower <- CI.lower + tol
        }
      }
      if (x != n) {
        CI.upper <- stats::qbeta(1 - (1 - conf.level) / 2, x +
          1, n - x)
        while (acceptbin(x, n, CI.upper - tol) < (1 -
          conf.level)) {
          CI.upper <- CI.upper - tol
        }
      }
    }
    )
    ci <- c(est = est, lwr.ci = max(0, CI.lower), upr.ci = min(
      1,
      CI.upper
    ))
    if (sides == "left") {
      ci[3] <- 1
    } else if (sides == "right") {
      ci[2] <- 0
    }
    return(ci)
  }
  lst <- list(
    x = x, n = n, conf.level = conf.level, sides = sides,
    method = method, rand = rand
  )
  maxdim <- max(unlist(lapply(lst, length)))
  lgp <- lapply(lst, rep, length.out = maxdim)
  lgn <- h_recycle(x = if (is.null(names(x))) {
    paste("x", seq_along(x), sep = ".")
  } else {
    names(x)
  }, n = if (is.null(names(n))) {
    paste("n", seq_along(n), sep = ".")
  } else {
    names(n)
  }, conf.level = conf.level, sides = sides, method = method)
  xn <- apply(as.data.frame(lgn[sapply(lgn, function(x) {
    length(unique(x)) !=
      1
  })]), 1, paste, collapse = ":")
  res <- t(sapply(1:maxdim, function(i) {
    iBinomCI(
      x = lgp$x[i],
      n = lgp$n[i], conf.level = lgp$conf.level[i], sides = lgp$sides[i],
      method = lgp$method[i], rand = lgp$rand[i]
    )
  }))
  colnames(res)[1] <- c("est")
  rownames(res) <- xn
  return(res)
}
