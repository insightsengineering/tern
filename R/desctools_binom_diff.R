#' Confidence Intervals for a Difference of Binomials
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Several confidence intervals for the difference between proportions.
#'
#' @name desctools_binom
NULL

#' Recycle List of Parameters
#'
#' This function recycles all supplied elements to the maximal dimension.
#'
#' @param ... (`any`)\cr Elements to recycle.
#'
#' @return A `list`.
#'
#' @keywords internal
#' @noRd
h_recycle <- function(...) {
  lst <- list(...)
  maxdim <- max(lengths(lst))
  res <- lapply(lst, rep, length.out = maxdim)
  attr(res, "maxdim") <- maxdim
  return(res)
}

#' @describeIn desctools_binom Several confidence intervals for the difference between proportions.
#'
#' @return A `matrix` of 3 values:
#'   * `est`: estimate of proportion difference.
#'   * `lwr.ci`: estimate of lower end of the confidence interval.
#'   * `upr.ci`: estimate of upper end of the confidence interval.
#'
#' @keywords internal
desctools_binom <- function(x1,
                            n1,
                            x2,
                            n2,
                            conf.level = 0.95, # nolint
                            sides = c("two.sided", "left", "right"),
                            method = c(
                              "ac", "wald", "waldcc", "score", "scorecc", "mn", "mee", "blj", "ha", "hal", "jp"
                            )) {
  if (missing(sides)) {
    sides <- match.arg(sides)
  }
  if (missing(method)) {
    method <- match.arg(method)
  }
  iBinomDiffCI <- function(x1, n1, x2, n2, conf.level, sides, method) { # nolint
    if (sides != "two.sided") {
      conf.level <- 1 - 2 * (1 - conf.level) # nolint
    }
    alpha <- 1 - conf.level
    kappa <- stats::qnorm(1 - alpha / 2)
    p1_hat <- x1 / n1
    p2_hat <- x2 / n2
    est <- p1_hat - p2_hat
    switch(method,
      wald = {
        vd <- p1_hat * (1 - p1_hat) / n1 + p2_hat * (1 - p2_hat) / n2
        term2 <- kappa * sqrt(vd)
        ci_lwr <- max(-1, est - term2)
        ci_upr <- min(1, est + term2)
      },
      waldcc = {
        vd <- p1_hat * (1 - p1_hat) / n1 + p2_hat * (1 - p2_hat) / n2
        term2 <- kappa * sqrt(vd)
        term2 <- term2 + 0.5 * (1 / n1 + 1 / n2)
        ci_lwr <- max(-1, est - term2)
        ci_upr <- min(1, est + term2)
      },
      ac = {
        n1 <- n1 + 2
        n2 <- n2 + 2
        x1 <- x1 + 1
        x2 <- x2 + 1
        p1_hat <- x1 / n1
        p2_hat <- x2 / n2
        est1 <- p1_hat - p2_hat
        vd <- p1_hat * (1 - p1_hat) / n1 + p2_hat * (1 - p2_hat) / n2
        term2 <- kappa * sqrt(vd)
        ci_lwr <- max(-1, est1 - term2)
        ci_upr <- min(1, est1 + term2)
      },
      exact = {
        ci_lwr <- NA
        ci_upr <- NA
      },
      score = {
        w1 <- desctools_binomci(
          x = x1, n = n1, conf.level = conf.level,
          method = "wilson"
        )
        w2 <- desctools_binomci(
          x = x2, n = n2, conf.level = conf.level,
          method = "wilson"
        )
        l1 <- w1[2]
        u1 <- w1[3]
        l2 <- w2[2]
        u2 <- w2[3]
        ci_lwr <- est - kappa * sqrt(l1 * (1 - l1) / n1 + u2 * (1 - u2) / n2)
        ci_upr <- est + kappa * sqrt(u1 * (1 - u1) / n1 + l2 * (1 - l2) / n2)
      },
      scorecc = {
        w1 <- desctools_binomci(
          x = x1, n = n1, conf.level = conf.level,
          method = "wilsoncc"
        )
        w2 <- desctools_binomci(
          x = x2, n = n2, conf.level = conf.level,
          method = "wilsoncc"
        )
        l1 <- w1[2]
        u1 <- w1[3]
        l2 <- w2[2]
        u2 <- w2[3]
        ci_lwr <- max(-1, est - sqrt((p1_hat - l1)^2 + (u2 - p2_hat)^2))
        ci_upr <- min(1, est + sqrt((u1 - p1_hat)^2 + (p2_hat - l2)^2))
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
            c <- dif * dif + dif * (2 * p1 + t + 1) + p1 + t * p2
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
          z <- (est - delta) / .score(p1_hat, n1, p2_hat, n2, delta)
          2 * min(stats::pnorm(z), 1 - stats::pnorm(z))
        }
        ci_lwr <- max(-1, stats::uniroot(function(delta) {
          pval(delta) - alpha
        }, interval = c(-1 + 1e-06, est - 1e-06))$root)
        ci_upr <- min(1, stats::uniroot(function(delta) {
          pval(delta) - alpha
        }, interval = c(est + 1e-06, 1 - 1e-06))$root)
      },
      blj = {
        p1_dash <- (x1 + 0.5) / (n1 + 1)
        p2_dash <- (x2 + 0.5) / (n2 + 1)
        vd <- p1_dash * (1 - p1_dash) / n1 + p2_dash * (1 - p2_dash) / n2
        term2 <- kappa * sqrt(vd)
        est_dash <- p1_dash - p2_dash
        ci_lwr <- max(-1, est_dash - term2)
        ci_upr <- min(1, est_dash + term2)
      },
      ha = {
        term2 <- 1 /
          (2 * min(n1, n2)) + kappa * sqrt(p1_hat * (1 - p1_hat) / (n1 - 1) + p2_hat * (1 - p2_hat) / (n2 - 1))
        ci_lwr <- max(-1, est - term2)
        ci_upr <- min(1, est + term2)
      },
      mn = {
        .conf <- function(x1, n1, x2, n2, z, lower = FALSE) {
          p1 <- x1 / n1
          p2 <- x2 / n2
          p_hat <- p1 - p2
          dp <- 1 + ifelse(lower, 1, -1) * p_hat
          i <- 1
          while (i <= 50) {
            dp <- 0.5 * dp
            y <- p_hat + ifelse(lower, -1, 1) * dp
            score <- .score(p1, n1, p2, n2, y)
            if (score < z) {
              p_hat <- y
            }
            if ((dp < 1e-07) || (abs(z - score) < 1e-06)) {
              (break)()
            } else {
              i <- i + 1
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
            c <- dif * dif + dif * (2 * p1 + t + 1) + p1 + t * p2
            d <- -p1 * dif * (1 + dif)
            v <- (b / a / 3)^3 - b * c / (6 * a * a) + d / a / 2
            s <- sqrt((b / a / 3)^2 - c / a / 3)
            u <- ifelse(v > 0, 1, -1) * s
            w <- (3.141592654 + acos(v / u^3)) / 3
            p1d <- 2 * u * cos(w) - b / a / 3
            p2d <- p1d - dif
            n <- n1 + n2
            var <- (p1d * (1 - p1d) / n1 + p2d * (1 - p2d) / n2) * n / (n - 1)
            res <- diff^2 / var
          }
          return(res)
        }
        z <- stats::qchisq(conf.level, 1)
        ci_lwr <- max(-1, .conf(x1, n1, x2, n2, z, TRUE))
        ci_upr <- min(1, .conf(x1, n1, x2, n2, z, FALSE))
      },
      beal = {
        a <- p1_hat + p2_hat
        b <- p1_hat - p2_hat
        u <- ((1 / n1) + (1 / n2)) / 4
        v <- ((1 / n1) - (1 / n2)) / 4
        V <- u * ((2 - a) * a - b^2) + 2 * v * (1 - a) * b # nolint
        z <- stats::qchisq(p = 1 - alpha / 2, df = 1)
        A <- sqrt(z * (V + z * u^2 * (2 - a) * a + z * v^2 * (1 - a)^2)) # nolint
        B <- (b + z * v * (1 - a)) / (1 + z * u) # nolint
        ci_lwr <- max(-1, B - A / (1 + z * u))
        ci_upr <- min(1, B + A / (1 + z * u))
      },
      hal = {
        psi <- (p1_hat + p2_hat) / 2
        u <- (1 / n1 + 1 / n2) / 4
        v <- (1 / n1 - 1 / n2) / 4
        z <- kappa
        theta <- ((p1_hat - p2_hat) + z^2 * v * (1 - 2 * psi)) / (1 + z^2 * u)
        w <- z / (1 + z^2 * u) * sqrt(u * (4 * psi * (1 - psi) - (p1_hat - p2_hat)^2) + 2 * v * (1 - 2 * psi) *
          (p1_hat - p2_hat) + 4 * z^2 * u^2 * (1 - psi) * psi + z^2 * v^2 * (1 - 2 * psi)^2) # nolint
        c(theta + w, theta - w)
        ci_lwr <- max(-1, theta - w)
        ci_upr <- min(1, theta + w)
      },
      jp = {
        psi <- 0.5 * ((x1 + 0.5) / (n1 + 1) + (x2 + 0.5) / (n2 + 1))
        u <- (1 / n1 + 1 / n2) / 4
        v <- (1 / n1 - 1 / n2) / 4
        z <- kappa
        theta <- ((p1_hat - p2_hat) + z^2 * v * (1 - 2 * psi)) / (1 + z^2 * u)
        w <- z / (1 + z^2 * u) * sqrt(u * (4 * psi * (1 - psi) - (p1_hat - p2_hat)^2) + 2 * v * (1 - 2 * psi) *
          (p1_hat - p2_hat) + 4 * z^2 * u^2 * (1 - psi) * psi + z^2 * v^2 * (1 - 2 * psi)^2) # nolint
        c(theta + w, theta - w)
        ci_lwr <- max(-1, theta - w)
        ci_upr <- min(1, theta + w)
      },
    )
    ci <- c(
      est = est, lwr.ci = min(ci_lwr, ci_upr),
      upr.ci = max(ci_lwr, ci_upr)
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

#' @describeIn desctools_binom Compute confidence intervals for binomial proportions.
#'
#' @param x (`count`)\cr number of successes
#' @param n (`count`)\cr number of trials
#' @param conf.level (`proportion`)\cr confidence level, defaults to 0.95.
#' @param sides (`character`)\cr side of the confidence interval to compute. Must be one of `"two-sided"` (default),
#'   `"left"`, or `"right"`.
#' @param method (`character`)\cr method to use. Can be one out of: `"wald"`, `"wilson"`, `"wilsoncc"`,
#' `"agresti-coull"`, `"jeffreys"`, `"modified wilson"`, `"modified jeffreys"`, `"clopper-pearson"`, `"arcsine"`,
#' `"logit"`, `"witting"`, `"pratt"`, `"midp"`, `"lik"`, and `"blaker"`.
#'
#' @return A `matrix` with 3 columns containing:
#'   * `est`: estimate of proportion difference.
#'   * `lwr.ci`: lower end of the confidence interval.
#'   * `upr.ci`: upper end of the confidence interval.
#'
#' @keywords internal
desctools_binomci <- function(x,
                              n,
                              conf.level = 0.95, # nolint
                              sides = c("two.sided", "left", "right"),
                              method = c(
                                "wilson", "wald", "waldcc", "agresti-coull",
                                "jeffreys", "modified wilson", "wilsoncc", "modified jeffreys",
                                "clopper-pearson", "arcsine", "logit", "witting", "pratt",
                                "midp", "lik", "blaker"
                              ),
                              rand = 123,
                              tol = 1e-05) {
  if (missing(method)) {
    method <- "wilson"
  }
  if (missing(sides)) {
    sides <- "two.sided"
  }
  iBinomCI <- function(x, n, conf.level = 0.95, sides = c("two.sided", "left", "right"), # nolint
                       method = c(
                         "wilson", "wilsoncc", "wald",
                         "waldcc", "agresti-coull", "jeffreys", "modified wilson",
                         "modified jeffreys", "clopper-pearson", "arcsine", "logit",
                         "witting", "pratt", "midp", "lik", "blaker"
                       ),
                       rand = 123,
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
    if (conf.level < 0.5 || conf.level > 1) {
      stop("'conf.level' has to be in [0.5, 1]")
    }
    sides <- match.arg(sides, choices = c(
      "two.sided", "left",
      "right"
    ), several.ok = FALSE)
    if (sides != "two.sided") {
      conf.level <- 1 - 2 * (1 - conf.level) # nolint
    }
    alpha <- 1 - conf.level
    kappa <- stats::qnorm(1 - alpha / 2)
    p_hat <- x / n
    q_hat <- 1 - p_hat
    est <- p_hat
    switch(match.arg(arg = method, choices = c(
      "wilson",
      "wald", "waldcc", "wilsoncc", "agresti-coull", "jeffreys",
      "modified wilson", "modified jeffreys", "clopper-pearson",
      "arcsine", "logit", "witting", "pratt", "midp", "lik",
      "blaker"
    )),
    wald = {
      term2 <- kappa * sqrt(p_hat * q_hat) / sqrt(n)
      ci_lwr <- max(0, p_hat - term2)
      ci_upr <- min(1, p_hat + term2)
    },
    waldcc = {
      term2 <- kappa * sqrt(p_hat * q_hat) / sqrt(n)
      term2 <- term2 + 1 / (2 * n)
      ci_lwr <- max(0, p_hat - term2)
      ci_upr <- min(1, p_hat + term2)
    },
    wilson = {
      term1 <- (x + kappa^2 / 2) / (n + kappa^2)
      term2 <- kappa * sqrt(n) / (n + kappa^2) * sqrt(p_hat * q_hat + kappa^2 / (4 * n))
      ci_lwr <- max(0, term1 - term2)
      ci_upr <- min(1, term1 + term2)
    },
    wilsoncc = {
      lci <- (
        2 * x + kappa^2 - 1 - kappa * sqrt(kappa^2 - 2 - 1 / n + 4 * p_hat * (n * q_hat + 1))
      ) / (2 * (n + kappa^2))
      uci <- (
        2 * x + kappa^2 + 1 + kappa * sqrt(kappa^2 + 2 - 1 / n + 4 * p_hat * (n * q_hat - 1))
      ) / (2 * (n + kappa^2))
      ci_lwr <- max(0, ifelse(p_hat == 0, 0, lci))
      ci_upr <- min(1, ifelse(p_hat == 1, 1, uci))
    },
    `agresti-coull` = {
      x_tilde <- x + kappa^2 / 2
      n_tilde <- n + kappa^2
      p_tilde <- x_tilde / n_tilde
      q_tilde <- 1 - p_tilde
      est <- p_tilde
      term2 <- kappa * sqrt(p_tilde * q_tilde) / sqrt(n_tilde)
      ci_lwr <- max(0, p_tilde - term2)
      ci_upr <- min(1, p_tilde + term2)
    },
    jeffreys = {
      if (x == 0) {
        ci_lwr <- 0
      } else {
        ci_lwr <- stats::qbeta(
          alpha / 2,
          x + 0.5, n - x + 0.5
        )
      }
      if (x == n) {
        ci_upr <- 1
      } else {
        ci_upr <- stats::qbeta(1 - alpha / 2, x + 0.5, n - x + 0.5)
      }
    },
    `modified wilson` = {
      term1 <- (x + kappa^2 / 2) / (n + kappa^2)
      term2 <- kappa * sqrt(n) / (n + kappa^2) * sqrt(p_hat * q_hat + kappa^2 / (4 * n))
      if ((n <= 50 & x %in% c(1, 2)) | (n >= 51 & x %in% c(1:3))) {
        ci_lwr <- 0.5 * stats::qchisq(alpha, 2 * x) / n
      } else {
        ci_lwr <- max(0, term1 - term2)
      }
      if ((n <= 50 & x %in% c(n - 1, n - 2)) | (n >= 51 & x %in% c(n - (1:3)))) {
        ci_upr <- 1 - 0.5 * stats::qchisq(
          alpha,
          2 * (n - x)
        ) / n
      } else {
        ci_upr <- min(1, term1 + term2)
      }
    },
    `modified jeffreys` = {
      if (x == n) {
        ci_lwr <- (alpha / 2)^(1 / n)
      } else {
        if (x <= 1) {
          ci_lwr <- 0
        } else {
          ci_lwr <- stats::qbeta(
            alpha / 2,
            x + 0.5, n - x + 0.5
          )
        }
      }
      if (x == 0) {
        ci_upr <- 1 - (alpha / 2)^(1 / n)
      } else {
        if (x >= n - 1) {
          ci_upr <- 1
        } else {
          ci_upr <- stats::qbeta(1 - alpha / 2, x + 0.5, n - x + 0.5)
        }
      }
    },
    `clopper-pearson` = {
      ci_lwr <- stats::qbeta(alpha / 2, x, n - x + 1)
      ci_upr <- stats::qbeta(1 - alpha / 2, x + 1, n - x)
    },
    arcsine = {
      p_tilde <- (x + 0.375) / (n + 0.75)
      est <- p_tilde
      ci_lwr <- sin(asin(sqrt(p_tilde)) - 0.5 * kappa / sqrt(n))^2
      ci_upr <- sin(asin(sqrt(p_tilde)) + 0.5 * kappa / sqrt(n))^2
    },
    logit = {
      lambda_hat <- log(x / (n - x))
      V_hat <- n / (x * (n - x)) # nolint
      lambda_lower <- lambda_hat - kappa * sqrt(V_hat)
      lambda_upper <- lambda_hat + kappa * sqrt(V_hat)
      ci_lwr <- exp(lambda_lower) / (1 + exp(lambda_lower))
      ci_upr <- exp(lambda_upper) / (1 + exp(lambda_upper))
    },
    witting = {
      set.seed(rand)
      x_tilde <- x + stats::runif(1, min = 0, max = 1)
      pbinom_abscont <- function(q, size, prob) {
        v <- trunc(q)
        term1 <- stats::pbinom(v - 1, size = size, prob = prob)
        term2 <- (q - v) * stats::dbinom(v, size = size, prob = prob)
        return(term1 + term2)
      }
      qbinom_abscont <- function(p, size, x) {
        fun <- function(prob, size, x, p) {
          pbinom_abscont(x, size, prob) - p
        }
        stats::uniroot(fun,
          interval = c(0, 1), size = size,
          x = x, p = p
        )$root
      }
      ci_lwr <- qbinom_abscont(1 - alpha, size = n, x = x_tilde)
      ci_upr <- qbinom_abscont(alpha, size = n, x = x_tilde)
    },
    pratt = {
      if (x == 0) {
        ci_lwr <- 0
        ci_upr <- 1 - alpha^(1 / n)
      } else if (x == 1) {
        ci_lwr <- 1 - (1 - alpha / 2)^(1 / n)
        ci_upr <- 1 - (alpha / 2)^(1 / n)
      } else if (x == (n - 1)) {
        ci_lwr <- (alpha / 2)^(1 / n)
        ci_upr <- (1 - alpha / 2)^(1 / n)
      } else if (x == n) {
        ci_lwr <- alpha^(1 / n)
        ci_upr <- 1
      } else {
        z <- stats::qnorm(1 - alpha / 2)
        A <- ((x + 1) / (n - x))^2 # nolint
        B <- 81 * (x + 1) * (n - x) - 9 * n - 8 # nolint
        C <- (0 - 3) * z * sqrt(9 * (x + 1) * (n - x) * (9 * n + 5 - z^2) + n + 1) # nolint
        D <- 81 * (x + 1)^2 - 9 * (x + 1) * (2 + z^2) + 1 # nolint
        E <- 1 + A * ((B + C) / D)^3 # nolint
        ci_upr <- 1 / E
        A <- (x / (n - x - 1))^2 # nolint
        B <- 81 * x * (n - x - 1) - 9 * n - 8 # nolint
        C <- 3 * z * sqrt(9 * x * (n - x - 1) * (9 * n + 5 - z^2) + n + 1) # nolint
        D <- 81 * x^2 - 9 * x * (2 + z^2) + 1 # nolint
        E <- 1 + A * ((B + C) / D)^3 # nolint
        ci_lwr <- 1 / E
      }
    },
    midp = {
      f_low <- function(pi, x, n) {
        1 / 2 * stats::dbinom(x, size = n, prob = pi) + stats::pbinom(x,
          size = n, prob = pi, lower.tail = FALSE
        ) -
          (1 - conf.level) / 2
      }
      f_up <- function(pi, x, n) {
        1 / 2 * stats::dbinom(x, size = n, prob = pi) + stats::pbinom(x - 1, size = n, prob = pi) - (1 - conf.level) / 2
      }
      ci_lwr <- 0
      ci_upr <- 1
      if (x != 0) {
        ci_lwr <- stats::uniroot(f_low,
          interval = c(0, p_hat),
          x = x, n = n
        )$root
      }
      if (x != n) {
        ci_upr <- stats::uniroot(f_up, interval = c(
          p_hat,
          1
        ), x = x, n = n)$root
      }
    },
    lik = {
      ci_lwr <- 0
      ci_upr <- 1
      z <- stats::qnorm(1 - alpha * 0.5)
      tol <- .Machine$double.eps^0.5
      BinDev <- function(y, x, mu, wt, bound = 0, tol = .Machine$double.eps^0.5, # nolint
                         ...) {
        ll_y <- ifelse(y %in% c(0, 1), 0, stats::dbinom(x, wt,
          y,
          log = TRUE
        ))
        ll_mu <- ifelse(mu %in% c(0, 1), 0, stats::dbinom(x,
          wt, mu,
          log = TRUE
        ))
        res <- ifelse(abs(y - mu) < tol, 0, sign(y - mu) * sqrt(-2 * (ll_y - ll_mu)))
        return(res - bound)
      }
      if (x != 0 && tol < p_hat) {
        ci_lwr <- if (BinDev(
          tol, x, p_hat, n, -z,
          tol
        ) <= 0) {
          stats::uniroot(
            f = BinDev, interval = c(tol, if (p_hat < tol || p_hat == 1) {
              1 - tol
            } else {
              p_hat
            }), bound = -z,
            x = x, mu = p_hat, wt = n
          )$root
        }
      }
      if (x != n && p_hat < (1 - tol)) {
        ci_upr <- if (
          BinDev(y = 1 - tol, x = x, mu = ifelse(p_hat > 1 - tol, tol, p_hat), wt = n, bound = z, tol = tol) < 0) { # nolint
          ci_lwr <- if (BinDev(
            tol, x, if (p_hat < tol || p_hat == 1) {
              1 - tol
            } else {
              p_hat
            }, n,
            -z, tol
          ) <= 0) {
            stats::uniroot(
              f = BinDev, interval = c(tol, p_hat),
              bound = -z, x = x, mu = p_hat, wt = n
            )$root
          }
        } else {
          stats::uniroot(
            f = BinDev, interval = c(if (p_hat > 1 - tol) {
              tol
            } else {
              p_hat
            }, 1 - tol), bound = z,
            x = x, mu = p_hat, wt = n
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
      ci_lwr <- 0
      ci_upr <- 1
      if (x != 0) {
        ci_lwr <- stats::qbeta((1 - conf.level) / 2, x, n - x + 1)
        while (acceptbin(x, n, ci_lwr + tol) < (1 - conf.level)) {
          ci_lwr <- ci_lwr + tol
        }
      }
      if (x != n) {
        ci_upr <- stats::qbeta(1 - (1 - conf.level) / 2, x + 1, n - x)
        while (acceptbin(x, n, ci_upr - tol) < (1 - conf.level)) {
          ci_upr <- ci_upr - tol
        }
      }
    }
    )
    ci <- c(est = est, lwr.ci = max(0, ci_lwr), upr.ci = min(
      1,
      ci_upr
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
