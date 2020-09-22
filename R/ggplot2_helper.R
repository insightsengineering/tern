#' Mean CI for `ggplot2`
#'
#' Convenient function for the addition of mean confidence intervals as
#' error bars.
#'
#' @param x A vector of values.
#' @param conf_level Level of confidence for the interval (aka 1 - alpha)
#' @param na.rm Remove the missing data.
#' @param n_lim The threshold number of non-missing \code{x} to estimate
#'  the confidence interval for mean of \code{x}.
#'
#' @importFrom stats sd
#' @export
#'
#' @examples
#' require(ggplot2)
#' d <- ggplot(mtcars, aes(cyl, mpg)) + geom_point()
#' d + stat_summary(
#'   fun.data = stat_mean_ci, geom = "errorbar",
#'   aes(group = 1, linetype = "mean")
#' )
#'
#' d + stat_summary(
#'   fun.data = function(x) stat_mean_ci(x, conf_level = 0.5),
#'   geom = "errorbar", aes(group = 1, linetype = "mean")
#' )
stat_mean_ci <- function(x,
                         conf_level = 0.95,
                         na.rm = TRUE,  # nolint
                         n_lim = 2) {
  if (na.rm) x <- na.omit(x)
  n <- length(x)
  mean <- mean(x)
  hci  <- qt((1 + conf_level) / 2, df = n - 1) * sd(x) / sqrt(n)

  lcl <-  mean - hci
  ucl <-  mean + hci

  y <- if (n <= n_lim) {
    data.frame(y = mean, ymin = NA, ymax = NA)
  } else {
    data.frame(y = mean, ymin = lcl, ymax = ucl)
  }

  return(y)
}


#' Median CI for `ggplot2`
#'
#' Convenient function for the addition of the median confidence intervals as
#' error bars.
#'
#' @param x A vector of values.
#' @param conf_level Level of confidence for the interval (aka 1 - alpha)
#' @param na.rm Remove the missing data.
#'
#' @details The function was adapted from `DescTools/versions/0.99.35/source`
#'
#' @importFrom  stats pbinom qbinom qt median
#' @export
#' @md
#'
#' @examples
#' require(ggplot2)
#' d <- ggplot(mtcars, aes(cyl, mpg)) + geom_point()
#' d + stat_summary(
#'   fun.data = stat_median_ci, geom = "errorbar",
#'   aes(group = 1, linetype = "median")
#' )
stat_median_ci <- function(x,
                           conf_level = 0.95,
                           na.rm = TRUE) { # nolint

  if (na.rm) x <- na.omit(x)
  n <- length(x)

  k <- qbinom(
    p = (1 - conf_level) / 2, size = n, prob = 0.5, lower.tail = TRUE
  )
  ci <- sort(x)[c(k, n - k + 1)]
  attr(ci, "conf_level") <- 1 - 2 * pbinom(k - 1, size = n, prob = 0.5)

  # confints for small samples can be outside the observed range e.g. n < 6
  if (identical(strip_attr(ci), NA_real_)) {
    ci <- c(-Inf, Inf)
    attr(ci, "conf_level") <- 1
  }

  med <- median(x, na.rm = na.rm)
  # do not report a CI if the median is not defined.
  if (is.na(med)) {
    ci <- rep(NA, 3)
  } else {
    ci <- c(median = med, ci)
  }
  names(ci) <- c("y", "ymin", "ymax")
  ci <- as.data.frame(t(ci))

  return(ci)
}
