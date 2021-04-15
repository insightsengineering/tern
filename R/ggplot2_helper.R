#' Mean CI for `ggplot2`
#'
#' Convenient function for the addition of mean confidence intervals as
#' error bars.
#'
#' @inheritParams argument_convention
#' @param n_min (`number`)\cr a minimum number of non-missing `x` to estimate
#'     the confidence interval for mean.
#'
#' @importFrom stats na.omit qt sd
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
#'
stat_mean_ci <- function(x,
                         conf_level = 0.95,
                         na.rm = TRUE,  # nolint
                         n_min = 2) {

  if (na.rm)
    x <- na.omit(x)
  n <- length(x)
  m <- mean(x)

  # is.na(m) instead of any(is.na(x)) is used below, as these are equivalent here
  if (n < n_min || is.na(m)) {
    data.frame(y = m, ymin = NA_real_, ymax = NA_real_)
  } else {
    hci <- qt((1 + conf_level) / 2, df = n - 1) * sd(x) / sqrt(n)
    data.frame(y = m, ymin = m - hci, ymax = m + hci)
  }

}


#' Median CI for `ggplot2`
#'
#' Convenient function for the addition of the median confidence intervals as
#' error bars.
#'
#' @inheritParams argument_convention
#'
#' @details The function was adapted from `DescTools/versions/0.99.35/source`
#'
#' @importFrom stats median na.omit pbinom qbinom qt
#' @export
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
  x <- unname(x)
  if (na.rm)
    x <- na.omit(x)
  n <- length(x)
  med <- median(x)

  k <- qbinom(p = (1 - conf_level) / 2, size = n, prob = 0.5, lower.tail = TRUE)

  # k == 0 - for small samples (e.g. n <= 5) ci can be outside the observed range
  # is.na(med) instead of any(is.na(x)) is used below, as these are equivalent here
  if (k == 0 || is.na(med)) {
    ci <- data.frame(y = med, ymin = NA_real_, ymax = NA_real_)
    attr(ci, "conf_level") <- NA_real_
  } else {
    x_sort <- sort(x)
    ci <- data.frame(y = med, ymin = x_sort[k], ymax = x_sort[n - k + 1])
    attr(ci, "conf_level") <- 1 - 2 * pbinom(k - 1, size = n, prob = 0.5)
  }

  ci

}
