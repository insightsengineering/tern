#' Confidence Interval for Mean
#'
#' Convenient function for calculating the mean confidence interval.
#' It can be used as a ggplot helper function for plotting.
#'
#' @inheritParams argument_convention
#' @param n_min (`number`)\cr a minimum number of non-missing `x` to estimate
#'     the confidence interval for mean.
#' @param gg_helper (`logical`)\cr `TRUE` when output should be aligned
#' for the use with ggplot.
#'
#' @export
#'
#' @examples
#'
#' stat_mean_ci(sample(10), gg_helper = FALSE)
#'
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(cyl, mpg)) + ggplot2::geom_point()
#'
#' p + stat_summary(
#'   fun.data = stat_mean_ci,
#'   geom = "errorbar"
#' )
#'
#' p + stat_summary(
#'   fun.data = stat_mean_ci,
#'   fun.args = list(conf_level = 0.5),
#'   geom = "errorbar"
#' )
#'
stat_mean_ci <- function(x,
                         conf_level = 0.95,
                         na.rm = TRUE,  # nolint
                         n_min = 2,
                         gg_helper = TRUE) {

  if (na.rm)
    x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)

  if (n < n_min || is.na(m)) {
    ci <- c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_)
  } else {
    hci <- stats::qt((1 + conf_level) / 2, df = n - 1) * sd(x) / sqrt(n)
    ci <- c(mean_ci_lwr = m - hci, mean_ci_upr = m + hci)
  }

  if (gg_helper) {
    ci <- data.frame(y = ifelse(is.na(m), NA_real_, m), ymin = ci[[1]], ymax = ci[[2]])
  }

  return(ci)

}

#' Confidence Interval for Median
#'
#' Convenient function for calculating the median confidence interval.
#' It can be used as a ggplot helper function for plotting.
#'
#' @inheritParams argument_convention
#' @param gg_helper (`logical`)\cr `TRUE` when output should be aligned
#' for the use with ggplot.
#'
#' @details The function was adapted from `DescTools/versions/0.99.35/source`
#'
#' @export
#'
#' @examples
#'
#' stat_median_ci(sample(10), gg_helper = FALSE)
#'
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(cyl, mpg)) + ggplot2::geom_point()
#' p + stat_summary(
#'   fun.data = stat_median_ci,
#'   geom = "errorbar"
#' )
#'
stat_median_ci <- function(x,
                           conf_level = 0.95,
                           na.rm = TRUE, # nolint
                           gg_helper = TRUE) {
  x <- unname(x)
  if (na.rm)
    x <- x[!is.na(x)]
  n <- length(x)
  med <- median(x)

  k <- stats::qbinom(p = (1 - conf_level) / 2, size = n, prob = 0.5, lower.tail = TRUE)

  # k == 0 - for small samples (e.g. n <= 5) ci can be outside the observed range
  if (k == 0 || is.na(med)) {
    ci <- c(median_ci_lwr = NA_real_, median_ci_upr = NA_real_)
    empir_conf_level <- NA_real_
  } else {
    x_sort <- sort(x)
    ci <- c(median_ci_lwr = x_sort[k], median_ci_upr = x_sort[n - k + 1])
    empir_conf_level <- 1 - 2 * stats::pbinom(k - 1, size = n, prob = 0.5)
  }

  if (gg_helper) {
    ci <- data.frame(y = med, ymin = ci[[1]], ymax = ci[[2]])
  }

  attr(ci, "conf_level")  <- empir_conf_level

  return(ci)

}
