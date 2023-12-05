#' Confidence Interval for Mean
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Convenient function for calculating the mean confidence interval. It calculates the arithmetic as well as the
#' geometric mean. It can be used as a `ggplot` helper function for plotting.
#'
#' @inheritParams argument_convention
#' @param n_min (`number`)\cr a minimum number of non-missing `x` to estimate the confidence interval for mean.
#' @param gg_helper (`logical`)\cr `TRUE` when output should be aligned for the use with `ggplot`.
#' @param geom_mean (`logical`)\cr `TRUE` when the geometric mean should be calculated.
#'
#' @return A named `vector` of values `mean_ci_lwr` and `mean_ci_upr`.
#'
#' @examples
#' stat_mean_ci(sample(10), gg_helper = FALSE)
#'
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(cyl, mpg)) +
#'   ggplot2::geom_point()
#'
#' p + ggplot2::stat_summary(
#'   fun.data = stat_mean_ci,
#'   geom = "errorbar"
#' )
#'
#' p + ggplot2::stat_summary(
#'   fun.data = stat_mean_ci,
#'   fun.args = list(conf_level = 0.5),
#'   geom = "errorbar"
#' )
#'
#' p + ggplot2::stat_summary(
#'   fun.data = stat_mean_ci,
#'   fun.args = list(conf_level = 0.5, geom_mean = TRUE),
#'   geom = "errorbar"
#' )
#'
#' @export
stat_mean_ci <- function(x,
                         conf_level = 0.95,
                         na.rm = TRUE, # nolint
                         n_min = 2,
                         gg_helper = TRUE,
                         geom_mean = FALSE) {
  if (na.rm) {
    x <- stats::na.omit(x)
  }
  n <- length(x)

  if (!geom_mean) {
    m <- mean(x)
  } else {
    negative_values_exist <- any(is.na(x[!is.na(x)]) <- x[!is.na(x)] <= 0)
    if (negative_values_exist) {
      m <- NA_real_
    } else {
      x <- log(x)
      m <- mean(x)
    }
  }

  if (n < n_min || is.na(m)) {
    ci <- c(mean_ci_lwr = NA_real_, mean_ci_upr = NA_real_)
  } else {
    hci <- stats::qt((1 + conf_level) / 2, df = n - 1) * stats::sd(x) / sqrt(n)
    ci <- c(mean_ci_lwr = m - hci, mean_ci_upr = m + hci)
    if (geom_mean) {
      ci <- exp(ci)
    }
  }

  if (gg_helper) {
    m <- ifelse(is.na(m), NA_real_, m)
    ci <- data.frame(y = ifelse(geom_mean, exp(m), m), ymin = ci[[1]], ymax = ci[[2]])
  }

  return(ci)
}

#' Confidence Interval for Median
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Convenient function for calculating the median confidence interval. It can be used as a `ggplot` helper
#' function for plotting.
#'
#' @inheritParams argument_convention
#' @param gg_helper (`logical`)\cr `TRUE` when output should be aligned for the use with `ggplot`.
#'
#' @details The function was adapted from `DescTools/versions/0.99.35/source`
#'
#' @return A named `vector` of values `median_ci_lwr` and `median_ci_upr`.
#'
#' @examples
#' stat_median_ci(sample(10), gg_helper = FALSE)
#'
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(cyl, mpg)) +
#'   ggplot2::geom_point()
#' p + ggplot2::stat_summary(
#'   fun.data = stat_median_ci,
#'   geom = "errorbar"
#' )
#'
#' @export
stat_median_ci <- function(x,
                           conf_level = 0.95,
                           na.rm = TRUE, # nolint
                           gg_helper = TRUE) {
  x <- unname(x)
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  med <- stats::median(x)

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

  attr(ci, "conf_level") <- empir_conf_level

  return(ci)
}

#' p-Value of the Mean
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Convenient function for calculating the two-sided p-value of the mean.
#'
#' @inheritParams argument_convention
#' @param n_min (`numeric`)\cr a minimum number of non-missing `x` to estimate the p-value of the mean.
#' @param test_mean (`numeric`)\cr mean value to test under the null hypothesis.
#'
#' @return A p-value.
#'
#' @examples
#' stat_mean_pval(sample(10))
#'
#' stat_mean_pval(rnorm(10), test_mean = 0.5)
#'
#' @export
stat_mean_pval <- function(x,
                           na.rm = TRUE, # nolint
                           n_min = 2,
                           test_mean = 0) {
  if (na.rm) {
    x <- stats::na.omit(x)
  }
  n <- length(x)

  x_mean <- mean(x)
  x_sd <- stats::sd(x)

  if (n < n_min) {
    pv <- c(p_value = NA_real_)
  } else {
    x_se <- stats::sd(x) / sqrt(n)
    ttest <- (x_mean - test_mean) / x_se
    pv <- c(p_value = 2 * stats::pt(-abs(ttest), df = n - 1))
  }

  return(pv)
}

#' Proportion Difference and Confidence Interval
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Function for calculating the proportion (or risk) difference and confidence interval between arm
#' X (reference group) and arm Y. Risk difference is calculated by subtracting cumulative incidence
#' in arm Y from cumulative incidence in arm X.
#'
#' @inheritParams argument_convention
#' @param x (`list` of `integer`)\cr list of number of occurrences in arm X (reference group).
#' @param y (`list` of `integer`)\cr list of number of occurrences in arm Y. Must be of equal length to `x`.
#' @param N_x (`numeric`)\cr total number of records in arm X.
#' @param N_y (`numeric`)\cr total number of records in arm Y.
#' @param list_names (`character`)\cr names of each variable/level corresponding to pair of proportions in
#'   `x` and `y`. Must be of equal length to `x` and `y`.
#' @param pct (`flag`)\cr whether output should be returned as percentages. Defaults to `TRUE`.
#'
#' @return List of proportion differences and CIs corresponding to each pair of number of occurrences in `x` and
#'   `y`. Each list element consists of 3 statistics: proportion difference, CI lower bound, and CI upper bound.
#'
#' @seealso Split function [add_riskdiff()] which, when used as `split_fun` within [rtables::split_cols_by()]
#'   with `riskdiff` argument is set to `TRUE` in subsequent analyze functions, adds a column containing
#'   proportion (risk) difference to an `rtables` layout.
#'
#' @examples
#' stat_propdiff_ci(
#'   x = list(0.375), y = list(0.01), N_x = 5, N_y = 5, list_names = "x", conf_level = 0.9
#' )
#'
#' stat_propdiff_ci(
#'   x = list(0.5, 0.75, 1), y = list(0.25, 0.05, 0.5), N_x = 10, N_y = 20, pct = FALSE
#' )
#'
#' @export
stat_propdiff_ci <- function(x,
                             y,
                             N_x, # nolint
                             N_y, # nolint
                             list_names = NULL,
                             conf_level = 0.95,
                             pct = TRUE) {
  checkmate::assert_list(x, types = "numeric")
  checkmate::assert_list(y, types = "numeric", len = length(x))
  checkmate::assert_character(list_names, len = length(x), null.ok = TRUE)
  rd_list <- lapply(seq_along(x), function(i) {
    p_x <- x[[i]] / N_x
    p_y <- y[[i]] / N_y
    rd_ci <- p_x - p_y + c(-1, 1) * stats::qnorm((1 + conf_level) / 2) *
      sqrt(p_x * (1 - p_x) / N_x + p_y * (1 - p_y) / N_y)
    c(p_x - p_y, rd_ci) * ifelse(pct, 100, 1)
  })
  names(rd_list) <- list_names
  rd_list
}
