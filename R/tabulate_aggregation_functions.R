# Collection of reusable function that can be reused in rtabulate

positives_and_proportion <- function(x, na.rm = TRUE) { # nolint
  stopifnot(is.logical(x))

  n <- if (na.rm) {
    sum(!is.na(x))
  } else {
    length(x)
  }

  sum(x, na.rm = TRUE) * c(1, 1 / n)
}


count_n <- function(x, na.rm = TRUE) { # nolint
  if (na.rm) {
    sum(!is.na(x))
  } else {
    length(x)
  }
}

mean_sd <- function(x, na.rm = TRUE) { # nolint
  c(mean(x, na.rm = na.rm), sd(x, na.rm = na.rm))
}

q1_q3 <- function(x, na.rm = TRUE) { #nolintr
  quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
}

#' get inter-quantile range
#'
#' @noRd
#'
#' @importFrom stats quantile sd median qnorm
#'
iqr_num <- function(x, na.rm = TRUE) { # nolint
  quantile(x, probs = .75, na.rm = na.rm) - quantile(x, probs = .25, na.rm = na.rm)
}


# checks if there is any case and derives counts (percentage), otherwise 0
count_perc_col_N <- function(x_cell, n_i) { # nolint
  if (length(x_cell$id) > 0 && n_i > 0) {
    length(x_cell$id) * c(1, 1 / n_i) # obtaining the total and getting percentage
  } else {
    rcell(0, format = "xx")
  }
}


# Version with test for if-all-NA, then display empty cell

positives_and_proportion2 <- function(x) {
  stopifnot(is.logical(x))

  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    sum(x) * c(1, 1 / length(x))
  }
}

n_not_na2 <- function(x) {
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    sum(!is.na(x))
  }
}

mean_sd2 <- function(x, na.rm = TRUE) { # nolint
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    c(mean(x, na.rm = na.rm), sd(x, na.rm = na.rm))
  }
}

median_t2 <- function(x, na.rm = TRUE) { # nolint
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    median(x, na.rm = TRUE)
  }
}

range_t2 <- function(x, na.rm = TRUE) { # nolint
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    range(x, na.rm = TRUE)
  }
}

iqr_num2 <- function(x, na.rm = TRUE) { # nolint
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    quantile(x, probs = .75, na.rm = na.rm) - quantile(x, probs = .25, na.rm = na.rm)
  }
}


# Version with test for if-all-NA, then display NE

positives_and_proportion3 <- function(x) {
  stopifnot(is.logical(x))

  if (all(is.na(x))) {
    rcell("NE", format = "xx")
  } else {
    sum(x) * c(1, 1 / length(x))
  }
}

n_not_na3 <- function(x) {
  sum(!is.na(x))
}

mean_sd3 <- function(x, na.rm = TRUE) { # nolint
  if (all(is.na(x))) {
    rcell("NE", format = function(x, output) paste0(x, " (", x, ")"))
  } else {
    c(mean(x, na.rm = na.rm), sd(x, na.rm = na.rm))
  }
}

median_t3 <- function(x, na.rm = TRUE) { # nolint
  if (all(is.na(x))) {
    rcell("NE", format = "xx")
  } else {
    median(x, na.rm = TRUE)
  }
}

range_t3 <- function(x, na.rm = TRUE) { # nolint
  if (all(is.na(x))) {
    rcell("NE", format = function(x, output) paste0(x, " - ", x))
  } else {
    range(x, na.rm = TRUE)
  }
}

iqr_num3 <- function(x, na.rm = TRUE) { # nolint
  if (all(is.na(x))) {
    rcell("NE", format = function(x, output) paste0(x))
  } else {
    quantile(x, probs = .75, na.rm = na.rm) - quantile(x, probs = .25, na.rm = na.rm)
  }
}


#' Performs 2-arm t-test between comparison group against the reference group.
#'
#' @param x (\code{numeric} vector)
#' @param col_by (\code{factor} vector)\cr
#'   Containing two or more levels. First level is taken as reference level
#' @param conf.level (\code{numeric} value)\cr
#'   Number indicating confidence level. Default is 0.95
#'   Must be greater than 0 and less than 1.
#' @param ... (optional)\cr
#'  Other arguments from \code{\link{stats}{t.test}}
#'
#' @return list of \code{lcl}, \code{ucl}, \code{diff}, \code{se}, \code{tvalue}, \code{pvalue}
#'
#' @importFrom stats t.test
#'
#' @export
#'
#' @examples
#' ttest_two_arm(x = c(0,1,2,3,4,5,6,7,8,NA),
#'               col_by = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B")),
#'               var.equal = TRUE)
ttest_two_arm <- function(x,
                          col_by,
                          conf.level = 0.95, # nolint
                          ...) {

  stopifnot(
    is.numeric(conf.level),
    conf.level <= 1 && conf.level >= 0,
    nlevels(col_by) == 2
  )

  col_by <- relevel(col_by, ref = levels(col_by)[2])
  fit <- t.test(x ~ col_by, conf.level = conf.level, ...)

  result <- list(
    ci = fit$conf.int,
    diff = fit$estimate[[1]] - fit$estimate[[2]],
    se = unname((fit$estimate[[1]] - fit$estimate[[2]]) / fit$statistic),
    tvalue = fit$statistic,
    pvalue = fit$p.value
  )

  attr(result, "ref.level") <- levels(col_by)[2]
  return(result)
}

#' Confidence level for mean estimate (one-arm t-test)
#'
#' The calculation of confidence interval for an estimation of population mean
#'  will use \code{qt} function with degree of freedom of \code{n - 1}
#'
#' @param x (\code{numeric} vector)
#' @param conf.level (\code{numeric} value)
#'   Specifies confidence level. Must be greater than 0 and less than 1.
#' @param ... (optional)
#'   Other arguments from \code{\link{stats}{t.test}}
#'
#' @return a vector of 2 numeric numbers representing the lower and upper bounds
#'
#' @export
#'
#' @examples
#' ttest_ci_one_arm(c(1,2,3,4,5,6,7,8, NA), conf.level = 0.95, mu = 1, alternative = "less")
ttest_ci_one_arm <- function(x,
                             conf.level = 0.95, # nolint
                             ...) {

  stopifnot(is.numeric(conf.level), conf.level <= 1 && conf.level >= 0)

  result <- if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    t.test(x, conf.level = conf.level, ...)$conf.int
  }

  attr(result, "conf.level") <- conf.level
  return(result)
}
