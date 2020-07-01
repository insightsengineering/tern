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


mean_sd <- function(x,
                    na.rm = TRUE,    # nolint
                    na.strings = NA, # nolint
                    rcell = FALSE) {

  # Cell values
  if (any(as.integer(x[!is.na(x)]))) {
    x_bar <- mean(x, na.rm = na.rm)
    x_sd  <- sd(x, na.rm = na.rm)
  } else {
    x_bar <- x_sd <- NA
  }

  # Cell format
  format_fun <- function(x, output, na_str = na.strings) {
    z <- paste0(
      ifelse(is.na(x[1]), na_str, x[1]),
      " (", ifelse(is.na(x[2]), na_str, x[2]), ")"
    )
    return(z)
  }

  # Return either a rcell or a vector
  y <- if (rcell) {
    rtables::rcell(c(x_bar, x_sd), format = format_fun)
  } else {
    c(x_bar, x_sd)
  }

  return(y)
}


q1_q3 <- function(x, na.rm = TRUE, type = 7) { #nolintr
  quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, type = type)
}

#' get inter-quantile range
#'
#' @noRd
#'
#' @importFrom stats quantile sd median qnorm
#'
iqr_num <- function(x, na.rm = TRUE, type = 7) { # nolint
  diff(q1_q3(x, na.rm = na.rm, type = type))
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

positives_and_proportion2 <- function(x) { # nousage # nolint
  stopifnot(is.logical(x))

  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    sum(x) * c(1, 1 / length(x))
  }
}

n_not_na2 <- function(x) { # nousage # nolint
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    sum(!is.na(x))
  }
}

mean_sd2 <- function(x, na.rm = TRUE) { # nolint # nousage
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    c(mean(x, na.rm = na.rm), sd(x, na.rm = na.rm))
  }
}

median_t2 <- function(x, na.rm = TRUE) { # nolint # nousage
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    median(x, na.rm = TRUE)
  }
}

range_t2 <- function(x, na.rm = TRUE) { # nolint # nousage
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    range(x, na.rm = TRUE)
  }
}

iqr_num2 <- function(x, na.rm = TRUE, type = 7) { # nolint # nousage
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    iqr_num(x, na.rm = na.rm, type = type)
  }
}


# Version with test for if-all-NA, then display NE

positives_and_proportion3 <- function(x) { # nousage # nolint
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

iqr_num3 <- function(x, na.rm = TRUE, type = 7) { # nolint
  if (all(is.na(x))) {
    rcell("NE", format = function(x, output) paste0(x))
  } else {
    iqr_num(x, na.rm = na.rm, type = type)
  }
}


#' Performs 2-arm t-test between comparison group against the reference group.
#'
#' @param x (\code{numeric} vector)
#' @param col_by (\code{factor} vector)\cr
#'   Containing two or more levels. First level is taken as reference level
#' @param conf_level (\code{numeric} value)\cr
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
                          conf_level = 0.95, # nolint
                          ...) {

  stopifnot(
    is.numeric(conf_level),
    conf_level <= 1 && conf_level >= 0,
    nlevels(col_by) == 2
  )

  col_by <- relevel(col_by, ref = levels(col_by)[2])
  fit <- t.test(x ~ col_by, conf.level = conf_level, ...)

  result <- list(
    ci = fit$conf.int,
    diff = fit$estimate[[1]] - fit$estimate[[2]],
    se = unname((fit$estimate[[1]] - fit$estimate[[2]]) / fit$statistic),
    tvalue = fit$statistic,
    pvalue = fit$p.value
  )

  names(attributes(result$ci))[
    which(names(attributes(result$ci)) == "conf.level")
    ] <- "conf_level";

  attr(result, "ref.level") <- levels(col_by)[2]
  return(result)
}

#' Confidence level for mean estimate (based on one-arm t-test statistic)
#'
#' The calculation of a two-sided confidence interval for an estimation of population mean
#' will use the \code{qt} function with \code{n - 1} degrees of freedom.
#'
#' @param x (\code{numeric} vector)
#' @inheritParams argument_convention
#'
#' @return a vector of 2 numeric numbers representing the lower and upper bounds.
#'   Missing values are discarded before the CI calculation.
#'   If not at least 2 `x` values are available, then an empty \code{rcell} object is returned.
#'
#' @importFrom stats var qt
#' @export
#'
#' @examples
#' ttest_ci_one_arm(c(1, 2, 3, 4, NA), conf_level = 0.95)
#' ttest_ci_one_arm(as.numeric(c(NA, NA)))
ttest_ci_one_arm <- function(x,
                             conf_level = 0.95) {
  stopifnot(is.numeric(x))
  check_conf_level(conf_level)
  x <- x[!is.na(x)]

  result <- if (length(x) < 2) {
    rcell(" ", format = "xx")
  } else {
    n <- length(x)
    df <- n - 1
    se <- sqrt(stats::var(x) / n)
    alpha <- 1 - conf_level
    t_quantile <- stats::qt(1 - alpha / 2, df)
    ci <- mean(x) + c(-t_quantile, t_quantile) * se
    ci
  }

  attr(result, "conf_level") <- conf_level
  return(result)
}
