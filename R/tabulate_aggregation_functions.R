#' `rtabulate`-friendly aggregation functions (`a_*`)
#'
#' These functions enhance the control of missing data, including `rcell`
#' dedicated formats and are intended to be used with `rtables::rtabulate`.
#' Included:
#' * `a_mean_sd` mean along with standard deviation.
#' * `a_median` median.
#' * `a_n_true_and_freq` number of positive cases and frequency.
#' * `a_count` number of items.
#' * `a_q1q3` first and third quartiles.
#' * `a_iqr` interquartile range.
#' * `a_range` range.
#'
#' @param x A vector of numeric values to be aggregated.
#' @param na.rm A logical value indicating whether NA values should be stripped
#'   before the computation proceeds. Note that in `a_count`, it indicates
#'   whether or not to return NA if all data are missing.
#' @param na The string to use for missing values.
#' @param rcell A logical value indicated when the return value should be a
#'   formatted `rcell`.
#'
#' @details
#' These functions offer similar control over the management of missing values,
#' and the possibility to return a `rcell` object.
#'
#' @note These functions come as a replacement of non-exported functions:
#' * `a_mean_sd` replaces `mean_sd`, `mean_sd2`, `mean_sd3`.
#' * `a_median` replaces `median_t2`, `median_t3`.
#' * `a_n_true_and_freq` replaces `positives_and_proportion`,
#'      `positives_and_proportion2`, `positives_and_proportion3`.
#' * `a_count` replaces `n_count`, `n_not_na2`, `n_not_na3`.
#' * `a_q1q3` replaces `q1_q3`.
#' * `a_iqr` replaces `iqr_num`, `iqr_num2`, `iqr_num3`.
#' * `a_range` range `range_t2`, `range_t3`.
#'
#' @return Either a numeric or a `rcell`.
#'
#' @importFrom stats sd
#' @importFrom rtables rcell
#' @export
#'
#' @md
#' @rdname rcell-aggregate
#' @examples
#' # Data
#' # ----
#'
#' dta <- list(
#'     a = c(1, 2, 3),
#'     b = c(1, 2, NA),
#'     c = c(1, 1, NA),
#'     d = c(1),
#'     e = c(),
#'     f = c(NA, NA),
#'     g = NULL
#'   )
#'
#' # Mean and standard deviation
#' # ---------------------------
#' lapply(dta, a_mean_sd)
a_mean_sd <- function(x,
                      na.rm = TRUE, # nolint
                      na = NA,
                      rcell = FALSE) {

  # Cell values.
  if (any(as.integer(x[!is.na(x)]))) {
    x_bar <- mean(x, na.rm = na.rm)
    x_sd  <- sd(x, na.rm = na.rm)
  } else {
    x_bar <- x_sd <- NA
  }

  # Cell format.
  format_fun <- function(x, output, na_str = na) {
    z <- paste0(
      ifelse(is.na(x[1]), na_str, x[1]),
      " (", ifelse(is.na(x[2]), na_str, x[2]), ")"
    )
    return(z)
  }

  # Cell format.
  # Return either a rcell or a vector.
  y <- if (rcell) {
    rcell(c(x_bar, x_sd), format = format_fun)
  } else {
    c(x_bar, x_sd)
  }

  return(y)
}


#' Function: mean
#'
#' This function will have to be deprecated as replaced by `a_mean_sd`.
#'
#' @inherit a_mean_sd
#' @noRd
mean_sd <- function(x,
                    na.rm = TRUE) { # nolint
  # .Deprecated("a_mean_sd") # nolint
  y <- a_mean_sd(x, na.rm = na.rm, rcell = FALSE)
  return(y)
}


#' Function: median
#'
#' Calculates median
#'
#' @importFrom stats median
#' @importFrom rtables rcell
#' @export
#' @md
#' @rdname rcell-aggregate
#' @examples
#'
#' # Median
#' # ------
#' lapply(dta, a_median, rcell = TRUE, na = "NE")
a_median <- function(x,               # nousage # nolint
                     na.rm = TRUE,    # nolint
                     na = NA, # nolint
                     rcell = FALSE) {

  # Cell value.
  if (any(as.integer(x[!is.na(x)]))) {
    x_med <- median(x, na.rm = na.rm)
  } else {
    x_med <- x_med <- NA
  }

  # Cell format.
  format_fun <- function(x, output, na_str = na) {
    z <- ifelse(is.na(x), na_str, x)
    return(z)
  }

  # Return either a rcell or a vector.
  y <- if (rcell) {
    rcell(x_med, format = format_fun)
  } else {
    x_med
  }

  return(y)
}


#' Function: proportions
#'
#' Calculates proportions
#'
#' @importFrom rtables rcell
#' @export
#' @md
#' @rdname rcell-aggregate
#' @examples
#'
#' # Count positive cases and proportion
#' # -----------------------------------
#' booleans <- list(
#'   a = c(TRUE, TRUE, TRUE),
#'   b = c(FALSE, FALSE, FALSE),
#'   c = c(TRUE, FALSE, NA),
#'   d = c(),
#'   e = NULL
#' )
#'
#' lapply(
#'   booleans,
#'   a_n_true_and_freq,
#'   na.rm = FALSE,
#'   na = "NE",
#'   rcell = TRUE
#'   )
a_n_true_and_freq <- function(x,
                              na.rm = TRUE, # nousage # nolint
                              na = NA,
                              rcell = FALSE) {

  # Cell format.
  format_fun <- function(x, output, na_str = na) {
    z <- paste0(
      ifelse(is.na(x[1]), na_str, x[1]),
      " (", ifelse(is.na(x[2]), na_str, x[2]), ")"
    )
    return(z)
  }

  if (!is.logical(x)) {

    # Cell values.
    y <- c(NA, NA)
    message(
      "Absolute and relative frequencies can't be estimated, x is not logical."
    )

  } else {

    if (!all(is.na(x))) {
      x_sum_true  <- sum(x, na.rm = na.rm)
      x_freq_true <- x_sum_true / sum(!is.na(x))
    } else {
      x_sum_true <- x_freq_true <- NA
    }

    y <- c(x_sum_true, x_freq_true)

  }

  # Return either a formatted rcell or a vector.
  if (rcell) y <- rcell(y, format = format_fun)

  return(y)
}

#' Function: counts
#'
#' Calculates counts
#'
#' @importFrom rtables rcell
#' @export
#' @md
#' @rdname rcell-aggregate
#' @examples
#'
#' # Count items with explicit rules for NA
#' # --------------------------------------
#' lapply(dta, a_count, na.rm = FALSE, rcell = TRUE, na = "NE")
a_count <- function(x,
                    na.rm = TRUE, # nolint
                    na = NA,
                    rcell = FALSE) {

  # Cell values.
  if (is.null(x) || all(is.na(x))) {
    y <- ifelse(na.rm, 0, NA)
  } else {
    y <- sum(!is.na(x))
  }

  # Cell format.
  format_fun <- function(x, output, na_str = na) {
    z <- ifelse(is.na(x), na_str, x)
    return(z)
  }

  # Return either a rcell or a vector.
  if (rcell) y <- rcell(y, format = format_fun)

  return(y)
}

# Deprecated, use `a_n_true_and_freq` instead.
positives_and_proportion <- function(x, na.rm = TRUE) { # nolint
  stopifnot(is.logical(x))

  n <- if (na.rm) {
    sum(!is.na(x))
  } else {
    length(x)
  }

  sum(x, na.rm = TRUE) * c(1, 1 / n)
}


# Deprecated, use `a_count` instead.
count_n <- function(x, na.rm = TRUE) { # nolint
  if (na.rm) {
    sum(!is.na(x))
  } else {
    length(x)
  }
}

#' Function: quartiles
#'
#' Calculates First and Third quartiles
#'
#' @param type An integer between 1 and 9 to select the quantile algorithm,
#'  see [stats::quantile()].
#'
#' @importFrom stats quantile
#' @importFrom rtables rcell
#' @export
#' @md
#' @rdname rcell-aggregate
#' @examples
#'
#' # First and third quartiles
#' # -------------------------
#' dta_quant <- list(
#'   a = c(1, 1, 2, 3, 4, 4, 5, 7, 8, 9),    # n = 10
#'   b = c(1, 1, 2, 3, 4, 4, 5, 7, 8, 9, 9), # n = 11
#'   c = c(1, 1, 2, 3, 4, 4, 5, 7, 8, NA),   # n = 9 + 1 NA
#'   d = c(NA, NA),
#'   e = c(),
#'   f = NULL
#' )
#' lapply(dta_quant, a_q1q3, na = "NE", rcell = TRUE, na.rm = FALSE)
a_q1q3 <- function(x,
                   na.rm = TRUE, # nolint
                   na = NA,
                   rcell = FALSE,
                   type = 7) {

  # Cell values.
  if (!any(as.integer(x[!is.na(x)]))) {
    # At least one numeric value which is not missing.
    y <- c(NA, NA)
  } else if (any(is.na(x)) & !na.rm) {
    y <- c(NA, NA)
  } else {
    y <- quantile(
      x, probs = c(0.25, 0.75),
      na.rm = na.rm,
      type = type,
      names = FALSE
    )
  }

  # Cell format.
  format_fun <- function(x, output, na_str = na) {
    z <- paste0(
      ifelse(is.na(x[1]), na_str, x[1]),
      " - ",
      ifelse(is.na(x[2]), na_str, x[2])
    )
    return(z)
  }

  # Return either a rcell or a vector.
  if (rcell) y <- rcell(y, format = format_fun)

  return(y)

}


#' Function: interquartiles
#'
#' Calculates Interquartile range
#'
#' @importFrom rtables rcell
#' @export
#' @md
#' @rdname rcell-aggregate
#' @examples
#'
#' # Interquartile range
#' # --------------------
#' lapply(dta_quant, a_iqr, rcell = TRUE)
a_iqr <- function(x,
                  na.rm = TRUE, # nolint
                  na = NA,
                  rcell = FALSE,
                  type = 7) {

  # Cell value.
  y <- a_q1q3(x, na.rm = na.rm, rcell = FALSE, type = type)
  y <- if (any(is.na(y))) {
    NA
  } else {
    diff(y)
  }

  # Cell format.
  format_fun <- function(x, output, na_str = na) {
    z <- ifelse(is.na(x), na_str, x)
    return(z)
  }

  # Return either a rcell or a vector.
  if (rcell) y <- rcell(y, format = format_fun)

  return(y)
}

#' Function: ranges
#'
#' Calculates Range
#'
#' @importFrom rtables rcell
#' @export
#' @md
#' @rdname rcell-aggregate
#' @examples
#'
#' # Range
#' # -----
#' lapply(dta_quant, a_range, rcell = TRUE, na = "not avail.")
a_range <- function(x,
                    na.rm = TRUE, # nolint
                    na = NA,
                    rcell = FALSE) {

  # Cell values.
  if (any(as.integer(x[!is.na(x)]))) {
    # If at least one valid numeric value
    y <- range(x, na.rm = na.rm)
  } else {
    y <- c(NA, NA)
  }

  # Cell format.
  format_fun <- function(x, output, na_str = na) {
    z <- paste0(
      ifelse(is.na(x[1]), na_str, x[1]),
      " - ",
      ifelse(is.na(x[2]), na_str, x[2])
    )
    return(z)
  }

  # Return either a rcell or a vector.
  if (rcell) y <- rcell(y, format = format_fun)

  return(y)
}

# Deprecated, use `a_q1q3` instead.
#' @importFrom stats quantile
q1_q3 <- function(x, na.rm = TRUE, type = 7) { #nolintr
  quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, type = type)
}

#' Function: interquartiles
#'
#' Calculates interquartile range
#'
#' @noRd
#'
iqr_num <- function(x, na.rm = TRUE, type = 7) { # nolint
  diff(q1_q3(x, na.rm = na.rm, type = type))
}


# checks if there is any case and derives counts (percentage), otherwise 0
#' @importFrom rtables rcell
count_perc_col_N <- function(x_cell, n_i) { # nolint
  if (length(x_cell$id) > 0 && n_i > 0) {
    length(x_cell$id) * c(1, 1 / n_i) # obtaining the total and getting percentage
  } else {
    rcell(0, format = "xx")
  }
}


# Version with test for if-all-NA, then display empty cell

# Deprecated, use `a_n_true_and_freq` instead.
#' @importFrom rtables rcell
positives_and_proportion2 <- function(x) { # nousage # nolint
  stopifnot(is.logical(x))

  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    sum(x) * c(1, 1 / length(x))
  }
}

# Deprecated, use `a_count` instead.
#' @importFrom rtables rcell
n_not_na2 <- function(x) { # nousage # nolint
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    sum(!is.na(x))
  }
}

# Deprecated, use `mean_sd` instead.
#' @importFrom rtables rcell
#' @importFrom stats sd
mean_sd2 <- function(x, na.rm = TRUE) { # nolint # nousage
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    c(mean(x, na.rm = na.rm), sd(x, na.rm = na.rm))
  }
}

# Deprecated, use `a_median` instead.
#' @importFrom rtables rcell
#' @importFrom stats median
median_t2 <- function(x, na.rm = TRUE) { # nolint # nousage
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    median(x, na.rm = TRUE)
  }
}

# Deprecated, use `a_range` instead.
#' @importFrom rtables rcell
range_t2 <- function(x, na.rm = TRUE) { # nolint # nousage
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    range(x, na.rm = TRUE)
  }
}

# Deprecated, use `a_iqr` instead.
#' @importFrom rtables rcell
iqr_num2 <- function(x, na.rm = TRUE, type = 7) { # nolint # nousage
  if (all(is.na(x))) {
    rcell(" ", format = "xx")
  } else {
    iqr_num(x, na.rm = na.rm, type = type)
  }
}


# Version with test for if-all-NA, then display NE

# Deprecated, use `a_n_true_and_freq` instead.
#' @importFrom rtables rcell
positives_and_proportion3 <- function(x) { # nousage # nolint
  stopifnot(is.logical(x))

  if (all(is.na(x))) {
    rcell("NE", format = "xx")
  } else {
    sum(x) * c(1, 1 / length(x))
  }
}

# Deprecated, use `a_count` instead.
n_not_na3 <- function(x) {
  sum(!is.na(x))
}

# Deprecated, use `mean_sd` instead.
#' @importFrom rtables rcell
#' @importFrom stats sd
mean_sd3 <- function(x, na.rm = TRUE) { # nolint
  if (all(is.na(x))) {
    rcell("NE", format = function(x, output) paste0(x, " (", x, ")"))
  } else {
    c(mean(x, na.rm = na.rm), sd(x, na.rm = na.rm))
  }
}

# Deprecated, use `a_median` instead.
#' @importFrom rtables rcell
#' @importFrom stats median
median_t3 <- function(x, na.rm = TRUE) { # nolint
  if (all(is.na(x))) {
    rcell("NE", format = "xx")
  } else {
    median(x, na.rm = TRUE)
  }
}

# Deprecated, use `a_range` instead.
#' @importFrom rtables rcell
range_t3 <- function(x, na.rm = TRUE) { # nolint
  if (all(is.na(x))) {
    rcell("NE", format = function(x, output) paste0(x, " - ", x))
  } else {
    range(x, na.rm = TRUE)
  }
}

# Deprecated, use `a_iqr` instead.
#' @importFrom rtables rcell
iqr_num3 <- function(x, na.rm = TRUE, type = 7) { # nolint
  if (all(is.na(x))) {
    rcell("NE", format = function(x, output) paste0(x))
  } else {
    iqr_num(x, na.rm = na.rm, type = type)
  }
}


#' Function: t-test
#'
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
#' @importFrom stats t.test relevel
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

#' Function: CI
#'
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
    se <- sqrt(var(x) / n)
    alpha <- 1 - conf_level
    t_quantile <- qt(1 - alpha / 2, df)
    ci <- mean(x) + c(-t_quantile, t_quantile) * se
    ci
  }

  attr(result, "conf_level") <- conf_level
  return(result)
}
