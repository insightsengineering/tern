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

q1_q3 <- function(x, na.rm = TRUE) {
  quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
}

#' get inter-quantile range
#'
#' @noRd
#'
#' @importFrom stats quantile sd median qnorm
#'
iqr_num <- function(x, na.rm = TRUE) { # nolint
  quantile(x, probs = c(.25, .75), na.rm = TRUE)
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
    quantile(x, probs = c(.25, .75), na.rm = TRUE)
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
    rcell("NE", format = function(x, output) paste0(x, " - ", x))
  } else {
    quantile(x, probs = c(.25, .75), na.rm = TRUE)
  }
}
