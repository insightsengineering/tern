#' Check if list or data.frame has elements/variables
#'
#' Checks if list names exist and throws an error otherwise
#'
#' @param data a \code{data.frame} or named list
#' @param names vector with names
#'
#' @return \code{TRUE} if all variables exist and an appropriate error if not.
#'
#' @author Adrian Waddell (waddella), \email{adrian.waddell@roche.com}
#'
#' @noRd
#'
#' @examples
#' # function is not exported
#' `%needs%` <- tern:::`%needs%`
#'
#' iris %needs% c("Sepal.Length", "Petal.Width")
#'
#' \dontrun{
#' iris %needs% "ABC"
#' }
`%needs%` <- function(data, names) {
  i <- is.na(match(names, names(data)))

  if (any(i)) {
    msg <- if (sum(i) == 1) {
      paste("variable ", names[i], " does not exist")
    } else {
      paste("variables", paste(names[i], collapse = ", "), "do not exist")
    }
    stop(msg)
  }

  invisible(TRUE)
}

#' Combine factor Levels
#'
#' @param x factor
#' @param levels level names to be combined
#' @param new_level name of new level
#'
#' @return a factor
#'
#' @export
#'
#' @examples
#' x <- factor(letters[1:5], levels = letters[5:1])
#' tern:::combine_levels(x, levels = c('a', 'b') )
#'
#' tern:::combine_levels(x, c('e', 'b'))
combine_levels <- function(x, levels, new_level = paste(levels, collapse = "/")) {
  stopifnot(
    is.factor(x),
    all(levels %in% levels(x))
  )

  lvls <- levels(x)

  lvls[lvls %in% levels] <- new_level

  levels(x) <- lvls

  x
}


#' re-attach labels to variables
#'
#' @param df a data.frame
#' @param labels a named vector with the labels. names are variable names
#'
#' @noRd
#'
#' @examples
#'
#' df <- data.frame(c = c(1, 2), b = c("a", "b"), a = c(3,4))
#'
#' labels <- setNames(c("a var", "b var"), c("a", "b"))
#'
#' X <- tern:::add_labels(df, labels)
#'
#' \dontrun{
#' View(X)
#' }
add_labels <- function(df, labels) {
  for (name in names(df)) {
    lab <- labels[name]
    if (!is.na(lab[1]) && length(lab) == 1) {
      attr(df[[name]], "label") <- lab
    }
  }
  df
}


start_with_null <- function(x) {
  c(list(NULL), x)
}

#' Remove Shared Variables
#'
#' Variables are considered shared if they have the same variable name
#'
#' @param x a data.frame
#' @param y a data.frame
#' @param keep optional, a vector with variable names that should not be removed
#'
#' @return a data.frame
#'
#' @export
#'
#' @examples
#' drop_shared_variables(iris, iris[, 1:3])
drop_shared_variables <- function(x, y, keep) {
  stopifnot(
    is.data.frame(x),
    is.data.frame(y)
  )

  if (missing(keep)) {
    keep <- character(0)
  }

  df <- x[, !(names(x) %in% setdiff(names(y), keep)), drop = FALSE]

  for (a in c("md5sum", "source", "access_by", "accessed_on")) {
    attr(df, a) <- attr(x, a)
  }

  df
}

to_n <- function(x, n) {
  if (is.null(x)) {
    NULL
  } else if (length(x) == 1) {
    rep(x, n)
  } else if (length(x) == n) {
    x
  } else {
    stop("dimension mismatch")
  }
}

has_no_na <- function(x) {
  !any(is.na(x))
}

#' Get total count
#'
#' Normally, you don't need to use this function as it is the default
#'
#' @param col_by (factor or matrix) to get counts from
#'
#' @return counts per factor level or column of \code{col_by}
#'
#' @export
#'
#' @examples
#' get_N(data.frame(A = c(TRUE, TRUE, FALSE), B = c(FALSE, FALSE, TRUE)))
get_N <- function(col_by) { #nolintr
  stopifnot(is.factor(col_by) || is.data.frame(col_by))
  colSums(col_by_to_matrix(col_by))
}

#' Add total to \code{col_N}
#'
#' It adds the sum of the vector as the last element.
#'
#' This is necessary when you manually specify \code{col_N} and \code{col_by} uses the total column
#'   (via \code{by_add_total} or similar)
#'
#' @param col_N count
#'
#' @return new count with total count appended to vector
#'
#' @export
#'
#' @examples
#' col_N_add_total(get_N(data.frame(A = c(TRUE, TRUE, FALSE), B = c(FALSE, FALSE, TRUE))))
col_N_add_total <- function(col_N) { #nolintr
  c(col_N, sum(col_N))
}

#' Convert to factor and keep attributes
#'
#' @param x atomic
#'
#' @return factor with same attributes (except class) as x
#'
#' @export
#'
#' @examples
#' as_factor_keep_attributes(with_label(c(1,1,2,3), "id"))
as_factor_keep_attributes <- function(x) {
  stopifnot(is.atomic(x))
  x_attrs <- attributes(x)
  x_attrs <- x_attrs[names(x_attrs) != "class"]
  do.call(structure, c(list(.Data = as.factor(x)), attributes(x)))
}

number_rows <- function(x) {
  if (is.data.frame(x)) {
    nrow(x)
  } else {
    length(x)
  }
}

row_subset <- function(x, rows) {
  # similar to subset function, but the latter is only recommended for interactive use
  if (is.data.frame(x)) {
    x[rows, ]
  } else {
    x[rows]
  }
}

#' string representation of the object's contents with names
#'
#' @param x object
#'
#' @return string of the form \code{"key1: val1, keey2: val2, ..."}
#'
#' @examples
#' tern:::to_string_with_names(list(a = 1, 2))
to_string_with_names <- function(x) {
  # also works for more general
  paste(names(x), x, sep = ":", collapse = ", ")
}

#' Capitalize First Letter
#'
#' @noRd
#'
#' @examples
#'
#' capitalize("hello world")
#' tools::toTitleCase("hello world")
#'
capitalize <- function(x) {
  stopifnot(is_character_single(x))

  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

#' Mean CI for ggplot2
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


#' Median CI for ggplot2
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
#' @importFrom  stats pbinom qbinom qt
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

# From DescTools/versions/0.99.35/source, used in tern::stat_median_ci
strip_attr <- function(x, attr_names = NULL) {

  if (is.null(attr_names))
    attributes(x) <- NULL
  else
    for (a in attr_names)
      attr(x, which = a) <- NULL

    return(x)
}

#' Cut a character string at a certain width
#' @noRd
trunc_if_longer <- function(x, width = 40) {
  stopifnot(is_character_single(x))

  if (nchar(x) > width) {
    warning(paste0("Label ", x, " is truncated to 40 characters"))
    res <- paste0(substr(x, 1, width - 3), "...")
  } else {
    res <- x
  }

  res

}
