#' Util function to create label for confidence interval
#'
#' @inheritParams argument_convention
#' @return a `string`
#' @examples
#' tern:::f_conf_level(0.95)
#' @noRd
#'
f_conf_level <- function(conf_level) {
  assert_that(is_proportion(conf_level))
  paste0(conf_level * 100, "% CI")
}

#' Combine Factor Levels
#'
#' Combine specified old factor Levels in a single new level.
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
#'
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


#' Add Labels for Data Frame Columns
#'
#' Re-attach labels to variables in a data frame.
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
#'
add_labels <- function(df, labels) { # nousage # nolint
  for (name in names(df)) {
    lab <- labels[name]
    if (!is.na(lab[1]) && length(lab) == 1) {
      attr(df[[name]], "label") <- lab
    }
  }
  df
}


#' Remove Shared Variables
#'
#' Variables are considered shared if they have the same variable name.
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
#'
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


start_with_null <- function(x) {
  c(list(NULL), x)
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

has_no_na <- function(x) { # nousage # nolint
  !any(is.na(x))
}

#' Conversion of a Vector to a Factor
#'
#' Converts `x` to a factor and keeps its attributes. Warns appropriately such that the user
#' can decide whether they prefer converting to factor manually (e.g. for full control of
#' factor levels).
#'
#' @param x (`atomic`)\cr object to convert
#'
#' @return The factor with same attributes (except class) as `x`.
#'
#' @export
#'
#' @examples
#' as_factor_keep_attributes(with_label(c(1, 1, 2, 3), "id"))
#'
as_factor_keep_attributes <- function(x) {
  assert_that(is.atomic(x))
  if (is.factor(x)) {
    return(x)
  }
  x_name <- deparse(substitute(x))
  x_class <- class(x)[1]
  warning(paste("automatically converting", x_class, "variable", x_name, "to factor"))
  do.call(structure, c(list(.Data = as.factor(x)), attributes(x)))
}

#' Create String Representation
#'
#' Create a string representation of the object's contents with names.
#'
#' @param x object
#'
#' @return string of the form \code{"key1: val1, keey2: val2, ..."}
#'
#' @examples
#' tern:::to_string_with_names(list(a = 1, 2))
#'
to_string_with_names <- function(x) { # nousage # nolint
  # also works for more general
  paste(names(x), x, sep = ":", collapse = ", ")
}

#' Capitalize a String
#'
#' Capitalize the first letters of the words in a single string.
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

check_col_by_factor <- function(x,
                                col_by,
                                col_N, # nolint
                                min_num_levels = 2) {
  stopifnot(
    is.factor(col_by),
    !any(is.na(col_by)) && !("" %in% levels(col_by)),
    length(col_N) == nlevels(col_by),
    nlevels(col_by) >= min_num_levels
  )
  if (is.data.frame(x)) {
    stopifnot(nrow(col_by) == nrow(x))
  } else {
    stopifnot(nrow(col_by) == length(x))
  }

  invisible(NULL)
}
