#' Additional Assertions for `assert_that`
#'
#' We provide additional assertion functions which can be used together with [assertthat::assert_that()].
#'
#' @param x object to test
#' @param df supposed data frame to test
#' @param variables supposed variables list to test
#' @param ... a collection of objects to test.
#' @return `flag` whether the assertion holds (`TRUE` or `FALSE`). When used inside
#'   [assertthat::assert_that()] produces a meaningful error message.
#' @name assertions
#'
NULL

#' @describeIn assertions Check whether `x` is a character or factor vector.
#' @export
#' @examples
#'
#' # Check whether `x` is a character or factor vector.
#' is_character_or_factor(-1)
#' is_character_or_factor(c("a", "b"))
#' is_character_or_factor(factor(c("a", "b")))
is_character_or_factor <- function(x) {
  is.character(x) || is.factor(x)
}
on_failure(is_character_or_factor) <- function(call, env) {
  paste0(deparse(call$x), " is not a character or factor vector")
}

#' @describeIn assertions Check whether `x` is a nonnegative count.
#' @export
#' @examples
#'
#' # Check whether `x` is a nonnegative count.
#' is_nonnegative_count(-1)
#' is_nonnegative_count(0L)
#' is_nonnegative_count(10L)
is_nonnegative_count <- function(x) {
  is_integer_single(x) && (x >= 0)
}
on_failure(is_nonnegative_count) <- function(call, env) {
  paste0(deparse(call$x), " is not a count (a single positive integer greater or equal to 0)")
}

#' @describeIn assertions Check whether `x` is a valid list of variable names.
#' @export
#' @examples
#'
#' # Check whether `x` is a valid list of variable names.
#' is_variables(list(val = "a"))
#' is_variables(list(1, 2))
#' is_variables(list("bla"))
is_variables <- function(x) {
  utils.nest::is_fully_named_list(x) &&
    all(vapply(x, is.string, TRUE))
}
on_failure(is_variables) <- function(call, env) {
  paste0(deparse(call$x), " is not a list of variable names")
}

#' @describeIn assertions Check whether `df` is a data frame with the analysis `variables`.
#' @export
#' @examples
#'
#' # Check whether `df` contains the analysis `variables`.
#' is_df_with_variables(df = data.frame(a = 5, b = 3), variables = list(val = "a"))
is_df_with_variables <- function(df, variables) {
  assert_that(
    is.data.frame(df),
    is_variables(variables)
  )
  all(variables %in% names(df))
}
on_failure(is_df_with_variables) <- function(call, env) {
  var_df <- colnames(eval(call$df))
  vars <- eval(call$variables, envir = env)
  vars <- vars[! unlist(vars) %in% var_df]
  paste(deparse(call$df), "does not contain all variables among:", deparse(vars))
}


#' @describeIn assertions Check that objects provided are of same length.
#' @export
#' @examples
#'
#' #' # Check whether `x` is a valid list of variable names.
#' a <- 1
#' b <- NULL
#' c <- c(1, "car")
#' d <- 5
#' is_equal_length(a, b, c, d)
#'
is_equal_length <- function(...) {
  y <- mapply(FUN = length, x = list(...))
  all(y == y[1])
}

on_failure(is_equal_length) <- function(call, env) {

  y <- unlist(as.list(call)[-1])
  y <- setNames(y, nm = y)
  y <- mapply(
    FUN = function(expr) eval(expr, envir = parent.frame(n = 2)),
    expr = y
  )
  y <- mapply(length, x = y)

  paste0(
    deparse(call), " - Objects must have the same length.\n",
    "However, variable `",
    names(y[1]), "` is of length ", y[1], " unlike `",
    paste(names(y[y != y[1]]), collapse = "`, `"), "`."
  )
}

#' @describeIn assertions Check whether `x` is a proportion: number between 0 and 1.
#' @export
#' @examples
#'
#' # Check whether `x` is between 0 and 1.
#' is_proportion(x = 0.3)
#' is_proportion(x = 1.3)
is_proportion <- function(x) {
  is_numeric_single(x) &&
    x > 0 && x < 1
}
on_failure(is_proportion) <- function(call, env) {
  paste(deparse(call$x), "is not a proportion: number between 0 and 1")
}

#' @describeIn assertions Check whether `x` is a valid factor (has levels and no empty string levels).
#' @export
#' @examples
#'
#' # Check whether `x` is a valid factor.
#' is_valid_factor(-1)
#' is_valid_factor(factor(c("a", "b")))
#' is_valid_factor(factor(c("a", "")))
#' is_valid_factor(factor())
#'
is_valid_factor <- function(x) {
  is.factor(x) &&
    length(levels(x)) > 0 &&
    all(is.na(levels(x)) | levels(x) != "")
}
on_failure(is_valid_factor) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid factor, please check the factor levels (no empty strings allowed)")
}

#' @describeIn assertions Check whether `x` is a proportion: number between 0 and 1.
#' @export
#' @examples
#'
#' # Check whether `x` is between 0 and 1.
#' is_proportion(x = 0.3)
#' is_proportion(x = 1)
is_proportion <- function(x) {
    is.number(x) &&
    x > 0 && x < 1
}
on_failure(is_proportion) <- function(call, env) {
  paste(deparse(call$x), "is not a proportion: number between 0 and 1")
}
