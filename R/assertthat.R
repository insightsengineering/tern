#' Additional Assertions for `assert_that`
#'
#' We provide additional assertion functions which can be used together with [assertthat::assert_that()].
#'
#' @param x object to test
#' @param df supposed data frame to test
#' @param variables supposed variables list to test
#' @return `flag` whether the assertion holds (`TRUE` or `FALSE`). When used inside
#'   [assertthat::assert_that()] produces a meaningful error message.
#' @name assertions
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
