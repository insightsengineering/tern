#' Additional Assertions for `checkmate`
#'
#' @description
#' We provide additional assertion functions which can be used together with [checkmate::assert()].
#'
#' @param x object to test
#' @param df supposed data frame to test
#' @param variables (named `list` of `character`)\cr supposed variables list to test
#' @param include_boundaries (`logical`)\cr whether to include boundaries when testing for proportions.
#' @param ... a collection of objects to test.
#' @name assertions
NULL

#' @describeIn assertions Check whether `x` is a character or factor vector.
#' @examples
#'
#' # Check whether `x` is a character or factor vector.
#' # assert_character_or_factor(-1) # it fails
#' tern:::assert_character_or_factor(c("a", "b"))
#' tern:::assert_character_or_factor(factor(c("a", "b")))
#'
#' @keywords internal
assert_character_or_factor <- function(x) {
  checkmate::assertMultiClass(x, classes = c("factor", "character"))
}

#' @describeIn assertions Check whether `x` is a nonnegative count.
#'
#' @examples
#' # Check whether `x` is a nonnegative count.
#' # tern:::assert_nonnegative_count(-1) # Error
#' # tern:::assert_nonnegative_count(NA) # Error
#' # tern:::assert_nonnegative_count(c(1, 2)) # Error
#' tern:::assert_nonnegative_count(0L)
#' tern:::assert_nonnegative_count(10L)
#' tern:::assert_nonnegative_count(0)
#' tern:::assert_nonnegative_count(10)
#'
#' @keywords internal
assert_nonnegative_count <- function(x) {
  checkmate::assert_integerish(x, lower = 0, len = 1, any.missing = FALSE)
}

check_list_of_variables <- function(x) {

  # drop NULL elements in list
  x <- Filter(Negate(is.null), x)

  res <- checkmate::check_list(x,
    names = "named",
    min.len = 1,
    any.missing = FALSE,
    types = "character"
  )

  if (!isTRUE(res)) { # is.string
    return(paste0(deparse(x, width.cutoff = 500L), " is not a list of variable names. ", res))
  } else {
    return(TRUE)
  }
}
#' @describeIn assertions Check whether `x` is a valid list of variable names.
#'   `NULL` elements of the list `x` are dropped out with `Filter(Negate(is.null), x)`.
#'
#' @examples
#' # Check whether `x` is a valid list of variable names.
#' tern:::assert_list_of_variables(list(val = "a"))
#' tern:::assert_list_of_variables(list(val = c("a", "b")))
#' tern:::assert_list_of_variables(list(val = c("a", "b"), val2 = NULL))
#' # tern:::assert_list_of_variables(list(1, 2))
#' # tern:::assert_list_of_variables(list("bla" = 2))
#'
#' @keywords internal
assert_list_of_variables <- checkmate::makeAssertionFunction(check_list_of_variables)

check_df_with_variables <- function(df, variables) {
  checkmate::assert_data_frame(df)
  assert_list_of_variables(variables)

  # flag for equal variables and column names
  err_flag <- all(unlist(variables) %in% colnames(df))
  checkmate::assert_flag(err_flag)

  if (isFALSE(err_flag)) {
    vars <- setdiff(unlist(variables), colnames(df))
    return(paste(
      deparse(substitute(df)),
      "does not contain all specified variables as column names. Missing from dataframe:",
      paste(vars, collapse = "")
    ))
  }
  return(TRUE)
}
#' @describeIn assertions Check whether `df` is a data frame with the analysis `variables`.
#'   Please notice how this produces an error when not all variables are present in the
#'   data.frame while the opposite is not required.
#' @examples
#' # Check whether `df` contains the analysis `variables`.
#' # tern:::assert_df_with_variables(df = matrix(1:5, ncol = 2, nrow = 3), variables = list(val = "a"))
#' # tern:::assert_df_with_variables(df = data.frame(a = 5, b = 3), variables = list(val = c("a", "b", "c")))
#' tern:::assert_df_with_variables(df = data.frame(a = 5, b = 3), variables = list(val = "a"))
#' tern:::assert_df_with_variables(df = data.frame(a = 5, b = 3), variables = list(val = c("a", "b")))
#' tern:::assert_df_with_variables(df = data.frame(a = 5, b = 3), variables = list(val = c("a", "b")))
#'
#' @keywords internal
assert_df_with_variables <- checkmate::makeAssertionFunction(check_df_with_variables)

check_valid_factor <- function(x, any.missing = TRUE) {
  res <- checkmate::check_factor(x,
    empty.levels.ok = TRUE,
    min.levels = 1,
    null.ok = TRUE,
    any.missing = any.missing
  )
  # no empty strings allowed
  if (isTRUE(res)) {
    res <- checkmate::check_character(levels(x), min.chars = 1)
  }
  return(res)
}
#' @describeIn assertions Check whether `x` is a valid factor (has levels and no empty string levels).
#'   Note that `NULL` and `NA` elements are allowed.
#' @param any.missing It defaults to `TRUE`, allowing `NA` values, but it does not allow them
#'   if `FALSE` is used.
#'
#' @examples
#' # Check whether `x` is a valid factor.
#' tern:::assert_valid_factor(factor(c("a", "b")))
#' tern:::assert_valid_factor(factor(c("a", NULL)))
#' tern:::assert_valid_factor(factor(c("a", NA)), any.missing = TRUE)
#' tern:::assert_valid_factor(factor("A", levels = c("A", "B")))
#' # failures
#' # tern:::assert_valid_factor(-1)
#' # tern:::assert_valid_factor(factor(c("a", "")))
#' # tern:::assert_valid_factor(factor(c("a", NA)), any.missing = FALSE)
#' # tern:::assert_valid_factor(factor(NULL))
#' # tern:::assert_valid_factor(factor(c(NULL, "")))
#' # tern:::assert_valid_factor(factor())
#'
#' @keywords internal
assert_valid_factor <- checkmate::makeAssertionFunction(check_valid_factor)


check_df_with_factors <- function(df, variables) {
  res <- check_df_with_variables(df, variables)
  # checking if all the columns specified by variables are valid factors
  if (isTRUE(res)) {
    res <- lapply(df[, unlist(variables)], check_valid_factor)
    res_lo <- unlist(vapply(res, Negate(isTRUE), logical(1)))
    if (any(res_lo)) {
      return(paste0(
        deparse(substitute(df)), " does not contain only factor variables among:",
        "\n* ", paste0(names(res)[res_lo], " -> ", res[res_lo], collapse = "\n* ")
      ))
    } else {
      res <- TRUE
    }
    # return(paste0(deparse(df), "does not contain only factor variables among:",
  }
  return(res)
}
#' @describeIn assertions Check whether `df` is a data frame where the analysis `variables`
#'   are all factors.
#'
#' @examples
#' # Check whether `df` contains all factor analysis `variables`.
#' adf <- data.frame(a = factor(c("A", "B")), b = 3)
#' assert_df_with_factors(df = adf, variables = list(val = "a"))
#' # failures
#' # assert_df_with_factors(df = adf, variables = list(val = "a", val = "b"))
#'
#' @export
assert_df_with_factors <- checkmate::makeAssertionFunction(check_df_with_factors)

# assertthat::on_failure(is_df_with_factors) <- function(call, env) {
#   var_df <- colnames(eval(call$df, envir = env))
#   vars <- eval(call$variables, envir = env)
#   vars <- vars[!unlist(vars) %in% var_df]
#   paste(
#     deparse(call$df), "does not contain only factor variables among:",
#     paste(deparse(vars, width.cutoff = 500L), collapse = "")
#   )
# }

#' @describeIn assertions Check whether `df` is a data frame where the analysis `variable`
#'   is a factor with `n_levels` number of levels.
#' @param variable (`string`)\cr name of the single variable.
#' @param n_levels (`count`)\cr number of levels to compare with.
#' @param relation (`string`)\cr which relational operator to use for the comparison.
#'
#' @examples
#' # Check whether `df` contains a factor `variable` with `n_levels` levels.
#' is_df_with_nlevels_factor(
#'   df = data.frame(a = factor("A", levels = c("A", "B")), b = 3),
#'   variable = "a",
#'   n_levels = 2
#' )
#' @export
is_df_with_nlevels_factor <- function(df,
                                      variable,
                                      n_levels,
                                      relation = c("==", ">=")) {
  assert_df_with_factors(df, variables = list(factor = variable))
  checkmate::assert_count(n_levels)
  checkmate::assert_string(variable)
  relation <- match.arg(relation)
  do.call(relation, list(x = nlevels(df[[variable]]), y = n_levels))
}

assertthat::on_failure(is_df_with_nlevels_factor) <- function(call, env) {
  variable <- eval(call$variable, envir = env)
  n_levels <- eval(call$n_levels, envir = env)
  actual_levels <- levels(eval(call$df, envir = env)[[variable]])
  actual_n_levels <- length(actual_levels)
  relation_text <- switch(match.arg(eval(call$relation, envir = env), c("==", ">=")),
    "==" = "exactly",
    ">=" = "at least"
  )
  paste(
    "variable", variable, "in data frame", deparse(call$df), "should have", relation_text, n_levels,
    "levels, but has", actual_n_levels, "levels:", paste(actual_levels, collapse = ", ")
  )
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
is_equal_length <- function(...) {
  y <- mapply(FUN = length, x = list(...))
  all(y == y[1])
}

assertthat::on_failure(is_equal_length) <- function(call, env) {
  y <- unlist(as.list(call)[-1])
  y <- stats::setNames(y, nm = y)
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
#' is_proportion(x = 0, include_boundaries = TRUE)
is_proportion <- function(x, include_boundaries = FALSE) {
  checkmate::test_number(x) &&
    is_proportion_vector(x, include_boundaries = include_boundaries)
}
assertthat::on_failure(is_proportion) <- function(call, env) {
  paste(deparse(call$x), "is not a proportion: number between 0 and 1")
}

#' @describeIn assertions Check whether `x` is a vector of proportions (numbers between 0 and 1).
#' @export
#' @examples
#'
#' # Check whether `x` is a vector of numbers between 0 and 1.
#' is_proportion_vector(c(0.3, 0.1))
#' is_proportion_vector(c(1.3, 0.1))
#' is_proportion_vector(0, include_boundaries = TRUE)
is_proportion_vector <- function(x, include_boundaries = FALSE) {
  if (include_boundaries) {
    all(x >= 0 & x <= 1)
  } else {
    all(x > 0 & x < 1)
  }
}
assertthat::on_failure(is_proportion_vector) <- function(call, env) {
  paste(deparse(call$x), "is not a vector of proportions (numbers between 0 and 1)")
}

#' @describeIn assertions Check whether `x` is a sorted vector of unique quantile proportions
#'   (numbers between 0 and 1).
#' @export
#' @examples
#'
#' # Check whether `x` is a vector of sorted quantile proportions between 0 and 1.
#' is_quantiles_vector(c(0.1, 0.3))
#' is_quantiles_vector(c(0.3, 0.1))
#' is_quantiles_vector(c(0.3, 0.3))
#' is_quantiles_vector(0, include_boundaries = TRUE)
is_quantiles_vector <- function(x, include_boundaries = FALSE) {
  is_proportion_vector(x, include_boundaries = include_boundaries) &&
    !is.unsorted(x) &&
    !any(duplicated(x))
}
assertthat::on_failure(is_quantiles_vector) <- function(call, env) {
  paste(deparse(call$x), "is not a sorted vector of unique quantile proportions")
}


#' @describeIn assertions Check whether all elements of `x` are in a reference vector.
#' @param ref (`vector`)\cr where matches from `x` are sought for `all_elements_in_ref`.
#' @export
#' @examples
#'
#' # Check whether all elements of `x` are in a reference vector.
#' all_elements_in_ref(c("a", "b"), c("a", "b", "c"))
#' all_elements_in_ref(c("a", "d"), c("a", "b", "c"))
#' all_elements_in_ref(c(1:3), c(1:5))
all_elements_in_ref <- function(x, ref) {
  assertthat::assert_that(
    is.vector(x),
    is.vector(ref),
    assertthat::not_empty(x),
    assertthat::not_empty(ref)
  )

  all(x %in% ref)
}
assertthat::on_failure(all_elements_in_ref) <- function(call, env) {
  x <- eval(call$x, envir = env)
  ref <- eval(call$ref, envir = env)
  not_in_ref <- x[!(x %in% ref)]

  paste0(
    "Some elements ", paste(deparse(not_in_ref, width.cutoff = 500L), collapse = ""),
    " are not in the reference (", paste(deparse(ref, width.cutoff = 500L), collapse = ""), ")."
  )
}

#' @describeIn assertions Check whether `rtables` object `x` has the specified column names.
#' @param col_names (`character`)\cr column names which should be present in the table.
#'
has_tabletree_colnames <- function(x, col_names) {
  inherits(x, "VTableNodeInfo") &&
    !is.null(names(x)) &&
    all(col_names %in% names(x))
}
assertthat::on_failure(has_tabletree_colnames) <- function(call, env) {
  x <- eval(call$x, envir = env)
  col_names <- eval(call$col_names, envir = env)
  missing_col_names <- x[!(x %in% col_names)]

  paste0(
    "required column names ", paste(deparse(missing_col_names, width.cutoff = 500L), collapse = ""),
    " are not found in table"
  )
}

#' @describeIn assertions check if the input `df` has `na_level` in its `variables`.
#' @param df (`data frame`)\cr input dataset.
#' @param variables (`named list`)\cr columns from dataset where you want to check if `na_level` exists.
#' @param na_level (`string`)\cr the string user has been using to represent NA or missing data.
#' @export
#'
#' @examples
#'
#' df <- data.frame(a = 1:3, b = 2:4)
#' df$a <- ifelse(df$a == 1, "<Missing>", df$a)
#' is_df_with_no_na_level(df, variables = list(a = "a"), na_level = "<Missing>")
#' is_df_with_no_na_level(df, variables = list(b = "b"), na_level = "<Missing>")
#' is_df_with_no_na_level(df, variables = list(a = "a", b = "b"), na_level = "<Missing>")
is_df_with_no_na_level <- function(df, variables, na_level) {
  assertthat::assert_that(
    assertthat::is.string(na_level)
  )
  assert_df_with_variables(df, variables)
  !any(df[, unlist(variables)] == na_level)
}
assertthat::on_failure(is_df_with_no_na_level) <- function(call, env) {
  paste(deparse(call$df), "contains missing data as defined by the argument na_level")
}
