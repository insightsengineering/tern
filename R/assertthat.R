#' Additional Assertions for `assert_that`
#'
#' We provide additional assertion functions which can be used together with [assertthat::assert_that()].
#'
#' @param x object to test
#' @param df supposed data frame to test
#' @param variables (named `list` of `character`)\cr supposed variables list to test
#' @param include_boundaries (`logical`)\cr whether to include boundaries when testing for proportions.
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
#' @importFrom rlang is_integerish
#' @export
#' @examples
#'
#' # Check whether `x` is a nonnegative count.
#' is_nonnegative_count(-1)
#' is_nonnegative_count(0L)
#' is_nonnegative_count(10L)
#' is_nonnegative_count(0)
#' is_nonnegative_count(10)
is_nonnegative_count <- function(x) {
  if (length(x) != 1)
    return(FALSE)
  if (!rlang::is_integerish(x, n = 1))
    return(FALSE)
  x >= 0 && !is.na(x)
}
on_failure(is_nonnegative_count) <- function(call, env) {
  paste0(deparse(call$x), " is not a non-negative count (a single integer greater than or equal to 0)")
}

#' @describeIn assertions Check whether `x` is a valid list of variable names.
#' @export
#' @examples
#'
#' # Check whether `x` is a valid list of variable names.
#' is_variables(list(val = "a"))
#' is_variables(list(val = c("a", "b")))
#' is_variables(list(1, 2))
#' is_variables(list("bla"))
is_variables <- function(x) {
  is_fully_named_list(x) &&
    all(vapply(x, is_character_vector, TRUE))
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
#' is_df_with_variables(df = data.frame(a = 5, b = 3), variables = list(val = c("a", "b")))
#'
is_df_with_variables <- function(df, variables) {
  assert_that(
    is.data.frame(df),
    is_variables(variables)
  )
  all(unlist(variables) %in% names(df))
}
on_failure(is_df_with_variables) <- function(call, env) {
  var_df <- colnames(eval(call$df, envir = env))
  vars <- eval(call$variables, envir = env)
  vars <- vars[! unlist(vars) %in% var_df]
  paste(deparse(call$df), "does not contain all variables among:",
        paste(deparse(vars), collapse = ""))
}

#' @describeIn assertions Check whether `df` is a data frame where the analysis `variables`
#'   are all factors.
#' @export
#' @examples
#'
#' # Check whether `df` contains all factor analysis `variables`.
#' is_df_with_factors(
#'   df = data.frame(a = factor("A", levels = c("A", "B")), b = 3),
#'   variables = list(val = "a")
#' )
#'
is_df_with_factors <- function(df, variables) {
  assert_that(
    is_df_with_variables(df, variables)
  )
  all(vapply(df[, unlist(variables)], is_valid_factor, logical(1)))
}

on_failure(is_df_with_factors) <- function(call, env) {
  var_df <- colnames(eval(call$df, envir = env))
  vars <- eval(call$variables, envir = env)
  vars <- vars[! unlist(vars) %in% var_df]
  paste(deparse(call$df), "does not contain only factor variables among:",
        paste(deparse(vars), collapse = ""))
}

#' @describeIn assertions Check whether `df` is a data frame where the analysis `variable`
#'   is a factor with `n_levels` number of levels.
#' @param variable (`string`)\cr name of the single variable.
#' @param n_levels (`count`)\cr number of levels to compare with.
#' @param relation (`string`)\cr which relational operator to use for the comparison.
#' @export
#' @examples
#'
#' # Check whether `df` contains a factor `variable` with `n_levels` levels.
#' is_df_with_nlevels_factor(
#'   df = data.frame(a = factor("A", levels = c("A", "B")), b = 3),
#'   variable = "a",
#'   n_levels = 2
#' )
#'
is_df_with_nlevels_factor <- function(df,
                                      variable,
                                      n_levels,
                                      relation = c("==", ">=")) {
  assert_that(
    is.string(variable),
    is_df_with_factors(df, variables = list(factor = variable)),
    is.count(n_levels)
  )
  relation <- match.arg(relation)
  do.call(relation, list(x = nlevels(df[[variable]]), y = n_levels))
}

on_failure(is_df_with_nlevels_factor) <- function(call, env) {
  variable <- eval(call$variable, envir = env)
  n_levels <- eval(call$n_levels, envir = env)
  actual_levels <- levels(eval(call$df, envir = env)[[variable]])
  actual_n_levels <- length(actual_levels)
  relation_text <- switch(
    match.arg(eval(call$relation, envir = env), c("==", ">=")),
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
#'
is_equal_length <- function(...) {
  y <- mapply(FUN = length, x = list(...))
  all(y == y[1])
}

#' @importFrom stats setNames
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
#' is_proportion(x = 0, include_boundaries = TRUE)
#'
is_proportion <- function(x, include_boundaries = FALSE) {
  is_numeric_single(x) &&
    is_proportion_vector(x, include_boundaries = include_boundaries)
}
on_failure(is_proportion) <- function(call, env) {
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
#'
is_proportion_vector <- function(x, include_boundaries = FALSE) {
  if (include_boundaries) {
    all(x >= 0 & x <= 1)
  } else {
    all(x > 0 & x < 1)
  }
}
on_failure(is_proportion_vector) <- function(call, env) {
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
#'
is_quantiles_vector <- function(x, include_boundaries = FALSE) {
  is_proportion_vector(x, include_boundaries = include_boundaries) &&
    !is.unsorted(x) &&
    !any(duplicated(x))
}
on_failure(is_quantiles_vector) <- function(call, env) {
  paste(deparse(call$x), "is not a sorted vector of unique quantile proportions")
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

#' @describeIn assertions Check whether `NA` has been handled when `x` is a factor.
#' @export
#' @examples
#'
#' # Check whether `NA` has been handled when `x` is a factor.
#' is_factor_no_na(factor(c("a", NA)))
#' is_factor_no_na(factor(c("a", "<Missing>")))
#'
is_factor_no_na <- function(x) {
  is_valid_factor(x) & !any(is.na(x))
}
on_failure(is_factor_no_na) <- function(call, env) {
  paste0("NA in ", deparse(call$x), " has not been conveyed to na_level, please use explicit factor levels.")
}

#' @describeIn assertions Check whether `x` is a valid character (has no NAs and no empty strings).
#' @export
#' @examples
#'
#' # Check whether `x` is a valid character vector
#' is_valid_character(-1)
#' is_valid_character(c("a", "b"))
#' is_valid_character(c("a", ""))
#' is_valid_character("")
#'
is_valid_character <- function(x) {
  is.character(x) &&
  all(x != "") &&
  all(!(is.na(x)))
}
on_failure(is_valid_character) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid character vector, please check contents (no NAs or empty strings allowed)")
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
#'
all_elements_in_ref <- function(x, ref) {

  assert_that(
    is.vector(x),
    is.vector(ref),
    not_empty(x),
    not_empty(ref)
  )

  all(x %in% ref)
}
on_failure(all_elements_in_ref) <- function(call, env) {

  x <- eval(call$x, envir = env)
  ref <- eval(call$ref, envir = env)
  not_in_ref <- x[!(x %in% ref)]

  paste0("Some elements ", paste(deparse(not_in_ref, width.cutoff = 500), collapse = ""),
         " are not in the reference (", paste(deparse(ref, width.cutoff = 500), collapse = ""), ").")
}

#' @describeIn assertions Check whether rtables object `x` has the specified column names.
#' @param col_names (`character`)\cr column names which should be present in the table.
#'
has_tabletree_colnames <- function(x, col_names) {
  is(x, "VTableNodeInfo") &&
    !is.null(names(x)) &&
    all(col_names %in% names(x))
}
on_failure(has_tabletree_colnames) <- function(call, env) {
  x <- eval(call$x, envir = env)
  col_names <- eval(call$col_names, envir = env)
  missing_col_names <- x[!(x %in% col_names)]

  paste0("required column names ", paste(deparse(missing_col_names, width.cutoff = 500), collapse = ""),
         " are not found in table")
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
#'
is_df_with_no_na_level <- function(df, variables, na_level) {
  assert_that(
    is_df_with_variables(df, variables),
    is.string(na_level)
  )
  !any(df[, unlist(variables)] == na_level)
}
on_failure(is_df_with_no_na_level) <- function(call, env) {
  paste(deparse(call$df), "contains missing data as defined by the argument na_level")
}
