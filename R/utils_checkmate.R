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

check_list_of_variables <- function(x) {

  # drop NULL elements in list
  x <- Filter(Negate(is.null), x)

  res <- checkmate::check_list(x,
    names = "named",
    min.len = 1,
    any.missing = FALSE,
    types = "character"
  )
  # no empty strings allowed
  if (isTRUE(res)) {
    res <- checkmate::check_character(unlist(x), min.chars = 1)
  }
  return(res)
}
#' @describeIn assertions Check whether `x` is a valid list of variable names.
#'   `NULL` elements of the list `x` are dropped out with `Filter(Negate(is.null), x)`.
#'
#' @examples
#' # Check whether `x` is a valid list of variable names.
#' tern:::assert_list_of_variables(list(val = "a"))
#' tern:::assert_list_of_variables(list(val = c("a", "b")))
#' tern:::assert_list_of_variables(list(val = c("a", "b"), val2 = NULL))
#'
#' # The following calls fail
#' \dontrun{
#' tern:::assert_list_of_variables(list(1, 2))
#' tern:::assert_list_of_variables(list("bla" = 2))
#' }
#'
#' @keywords internal
assert_list_of_variables <- checkmate::makeAssertionFunction(check_list_of_variables)

check_df_with_variables <- function(df, variables, na_level = NULL) {
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
      paste(vars, collapse = ", ")
    ))
  }
  # checking if na_level is present and in which column
  if (!is.null(na_level)) {
    checkmate::assert_string(na_level)
    res <- unlist(lapply(as.list(df)[unlist(variables)], function(x) any(x == na_level)))
    if (any(res)) {
      return(paste0(
        deparse(substitute(df)), " contains explicit na_level (", na_level,
        ") in the following columns: ", paste0(unlist(variables)[res],
          collapse = ", "
        )
      ))
    }
  }
  return(TRUE)
}
#' @describeIn assertions Check whether `df` is a data frame with the analysis `variables`.
#'   Please notice how this produces an error when not all variables are present in the
#'   data.frame while the opposite is not required.
#' @param na_level (`string`)\cr the string user has been using to represent NA or
#'   missing data. For `NA` values please consider using directly `base::is.na` or
#'   similar approaches.
#'
#' @examples
#' # Check whether `df` contains the analysis `variables`.
#' tern:::assert_df_with_variables(
#'   df = data.frame(a = 5, b = 3),
#'   variables = list(val = "a")
#' )
#' tern:::assert_df_with_variables(
#'   df = data.frame(a = 5, b = 3),
#'   variables = list(val = c("a", "b"))
#' )
#' tern:::assert_df_with_variables(
#'   df = data.frame(a = 5, b = 3),
#'   variables = list(val = c("a", "b"))
#' )
#' tern:::assert_df_with_variables(
#'   df = data.frame(a = 5, b = 3, e = "<Missing>"),
#'   variables = list(val = c("a", "b")), na_level = "<Missing>"
#' )
#'
#' # The following calls fail
#' \dontrun{
#' tern:::assert_df_with_variables(
#'   df = matrix(1:5, ncol = 2, nrow = 3),
#'   variables = list(val = "a")
#' )
#' tern:::assert_df_with_variables(
#'   df = data.frame(a = 5, b = 3),
#'   variables = list(val = c("a", "b", "c"))
#' )
#' tern:::assert_df_with_variables(
#'   df = data.frame(a = 5, b = 3, e = "<Missing>"),
#'   variables = list(val = c("a", "b", "e")), na_level = "<Missing>"
#' )
#' }
#'
#' @keywords internal
assert_df_with_variables <- checkmate::makeAssertionFunction(check_df_with_variables)

check_valid_factor <- function(x,
                               min.levels = 1,
                               max.levels = NULL,
                               any.missing = TRUE) {
  # checks on levels insertion
  checkmate::assert_int(min.levels, lower = 1)
  # no check of max.levels if it is NULL
  if (!is.null(max.levels)) {
    checkmate::assert_int(max.levels, lower = min.levels)
  }
  # main factor check
  res <- checkmate::check_factor(x,
    empty.levels.ok = TRUE,
    min.levels = min.levels,
    null.ok = TRUE,
    max.levels = max.levels,
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
#' @param min.levels Minimum number of levels for `x`.
#' @param max.levels Maximum number of levels for `x`.
#' @param any.missing Default is `TRUE`, allowing missing values (`NA`).
#'
#' @examples
#' # Check whether `x` is a valid factor.
#' tern:::assert_valid_factor(factor(c("a", "b")))
#' tern:::assert_valid_factor(factor(c("a", NULL)))
#' tern:::assert_valid_factor(factor(c("a", NA)), any.missing = TRUE)
#' tern:::assert_valid_factor(factor("A", levels = c("A", "B")))
#'
#' # The following calls fail
#' \dontrun{
#' # tern:::assert_valid_factor(-1)
#' # tern:::assert_valid_factor(factor(c("a", "")))
#' # tern:::assert_valid_factor(factor(c("a", NA)), any.missing = FALSE)
#' # tern:::assert_valid_factor(factor(NULL))
#' # tern:::assert_valid_factor(factor(c(NULL, "")))
#' # tern:::assert_valid_factor(factor())
#' }
#'
#' @keywords internal
assert_valid_factor <- checkmate::makeAssertionFunction(check_valid_factor)


check_df_with_factors <- function(df,
                                  variables,
                                  min.levels = 1,
                                  max.levels = NULL,
                                  any.missing = TRUE,
                                  na_level = NULL) {
  res <- check_df_with_variables(df, variables, na_level)
  # checking if all the columns specified by variables are valid factors
  if (isTRUE(res)) {
    # searching the data.frame with selected columns (variables) as a list
    res <- lapply(
      X = as.list(df)[unlist(variables)],
      FUN = check_valid_factor,
      min.levels = min.levels,
      max.levels = max.levels,
      any.missing = any.missing
    )
    res_lo <- unlist(vapply(res, Negate(isTRUE), logical(1)))
    if (any(res_lo)) {
      return(paste0(
        deparse(substitute(df)), " does not contain only factor variables among:",
        "\n* Column `", paste0(unlist(variables)[res_lo],
          "` of the data.frame -> ", res[res_lo],
          collapse = "\n* "
        )
      ))
    } else {
      res <- TRUE
    }
  }
  return(res)
}
#' @describeIn assertions Check whether `df` is a data frame where the analysis `variables`
#'   are all factors. Note that the creation of `NA` by direct call of `factor()` will
#'   trim `NA` levels out of the vector list itself.
#' @param min.levels Minimum number of levels for `x`.
#' @param max.levels Maximum number of levels for `x`.
#' @param any.missing Default is `TRUE`, allowing missing values (`NA`).
#' @param na_level (`string`)\cr the string user has been using to represent NA or
#'   missing data. For `NA` values please consider using directly `base::is.na` or
#'   similar approaches.
#'
#' @examples
#' # Check whether `df` contains all factor analysis `variables`.
#' adf <- data.frame(a = factor(c("A", "B")), b = 3)
#' bdf <- data.frame(a = factor(letters[1:3]), b = factor(c(1, 2, 3)), d = 3)
#' assert_df_with_factors(df = adf, variables = list(val = "a"))
#' assert_df_with_factors(df = adf, variables = list(val = "a"), min.levels = 1)
#' assert_df_with_factors(df = adf, variables = list(val = "a"), min.levels = 2, max.levels = 2)
#' assert_df_with_factors(
#'   df = data.frame(a = factor(c("A", NA, "B")), b = 3),
#'   variable = list(val = "a"),
#'   min.levels = 2,
#'   max.levels = 2
#' )
#' # The following calls fail
#' \dontrun{
#' assert_df_with_factors(df = adf, variables = list(val = "a"), min.levels = 1, max.levels = 1)
#' assert_df_with_factors(df = adf, variables = list(val = "a"), min.levels = 1, max.levels = 1)
#' assert_df_with_factors(df = adf, variables = list(val = "a", val = "b", val = ""))
#' assert_df_with_factors(df = adf, variables = list(val = "a", val = "b", val = "d"))
#' assert_df_with_factors(df = bdf, variables = list(val = "a", val = "b"), min.levels = 1, max.levels = 1)
#' }
#'
#' @export
assert_df_with_factors <- checkmate::makeAssertionFunction(check_df_with_factors)

check_equal_length <- function(...) {
  args <- tibble::lst(...)
  args_lens <- vapply(args, length, numeric(1))
  res <- args_lens != args_lens[1]

  if (any(res)) {
    paste0(
      deparse(args), " - Objects must have the same length.\n",
      "However, variable `",
      names(args[1]), "` is of length ", args_lens[1], " unlike `",
      paste(names(args[res]), collapse = "`, `"), "`."
    )
  } else {
    return(TRUE)
  }
}
#' @describeIn assertions Check that objects provided are of same length.
#'
#' @examples
#' #' # Check whether `x` is a valid list of variable names.
#' a <- 1
#' b <- NULL
#' c <- c(1, "car")
#' d <- 5
#' # These fails
#' \dontrun{
#' assert_equal_length(a, b, c, d)
#' }
#'
#' @export
assert_equal_length <- function(...) {
  checkmate::assert(check_equal_length(...))
}

#' @describeIn assertions Check whether `x` is a proportion: number between 0 and 1.
#'
#' @examples
#' # Check whether `x` is between 0 and 1.
#' assert_proportion_value(x = 0, include_boundaries = TRUE)
#' assert_proportion_value(x = 0.3)
#' # These fails
#' \dontrun{
#' assert_proportion_value(x = 1.3)
#' assert_proportion_value(x = 1)
#' }
#'
#' @export
assert_proportion_value <- function(x, include_boundaries = FALSE) {
  checkmate::assert_number(x, lower = 0, upper = 1)
  checkmate::assert_flag(include_boundaries)
  if (isFALSE(include_boundaries)) {
    checkmate::assert_true(x > 0)
    checkmate::assert_true(x < 1)
  }
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
