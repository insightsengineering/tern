#' Encode Categorical Missing Values in a Data Frame
#'
#' This is a helper function to encode missing entries across groups of categorical
#'   variables in a data frame.
#'
#' @details Missing entries are those with `NA` or empty strings and will
#'   be replaced with a specified value. If factor variables include missing
#'   values, the missing value will be inserted as the last level.
#'   Similarly, in case character or logical variables should be converted to factors with
#'   `char_as_factor` or `logical_as_factor` option, the missing values will be set as the last level.
#'
#' @param data (`data frame`)\cr data set.
#' @param omit_columns (`character`)\cr names of variables from `data` that should
#'   not be modified by this function.
#' @param char_as_factor (`flag`)\cr whether to convert character variables
#'   in `data` to factors.
#' @param logical_as_factor (`flag`)\cr whether to convert logical variables
#'   in `data` to factors.
#' @param na_level (`string`)\cr used to replace all `NA` or empty
#'   values inside non-`omit_columns` columns.
#'
#' @return The data frame with the desired changes made.
#'
#' @export
#' @seealso [sas_na()] and [explicit_na()] for other missing data helper functions.
#' @examples
#'
#' my_data <- data.frame(
#'   u = c(TRUE, FALSE, NA, TRUE),
#'   v = factor(c("A", NA, NA, NA), levels = c("Z", "A")),
#'   w = c("A", "B", NA, "C"),
#'   x = c("D", "E", "F", NA),
#'   y = c("G", "H", "I", ""),
#'   z = c(1, 2, 3, 4),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Encode missing values in all character or factor columns.
#' df_explicit_na(my_data)
#' # Encode missing values in a subset of columns.
#' df_explicit_na(my_data, omit_columns = c("x", "y"))
#'
df_explicit_na <- function(data, omit_columns = NULL, char_as_factor = TRUE, logical_as_factor = FALSE, na_level = "<Missing>") {

  assert_that(
    is.data.frame(data),
    is.null(omit_columns) || is_character_vector(omit_columns),
    is.flag(char_as_factor),
    is.flag(logical_as_factor),
    is.string(na_level)
  )

  target_vars <- if (is.null(omit_columns)) {
    names(data)
  } else {
    setdiff(names(data), omit_columns) # May have duplicates.
  }

  assert_that(
    is_character_vector(target_vars)
  )

  l_target_vars <- split(target_vars, target_vars)

  # Makes sure target_vars exist in data and names are not duplicated.
  assert_that(
    is_df_with_variables(data, l_target_vars)
  )

  for (x in target_vars) {

    xi <- data[[x]]
    xi_label <- obj_label(xi)

    if (is.factor(xi) || is.character(xi)) {

      # Handle empty strings and NA values.
      xi <- explicit_na(sas_na(xi), label = na_level)

      # Convert characters to factors, set na_level as the last value.
      if (is.character(xi) && char_as_factor) {
        levels_xi <- setdiff(sort(unique(xi)), na_level)
        if (na_level %in% unique(xi)) {
          levels_xi <- c(levels_xi, na_level)
        }

        xi <- factor(xi, levels = levels_xi)
      }

      data[, x] <- with_label(xi, label = xi_label)

    }

    if (is.logical(xi)) {

      # Handle empty strings and NA values.
      # xi <- explicit_na(sas_na(xi), label = na_level)

      # Convert logical variables to factors, set na_level as the last value.
      if (is.logical(xi) && logical_as_factor) {
        levels_xi <- setdiff(sort(unique(xi)), na_level)
        if (na_level %in% unique(xi)) {
          levels_xi <- c(levels_xi, na_level)
        }

        xi <- factor(xi, levels = levels_xi)
      }

      data[, x] <- with_label(xi, label = xi_label)

    }

  }
  return(data)
}
