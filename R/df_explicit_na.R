#' Encode categorical missing values in a data frame
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This is a helper function to encode missing entries across groups of categorical
#' variables in a data frame.
#'
#' @details Missing entries are those with `NA` or empty strings and will
#'   be replaced with a specified value. If factor variables include missing
#'   values, the missing value will be inserted as the last level.
#'   Similarly, in case character or logical variables should be converted to factors
#'   with the `char_as_factor` or `logical_as_factor` options, the missing values will
#'   be set as the last level.
#'
#' @param data (`data.frame`)\cr data set.
#' @param omit_columns (`character`)\cr names of variables from `data` that should
#'   not be modified by this function.
#' @param char_as_factor (`flag`)\cr whether to convert character variables
#'   in `data` to factors.
#' @param logical_as_factor (`flag`)\cr whether to convert logical variables
#'   in `data` to factors.
#' @param na_level (`string`)\cr string used to replace all `NA` or empty
#'   values inside non-`omit_columns` columns.
#' @param factor_as_factor (`flag`)\cr whether to re-encode existing factor variables
#'   using `factor_level_method`. When `FALSE` (default), existing factor levels are
#'   preserved as-is (original behavior).
#' @param factor_level_method (`string`)\cr method used to order factor levels when
#'   converting character or logical variables (or existing factors when
#'   `factor_as_factor = TRUE`). One of:
#'   \describe{
#'     \item{`"sort_auto"`}{`sort(unique(x))` — default R sort, locale-aware (default).
#'       Preserves the original behavior of this function.}
#'     \item{`"sort_radix"`}{`sort(unique(x), method = "radix")` — byte-order (ASCII) sort.
#'       Unlike `"sort_auto"`, this is not locale-sensitive: uppercase letters always sort
#'       before lowercase. On data where all values share the same case (e.g. all-caps
#'       ADaM variables) the two methods produce identical results.}
#'     \item{`"data"`}{`unique(x)` — levels in order of first appearance in the data.}
#'   }
#' @param factor_level_last_pattern (`string` or `NULL`)\cr regular expression. Any
#'   factor levels matching this pattern are moved to the end (before `na_level`).
#'   `NULL` (default) disables this behaviour. Note: this parameter only takes effect
#'   when factor levels are being re-encoded (i.e. for character/logical columns with
#'   `char_as_factor`/`logical_as_factor`, or for existing factor columns with
#'   `factor_as_factor = TRUE`). Existing factor columns where `factor_as_factor = FALSE`
#'   are not affected.
#'
#' @return A `data.frame` with the chosen modifications applied.
#'
#' @seealso [sas_na()] and [explicit_na()] for other missing data helper functions.
#'
#' @examples
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
#' # Example 1
#' # Encode missing values in all character or factor columns.
#' df_explicit_na(my_data)
#' # Also convert logical columns to factor columns.
#' df_explicit_na(my_data, logical_as_factor = TRUE)
#' # Encode missing values in a subset of columns.
#' df_explicit_na(my_data, omit_columns = c("x", "y"))
#'
#' # Example 2
#' # Here we purposefully convert all `M` values to `NA` in the `SEX` variable.
#' # After running `df_explicit_na` the `NA` values are encoded as `<Missing>` but they are not
#' # included when generating `rtables`.
#' adsl <- tern_ex_adsl
#' adsl$SEX[adsl$SEX == "M"] <- NA
#' adsl <- df_explicit_na(adsl)
#'
#' # If you want the `Na` values to be displayed in the table use the `na_level` argument.
#' adsl <- tern_ex_adsl
#' adsl$SEX[adsl$SEX == "M"] <- NA
#' adsl <- df_explicit_na(adsl, na_level = "Missing Values")
#'
#' # Example 3
#' # Numeric variables that have missing values are not altered. This means that any `NA` value in
#' # a numeric variable will not be included in the summary statistics, nor will they be included
#' # in the denominator value for calculating the percent values.
#' adsl <- tern_ex_adsl
#' adsl$AGE[adsl$AGE < 30] <- NA
#' adsl <- df_explicit_na(adsl)
#'
#' # Example 4: Control factor level ordering
#' # Use radix sort to match SAS PROC SORT behavior.
#' df_explicit_na(my_data, factor_level_method = "sort_radix")
#' # Use data order (first appearance).
#' df_explicit_na(my_data, factor_level_method = "data")
#'
#' # Example 5: Move matching levels to the end
#' # Levels matching "^Other" are placed last (before na_level).
#' df_explicit_na(my_data, factor_level_last_pattern = "^Other")
#'
#' @export
df_explicit_na <- function(data,
                           omit_columns = NULL,
                           char_as_factor = TRUE,
                           logical_as_factor = FALSE,
                           na_level = "<Missing>",
                           factor_as_factor = FALSE,
                           factor_level_method = c("sort_auto", "sort_radix", "data"),
                           factor_level_last_pattern = NULL) {
  checkmate::assert_character(omit_columns, null.ok = TRUE, min.len = 1, any.missing = FALSE)
  checkmate::assert_data_frame(data)
  checkmate::assert_flag(char_as_factor)
  checkmate::assert_flag(logical_as_factor)
  checkmate::assert_flag(factor_as_factor)
  checkmate::assert_string(na_level)
  checkmate::assert_string(factor_level_last_pattern, null.ok = TRUE)
  factor_level_method <- factor_level_method[[1]]
  checkmate::assert_choice(factor_level_method, c("sort_auto", "sort_radix", "data"))

  target_vars <- if (is.null(omit_columns)) {
    names(data)
  } else {
    setdiff(names(data), omit_columns) # May have duplicates.
  }
  if (length(target_vars) == 0) {
    return(data)
  }

  l_target_vars <- split(target_vars, target_vars)

  # Makes sure target_vars exist in data and names are not duplicated.
  assert_df_with_variables(data, l_target_vars)

  for (x in target_vars) {
    xi <- data[[x]]
    xi_label <- obj_label(xi)

    # Determine whether to convert character or logical input.
    do_char_conversion <- is.character(xi) && char_as_factor
    do_logical_conversion <- is.logical(xi) && logical_as_factor
    do_factor_conversion <- is.factor(xi) && factor_as_factor

    # Pre-convert logical to character to deal correctly with replacing NA
    # values below.
    if (do_logical_conversion) {
      xi <- as.character(xi)
    }

    if (is.factor(xi) || is.character(xi)) {
      # Handle empty strings and NA values.
      xi <- explicit_na(sas_na(xi), label = na_level)

      # Convert to factors if requested for the original type,
      # set na_level as the last value.
      if (do_char_conversion || do_logical_conversion || do_factor_conversion) {
        if (do_factor_conversion) {
          xi <- as.character(xi)
        }

        sort_xi <- switch(factor_level_method,
          "data" = unique(xi),
          "sort_radix" = sort(unique(xi), method = "radix"),
          sort(unique(xi))
        )

        if (!is.null(factor_level_last_pattern)) {
          last_levels <- grep(factor_level_last_pattern, sort_xi, value = TRUE)
          sort_xi <- c(setdiff(sort_xi, last_levels), last_levels)
        }

        levels_xi <- setdiff(sort_xi, na_level)
        if (na_level %in% unique(xi)) {
          levels_xi <- c(levels_xi, na_level)
        }

        xi <- factor(xi, levels = levels_xi)
      }

      data[, x] <- formatters::with_label(xi, label = xi_label)
    }
  }
  return(data)
}
