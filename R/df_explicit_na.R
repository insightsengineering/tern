#' Prepare Data to be Used in a Teal App
#'
#' teal does not take care of missing data provided by the app author. This is a helper
#' function to encode \code{NA} values with a specific character char.
#'
#' Additionally teal requires character variables to be converted to factors. This can
#' be executed by setting the \code{char_as_factor} variable to \code{TRUE}.
#'
#' @param data (\code{data.frame}) Any \code{cdisc} data set
#' @param omit_columns (\code{character}) vector of columns that should not be touched
#'   by this function
#' @param char_as_factor (\code{logical}) Whether to make all \code{character} variables
#'   inside the \code{data.frame} that are not \code{omit_columns} \code{factor} variables.
#' @param na_level (\code{character}) that is used to replace all \code{NA} levels
#'   inside non - \code{omit_column} columns
#'
#' @return The \code{data.frame} inserted with the desired changes made.
#'
#' @export
#'
#' @examples
#'
#' my_data <- data.frame(
#'   v = c("A", NA, NA, NA),
#'   w = c("A", "B", NA, "C"),
#'   x = c("D", "E", "F", NA),
#'   y = c("G", "H", "I", NA),
#'   z = c(1, 2, 3, 4)
#' )
#'
#' df_explicit_na(my_data, omit_columns = c("x", "y"))
#'
df_explicit_na <- function(data, omit_columns = NULL, char_as_factor = TRUE, na_level = "<Missing>") {

  stopifnot(is.data.frame(data))
  stopifnot(!any(duplicated(names(data))))

  stopifnot(is.null(omit_columns) || is_character_vector(omit_columns))
  if (!is.null(omit_columns)) {
    stopifnot(length(intersect(names(data), omit_columns)) == length(omit_columns))
  }
  stopifnot(is_logical_single(char_as_factor))
  stopifnot(is_character_single(na_level))

  for (var in setdiff(names(data), omit_columns)) {
    xi <- data[[var]]

    # Conver
    if (is.factor(xi) || is.character(xi)) {
      xi <- explicit_na(sas_na(xi), label = na_level)
    }

    # convert characters to factors (argument)
    if ((is.character(xi) || is.factor(xi)) && char_as_factor) {
     # Make it a factor with NA as last value
     xi <- factor(as.character(xi), levels = c(setdiff(unique(xi), na_level), na_level))
    }

    data[, var] <- xi

  }
  return(data)
}
