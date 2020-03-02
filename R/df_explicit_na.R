#' Prepare data to be used in a teal app
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
#'   inside the \code{data.frame} that are not omit_columnsd \code{factor} variables.
#' @param na_level (\code{character}) that is used to replace all \code{NA} levels
#'   inside non-omit_columnsd columns
#'
#' @return The \code{data.frame} inserted with the desired changes made.
#'
#' @export
#'
#' @examples
#'
#' my_data <- data.frame(
#'   x = c("A", "B", NA, "C"),
#'   y = c("D", "E", "F", "E"),
#'   z = c(1, 2, 3, 4)
#' )
#'
#' df_explicit_na(my_data)
#'
#' library(random.cdisc.data)
#' ADSL <- radsl(N = 100, na_percentage = 0.1)
#' df_explicit_na(ADSL, omit_columns = c("USUBJID", "STUDYID"))
#'
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
