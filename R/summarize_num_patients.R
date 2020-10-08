#' Number of patients
#'
#' Count the number of unique and non-unique patients in a column (variable).
#'
#' @inheritParams argument_convention
#' @param x (`character` or `factor`) \cr vector of patient IDs.
#' @template formatting_arguments
#' @name summarize_num_patients
NULL

#' @describeIn summarize_num_patients Statistics function which counts the number of
#' unique patients, the corresponding percentage taken with respect to the
#' total number of patients, and the number of non-unique patients.
#'
#' @return A list with:
#'   - `unique`: vector of count and percentage.
#'   - `nonunique` : vector of count.
#'
#' @export
#'
#' @examples
#' # Use the statistics function to count number of unique and nonunique patients.
#' s_num_patients(x = as.character(c(1, 2, 1, 4, NA)), .N_col = 5L)
s_num_patients <- function(x, .N_col) { # nolint

  assert_that(
    is_character_or_factor(x),
    is.count(.N_col)
  )

  count1 <- sum(!is.na(unique(x)))
  count2 <- sum(!is.na(x))
  list(unique = c(count1, count1 / .N_col),
       nonunique = count2)

}

#' @describeIn summarize_num_patients Counts the number of unique patients in a column
#' (variable), the corresponding percentage taken with respect to the total
#' number of patients, and the number of non-unique patients in the column.
#' Function serves as a wrapper that carries over both expected arguments `df`
#' and `labelstr` in `cfun` of [summarize_row_groups()].
#'
#' @export
#'
#' @examples
#' # Count number of unique and non-unique patients.
#' df <- data.frame(
#'   USUBJID = as.character(c(1, 2, 1, 4, NA)),
#'   AGE = c(10, 15, 10, 17, 8)
#' )
#' s_num_patients_content(df, .N_col = 5, .var = "USUBJID")
s_num_patients_content <- function(df, labelstr="", .N_col, .var) { # nolint

  assert_that(
    is.data.frame(df),
    is.string(.var)
  )

  x <- df[[.var]]
  s_num_patients(x = x, .N_col = .N_col)

}

c_num_patients <- make_afun(
  s_num_patients_content,
  .stats = c("unique", "nonunique"),
  .formats = c(unique = "xx (xx.x%)", nonunique = "xx"),
  .labels = c(unique = "Number of patients with at least one event",
              nonunique = "Number of events")
)

#' @describeIn summarize_num_patients Layout creating function which adds content rows using the statistics
#' function [s_num_patients_content()] and desired format.
#'
#' @export
#'
#' @examples
#'
summarize_num_patients <- function(lyt,
                                   var,
                                   .stats = NULL,
                                   .formats = NULL,
                                   .labels = NULL) {

  cfun <- make_afun(
    c_num_patients,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels
  )

  summarize_row_groups(
    lyt = lyt,
    var = var,
    cfun = cfun
  )

}
