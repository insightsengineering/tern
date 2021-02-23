#' Number of patients
#'
#' Count the number of unique and non-unique patients in a column (variable).
#'
#' @inheritParams argument_convention
#' @param x (`character` or `factor`) \cr vector of patient IDs.
#' @param count_by (`character` or `factor`) \cr optional vector to be combined with `x` when counting
#' `nonunqiue` records.
#'
#' @name summarize_num_patients
NULL

#' @describeIn summarize_num_patients Statistics function which counts the number of
#' unique patients, the corresponding percentage taken with respect to the
#' total number of patients, and the number of non-unique patients.
#'
#' @return A list with:
#'   - `unique`: vector of count and percentage.
#'   - `nonunique` : vector of count.
#'   - `unique_count`: count.
#'
#'
#' @export
#'
#' @examples
#' # Use the statistics function to count number of unique and nonunique patients.
#' s_num_patients(x = as.character(c(1, 1, 1, 2, 4, NA)), labelstr = "", .N_col = 6L)
#' s_num_patients(x = as.character(c(1, 1, 1, 2, 4, NA)),
#'                labelstr = "",
#'                .N_col = 6L,
#'                count_by = as.character(c(1, 1, 2, 1, 1, 1))
#'                )
s_num_patients <- function(x, labelstr, .N_col, count_by = NULL){ # nolint

  assert_that(
    is_character_or_factor(x),
    is.string(labelstr),
    is_nonnegative_count(.N_col)
  )

  count1 <- sum(!is.na(unique(x)))
  count2 <- sum(!is.na(x))

  if (!is.null(count_by)){
    assert_that(
      is_character_or_factor(count_by),
      is_equal_length(count_by, x)
    )
    count2 <- sum(!is.na(unique(interaction(x, count_by))))
  }

  out <- list(
    unique = c(count1, count1 / .N_col),
    nonunique = count2,
    unique_count = with_label(count1, paste(labelstr, "(n)"))
  )

  out
}

#' @describeIn summarize_num_patients Counts the number of unique patients in a column
#' (variable), the corresponding percentage taken with respect to the total
#' number of patients, and the number of non-unique patients in the column.
#' Function serves as a wrapper that carries over both expected arguments `df`
#' and `labelstr` in `cfun` of [summarize_row_groups()].
#'
#' @param required (`character` or `NULL`) optional name of a variable that is required to be non-missing.
#' @export
#'
#' @examples
#' # Count number of unique and non-unique patients.
#' df <- data.frame(
#'   USUBJID = as.character(c(1, 2, 1, 4, NA)),
#'   EVENT = as.character(c(10, 15, 10, 17, 8))
#' )
#' s_num_patients_content(df, .N_col = 5, .var = "USUBJID")
#'
#' df_by_event <- data.frame(
#'   USUBJID = as.character(c(1, 2, 1, 4, NA)),
#'   EVENT = as.character(c(10, 15, 10, 17, 8))
#' )
#' s_num_patients_content(df_by_event, .N_col = 5, .var = "USUBJID")
#' s_num_patients_content(df_by_event, .N_col = 5, .var = "USUBJID", count_by = "EVENT")
s_num_patients_content <- function(df, labelstr="", .N_col, .var, required = NULL, count_by = NULL) { # nolint

  assert_that(
    is.data.frame(df),
    is.string(.var),
    ifelse(
      is.null(count_by),
      is_df_with_variables(df, list(id = .var)),
      is_df_with_variables(df, list(id = .var, count_by = count_by))
    )
  )

  if (!is.null(required)) {
    assert_that(
      is_df_with_variables(df, list(required = required)),
      is.string(required)
    )
    df <- df[!is.na(df[[required]]), , drop = FALSE]
  }

  x <- df[[.var]]
  y <- switch(as.numeric(!is.null(count_by)) + 1, NULL, df[[count_by]])

  s_num_patients(
    x = x,
    labelstr = labelstr,
    .N_col = .N_col,
    count_by = y
  )
}

c_num_patients <- make_afun(
  s_num_patients_content,
  .stats = c("unique", "nonunique", "unique_count"),
  .formats = c(unique = format_count_fraction, nonunique = "xx", unique_count = "xx"),
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
                                   .labels = NULL,
                                   ...) {

  cfun <- make_afun(
    c_num_patients,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels
  )

  summarize_row_groups(
    lyt = lyt,
    var = var,
    cfun = cfun,
    extra_args = list(...)
  )

}
