#' Cumulative Counts with Thresholds
#'
#' Summarize cumulative counts of a (`numeric`) vector that is less than, less or equal to,
#' greater than, or greater or equal to user-specific thresholds.
#'
#' @template formatting_arguments
#'
#' @name count_cumulative
#'
NULL

#' @describeIn count_cumulative Helper function to calculate count and percentage of
#'   `x` values in the lower or upper tail given a threshold.
#' @inheritParams argument_convention
#' @param threshold (`number`)\cr a cutoff value as threshold to count values of `x`.
#' @param lower_tail (`logical`)\cr whether to count lower tail, default is `TRUE`.
#' @param include_eq (`logical`)\cr whether to include value equal to the `threshold` in
#' count, default is `TRUE`.
#' @param .N_col (`count`)\cr denominator for percentage calculation.
#' @return a named vector of 2:
#'   - `count`: the count of values less than, less or equal to, greater than, or
#'   greater or equal to a threshold of user specification.
#'   - `percent`: the percentage of the count.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' x <- c(sample(1:10, 10), NA)
#' .N_col <- length(x)
#' h_count_cumulative(x, 5, .N_col = .N_col)
#' h_count_cumulative(x, 5, lower_tail = FALSE, include_eq = FALSE, na.rm = FALSE, .N_col = .N_col)
#' h_count_cumulative(x, 0, lower_tail = FALSE, .N_col = .N_col)
#' h_count_cumulative(x, 100, lower_tail = FALSE, .N_col = .N_col)
#'
h_count_cumulative <- function(x,
                               threshold,
                               lower_tail = TRUE,
                               include_eq = TRUE,
                               na.rm = TRUE, # nolint
                               .N_col) { # nolint
  assert_that(
    is.numeric(x),
    is.numeric(threshold),
    is.flag(lower_tail),
    is.flag(include_eq),
    is.flag(na.rm),
    is.numeric(.N_col)
  )

  is_keep <- if (na.rm) !is.na(x) else rep(TRUE, length(x))
  count <- if (lower_tail & include_eq) {
    length(x[is_keep & x <= threshold])
  } else if (lower_tail & !include_eq) {
    length(x[is_keep & x < threshold])
  } else if (!lower_tail & include_eq) {
    length(x[is_keep & x >= threshold])
  } else if (!lower_tail & !include_eq) {
    length(x[is_keep & x > threshold])
  }

  result <- c(count = count, percent = count / .N_col)
  result
}

#' Description of Cumulative Count
#'
#' This is a helper function that describes analysis in `s_count_cumulative`
#' @inheritParams h_count_cumulative
#' @return a `string`
#'
d_count_cumulative <- function(threshold, lower_tail, include_eq) {
  assert_that(
    is.numeric(threshold)
  )
  lg <- if (lower_tail) "<" else ">"
  eq <- if (include_eq) "=" else ""
  paste0(lg, eq, " ", threshold)
}

#' @describeIn count_cumulative Statistics function that produces a named lists given a
#' (`numeric`) vector of thresholds.
#' @inheritParams h_count_cumulative
#' @param thresholds (`numeric`)\cr vector of cutoff value for the counts.
#' @return a named list:
#'   - `count_percent`: a list with each `thresholds` value as a component, each component
#'     contains a vector for the count and percentage.
#' @export
#' @examples
#' s_count_cumulative(x, thresholds = c(0, 5, 11), .N_col = .N_col)
#' s_count_cumulative(x, thresholds = c(0, 5, 11), include_eq = FALSE, na.rm = FALSE, .N_col = .N_col)
#'
s_count_cumulative <- function(x,
                               thresholds,
                               lower_tail = TRUE,
                               include_eq = TRUE,
                               .N_col, # nolint
                               ...) {
  assert_that(
    is_numeric_vector(thresholds)
  )

  count_percent_list <- Map(function(thres) {
    result <- h_count_cumulative(x, thres, lower_tail, include_eq, .N_col = .N_col, ...)
    label <- d_count_cumulative(thres, lower_tail, include_eq)
    with_label(result, label)
  }, thresholds)

  names(count_percent_list) <- thresholds
  list(count_percent = count_percent_list)
}

#' @describeIn count_cumulative Statistics function to count non-missing values.
#' @return a list with named item:
#'   - `n`: the count of non-missing values.
#' @export
#' @examples
#' set.seed(1)
#' x <- c(sample(1:10, 10), NA)
#' s_count_nonmissing(x)
s_count_nonmissing <- function(x) {
  list(n = with_label(sum(!is.na(x)), label = "n"))
}
