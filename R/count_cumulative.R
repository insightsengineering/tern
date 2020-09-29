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

#' @describeIn count_cumulative Helper function to calculate count and fraction of
#'   `x` values in the lower or upper tail given a threshold.
#' @inheritParams argument_convention
#' @param threshold (`number`)\cr a cutoff value as threshold to count values of `x`.
#' @param lower_tail (`logical`)\cr whether to count lower tail, default is `TRUE`.
#' @param include_eq (`logical`)\cr whether to include value equal to the `threshold` in
#' count, default is `TRUE`.
#' @param .N_col (`count`)\cr denominator for fraction calculation.
#' @return A named vector of 2:
#'   - `count`: the count of values less than, less or equal to, greater than, or
#'   greater or equal to a threshold of user specification.
#'   - `fraction`: the fraction of the count.
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

  result <- c(count = count, fraction = count / .N_col)
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
#' @return A named list:
#'   - `count_fraction`: a list with each `thresholds` value as a component, each component
#'     contains a vector for the count and fraction.
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

  count_fraction_list <- Map(function(thres) {
    result <- h_count_cumulative(x, thres, lower_tail, include_eq, .N_col = .N_col, ...)
    label <- d_count_cumulative(thres, lower_tail, include_eq)
    with_label(result, label)
  }, thresholds)

  names(count_fraction_list) <- thresholds
  list(count_fraction = count_fraction_list)
}

#' @describeIn count_cumulative Statistics function to count non-missing values.
#' @return A list with named item:
#'   - `n`: the count of non-missing values.
#' @export
#' @examples
#' set.seed(1)
#' x <- c(sample(1:10, 10), NA)
#' s_count_nonmissing(x)
#'
s_count_nonmissing <- function(x) {
  list(n = with_label(sum(!is.na(x)), label = "n"))
}

#' @describeIn count_cumulative Layout creating function which can be be used for creating
#'   summary tables for cumulative counts of a variable. The ellipsis (`...`) conveys
#'   arguments to `s_count_cumulative()`, for instance `lower_tail = FALSE` if upper tail
#'   should be accounted for.
#' @inheritParams argument_convention
#' @export
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' split_cols_by(lyt = NULL, "ARM") %>%
#'   add_colcounts() %>%
#'   count_cumulative(
#'     vars = "AGE",
#'     thresholds = c(40, 60)
#'   ) %>%
#'   build_table(ADSL)
#'
count_cumulative <- function(lyt,
                             vars,
                             var_labels = vars,
                             show_labels = "visible",
                             ...) {
  a_count_cumulative <- format_wrap_x(
    s_count_cumulative,
    indent_mods = c(count_fraction = 2L),
    formats = c(count_fraction = "xx (xx.xx%)")
  )
  analyze(
    lyt,
    vars,
    afun = a_count_cumulative,
    var_labels = var_labels,
    show_labels = show_labels,
    extra_args = list(...)
  )
}

#' Description of Cumulative Count for Missed Doses
#'
#' This is a helper function that describes analysis in `count_missed_doses`
#' @inheritParams s_count_cumulative
#' @return A named `character` vector.
#'
d_count_missed_doses <- function(thresholds) {
  labels <- paste0("At least ", thresholds, " missed dose", ifelse(thresholds > 1, "s", ""))
  setNames(labels, thresholds)
}

#' @describeIn count_cumulative Layout creating function which can be be used for creating
#'   summary tables for summarizing missed doses given user-specified `thresholds`. This is
#'   an additional layer on top of `count_cumulative` specifically for missed doses.
#' @inheritParams argument_convention
#' @inheritParams s_count_cumulative
#' @param var (`string`)\cr variable name for missed doses.
#' @export
#' @examples
#' # data processing to get wide data
#' library(dplyr)
#' ADSL <- radsl(cached = TRUE)
#' ADEX <- radex(cached = TRUE)
#' ANL <- ADEX %>%
#'   distinct(STUDYID, USUBJID, ARM) %>%
#'   mutate(
#'     PARAMCD = "TNDOSMIS",
#'     PARAM = "Total number of missed doses during study",
#'     AVAL = sample(0:20, size = nrow(ADSL), replace = TRUE),
#'     AVALC = ""
#'   )
#' N_per_arm = table(ADSL$ARM)
#' split_cols_by(lyt = NULL, "ARM") %>%
#'   add_colcounts() %>%
#'   count_missed_doses(var = "AVAL", thresholds = c(1, 5, 10, 15)) %>%
#'   build_table(ANL, col_counts = N_per_arm)
#'
count_missed_doses <- function(lyt, var, thresholds, ...) {
  assert_that(is.string(var))

  lyt <- analyze(
    lyt = lyt,
    vars = var,
    afun = s_count_nonmissing,
    var_labels = "Missed Doses",
    format = "xx"
  )
  count_cumulative(
    lyt = lyt,
    vars = var,
    thresholds = thresholds,
    lower_tail = FALSE,
    include_eq = TRUE,
    .labels = d_count_missed_doses(thresholds),
    show_labels = "hidden",
    ...
  )
}
