#' Occurrence Counts
#'
#' Functions for analyzing frequencies and fractions of occurrences for patients with occurrence
#' data. Primary analysis variables are the dictionary terms. All occurrences are counted for total
#' counts. Multiple occurrences within patient at the lowest term level displayed in the table are
#' counted only once.
#'
#' @inheritParams argument_convention
#'
#' @name count_occurrences
#'
NULL

#' @describeIn count_occurrences Statistics function which counts number of patients that report an
#' occurrence.
#'
#' @returns A list with:
#'   - `count`: list of counts with one element per occurrence
#'   - `count_fraction`: list of counts and fractions with one element per occurrence.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
#'   MHDECOD = c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3")
#' )
#'
#' N_per_col <- 4L
#'
#' # Count unique occurrences per subject.
#' s_count_occurrences(
#'   df,
#'   N_per_col,
#'   .var = "MHDECOD",
#'   id = "USUBJID"
#' )
#'
s_count_occurrences <- function(df,
                                .N_col, # nolint
                                .var = "MHDECOD",
                                id = "USUBJID") {

  assert_that(
    is_df_with_variables(df, list(range = .var, id = id)),
    is_nonnegative_count(.N_col),
    is_character_or_factor(df[[.var]]),
    is_character_or_factor(df[[id]])
  )

  occurrences <- df[[.var]] # vector of all occurrences
  ids <- df[[id]] # vector of all subjids
  occurrences_count <- table(occurrences, ids) > 0 # logical indicating whether a subject reported a term at least once
  sum_across_subjects <- as.list(apply(occurrences_count, 1, sum)) # sum across subjects
  list(
    count = sum_across_subjects,
    count_fraction = lapply(sum_across_subjects, function(i, denom) c(i, i / denom), denom = .N_col)
  )

}

#' @describeIn count_occurrences Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' a_count_occurrences(
#'   df,
#'   N_per_col,
#'   .var = "MHDECOD",
#'   id = "USUBJID"
#' )
#'
a_count_occurrences <- make_afun(
  s_count_occurrences,
  .formats = c(count = "xx", count_fraction = format_count_fraction)
)

#' @describeIn count_occurrences Analyze Function that counts occurrences as part of rtables layouts.
#'
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#'   USUBJID = as.character(c(1, 1, 2, 4, 4, 4,
#'                            6, 6, 6, 7, 7, 8)),
#'   MHDECOD = c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3",
#'               "MH2", "MH2", "MH3", "MH1", "MH2", "MH4"),
#'   ARM = rep(c("A", "B"), each=6)
#' )
#' N_per_arm <- c(5L, 4L)
#'
#' # Create table layout
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_occurrences(vars = "MHDECOD", .stats = c("count"))
#'
#' # Apply table layout to data and produce rtable object
#' lyt %>%
#'   build_table(df, col_counts = N_per_arm) %>%
#'   prune_table()
#'
count_occurrences <- function(lyt,
                              vars,
                              ...,
                              .stats ="count_fraction",
                              .formats = NULL,
                              .labels = NULL,
                              .indent_mods = NULL) {
  afun <- make_afun(
    a_count_occurrences,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .ungroup_stats = .stats
  )

  analyze(
    lyt = lyt,
    vars = vars,
    afun = afun,
    show_labels = "hidden",
    extra_args = list(...)
  )
}
