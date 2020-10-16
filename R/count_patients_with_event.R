#' Count the Number of Patients with a Particular Event
#'
#' The primary analysis variable `.var` denotes the unique patient identifier.
#'
#' @template formatting_arguments
#'
#' @name count_patients_with_event
#'
NULL

#' @describeIn count_patients_with_event Statistics Function that returns the number and the fraction
#'   of unique identifiers with a particular type of event, e.g. the number and the fraction of patients who
#'   had treatment-emergent adverse events. Note that the user can define a new data column containing
#'   the event of interest.
#' @inheritParams argument_convention
#' @param .var (`character`)\cr name of the column that contains the unique identifier.
#' @param filters (`character`)\cr a character vector specifying the column names and flag variables
#'   to be used for counting the number of unique identifiers satisfying such conditions.
#'   Multiple column names and flags are accepted in this format
#'   `c("column_name1" = "flag1", "column_name2" = "flag2")`.
#'   Note that only equality is being accepted as condition.
#' @inheritParams summarize_variables
#'
#' @return [s_count_patients_with_event()] returns the count and fraction of patients with the
#'   defined event.
#'
#' @export
#'
#' @examples
#'
#' library(tern)
#' library(random.cdisc.data)
#' library(dplyr)
#' set.seed(99)
#' adsl <- radsl(cached = TRUE)
#' adae <- radae(cached = TRUE)
#'
#' # `s_count_patients_with_event()`
#'
#' s_count_patients_with_event(
#'   adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y")
#' )
#' s_count_patients_with_event(
#'   adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL")
#' )
#' s_count_patients_with_event(
#'   adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
#'   denom = "N_col",
#'   .N_col = 456
#' )
s_count_patients_with_event <- function(df,
                                        .var,
                                        filters,
                                        .N_col, #nolint
                                        .N_row, #nolint
                                        denom = c("n", "N_row", "N_col")) {
  col_names <- names(filters)
  filter_values <- filters

  assert_that(all(col_names %in% colnames(df)))

  temp <- Map(
    function(x, y) which(df[[x]] == y),
    col_names,
    filter_values
  )
  position_satisfy_filters <- Reduce(intersect, temp)
  id_satisfy_filters <- unique(df[position_satisfy_filters, ][[.var]])
  s_count_values(
    as.character(unique(df[[.var]])),
    id_satisfy_filters,
    denom = denom,
    .N_col = .N_col,
    .N_row = .N_row
  )
}

#' @describeIn count_patients_with_event Analyze Function which adds the count statistics
#' to the input layout. Note that additional formatting arguments can be used here.
#'
#' @inheritParams argument_convention
#'
#' @export
#' @examples
#'
#' # `count_patients_with_event()`
#'
#' l <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_values(
#'     "STUDYID",
#'     values = "AB12345",
#'     .stats = "count",
#'     .labels = c(count = "Total AEs")
#'   ) %>%
#'   count_patients_with_event(
#'     "SUBJID",
#'     filters = c("TRTEMFL" = "Y"),
#'     .labels = c(count_fraction = "Total number of patients with at least one adverse event")
#'   ) %>%
#'   count_patients_with_event(
#'     "SUBJID",
#'     filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
#'     .labels = c(count_fraction = "Total number of patients with fatal AEs")
#'   ) %>%
#'   count_patients_with_event(
#'     "SUBJID",
#'     filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL", "AEREL" = "Y"),
#'     .labels = c(count_fraction = "Total number of patients with related fatal AEs"),
#'     .indent_mods = c(count_fraction = 2L)
#'   )
#' build_table(l, adae, col_counts = table(adsl$ARM))
#'
count_patients_with_event <- function(lyt,
                                      vars,
                                      ...,
                                      .stats = "count_fraction") {

  afun <- format_wrap_df(
    s_count_patients_with_event,
    indent_mods = c(count_fraction = 0L),
    formats = c(count_fraction = "xx (xx.xx%)")
  )
  analyze(
    lyt,
    vars,
    afun = afun,
    extra_args = c(list(.stats = .stats), list(...)),
    show_labels = ifelse(length(vars) > 1, "visible", "hidden")
  )
}
