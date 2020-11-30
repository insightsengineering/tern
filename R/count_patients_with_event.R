#' Count the Number of Patients with a Particular Event
#'
#' The primary analysis variable `.var` denotes the unique patient identifier.
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
#' library(random.cdisc.data)
#' library(dplyr)
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
  result <- s_count_values(
    as.character(unique(df[[.var]])),
    id_satisfy_filters,
    denom = denom,
    .N_col = .N_col,
    .N_row = .N_row
  )

  result
}

#' @describeIn count_patients_with_event Formatted Analysis function which can be further
#'   customized by calling [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' # `a_count_patients_with_event()`
#' a_count_patients_with_event(
#'   adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y"),
#'   .N_col = 100,
#'   .N_row = 100
#' )
#'
a_count_patients_with_event <- make_afun(
  s_count_patients_with_event,
  .formats = c(count_fraction = "xx (xx.xx%)")
)

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
#' lyt <- basic_table() %>%
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
#'     .labels = c(count_fraction = "Total number of patients with at least one adverse event"),
#'     table_names = "tbl_all"
#'   ) %>%
#'   count_patients_with_event(
#'     "SUBJID",
#'     filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
#'     .labels = c(count_fraction = "Total number of patients with fatal AEs"),
#'     table_names = "tbl_fatal"
#'   ) %>%
#'   count_patients_with_event(
#'     "SUBJID",
#'     filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL", "AEREL" = "Y"),
#'     .labels = c(count_fraction = "Total number of patients with related fatal AEs"),
#'     .indent_mods = c(count_fraction = 2L),
#'     table_names = "tbl_rel_fatal"
#'   )
#' build_table(lyt, adae, col_counts = table(adsl$ARM))
#'
count_patients_with_event <- function(lyt,
                                      vars,
                                      ...,
                                      table_names = vars,
                                      .stats = "count_fraction",
                                      .formats = NULL,
                                      .labels = NULL,
                                      .indent_mods = NULL) {

  afun <- make_afun(
    a_count_patients_with_event,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )

  analyze(
    lyt,
    vars,
    afun = afun,
    extra_args = list(...),
    show_labels = ifelse(length(vars) > 1, "visible", "hidden"),
    table_names = table_names
  )
}

#' @describeIn count_patients_with_event Analyze Function which is a wrapper for [count_patients_with_event()].
#'  Adds the count statistics to the input layout for multiple flag variables at once.
#'
#' @inheritParams argument_convention
#' @param flag_variables (`character`)\cr names of `logical` variables from analysis dataset
#'   used for counting the number of unique identifiers. If `flag_variables` is named, the names
#'   are used as row labels in table generated by the layout.
#'
#' @export
#' @examples
#'
#' # `count_patients_with_flags()`
#' # Add labelled flag variables to analysis dataset.
#' adae <- adae %>%
#'   mutate(
#'     fl1 = TRUE,
#'     fl2 = TRTEMFL == "Y",
#'     fl3 = TRTEMFL == "Y" & AEOUT == "FATAL",
#'     fl4 = TRTEMFL == "Y" & AEOUT == "FATAL" & AEREL == "Y"
#'   ) %>%
#'   var_relabel(
#'     fl1 = "Total AEs",
#'     fl2 = "Total number of patients with at least one adverse event",
#'     fl3 = "Total number of patients with fatal AEs",
#'     fl4 = "Total number of patients with related fatal AEs"
#'  )
#'
#' lyt2 <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_patients_with_flags(
#'     "SUBJID",
#'     flag_variables = var_labels(adae[, c("fl1", "fl2", "fl3", "fl4")]),
#'   )
#'  build_table(lyt2, adae, col_counts = table(adsl$ARM))
#'
count_patients_with_flags <- function(lyt,
                                      var,
                                      flag_variables,
                                      table_names = flag_variables,
                                      .stats = "count_fraction",
                                      .formats = c(count_fraction = "xx (xx.xx%)"),
                                      .indent_mods = 0) {

  assert_that(
    length(flag_variables) == length(table_names)
  )

  if (is.null(names(flag_variables))) {
    flag_variables <- setNames(flag_variables, flag_variables)
  }

  for (i in seq_along(flag_variables)) {

    flag_var <- names(flag_variables[i])

    lyt <- count_patients_with_event(
      lyt,
      vars = var,
      filters = setNames(TRUE, flag_var),
      denom = "N_col",
      table_names = table_names[i],
      .stats = .stats,
      .formats = .formats,
      .labels = unname(flag_variables[i]),
      .indent_mods = .indent_mods
    )
  }

  lyt
}
