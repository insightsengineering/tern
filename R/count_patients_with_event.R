#' Count the Number of Patients with a Particular Event
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The primary analysis variable `.var` denotes the unique patient identifier.
#'
#' @inheritParams argument_convention
#' @param filters (`character`)\cr a character vector specifying the column names and flag variables
#'   to be used for counting the number of unique identifiers satisfying such conditions.
#'   Multiple column names and flags are accepted in this format
#'   `c("column_name1" = "flag1", "column_name2" = "flag2")`.
#'   Note that only equality is being accepted as condition.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("count_patients_with_event")`
#'   to see available statistics for this function.
#'
#' @seealso [count_patients_with_flags]
#'
#' @name count_patients_with_event
#' @order 1
NULL

#' @describeIn count_patients_with_event Statistics function which counts the number of patients for which
#'   the defined event has occurred.
#'
#' @inheritParams analyze_variables
#' @param .var (`character`)\cr name of the column that contains the unique identifier.
#'
#' @return
#' * `s_count_patients_with_event()` returns the count and fraction of unique identifiers with the defined event.
#'
#' @examples
#' # `s_count_patients_with_event()`
#'
#' s_count_patients_with_event(
#'   tern_ex_adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y")
#' )
#'
#' s_count_patients_with_event(
#'   tern_ex_adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL")
#' )
#'
#' s_count_patients_with_event(
#'   tern_ex_adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
#'   denom = "N_col",
#'   .N_col = 456
#' )
#'
#' @export
s_count_patients_with_event <- function(df,
                                        .var,
                                        filters,
                                        .N_col, # nolint
                                        .N_row, # nolint
                                        denom = c("n", "N_row", "N_col")) {
  col_names <- names(filters)
  filter_values <- filters

  checkmate::assert_subset(col_names, colnames(df))

  temp <- Map(
    function(x, y) which(df[[x]] == y),
    col_names,
    filter_values
  )
  position_satisfy_filters <- Reduce(intersect, temp)
  id_satisfy_filters <- as.character(unique(df[position_satisfy_filters, ][[.var]]))
  result <- s_count_values(
    as.character(unique(df[[.var]])),
    id_satisfy_filters,
    denom = denom,
    .N_col = .N_col,
    .N_row = .N_row
  )
  result
}

#' @describeIn count_patients_with_event Formatted analysis function which is used as `afun`
#'   in `count_patients_with_event()`.
#'
#' @return
#' * `a_count_patients_with_event()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' # `a_count_patients_with_event()`
#'
#' a_count_patients_with_event(
#'   tern_ex_adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y"),
#'   .N_col = 100,
#'   .N_row = 100
#' )
#'
#' @export
a_count_patients_with_event <- make_afun(
  s_count_patients_with_event,
  .formats = c(count_fraction = format_count_fraction_fixed_dp)
)

#' @describeIn count_patients_with_event Layout-creating function which can take statistics function
#'   arguments and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_patients_with_event()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_patients_with_event()` to the table layout.
#'
#' @examples
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
#'
#' build_table(lyt, tern_ex_adae, alt_counts_df = tern_ex_adsl)
#'
#' @export
#' @order 2
count_patients_with_event <- function(lyt,
                                      vars,
                                      filters,
                                      riskdiff = FALSE,
                                      na_str = default_na_str(),
                                      nested = TRUE,
                                      ...,
                                      table_names = vars,
                                      .stats = "count_fraction",
                                      .formats = NULL,
                                      .labels = NULL,
                                      .indent_mods = NULL) {
  checkmate::assert_flag(riskdiff)

  s_args <- list(filters = filters, ...)

  afun <- make_afun(
    a_count_patients_with_event,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )

  extra_args <- if (isFALSE(riskdiff)) {
    s_args
  } else {
    list(
      afun = list("s_count_patients_with_event" = afun),
      .stats = .stats,
      .indent_mods = .indent_mods,
      s_args = s_args
    )
  }

  analyze(
    lyt,
    vars,
    afun = ifelse(isFALSE(riskdiff), afun, afun_riskdiff),
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    show_labels = ifelse(length(vars) > 1, "visible", "hidden"),
    table_names = table_names
  )
}
