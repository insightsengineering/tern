#' Count the Number of Patients with a Particular Event
#'
#' The primary analysis variable `.var` denotes the unique patient identifier.
#'
#' @name count_patients_with_event
#'
#' @description `r lifecycle::badge("stable")`
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
#' library(dplyr)
#'
#' # `s_count_patients_with_event()`
#'
#' s_count_patients_with_event(
#'   tern_ex_adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y")
#' )
#' s_count_patients_with_event(
#'   tern_ex_adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL")
#' )
#' s_count_patients_with_event(
#'   tern_ex_adae,
#'   .var = "SUBJID",
#'   filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
#'   denom = "N_col",
#'   .N_col = 456
#' )
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

#' @describeIn count_patients_with_event Formatted Analysis function which can be further
#'   customized by calling [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
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
a_count_patients_with_event <- make_afun(
  s_count_patients_with_event,
  .formats = c(count_fraction = format_count_fraction_fixed_dp)
)

#' @describeIn count_patients_with_event Analyze Function which adds the count statistics
#' to the input layout. Note that additional formatting arguments can be used here.
#'
#' @inheritParams argument_convention
#'
#' @export
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
#' build_table(lyt, tern_ex_adae, alt_counts_df = tern_ex_adsl)
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

#' @describeIn count_patients_with_event Statistics function that returns the number and the fraction
#'   of unique identifiers with each particular flag. Returns a list of totals, counts, counts and
#'   fractions with one element per flag.
#' @inheritParams argument_convention
#' @param .var (`character`)\cr name of the column that contains the unique identifier.
#' @param flag_variables (`character`)\cr a character vector specifying the names of `logical`
#'   variables from analysis dataset used for counting the number of unique identifiers.
#' @inheritParams summarize_variables
#' @export
#'
#' @examples
#' # `s_count_patients_with_flags()`
#'
#' # Add labelled flag variables to analysis dataset.
#' adae <- tern_ex_adae %>%
#'   mutate(
#'     fl1 = TRUE,
#'     fl2 = TRTEMFL == "Y",
#'     fl3 = TRTEMFL == "Y" & AEOUT == "FATAL",
#'     fl4 = TRTEMFL == "Y" & AEOUT == "FATAL" & AEREL == "Y"
#'   )
#' labels <- c(
#'   "fl1" = "Total AEs",
#'   "fl2" = "Total number of patients with at least one adverse event",
#'   "fl3" = "Total number of patients with fatal AEs",
#'   "fl4" = "Total number of patients with related fatal AEs"
#' )
#' formatters::var_labels(adae)[names(labels)] <- labels
#'
#' s_count_patients_with_flags(
#'   adae,
#'   "SUBJID",
#'   flag_variables = c("fl1", "fl2", "fl3", "fl4"),
#'   denom = "N_col",
#'   .N_col = 1000
#' )
s_count_patients_with_flags <- function(df,
                                        .var,
                                        flag_variables,
                                        .N_col, # nolint
                                        .N_row, # nolint
                                        denom = c("n", "N_row", "N_col")) {
  if (is.null(names(flag_variables))) flag_variables <- stats::setNames(flag_variables, flag_variables)
  flag_names <- unname(flag_variables)
  flag_variables <- names(flag_variables)

  checkmate::assert_subset(flag_variables, colnames(df))
  temp <- sapply(flag_variables, function(x) {
    tmp <- Map(function(y) which(df[[y]]), x)
    position_satisfy_flags <- Reduce(intersect, tmp)
    id_satisfy_flags <- as.character(unique(df[position_satisfy_flags, ][[.var]]))
    s_count_values(
      as.character(unique(df[[.var]])),
      id_satisfy_flags,
      denom = denom,
      .N_col = .N_col,
      .N_row = .N_row
    )
  })
  colnames(temp) <- flag_names
  temp <- data.frame(t(temp))
  result <- temp %>% as.list()
  if (length(flag_variables) == 1) {
    for (i in 1:3) names(result[[i]]) <- flag_names[1]
  }
  result
}

#' @describeIn count_patients_with_event Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' #  We need to ungroup `count_fraction` first so that the `rtables` formatting
#' # function `format_count_fraction()` can be applied correctly.
#'
#' # `a_count_patients_with_flags()`
#'
#' afun <- make_afun(a_count_patients_with_flags,
#'   .stats = "count_fraction",
#'   .ungroup_stats = "count_fraction"
#' )
#' afun(
#'   adae,
#'   .N_col = 10L, # nolint
#'   .N_row = 10L,
#'   .var = "USUBJID",
#'   flag_variables = c("fl1", "fl2", "fl3", "fl4")
#' )
a_count_patients_with_flags <- make_afun(
  s_count_patients_with_flags,
  .formats = c("count_fraction" = format_count_fraction_fixed_dp)
)

#' @describeIn count_patients_with_event Analyze Function which is a modified version of [count_patients_with_event()].
#'  Adds the count statistics to the input layout for multiple flag variables at once.
#'
#' @inheritParams argument_convention
#'
#' @export
#'
#' @examples
#' # `count_patients_with_flags()`
#'
#' lyt2 <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_patients_with_flags(
#'     "SUBJID",
#'     flag_variables = formatters::var_labels(adae[, c("fl1", "fl2", "fl3", "fl4")]),
#'     denom = "N_col"
#'   )
#' build_table(lyt2, adae, alt_counts_df = tern_ex_adsl)
count_patients_with_flags <- function(lyt,
                                      var,
                                      flag_variables,
                                      var_labels = var,
                                      show_labels = "hidden",
                                      riskdiff = NULL,
                                      ...,
                                      table_names = paste0("tbl_flags_", var),
                                      .stats = "count_fraction",
                                      .formats = NULL,
                                      .indent_mods = NULL) {
  afun <- make_afun(
    a_count_patients_with_flags,
    flag_variables = flag_variables,
    .stats = .stats,
    .formats = .formats,
    .indent_mods = .indent_mods,
    .ungroup_stats = .stats
  )

  afun_riskdiff <- function(df, .var, .N_col, .N_row, .spl_context) { # nolint
    checkmate::assert_names(riskdiff, permutation.of = c("arm_x", "arm_y"))
    n_spl <- length(.spl_context$split)
    n_riskdiff_col <- sum(.spl_context[[riskdiff$arm_x]][[n_spl]], .spl_context[[riskdiff$arm_y]][[n_spl]])
    if (.spl_context$cur_col_n[n_spl] == n_riskdiff_col) {
      N_col_x <- round(.N_col / 2) # fix value after rtables#517 # nolint
      N_col_y <- round(.N_col / 2) # fix value after rtables#517 # nolint
      s_x <- s_count_patients_with_flags(df[df$ARM == riskdiff$arm_x, ], .var, flag_variables, N_col_x, .N_row, "N_col")
      s_y <- s_count_patients_with_flags(df[df$ARM == riskdiff$arm_y, ], .var, flag_variables, N_col_y, .N_row, "N_col")
      rd_ci <- rep(stat_propdiff_ci(
        lapply(s_x["count_fraction"]$count_fraction, `[`, 2), lapply(s_y["count_fraction"]$count_fraction, `[`, 2),
        N_col_x, N_col_y, flag_variables
      ), length(.stats))
      in_rows(.list = rd_ci, .formats = "xx.x (xx.x - xx.x)", .indent_mods = .indent_mods)
    } else {
      afun(
        df = df, .var = .var, flag_variables = flag_variables, .N_col = .N_col, .N_row = .N_row, denom = "N_col", ...
      )
    }
  }

  lyt <- analyze(
    lyt = lyt,
    vars = var,
    var_labels = var_labels,
    show_labels = show_labels,
    afun = ifelse(is.null(riskdiff), afun, afun_riskdiff),
    table_names = table_names,
    extra_args = list(...)
  )

  lyt
}
