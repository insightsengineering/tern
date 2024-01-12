#' Number of Patients
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Count the number of unique and non-unique patients in a column (variable).
#'
#' @inheritParams argument_convention
#' @param count_by (`vector`)\cr optional vector of any type to be combined with `x` when counting `nonunique`
#'   records.
#' @param unique_count_suffix (`logical`)\cr should `"(n)"` suffix be added to `unique_count` labels.
#'   Defaults to `TRUE`.
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("summarize_num_patients")`
#'   to see available statistics for this function.
#'
#' @name summarize_num_patients
#' @order 1
NULL

#' @describeIn summarize_num_patients Statistics function which counts the number of
#'   unique patients, the corresponding percentage taken with respect to the
#'   total number of patients, and the number of non-unique patients.
#'
#' @param x (`character` or `factor`)\cr vector of patient IDs.
#'
#' @return
#' * `s_num_patients()` returns a named `list` of 3 statistics:
#'   * `unique`: Vector of counts and percentages.
#'   * `nonunique`: Vector of counts.
#'   * `unique_count`: Counts.
#'
#' @examples
#' # Use the statistics function to count number of unique and nonunique patients.
#' s_num_patients(x = as.character(c(1, 1, 1, 2, 4, NA)), labelstr = "", .N_col = 6L)
#' s_num_patients(
#'   x = as.character(c(1, 1, 1, 2, 4, NA)),
#'   labelstr = "",
#'   .N_col = 6L,
#'   count_by = c(1, 1, 2, 1, 1, 1)
#' )
#'
#' @export
s_num_patients <- function(x, labelstr, .N_col, count_by = NULL, unique_count_suffix = TRUE) { # nolint

  checkmate::assert_string(labelstr)
  checkmate::assert_count(.N_col)
  checkmate::assert_multi_class(x, classes = c("factor", "character"))
  checkmate::assert_flag(unique_count_suffix)

  count1 <- n_available(unique(x))
  count2 <- n_available(x)

  if (!is.null(count_by)) {
    checkmate::assert_vector(count_by, len = length(x))
    count2 <- n_available(unique(interaction(x, count_by)))
  }

  out <- list(
    unique = formatters::with_label(c(count1, ifelse(count1 == 0 && .N_col == 0, 0, count1 / .N_col)), labelstr),
    nonunique = formatters::with_label(count2, labelstr),
    unique_count = formatters::with_label(
      count1, ifelse(unique_count_suffix, paste0(labelstr, if (nzchar(labelstr)) " ", "(n)"), labelstr)
    )
  )

  out
}

#' @describeIn summarize_num_patients Statistics function which counts the number of unique patients
#'   in a column (variable), the corresponding percentage taken with respect to the total number of
#'   patients, and the number of non-unique patients in the column.
#'
#' @param required (`character` or `NULL`)\cr optional name of a variable that is required to be non-missing.
#'
#' @return
#' * `s_num_patients_content()` returns the same values as `s_num_patients()`.
#'
#' @examples
#' # Count number of unique and non-unique patients.
#'
#' df <- data.frame(
#'   USUBJID = as.character(c(1, 2, 1, 4, NA)),
#'   EVENT = as.character(c(10, 15, 10, 17, 8))
#' )
#' s_num_patients_content(df, .N_col = 5, .var = "USUBJID")
#'
#' df_by_event <- data.frame(
#'   USUBJID = as.character(c(1, 2, 1, 4, NA)),
#'   EVENT = c(10, 15, 10, 17, 8)
#' )
#' s_num_patients_content(df_by_event, .N_col = 5, .var = "USUBJID", count_by = "EVENT")
#'
#' @export
s_num_patients_content <- function(df,
                                   labelstr = "",
                                   .N_col, # nolint
                                   .var,
                                   required = NULL,
                                   count_by = NULL,
                                   unique_count_suffix = TRUE) {
  checkmate::assert_string(.var)
  checkmate::assert_data_frame(df)
  if (is.null(count_by)) {
    assert_df_with_variables(df, list(id = .var))
  } else {
    assert_df_with_variables(df, list(id = .var, count_by = count_by))
  }
  if (!is.null(required)) {
    checkmate::assert_string(required)
    assert_df_with_variables(df, list(required = required))
    df <- df[!is.na(df[[required]]), , drop = FALSE]
  }

  x <- df[[.var]]
  y <- if (is.null(count_by)) NULL else df[[count_by]]

  s_num_patients(
    x = x,
    labelstr = labelstr,
    .N_col = .N_col,
    count_by = y,
    unique_count_suffix = unique_count_suffix
  )
}

c_num_patients <- make_afun(
  s_num_patients_content,
  .stats = c("unique", "nonunique", "unique_count"),
  .formats = c(unique = format_count_fraction_fixed_dp, nonunique = "xx", unique_count = "xx")
)

#' @describeIn summarize_num_patients Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::summarize_row_groups()].
#'
#' @return
#' * `summarize_num_patients()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_num_patients_content()` to the table layout.
#'
#' @export
#' @order 3
summarize_num_patients <- function(lyt,
                                   var,
                                   required = NULL,
                                   count_by = NULL,
                                   unique_count_suffix = TRUE,
                                   na_str = default_na_str(),
                                   .stats = NULL,
                                   .formats = NULL,
                                   .labels = c(
                                     unique = "Number of patients with at least one event",
                                     nonunique = "Number of events"
                                   ),
                                   indent_mod = lifecycle::deprecated(),
                                   .indent_mods = 0L,
                                   riskdiff = FALSE,
                                   ...) {
  checkmate::assert_flag(riskdiff)

  if (lifecycle::is_present(indent_mod)) {
    lifecycle::deprecate_warn("0.8.2", "summarize_num_patients(indent_mod)", "summarize_num_patients(.indent_mods)")
    .indent_mods <- indent_mod
  }

  if (is.null(.stats)) .stats <- c("unique", "nonunique", "unique_count")
  if (length(.labels) > length(.stats)) .labels <- .labels[names(.labels) %in% .stats]

  s_args <- list(required = required, count_by = count_by, unique_count_suffix = unique_count_suffix, ...)

  cfun <- make_afun(
    c_num_patients,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels
  )

  extra_args <- if (isFALSE(riskdiff)) {
    s_args
  } else {
    list(
      afun = list("s_num_patients_content" = cfun),
      .stats = .stats,
      .indent_mods = .indent_mods,
      s_args = s_args
    )
  }

  summarize_row_groups(
    lyt = lyt,
    var = var,
    cfun = ifelse(isFALSE(riskdiff), cfun, afun_riskdiff),
    na_str = na_str,
    extra_args = extra_args,
    indent_mod = .indent_mods
  )
}

#' @describeIn summarize_num_patients Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `analyze_num_patients()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_num_patients_content()` to the table layout.
#'
#' @details In general, functions that starts with `analyze*` are expected to
#'   work like [rtables::analyze()], while functions that starts with `summarize*`
#'   are based upon [rtables::summarize_row_groups()]. The latter provides a
#'   value for each dividing split in the row and column space, but, being it
#'   bound to the fundamental splits, it is repeated by design in every page
#'   when pagination is involved.
#'
#' @note As opposed to [summarize_num_patients()], this function does not repeat the produced rows.
#'
#' @examples
#' df <- data.frame(
#'   USUBJID = as.character(c(1, 2, 1, 4, NA, 6, 6, 8, 9)),
#'   ARM = c("A", "A", "A", "A", "A", "B", "B", "B", "B"),
#'   AGE = c(10, 15, 10, 17, 8, 11, 11, 19, 17)
#' )
#'
#' tbl <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   analyze_num_patients("USUBJID", .stats = c("unique")) %>%
#'   build_table(df)
#'
#' tbl
#'
#' @export
#' @order 2
analyze_num_patients <- function(lyt,
                                 vars,
                                 required = NULL,
                                 count_by = NULL,
                                 unique_count_suffix = TRUE,
                                 na_str = default_na_str(),
                                 nested = TRUE,
                                 .stats = NULL,
                                 .formats = NULL,
                                 .labels = c(
                                   unique = "Number of patients with at least one event",
                                   nonunique = "Number of events"
                                 ),
                                 show_labels = c("default", "visible", "hidden"),
                                 indent_mod = lifecycle::deprecated(),
                                 .indent_mods = 0L,
                                 riskdiff = FALSE,
                                 ...) {
  checkmate::assert_flag(riskdiff)

  if (lifecycle::is_present(indent_mod)) {
    lifecycle::deprecate_warn("0.8.2", "analyze_num_patients(indent_mod)", "analyze_num_patients(.indent_mods)")
    .indent_mods <- indent_mod
  }

  if (is.null(.stats)) .stats <- c("unique", "nonunique", "unique_count")
  if (length(.labels) > length(.stats)) .labels <- .labels[names(.labels) %in% .stats]

  s_args <- list(required = required, count_by = count_by, unique_count_suffix = unique_count_suffix, ...)

  afun <- make_afun(
    c_num_patients,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels
  )

  extra_args <- if (isFALSE(riskdiff)) {
    s_args
  } else {
    list(
      afun = list("s_num_patients_content" = afun),
      .stats = .stats,
      .indent_mods = .indent_mods,
      s_args = s_args
    )
  }

  analyze(
    afun = ifelse(isFALSE(riskdiff), afun, afun_riskdiff),
    lyt = lyt,
    vars = vars,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    show_labels = show_labels,
    indent_mod = .indent_mods
  )
}
