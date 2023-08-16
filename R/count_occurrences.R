#' Occurrence Counts
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Functions for analyzing frequencies and fractions of occurrences for patients with occurrence
#' data. Primary analysis variables are the dictionary terms. All occurrences are counted for total
#' counts. Multiple occurrences within patient at the lowest term level displayed in the table are
#' counted only once.
#'
#' @inheritParams argument_convention
#'
#' @note By default, occurrences which don't appear in a given row split are dropped from the table and
#'   the occurrences in the table are sorted alphabetically per row split. Therefore, the corresponding layout
#'   needs to use `split_fun = drop_split_levels` in the `split_rows_by` calls. Use `drop = FALSE` if you would
#'   like to show all occurrences.
#'
#' @name count_occurrences
NULL

#' @describeIn count_occurrences Statistics function which counts number of patients that report an
#' occurrence.
#'
#' @param denom (`string`)\cr choice of denominator for patient proportions. Can be:
#'   - `N_col`: total number of patients in this column across rows
#'   - `n`: number of patients with any occurrences
#'
#' @return
#' * `s_count_occurrences()` returns a list with:
#'   * `count`: list of counts with one element per occurrence.
#'   * `count_fraction`: list of counts and fractions with one element per occurrence.
#'   * `fraction`: list of numerators and denominators with one element per occurrence.
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
#'   .N_col = N_per_col,
#'   .df_row = df,
#'   .var = "MHDECOD",
#'   id = "USUBJID"
#' )
#'
#' @export
s_count_occurrences <- function(df,
                                denom = c("N_col", "n"),
                                .N_col, # nolint
                                .df_row,
                                drop = TRUE,
                                .var = "MHDECOD",
                                id = "USUBJID") {
  checkmate::assert_flag(drop)
  assert_df_with_variables(df, list(range = .var, id = id))
  checkmate::assert_count(.N_col)
  checkmate::assert_multi_class(df[[.var]], classes = c("factor", "character"))
  checkmate::assert_multi_class(df[[id]], classes = c("factor", "character"))
  denom <- match.arg(denom)

  occurrences <- if (drop) {
    # Note that we don't try to preserve original level order here since a) that would required
    # more time to look up in large original levels and b) that would fail for character input variable.
    occurrence_levels <- sort(unique(.df_row[[.var]]))
    if (length(occurrence_levels) == 0) {
      stop(
        "no empty `.df_row` input allowed when `drop = TRUE`,",
        " please use `split_fun = drop_split_levels` in the `rtables` `split_rows_by` calls"
      )
    }
    factor(df[[.var]], levels = occurrence_levels)
  } else {
    df[[.var]]
  }
  ids <- factor(df[[id]])
  dn <- switch(denom,
    n = nlevels(ids),
    N_col = .N_col
  )
  has_occurrence_per_id <- table(occurrences, ids) > 0
  n_ids_per_occurrence <- as.list(rowSums(has_occurrence_per_id))
  list(
    count = n_ids_per_occurrence,
    count_fraction = lapply(
      n_ids_per_occurrence,
      function(i, denom) {
        if (i == 0 && denom == 0) {
          c(0, 0)
        } else {
          c(i, i / denom)
        }
      },
      denom = dn
    ),
    fraction = lapply(
      n_ids_per_occurrence,
      function(i, denom) c("num" = i, "denom" = denom),
      denom = dn
    )
  )
}

#' @describeIn count_occurrences Formatted analysis function which is used as `afun`
#'   in `count_occurrences()`.
#'
#' @return
#' * `a_count_occurrences()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' #  We need to ungroup `count_fraction` first so that the `rtables` formatting
#' # function `format_count_fraction()` can be applied correctly.
#' afun <- make_afun(a_count_occurrences, .ungroup_stats = c("count", "count_fraction", "fraction"))
#' afun(
#'   df,
#'   .N_col = N_per_col,
#'   .df_row = df,
#'   .var = "MHDECOD",
#'   id = "USUBJID"
#' )
#'
#' @export
a_count_occurrences <- make_afun(
  s_count_occurrences,
  .formats = c(count = "xx", count_fraction = format_count_fraction_fixed_dp, fraction = format_fraction_fixed_dp)
)

#' @describeIn count_occurrences Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_occurrences()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_occurrences()` to the table layout.
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(
#'   USUBJID = as.character(c(
#'     1, 1, 2, 4, 4, 4,
#'     6, 6, 6, 7, 7, 8
#'   )),
#'   MHDECOD = c(
#'     "MH1", "MH2", "MH1", "MH1", "MH1", "MH3",
#'     "MH2", "MH2", "MH3", "MH1", "MH2", "MH4"
#'   ),
#'   ARM = rep(c("A", "B"), each = 6)
#' )
#' df_adsl <- df %>%
#'   select(USUBJID, ARM) %>%
#'   unique()
#'
#' # Create table layout
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_occurrences(vars = "MHDECOD", .stats = c("count_fraction"))
#'
#' # Apply table layout to data and produce `rtable` object
#' lyt %>%
#'   build_table(df, alt_counts_df = df_adsl) %>%
#'   prune_table()
#'
#' @export
count_occurrences <- function(lyt,
                              vars,
                              var_labels = vars,
                              show_labels = "hidden",
                              na_level = NA_character_,
                              nested = TRUE,
                              add_total_num_patients = get_stat("summarize_num_patients")[c(1, 2)],
                              ...,
                              table_names = vars,
                              .stats = get_stat("count_occurrences")[2],
                              .formats = NULL,
                              .labels = NULL,
                              .indent_mods = NULL) {
  # Checking methods
  add_total <- NULL
  if (is.logical(add_total_num_patients)) {
    checkmate::assert_logical(add_total_num_patients)
    if (add_total_num_patients) {
      add_total <- get_stat("summarize_num_patients")
    } else {
      add_total <- NULL
    }
  } else {
    add_total <- add_total_num_patients
  }
  checkmate::assert_character(add_total, null.ok = TRUE, min.len = 1)
  checkmate::assert_subset(add_total, choices = get_stat("summarize_num_patients"), empty.ok = TRUE)

  checkmate::assert_character(.stats, null.ok = TRUE, min.len = 1)
  checkmate::assert_subset(.stats, choices = get_stat("count_occurrences"), empty.ok = FALSE)

  # Checking misc.
  checkmate::assert_flag(nested)
  checkmate::assert_int(.indent_mods, null.ok = TRUE)

  # Checking formats
  if (is.null(.formats)) {
    fmt <- get_format_from_stat(.stats)
    if (!is.null(add_total)) {
      fmt <- c(fmt, get_format_from_stat(add_total))
    }
  } else {
    fmt <- .formats
  }

  # Checking labels
  if (is.null(.labels)) {
    lbl_stats <- get_label_from_stat(.stats)
  } else {
    lbl_stats <- .labels
  }

  afun <- function(x, .spl_context, .N_col, ...) {
        browser()
        # Main statistics
        res <- s_count_occurrences(x, ...)[[.stats]]

        # Add totals before
        if (!is.null(add_total)) {
          res_tot <- s_num_patients_content(x, .var = NULL, ...)[[add_total]]
        }

        # Cell creation
        rcell(res,
              label = lbl_stats,
              format = fmt,
              format_na_str = na_level,
              indent_mod = ifelse(is.null(.indent_mods), 0L, .indent_mods)
        )
    }
#
#   afun <- make_afun(
#     a_count_occurrences,
#     .stats = .stats,
#     .formats = .formats,
#     .labels = .labels,
#     .indent_mods = .indent_mods,
#     .ungroup_stats = .stats
#   )

  analyze(
    lyt = lyt,
    vars = vars,
    afun = afun,
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names,
    nested = nested,
    extra_args = list(...)
  )
}

#' @describeIn count_occurrences Utility function to get valid statistic methods for
#'  different method groups.
#'
#' @param method_group (`string`) \cr Indicates the group of statistical methods that
#'  we need the defaults from.
#'
#' @export
get_stat <- function(method_group) {
  checkmate::assert_string(method_group)
  switch (method_group,
    "count_occurrences" = c("count", "count_fraction", "fraction"),
    "summarize_num_patients" = c("unique", "nonunique", "unique_count"),
    stop(method_group, " is a method_group that has no default statistical method.")
  )
}

#' @describeIn count_occurrences
#'
#' @export
get_format_from_stat <- function(stats) {
  checkmate::assert_character(stats, min.len = 1)
  # XXX this is experimental and needs #983 first
  # it is like tern:::summary_formats() but it should be exported and more general
  default_formats <- tern_default_formats()
  which_fmt <- match(stats, names(default_formats))

  ret <- vector("list", length = length(stats))
  ret[which(!is.na(which_fmt))] <- default_formats[which_fmt]

  setNames(ret, stats)
}

#' @describeIn count_occurrences
#'
#' @export
get_label_from_stat <- function(stats) {
  checkmate::assert_character(stats, min.len = 1)
  default_lbl <- tern_default_labels()
  which_lbl <- match(stats, names(default_lbl))

  ret <- vector("list", length = length(stats))
  ret[which(!is.na(which_lbl))] <- default_lbl[which_lbl]

  setNames(ret, stats)
}
#' @describeIn count_occurrences XXX This will be very general
#'
#' @export
tern_default_formats <- function() {
  c(
    list(
      "count" = "xx",
      "count_fraction" = format_count_fraction_fixed_dp,
      "fraction" = format_fraction_fixed_dp
    ),
    list(
      "unique" = format_count_fraction_fixed_dp,
      "nonunique" = "xx",
      "unique_count" = "xx"
    )
  )
}
#' @describeIn count_occurrences XXX This will be very general
#'
#' @export
tern_default_labels <- function() {
  c(
    list(
      "unique" = "Number of patients with at least one event",
      "nonunique" = "Number of events"
    )
  )
}

