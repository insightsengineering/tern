#' Occurrence Counts
#'
#' Functions for analyzing frequencies and fractions of occurrences for patients with occurrence
#' data. Primary analysis variables are the dictionary terms. All occurrences are counted for total
#' counts. Multiple occurrences within patient at the lowest term level displayed in the table are
#' counted only once. Note that by default occurrences which don't appear in a given row split
#' are dropped from the table and the occurrences in the table are sorted alphabetically per row split.
#' Therefore the corresponding layout needs to use `split_fun = drop_split_levels` in the `split_rows_by`
#' calls. Use `drop = FALSE` if you would like to show all occurrences.
#'
#' @description `r lifecycle::badge("stable")`
#' @inheritParams argument_convention
#'
#' @name count_occurrences
#'
NULL

#' @describeIn count_occurrences Statistics function which counts number of patients that report an
#' occurrence.
#' @param denom (`string`)\cr choice of denominator for patient proportions:\cr
#'   can be `N_col` (total number of patients in this column across rows) or
#'   `n` (number of patients with any occurrences).
#'
#'
#' @returns A list with:
#'   - `count`: list of counts with one element per occurrence
#'   - `count_fraction`: list of counts and fractions with one element per occurrence.
#'   - `fraction`: list of numerators and denominators with one element per occurrence.
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
#'   .N_col = N_per_col,
#'   .df_row = df,
#'   .var = "MHDECOD",
#'   id = "USUBJID"
#' )
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

#' @describeIn count_occurrences Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
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
a_count_occurrences <- make_afun(
  s_count_occurrences,
  .formats = c(count = "xx", count_fraction = format_count_fraction_fixed_dp, fraction = format_fraction_fixed_dp)
)

#' @describeIn count_occurrences Analyze Function that counts occurrences as part of `rtables` layouts.
#'
#' @export
#'
#' @examples
#'
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
count_occurrences <- function(lyt,
                              vars,
                              var_labels = vars,
                              show_labels = "hidden",
                              riskdiff = NULL,
                              ...,
                              table_names = vars,
                              .stats = "count_fraction",
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

  afun_riskdiff <- function(df, .var, .N_col, .spl_context) { # nolint
    checkmate::assert_names(names(riskdiff), permutation.of = c("arm_x", "arm_y"))
    n_spl <- length(.spl_context$split)
    n_riskdiff_col <- sum(.spl_context[[riskdiff$arm_x]][[n_spl]], .spl_context[[riskdiff$arm_y]][[n_spl]])
    if (.spl_context$cur_col_n[n_spl] == n_riskdiff_col) {
      N_col_x <- round(.N_col / 2) # fix value after rtables#517 # nolint
      N_col_y <- round(.N_col / 2) # fix value after rtables#517 # nolint
      s_x <- s_count_occurrences(df[df$ARM == riskdiff$arm_x, ], "N_col", N_col_x, df, .var = .var, ...)
      s_y <- s_count_occurrences(df[df$ARM == riskdiff$arm_y, ], "N_col", N_col_y, df, .var = .var, ...)
      rd_ci <- rep(stat_propdiff_ci(
        lapply(s_x["count_fraction"]$count_fraction, `[`, 2), lapply(s_y["count_fraction"]$count_fraction, `[`, 2),
        N_col_x, N_col_y, names(s_x$count)
      ), length(.stats))
      in_rows(.list = rd_ci, .formats = "xx.x (xx.x - xx.x)", .indent_mods = .indent_mods)
    } else {
      afun(df = df, .var = .var, .df_row = df, .N_col = .N_col, denom = "N_col", ...)
    }
  }

  analyze(
    lyt = lyt,
    vars = vars,
    afun = ifelse(is.null(riskdiff), afun, afun_riskdiff),
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names,
    extra_args = list(...)
  )
}
