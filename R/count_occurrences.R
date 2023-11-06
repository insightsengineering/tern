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
#' @param .stats (`character`)\cr statistics to select for the table. Run `get_stats("count_occurrences")`
#'   to see available statistics for this function.
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
#' a_count_occurrences(
#'   df,
#'   .N_col = N_per_col,
#'   .df_row = df,
#'   .var = "MHDECOD",
#'   id = "USUBJID"
#' )
#'
#' @export
a_count_occurrences <- function(df,
                                labelstr = "",
                                id = "USUBJID",
                                denom = c("N_col", "n"),
                                drop = TRUE,
                                .N_col, # nolint
                                .var = NULL,
                                .df_row = NULL,
                                .stats = NULL,
                                .formats = NULL,
                                .labels = NULL,
                                .indent_mods = NULL,
                                na_str = NA_character_) {
  denom <- match.arg(denom)
  x_stats <- s_count_occurrences(
    df = df, denom = denom, .N_col = .N_col, .df_row = .df_row, drop = drop, .var = .var, id = id
  )
  if (is.null(unlist(x_stats))) {
    return(NULL)
  }
  x_lvls <- names(x_stats[[1]])

  # Fill in with formatting defaults if needed
  .stats <- get_stats("count_occurrences", stats_in = .stats)
  .formats <- get_formats_from_stats(.stats, .formats)
  .labels <- get_labels_from_stats(.stats, .labels, row_nms = x_lvls)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods, row_nms = x_lvls)

  if ("count_fraction_fixed_dp" %in% .stats) x_stats[["count_fraction_fixed_dp"]] <- x_stats[["count_fraction"]]
  x_stats <- x_stats[.stats]

  # Ungroup statistics with values for each level of x
  x_ungrp <- ungroup_stats(x_stats, .formats, list(), list())
  x_stats <- x_ungrp[["x"]]
  .formats <- x_ungrp[[".formats"]]

  # Auto format handling
  fmt_is_auto <- vapply(.formats, function(ii) is.character(ii) && ii == "auto", logical(1))
  if (any(fmt_is_auto)) {
    res_l_auto <- x_stats[fmt_is_auto]
    tmp_dt_var <- .df_row[[.var]] # xxx this can be extended for the WHOLE data or single facets
    .formats[fmt_is_auto] <- lapply(seq_along(res_l_auto), function(rla) {
      format_auto(tmp_dt_var, names(res_l_auto)[rla])
    })
  }

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = .labels,
    .labels = .labels,
    .indent_mods = .indent_mods,
    .format_na_strs = na_str
  )
}

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
#'   ARM = rep(c("A", "B"), each = 6),
#'   SEX = c("F", "F", "M", "M", "M", "M", "F", "F", "F", "M", "M", "F")
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
                              riskdiff = FALSE,
                              na_str = NA_character_,
                              nested = TRUE,
                              ...,
                              table_names = vars,
                              .stats = "count_fraction_fixed_dp",
                              .formats = NULL,
                              .labels = NULL,
                              .indent_mods = NULL) {
  checkmate::assert_flag(riskdiff)

  extra_args <- list(
    .stats = .stats, .formats = .formats, .labels = .labels, .indent_mods = .indent_mods, na_str = na_str
  )

  if (isFALSE(riskdiff)) {
    extra_args <- c(extra_args, list(...))
  } else {
    extra_args <- c(
      extra_args,
      list(
        afun = list("s_count_occurrences" = a_count_occurrences),
        s_args = list(...)
      )
    )
  }

  analyze(
    lyt = lyt,
    vars = vars,
    afun = ifelse(isFALSE(riskdiff), a_count_occurrences, afun_riskdiff),
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args
  )
}

#' @describeIn count_occurrences Layout-creating function which can take content function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::summarize_row_groups()].
#'
#' @return
#' * `summarize_occurrences()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted content rows
#'   containing the statistics from `s_count_occurrences()` to the table layout.
#'
#' @examples
#' # Layout creating function with custom format.
#' basic_table() %>%
#'   add_colcounts() %>%
#'   split_rows_by("SEX", child_labels = "visible") %>%
#'   summarize_occurrences(
#'     var = "MHDECOD",
#'     .formats = c("count_fraction" = "xx.xx (xx.xx%)")
#'   ) %>%
#'   build_table(df, alt_counts_df = df_adsl)
#'
#' @export
summarize_occurrences <- function(lyt,
                                  var,
                                  riskdiff = FALSE,
                                  na_str = NA_character_,
                                  ...,
                                  .stats = "count_fraction_fixed_dp",
                                  .formats = NULL,
                                  .indent_mods = NULL,
                                  .labels = NULL) {
  checkmate::assert_flag(riskdiff)

  extra_args <- list(
    .stats = .stats, .formats = .formats, .labels = .labels, .indent_mods = .indent_mods, na_str = na_str
  )

  if (isFALSE(riskdiff)) {
    extra_args <- c(extra_args, list(...))
  } else {
    extra_args <- c(
      extra_args,
      list(
        afun = list("s_count_occurrences" = a_count_occurrences),
        s_args = list(...)
      )
    )
  }

  summarize_row_groups(
    lyt = lyt,
    var = var,
    cfun = ifelse(isFALSE(riskdiff), a_count_occurrences, afun_riskdiff),
    na_str = na_str,
    extra_args = extra_args
  )
}
