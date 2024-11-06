#' Count the number of patients with particular flags
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [count_patients_with_flags()] creates a layout element to calculate counts of patients for
#' which user-specified flags are present.
#'
#' This function analyzes primary analysis variable `var` which indicates unique subject identifiers. Flags
#' variables to analyze are specified by the user via the `flag_variables` argument, and must either take value
#' `TRUE` (flag present) or `FALSE` (flag absent) for each record.
#'
#' If there are multiple records with the same flag present for a patient, only one occurrence is counted.
#'
#' @inheritParams argument_convention
#' @param flag_variables (`character`)\cr a vector specifying the names of `logical` variables from analysis dataset
#'   used for counting the number of unique identifiers.
#' @param flag_labels (`character`)\cr vector of labels to use for flag variables. If any labels are also specified via
#'   the `.labels` parameter, the `.labels` values will take precedence and replace these labels.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("count_patients_with_flags"))``
#'
#' @seealso [count_patients_with_event]
#'
#' @name count_patients_with_flags
#' @order 1
NULL

#' @describeIn count_patients_with_flags Statistics function which counts the number of patients for which
#'   a particular flag variable is `TRUE`.
#'
#' @inheritParams analyze_variables
#' @param .var (`string`)\cr name of the column that contains the unique identifier.
#'
#' @note If `flag_labels` is not specified, variables labels will be extracted from `df`. If variables are not
#'   labeled, variable names will be used instead. Alternatively, a named `vector` can be supplied to
#'   `flag_variables` such that within each name-value pair the name corresponds to the variable name and the value is
#'   the label to use for this variable.
#'
#' @return
#' * `s_count_patients_with_flags()` returns the count and the fraction of unique identifiers with each particular
#'   flag as a list of statistics `n`, `count`, `count_fraction`, and `n_blq`, with one element per flag.
#'
#' @examples
#' # `s_count_patients_with_flags()`
#'
#' s_count_patients_with_flags(
#'   adae,
#'   "SUBJID",
#'   flag_variables = c("fl1", "fl2", "fl3", "fl4"),
#'   denom = "N_col",
#'   .N_col = 1000
#' )
#'
#' @export
s_count_patients_with_flags <- function(df,
                                        .var,
                                        flag_variables,
                                        flag_labels = NULL,
                                        .N_col, # nolint
                                        .N_row, # nolint
                                        denom = c("n", "N_col", "N_row")) {
  checkmate::assert_character(flag_variables)
  if (!is.null(flag_labels)) {
    checkmate::assert_character(flag_labels, len = length(flag_variables), any.missing = FALSE)
    flag_names <- flag_labels
  } else {
    if (is.null(names(flag_variables))) {
      flag_names <- formatters::var_labels(df[flag_variables], fill = TRUE)
    } else {
      flag_names <- unname(flag_variables)
      flag_variables <- names(flag_variables)
    }
  }

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

#' @describeIn count_patients_with_flags Formatted analysis function which is used as `afun`
#'   in `count_patients_with_flags()`.
#'
#' @return
#' * `a_count_patients_with_flags()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' a_count_patients_with_flags(
#'   adae,
#'   .N_col = 10L,
#'   .N_row = 10L,
#'   .var = "USUBJID",
#'   flag_variables = c("fl1", "fl2", "fl3", "fl4")
#' )
#'
#' @export
a_count_patients_with_flags <- function(df,
                                        labelstr = "",
                                        flag_variables,
                                        flag_labels = NULL,
                                        denom = c("n", "N_col", "N_row"),
                                        .N_col, # nolint
                                        .N_row, # nolint
                                        .df_row,
                                        .var = NULL,
                                        .stats = NULL,
                                        .formats = NULL,
                                        .labels = NULL,
                                        .indent_mods = NULL,
                                        na_str = default_na_str()) {
  x_stats <- s_count_patients_with_flags(
    df = df, .var = .var, flag_variables = flag_variables, flag_labels = flag_labels,
    .N_col = .N_col, .N_row = .N_row, denom = denom
  )

  if (is.null(unlist(x_stats))) {
    return(NULL)
  }
  x_lvls <- names(x_stats[[1]])

  # Fill in with formatting defaults if needed
  .stats <- get_stats("count_patients_with_flags", stats_in = .stats)
  .formats <- get_formats_from_stats(.stats, .formats)

  # label formatting
  x_nms <- paste(rep(.stats, each = length(flag_variables)), flag_variables, sep = ".")
  new_lbls <- if (!is.null(.labels)) .labels[names(.labels) %in% x_nms] else NULL
  .labels <- get_labels_from_stats(.stats, .labels, row_nms = x_lvls) %>% setNames(x_nms)
  if (!is.null(new_lbls)) {
    which_lbls <- which(names(new_lbls) %in% names(.labels))
    .labels[which_lbls] <- new_lbls
  }

  # indent mod formatting
  indent_stat_def <- if (any(.stats %in% names(.indent_mods))) {
    .indent_mods[.stats[.stats %in% names(.indent_mods)]]
  } else {
    NULL
  }
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods, row_nms = flag_variables)
  if (length(names(.indent_mods)) > 1) {
    .indent_mods <- sapply(names(.indent_mods), function(x) {
      if (.indent_mods[x] == 0 && !is.null(length(indent_stat_def))) {
        idx <- which(names(indent_stat_def) == gsub("\\..*", "", x))
        if (length(idx) > 0) .indent_mods[[x]] <- indent_stat_def[[idx]]
      }
      .indent_mods[x]
    })
  }

  if ("count_fraction_fixed_dp" %in% .stats) x_stats[["count_fraction_fixed_dp"]] <- x_stats[["count_fraction"]]
  x_stats <- x_stats[.stats]

  # Ungroup statistics with values for each level of x
  x_ungrp <- ungroup_stats(x_stats, .formats, .labels, list())
  x_stats <- x_ungrp[["x"]] %>% setNames(x_nms)
  .formats <- x_ungrp[[".formats"]] %>% setNames(x_nms)

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, .df_row, .var)

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = names(.labels),
    .labels = unlist(.labels),
    .indent_mods = .indent_mods,
    .format_na_strs = na_str
  )
}

#' @describeIn count_patients_with_flags Layout-creating function which can take statistics function
#'   arguments and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `count_patients_with_flags()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_count_patients_with_flags()` to the table layout.
#'
#' @examples
#' # Add labelled flag variables to analysis dataset.
#' adae <- tern_ex_adae %>%
#'   dplyr::mutate(
#'     fl1 = TRUE %>% with_label("Total AEs"),
#'     fl2 = (TRTEMFL == "Y") %>%
#'       with_label("Total number of patients with at least one adverse event"),
#'     fl3 = (TRTEMFL == "Y" & AEOUT == "FATAL") %>%
#'       with_label("Total number of patients with fatal AEs"),
#'     fl4 = (TRTEMFL == "Y" & AEOUT == "FATAL" & AEREL == "Y") %>%
#'       with_label("Total number of patients with related fatal AEs")
#'   )
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   add_colcounts() %>%
#'   count_patients_with_flags(
#'     "SUBJID",
#'     flag_variables = c("fl1", "fl2", "fl3", "fl4"),
#'     denom = "N_col"
#'   )
#'
#' build_table(lyt, adae, alt_counts_df = tern_ex_adsl)
#'
#' @export
#' @order 2
count_patients_with_flags <- function(lyt,
                                      var,
                                      flag_variables,
                                      flag_labels = NULL,
                                      var_labels = var,
                                      show_labels = "hidden",
                                      riskdiff = FALSE,
                                      na_str = default_na_str(),
                                      nested = TRUE,
                                      ...,
                                      table_names = paste0("tbl_flags_", var),
                                      .stats = "count_fraction",
                                      .formats = list(count_fraction = format_count_fraction_fixed_dp),
                                      .indent_mods = NULL,
                                      .labels = NULL) {
  checkmate::assert_flag(riskdiff)
  extra_args <- list(
    .stats = .stats, .formats = .formats, .labels = .labels, .indent_mods = .indent_mods, na_str = na_str
  )
  s_args <- list(flag_variables = flag_variables, flag_labels = flag_labels, ...)

  if (isFALSE(riskdiff)) {
    extra_args <- c(extra_args, s_args)
  } else {
    extra_args <- c(
      extra_args,
      list(
        afun = list("s_count_patients_with_flags" = a_count_patients_with_flags),
        s_args = s_args
      )
    )
  }

  analyze(
    lyt = lyt,
    vars = var,
    afun = ifelse(isFALSE(riskdiff), a_count_patients_with_flags, afun_riskdiff),
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args
  )
}
