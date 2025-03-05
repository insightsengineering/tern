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
#'   Options are: ``r shQuote(get_stats("count_patients_with_flags"), type = "sh")``
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
                                        .N_col = ncol(df), # nolint
                                        .N_row = nrow(df), # nolint
                                        ...,
                                        flag_variables,
                                        flag_labels = NULL,
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
      x = as.character(unique(df[[.var]])),
      values = id_satisfy_flags,
      denom = denom,
      .N_col = .N_col,
      .N_row = .N_row
    )
  })
  colnames(temp) <- flag_names
  temp <- data.frame(t(temp))
  result <- as.list(temp)
  if (length(flag_variables) == 1) {
    for (i in seq(3)) names(result[[i]]) <- flag_names[1]
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
                                        ...,
                                        .stats = NULL,
                                        .stat_names = NULL,
                                        .formats = NULL,
                                        .labels = NULL,
                                        .indent_mods = NULL) {
  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)
  extra_afun_params <- retrieve_extra_afun_params(names(dots_extra_args$.additional_fun_parameters))
  dots_extra_args$.additional_fun_parameters <- NULL
  flag_variables <- dots_extra_args[["flag_variables"]]
  flag_labels <- dots_extra_args[["flag_labels"]]

  if (is.null(names(flag_variables))) flag_variables <- formatters::var_labels(df, fill = TRUE)[flag_variables]
  if (is.null(flag_labels)) flag_labels <- flag_variables

  # Check for user-defined functions
  default_and_custom_stats_list <- .split_std_from_custom_stats(.stats)
  .stats <- default_and_custom_stats_list$all_stats
  custom_stat_functions <- default_and_custom_stats_list$custom_stats

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_count_patients_with_flags,
    custom_stat_fnc_list = custom_stat_functions,
    args_list = c(
      df = list(df),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in formatting defaults
  .stats <- get_stats("count_patients_with_flags", stats_in = .stats, custom_stats_in = names(custom_stat_functions))
  levels_per_stats <- rep(list(names(flag_variables)), length(.stats)) %>% stats::setNames(.stats)
  .formats <- get_formats_from_stats(.stats, .formats, levels_per_stats)
  .labels <- get_labels_from_stats(
    .stats, .labels, levels_per_stats,
    tern_defaults = flag_labels %>% stats::setNames(names(flag_variables))
  )
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods, levels_per_stats)

  x_stats <- x_stats[.stats] %>% .unlist_keep_nulls()

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, extra_afun_params$.df_row, extra_afun_params$.var)

  # Get and check statistical names
  .stat_names <- get_stat_names(x_stats, .stat_names)

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = names(.labels),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
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
                                      .stat_names = NULL,
                                      .formats = list(count_fraction = format_count_fraction_fixed_dp),
                                      .indent_mods = NULL,
                                      .labels = NULL) {
  checkmate::assert_flag(riskdiff)
  afun <- if (isFALSE(riskdiff)) a_count_patients_with_flags else afun_riskdiff

  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    flag_variables = list(flag_variables), flag_labels = list(flag_labels),
    if (!isFALSE(riskdiff)) list(afun = list("s_count_patients_with_flags" = a_count_patients_with_flags)),
    ...
  )

  # Append additional info from layout to the analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(afun) <- c(formals(afun), extra_args[[".additional_fun_parameters"]])

  analyze(
    lyt = lyt,
    vars = var,
    afun = afun,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names
  )
}
