#' Count number of patients and sum exposure across all patients in columns
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [analyze_patients_exposure_in_cols()] creates a layout element to count total numbers of
#' patients and sum an analysis value (i.e. exposure) across all patients in columns.
#'
#' The primary analysis variable `ex_var` is the exposure variable used to calculate the `sum_exposure` statistic. The
#' `id` variable is used to uniquely identify patients in the data such that only unique patients are counted in the
#' `n_patients` statistic, and the `var` variable is used to create a row split if needed. The percentage returned as
#' part of the `n_patients` statistic is the proportion of all records that correspond to a unique patient.
#'
#' The summarize function [summarize_patients_exposure_in_cols()] performs the same function as
#' [analyze_patients_exposure_in_cols()] except it creates content rows, not data rows, to summarize the current table
#' row/column context and operates on the level of the latest row split or the root of the table if no row splits have
#' occurred.
#'
#' If a column split has not yet been performed in the table, `col_split` must be set to `TRUE` for the first call of
#' [analyze_patients_exposure_in_cols()] or [summarize_patients_exposure_in_cols()].
#'
#' @inheritParams argument_convention
#' @param ex_var (`string`)\cr name of the variable in `df` containing exposure values.
#' @param custom_label (`string` or `NULL`)\cr if provided and `labelstr` is empty, this will be used as label.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("analyze_patients_exposure_in_cols"), type = "sh")``
#'
#' @name summarize_patients_exposure_in_cols
#' @order 1
NULL

#' @describeIn summarize_patients_exposure_in_cols Statistics function which counts numbers
#'   of patients and the sum of exposure across all patients.
#'
#' @return
#' * `s_count_patients_sum_exposure()` returns a named `list` with the statistics:
#'   * `n_patients`: Number of unique patients in `df`.
#'   * `sum_exposure`: Sum of `ex_var` across all patients in `df`.
#'
#' @keywords internal
s_count_patients_sum_exposure <- function(df,
                                          labelstr = "",
                                          .stats = c("n_patients", "sum_exposure"),
                                          .N_col, # nolint
                                          ...,
                                          ex_var = "AVAL",
                                          id = "USUBJID",
                                          custom_label = NULL,
                                          var_level = NULL) {
  assert_df_with_variables(df, list(ex_var = ex_var, id = id))
  checkmate::assert_string(id)
  checkmate::assert_string(labelstr)
  checkmate::assert_string(custom_label, null.ok = TRUE)
  checkmate::assert_numeric(df[[ex_var]])
  checkmate::assert_true(all(.stats %in% c("n_patients", "sum_exposure")))

  row_label <- if (labelstr != "") {
    labelstr
  } else if (!is.null(var_level)) {
    var_level
  } else if (!is.null(custom_label)) {
    custom_label
  } else {
    "Total patients numbers/person time"
  }

  y <- list()

  if ("n_patients" %in% .stats) {
    y$n_patients <-
      formatters::with_label(
        s_num_patients_content(
          df = df,
          .N_col = .N_col, # nolint
          .var = id,
          labelstr = ""
        )$unique,
        row_label
      )
  }
  if ("sum_exposure" %in% .stats) {
    y$sum_exposure <- formatters::with_label(sum(df[[ex_var]]), row_label)
  }
  y
}

#' @describeIn summarize_patients_exposure_in_cols Analysis function which is used as `afun` in
#'   [rtables::analyze_colvars()] within `analyze_patients_exposure_in_cols()` and as `cfun` in
#'   [rtables::summarize_row_groups()] within `summarize_patients_exposure_in_cols()`.
#'
#' @return
#' * `a_count_patients_sum_exposure()` returns formatted [rtables::CellValue()].
#'
#' @keywords internal
a_count_patients_sum_exposure <- function(df,
                                          labelstr = "",
                                          ...,
                                          .stats = NULL,
                                          .stat_names = NULL,
                                          .formats = NULL,
                                          .labels = NULL,
                                          .indent_mods = NULL) {
  checkmate::assert_character(.stats, len = 1)

  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)
  extra_afun_params <- retrieve_extra_afun_params(names(dots_extra_args$.additional_fun_parameters))
  dots_extra_args$.additional_fun_parameters <- NULL

  add_total_level <- dots_extra_args$add_total_level
  checkmate::assert_flag(add_total_level)

  var <- dots_extra_args$var
  if (!is.null(var)) {
    assert_df_with_variables(df, list(var = var))
    df[[var]] <- as.factor(df[[var]])
  }

  x_stats <- list()
  if (!is.null(var)) {
    for (lvl in levels(df[[var]])) {
      x_stats_i <- .apply_stat_functions(
        default_stat_fnc = s_count_patients_sum_exposure,
        custom_stat_fnc_list = NULL,
        args_list = c(
          df = list(subset(df, get(var) == lvl)),
          labelstr = list(labelstr),
          var_level = lvl,
          extra_afun_params,
          dots_extra_args
        )
      )
      x_stats[[.stats]][[lvl]] <- x_stats_i[[.stats]]
    }
  }

  if (add_total_level || is.null(var)) {
    x_stats_total <- .apply_stat_functions(
      default_stat_fnc = s_count_patients_sum_exposure,
      custom_stat_fnc_list = NULL,
      args_list = c(
        df = list(df),
        labelstr = list(labelstr),
        extra_afun_params,
        dots_extra_args
      )
    )
    x_stats[[.stats]][["Total"]] <- x_stats_total[[.stats]]
  }

  # Fill in formatting defaults
  .stats <- get_stats("analyze_patients_exposure_in_cols", stats_in = .stats)
  x_stats <- x_stats[.stats]
  levels_per_stats <- lapply(x_stats, names)
  .formats <- get_formats_from_stats(.stats, .formats, levels_per_stats)
  .labels <- get_labels_from_stats(
    .stats, .labels, levels_per_stats,
    tern_defaults = c(lapply(x_stats[[1]], attr, "label"), tern_default_labels)
  )
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods, levels_per_stats)

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, .df_row, .var)

  in_rows(
    .list = x_stats %>% .unlist_keep_nulls(),
    .formats = .formats,
    .names = .labels %>% .unlist_keep_nulls(),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

#' @describeIn summarize_patients_exposure_in_cols Layout-creating function which can take statistics
#'   function arguments and additional format arguments. This function is a wrapper for
#'   [rtables::split_cols_by_multivar()] and [rtables::summarize_row_groups()].
#'
#' @return
#' * `summarize_patients_exposure_in_cols()` returns a layout object suitable for passing to further
#'   layouting functions, or to [rtables::build_table()]. Adding this function to an `rtable` layout will
#'   add formatted content rows, with the statistics from `s_count_patients_sum_exposure()` arranged in
#'   columns, to the table layout.
#'
#' @examples
#' lyt5 <- basic_table() %>%
#'   summarize_patients_exposure_in_cols(var = "AVAL", col_split = TRUE)
#'
#' result5 <- build_table(lyt5, df = df, alt_counts_df = adsl)
#' result5
#'
#' lyt6 <- basic_table() %>%
#'   summarize_patients_exposure_in_cols(var = "AVAL", col_split = TRUE, .stats = "sum_exposure")
#'
#' result6 <- build_table(lyt6, df = df, alt_counts_df = adsl)
#' result6
#'
#' @export
#' @order 3
summarize_patients_exposure_in_cols <- function(lyt,
                                                var,
                                                ex_var = "AVAL",
                                                id = "USUBJID",
                                                add_total_level = FALSE,
                                                custom_label = NULL,
                                                col_split = TRUE,
                                                na_str = default_na_str(),
                                                ...,
                                                .stats = c("n_patients", "sum_exposure"),
                                                .stat_names = NULL,
                                                .formats = NULL,
                                                .labels = c(n_patients = "Patients", sum_exposure = "Person time"),
                                                .indent_mods = NULL) {
  # Process standard extra arguments
  extra_args <- list()
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  col_labels <- unlist(.labels[.stats])
  .labels <- .labels[!names(.labels) %in% c("n_patients", "sum_exposure")]
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    ex_var = ex_var, id = id, add_total_level = add_total_level, custom_label = custom_label,
    ...
  )

  # Adding additional info from layout to analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_count_patients_sum_exposure) <- c(
    formals(a_count_patients_sum_exposure), extra_args[[".additional_fun_parameters"]]
  )

  if (col_split) {
    lyt <- split_cols_by_multivar(
      lyt = lyt,
      vars = rep(var, length(.stats)),
      varlabels = col_labels,
      extra_args = list(.stats = .stats)
    )
  }
  summarize_row_groups(
    lyt = lyt,
    var = var,
    cfun = a_count_patients_sum_exposure,
    na_str = na_str,
    extra_args = extra_args
  )
}

#' @describeIn summarize_patients_exposure_in_cols Layout-creating function which can take statistics
#'   function arguments and additional format arguments. This function is a wrapper for
#'   [rtables::split_cols_by_multivar()] and [rtables::analyze_colvars()].
#'
#' @param col_split (`flag`)\cr whether the columns should be split. Set to `FALSE` when the required
#'   column split has been done already earlier in the layout pipe.
#'
#' @return
#' * `analyze_patients_exposure_in_cols()` returns a layout object suitable for passing to further
#'   layouting functions, or to [rtables::build_table()]. Adding this function to an `rtable` layout will
#'   add formatted data rows, with the statistics from `s_count_patients_sum_exposure()` arranged in
#'   columns, to the table layout.
#'
#' @note As opposed to [summarize_patients_exposure_in_cols()] which generates content rows,
#'   `analyze_patients_exposure_in_cols()` generates data rows which will _not_ be repeated on multiple
#'   pages when pagination is used.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   USUBJID = c(paste("id", seq(1, 12), sep = "")),
#'   ARMCD = c(rep("ARM A", 6), rep("ARM B", 6)),
#'   SEX = c(rep("Female", 6), rep("Male", 6)),
#'   AVAL = as.numeric(sample(seq(1, 20), 12)),
#'   stringsAsFactors = TRUE
#' )
#' adsl <- data.frame(
#'   USUBJID = c(paste("id", seq(1, 12), sep = "")),
#'   ARMCD = c(rep("ARM A", 2), rep("ARM B", 2)),
#'   SEX = c(rep("Female", 2), rep("Male", 2)),
#'   stringsAsFactors = TRUE
#' )
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARMCD", split_fun = add_overall_level("Total", first = FALSE)) %>%
#'   summarize_patients_exposure_in_cols(var = "AVAL", col_split = TRUE) %>%
#'   analyze_patients_exposure_in_cols(var = "SEX", col_split = FALSE)
#' result <- build_table(lyt, df = df, alt_counts_df = adsl)
#' result
#'
#' lyt2 <- basic_table() %>%
#'   split_cols_by("ARMCD", split_fun = add_overall_level("Total", first = FALSE)) %>%
#'   summarize_patients_exposure_in_cols(
#'     var = "AVAL", col_split = TRUE,
#'     .stats = "n_patients", custom_label = "some custom label"
#'   ) %>%
#'   analyze_patients_exposure_in_cols(var = "SEX", col_split = FALSE, ex_var = "AVAL")
#' result2 <- build_table(lyt2, df = df, alt_counts_df = adsl)
#' result2
#'
#' lyt3 <- basic_table() %>%
#'   analyze_patients_exposure_in_cols(var = "SEX", col_split = TRUE, ex_var = "AVAL")
#' result3 <- build_table(lyt3, df = df, alt_counts_df = adsl)
#' result3
#'
#' # Adding total levels and custom label
#' lyt4 <- basic_table(
#'   show_colcounts = TRUE
#' ) %>%
#'   analyze_patients_exposure_in_cols(
#'     var = "ARMCD",
#'     col_split = TRUE,
#'     add_total_level = TRUE,
#'     custom_label = "TOTAL"
#'   ) %>%
#'   append_topleft(c("", "Sex"))
#'
#' result4 <- build_table(lyt4, df = df, alt_counts_df = adsl)
#' result4
#'
#' @export
#' @order 2
analyze_patients_exposure_in_cols <- function(lyt,
                                              var = NULL,
                                              ex_var = "AVAL",
                                              id = "USUBJID",
                                              add_total_level = FALSE,
                                              custom_label = NULL,
                                              col_split = TRUE,
                                              na_str = default_na_str(),
                                              .stats = c("n_patients", "sum_exposure"),
                                              .stat_names = NULL,
                                              .formats = NULL,
                                              .labels = c(n_patients = "Patients", sum_exposure = "Person time"),
                                              .indent_mods = NULL,
                                              ...) {
  # Process standard extra arguments
  extra_args <- list()
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  col_labels <- unlist(.labels[.stats])
  .labels <- .labels[!names(.labels) %in% c("n_patients", "sum_exposure")]
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    var = var, ex_var = ex_var, id = id, add_total_level = add_total_level, custom_label = custom_label,
    ...
  )

  # Adding additional info from layout to analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_count_patients_sum_exposure) <- c(
    formals(a_count_patients_sum_exposure), extra_args[[".additional_fun_parameters"]]
  )

  if (col_split) {
    lyt <- split_cols_by_multivar(
      lyt = lyt,
      vars = rep(ex_var, length(.stats)),
      varlabels = col_labels,
      extra_args = list(.stats = .stats)
    )
  }

  analyze_colvars(
    lyt = lyt,
    afun = a_count_patients_sum_exposure,
    na_str = na_str,
    extra_args = extra_args
  )
}
