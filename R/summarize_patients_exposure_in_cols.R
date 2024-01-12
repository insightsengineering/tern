#' Counting Patients Summing Exposure Across All Patients in Columns
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Counting the number of patients and summing analysis value (i.e exposure values) across all patients
#' when a column table layout is required.
#'
#' @inheritParams argument_convention
#' @param ex_var (`character`)\cr name of the variable within `df` containing exposure values.
#' @param custom_label (`string` or `NULL`)\cr if provided and `labelstr` is empty then this will be used as label.
#' @param .stats (`character`)\cr statistics to select for the table. Run
#' `get_stats("analyze_patients_exposure_in_cols")` to see available statistics for this function.
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
                                          ex_var = "AVAL",
                                          id = "USUBJID",
                                          labelstr = "",
                                          .stats = c("n_patients", "sum_exposure"),
                                          .N_col, # nolint
                                          custom_label = NULL) {
  assert_df_with_variables(df, list(ex_var = ex_var, id = id))
  checkmate::assert_string(id)
  checkmate::assert_string(labelstr)
  checkmate::assert_string(custom_label, null.ok = TRUE)
  checkmate::assert_numeric(df[[ex_var]])
  checkmate::assert_true(all(.stats %in% c("n_patients", "sum_exposure")))

  row_label <- if (labelstr != "") {
    labelstr
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
#' @examples
#' a_count_patients_sum_exposure(
#'   df = df,
#'   var = "SEX",
#'   .N_col = nrow(df),
#'   .stats = "n_patients"
#' )
#'
#' @export
a_count_patients_sum_exposure <- function(df,
                                          var = NULL,
                                          ex_var = "AVAL",
                                          id = "USUBJID",
                                          add_total_level = FALSE,
                                          custom_label = NULL,
                                          labelstr = "",
                                          .N_col, # nolint
                                          .stats,
                                          .formats = list(n_patients = "xx (xx.x%)", sum_exposure = "xx")) {
  checkmate::assert_flag(add_total_level)

  if (!is.null(var)) {
    assert_df_with_variables(df, list(var = var))
    df[[var]] <- as.factor(df[[var]])
  }

  y <- list()
  if (is.null(var)) {
    y[[.stats]] <- list(Total = s_count_patients_sum_exposure(
      df = df,
      ex_var = ex_var,
      id = id,
      labelstr = labelstr,
      .N_col = .N_col,
      .stats = .stats,
      custom_label = custom_label
    )[[.stats]])
  } else {
    for (lvl in levels(df[[var]])) {
      y[[.stats]][[lvl]] <- s_count_patients_sum_exposure(
        df = subset(df, get(var) == lvl),
        ex_var = ex_var,
        id = id,
        labelstr = labelstr,
        .N_col = .N_col,
        .stats = .stats,
        custom_label = lvl
      )[[.stats]]
    }
    if (add_total_level) {
      y[[.stats]][["Total"]] <- s_count_patients_sum_exposure(
        df = df,
        ex_var = ex_var,
        id = id,
        labelstr = labelstr,
        .N_col = .N_col,
        .stats = .stats,
        custom_label = custom_label
      )[[.stats]]
    }
  }

  in_rows(.list = y[[.stats]], .formats = .formats[[.stats]])
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
summarize_patients_exposure_in_cols <- function(lyt, # nolint
                                                var,
                                                ex_var = "AVAL",
                                                id = "USUBJID",
                                                add_total_level = FALSE,
                                                custom_label = NULL,
                                                col_split = TRUE,
                                                na_str = default_na_str(),
                                                ...,
                                                .stats = c("n_patients", "sum_exposure"),
                                                .labels = c(n_patients = "Patients", sum_exposure = "Person time"),
                                                .indent_mods = NULL) {
  extra_args <- list(ex_var = ex_var, id = id, add_total_level = add_total_level, custom_label = custom_label, ...)

  if (col_split) {
    lyt <- split_cols_by_multivar(
      lyt = lyt,
      vars = rep(var, length(.stats)),
      varlabels = .labels[.stats],
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
analyze_patients_exposure_in_cols <- function(lyt, # nolint
                                              var = NULL,
                                              ex_var = "AVAL",
                                              id = "USUBJID",
                                              add_total_level = FALSE,
                                              custom_label = NULL,
                                              col_split = TRUE,
                                              na_str = default_na_str(),
                                              .stats = c("n_patients", "sum_exposure"),
                                              .labels = c(n_patients = "Patients", sum_exposure = "Person time"),
                                              .indent_mods = 0L,
                                              ...) {
  extra_args <- list(
    var = var, ex_var = ex_var, id = id, add_total_level = add_total_level, custom_label = custom_label, ...
  )

  if (col_split) {
    lyt <- split_cols_by_multivar(
      lyt = lyt,
      vars = rep(ex_var, length(.stats)),
      varlabels = .labels[.stats],
      extra_args = list(.stats = .stats)
    )
  }
  lyt <- lyt %>% analyze_colvars(
    afun = a_count_patients_sum_exposure,
    indent_mod = .indent_mods,
    na_str = na_str,
    extra_args = extra_args
  )
  lyt
}
