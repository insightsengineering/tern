#' Helper Function for Tabulation of a Single Biomarker Result
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Please see [h_tab_surv_one_biomarker()] and [h_tab_rsp_one_biomarker()], which use this function for examples.
#' This function is a wrapper for [rtables::summarize_row_groups()].
#'
#' @inheritParams argument_convention
#' @param df (`data.frame`)\cr results for a single biomarker.
#' @param afuns (named `list` of `function`)\cr analysis functions.
#' @param colvars (`list` with `vars` and `labels`)\cr variables to tabulate and their labels.
#'
#' @return An `rtables` table object with statistics in columns.
#'
#' @export
h_tab_one_biomarker <- function(df,
                                afuns,
                                colvars,
                                na_str = default_na_str(),
                                .indent_mods = 0L,
                                ...) {
  extra_args <- list(...)

  lyt <- basic_table()

  # Row split by row type - only keep the content rows here.
  lyt <- split_rows_by(
    lyt = lyt,
    var = "row_type",
    split_fun = keep_split_levels("content"),
    nested = FALSE
  )

  # Summarize rows with all patients.
  lyt <- summarize_row_groups(
    lyt = lyt,
    var = "var_label",
    cfun = afuns,
    na_str = na_str,
    indent_mod = .indent_mods,
    extra_args = extra_args
  )

  # Split cols by the multiple variables to populate into columns.
  lyt <- split_cols_by_multivar(
    lyt = lyt,
    vars = colvars$vars,
    varlabels = colvars$labels
  )

  # If there is any subgroup variables, we extend the layout accordingly.
  if ("analysis" %in% df$row_type) {
    # Now only continue with the subgroup rows.
    lyt <- split_rows_by(
      lyt = lyt,
      var = "row_type",
      split_fun = keep_split_levels("analysis"),
      nested = FALSE,
      child_labels = "hidden"
    )

    # Split by the subgroup variable.
    lyt <- split_rows_by(
      lyt = lyt,
      var = "var",
      labels_var = "var_label",
      nested = TRUE,
      child_labels = "visible",
      indent_mod = .indent_mods * 2
    )

    # Then analyze colvars for each subgroup.
    lyt <- summarize_row_groups(
      lyt = lyt,
      cfun = afuns,
      var = "subgroup",
      na_str = na_str,
      extra_args = extra_args
    )
  }
  build_table(lyt, df = df)
}
