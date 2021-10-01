#' Helper Function for Tabulation of a Single Biomarker Result
#'
#' This is used by [h_tab_surv_one_biomarker()] and [h_tab_rsp_one_biomarker()],
#' please see there for examples.
#'
#' @param df (`data.frame`)\cr results for a single biomarker.
#' @param afuns (named `list` of `function`)\cr analysis functions.
#' @param colvars (`list` with `vars` and `labels`)\cr variables to tabulate and their labels.
#'
#' @return The `rtables` table object.
#' @export
h_tab_one_biomarker <- function(df,
                                afuns,
                                colvars) {
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
    cfun = afuns
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
      child_labels = "visible"
    )

    # Then analyze colvars for each subgroup.
    lyt <- summarize_row_groups(
      lyt = lyt,
      cfun = afuns,
      var = "subgroup"
    )
  }
  build_table(lyt, df = df)
}
