#' Helper function for tabulation of a single biomarker result
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Please see [h_tab_surv_one_biomarker()] and [h_tab_rsp_one_biomarker()], which use this function for examples.
#' This function is a wrapper for [rtables::summarize_row_groups()].
#'
#' @inheritParams argument_convention
#' @param df (`data.frame`)\cr results for a single biomarker.
#' @param afuns (named `list` of `function`)\cr analysis functions.
#' @param colvars (named `list`)\cr named list with elements `vars` (variables to tabulate) and `labels` (their labels).
#'
#' @return An `rtables` table object with statistics in columns.
#'
#' @export
h_tab_one_biomarker <- function(df,
                                afuns,
                                colvars,
                                na_str = default_na_str(),
                                ...,
                                .stats = NULL,
                                .stat_names = NULL,
                                .formats = NULL,
                                .labels = NULL,
                                .indent_mods = NULL) {
  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(extra_args, biomarker = TRUE, ...)

  # Adding additional info from layout to analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(afuns) <- c(formals(afuns), extra_args[[".additional_fun_parameters"]])

  # Create "ci" column from "lcl" and "ucl"
  df$ci <- combine_vectors(df$lcl, df$ucl)

  colvars$vars <- intersect(colvars$vars, names(df))
  colvars$labels <- colvars$labels[colvars$vars]

  lyt <- basic_table()

  # Split cols by the multiple variables to populate into columns.
  lyt <- split_cols_by_multivar(
    lyt = lyt,
    vars = colvars$vars,
    varlabels = colvars$labels
  )

  # Add "All Patients" row
  lyt <- split_rows_by(
    lyt = lyt,
    var = "row_type",
    split_fun = keep_split_levels("content"),
    nested = TRUE,
    child_labels = "hidden"
  )
  lyt <- analyze_colvars(
    lyt = lyt,
    afun = afuns,
    na_str = na_str,
    extra_args = c(extra_args)
  )

  # Add analysis rows
  if ("analysis" %in% df$row_type) {
    lyt <- split_rows_by(
      lyt = lyt,
      var = "row_type",
      split_fun = keep_split_levels("analysis"),
      nested = TRUE,
      child_labels = "hidden"
    )
    lyt <- split_rows_by(
      lyt = lyt,
      var = "var_label",
      nested = TRUE,
      indent_mod = 1L
    )
    lyt <- analyze_colvars(
      lyt = lyt,
      afun = afuns,
      na_str = na_str,
      inclNAs = TRUE,
      extra_args = extra_args
    )
  }

  build_table(lyt, df = df)
}
