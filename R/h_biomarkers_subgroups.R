## Deprecated ------------------------------------------------------------

#' Helper functions for tabulation of a single biomarker result
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' @inheritParams argument_convention
#' @param df (`data.frame`)\cr results for a single biomarker. For `h_tab_rsp_one_biomarker()`, the results returned by
#'   [extract_rsp_biomarkers()]. For `h_tab_surv_one_biomarker()`, the results returned by
#'   [extract_survival_biomarkers()].#' @param afuns (named `list` of `function`)\cr analysis functions.
#' @param afuns (named `list` of `function`)\cr analysis functions.
#' @param colvars (named `list`)\cr named list with elements `vars` (variables to tabulate) and `labels` (their labels).
#'
#' @return An `rtables` table object with statistics in columns.
#'
#' @name h_tab_one_biomarker
NULL

#' @describeIn h_tab_one_biomarker Helper function to calculate statistics in columns for one biomarker.
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
  lifecycle::deprecate_warn(
    "0.9.8", "h_tab_one_biomarker()",
    details = "This function is no longer used within `tern`."
  )

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

#' @describeIn h_tab_one_biomarker Helper function that prepares a single response sub-table given the results for a
#'   single biomarker.
#'
#' @examples
#' library(dplyr)
#' library(forcats)
#'
#' adrs <- tern_ex_adrs
#' adrs_labels <- formatters::var_labels(adrs)
#'
#' adrs_f <- adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   mutate(rsp = AVALC == "CR")
#' formatters::var_labels(adrs_f) <- c(adrs_labels, "Response")
#'
#' # For a single population, separately estimate the effects of two biomarkers.
#' df <- h_logistic_mult_cont_df(
#'   variables = list(
#'     rsp = "rsp",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "SEX"
#'   ),
#'   data = adrs_f
#' )
#'
#' # Starting from above `df`, zoom in on one biomarker and add required columns.
#' df1 <- df[1, ]
#' df1$subgroup <- "All patients"
#' df1$row_type <- "content"
#' df1$var <- "ALL"
#' df1$var_label <- "All patients"
#'
#' h_tab_rsp_one_biomarker(
#'   df1,
#'   vars = c("n_tot", "n_rsp", "prop", "or", "ci", "pval")
#' )
#'
#' @export
h_tab_rsp_one_biomarker <- function(df,
                                    vars,
                                    na_str = default_na_str(),
                                    .indent_mods = 0L,
                                    ...) {
  lifecycle::deprecate_warn(
    "0.9.8", "h_tab_rsp_one_biomarker()",
    details = "This function is no longer used within `tern`."
  )

  colvars <- d_rsp_subgroups_colvars(
    vars,
    conf_level = df$conf_level[1],
    method = df$pval_label[1]
  )

  h_tab_one_biomarker(
    df = df,
    afuns = a_response_subgroups,
    colvars = colvars,
    na_str = na_str,
    .indent_mods = .indent_mods,
    ...
  )
}

#' @describeIn h_tab_one_biomarker Helper function that prepares a single survival sub-table given the results for a
#'   single biomarker.
#'
#' @examples
#' adtte <- tern_ex_adtte
#'
#' # Save variable labels before data processing steps.
#' adtte_labels <- formatters::var_labels(adtte, fill = FALSE)
#'
#' adtte_f <- adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(
#'     AVALU = as.character(AVALU),
#'     is_event = CNSR == 0
#'   )
#' labels <- c("AVALU" = adtte_labels[["AVALU"]], "is_event" = "Event Flag")
#' formatters::var_labels(adtte_f)[names(labels)] <- labels
#'
#' # For a single population, separately estimate the effects of two biomarkers.
#' df <- h_coxreg_mult_cont_df(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "SEX",
#'     strata = c("STRATA1", "STRATA2")
#'   ),
#'   data = adtte_f
#' )
#'
#' # Starting from above `df`, zoom in on one biomarker and add required columns.
#' df1 <- df[1, ]
#' df1$subgroup <- "All patients"
#' df1$row_type <- "content"
#' df1$var <- "ALL"
#' df1$var_label <- "All patients"
#' h_tab_surv_one_biomarker(
#'   df1,
#'   vars = c("n_tot", "n_tot_events", "median", "hr", "ci", "pval"),
#'   time_unit = "days"
#' )
#'
#' @export
h_tab_surv_one_biomarker <- function(df,
                                     vars,
                                     time_unit,
                                     na_str = default_na_str(),
                                     .indent_mods = 0L,
                                     ...) {
  lifecycle::deprecate_warn(
    "0.9.8", "h_tab_surv_one_biomarker()",
    details = "This function is no longer used within `tern`."
  )

  colvars <- d_survival_subgroups_colvars(
    vars,
    conf_level = df$conf_level[1],
    method = df$pval_label[1],
    time_unit = time_unit
  )

  h_tab_one_biomarker(
    df = df,
    afuns = a_survival_subgroups,
    colvars = colvars,
    na_str = na_str,
    .indent_mods = .indent_mods,
    ...
  )
}
