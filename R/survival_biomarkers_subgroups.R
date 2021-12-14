#' Tabulate Biomarker Effects on Survival by Subgroup
#'
#' Tabulate the estimated effects of multiple continuous biomarker variables
#' across population subgroups.
#'
#' @details These functions create a layout starting from a data frame which contains
#'   the required statistics. The tables are then typically used as input for forest plots.
#'
#' @inheritParams argument_convention
#' @inheritParams fit_coxreg_multivar
#' @inheritParams survival_duration_subgroups
#' @name survival_biomarkers_subgroups
#' @order 1
#' @examples
#' # Testing dataset.
#' library(scda)
#' library(dplyr)
#' library(forcats)
#' library(rtables)
#'
#' adtte <- synthetic_cdisc_data("latest")$adtte
#'
#' # Save variable labels before data processing steps.
#' adtte_labels <- var_labels(adtte)
#'
#' adtte_f <- adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(
#'     AVALU = as.character(AVALU),
#'     is_event = CNSR == 0
#'   ) %>%
#'   var_relabel(
#'     AVALU = adtte_labels["AVALU"],
#'     is_event = "Event Flag"
#'   )
#'
NULL

#' @describeIn survival_biomarkers_subgroups prepares estimates for number of events, patients and median survival
#'   times, as well as hazard ratio estimates, confidence intervals and p-values, for multiple biomarkers across
#'   population subgroups in a single data frame.
#'   `variables` corresponds to the names of variables found in `data`, passed as a named list and requires elements
#'   `tte`, `is_event`, `biomarkers` (vector of continuous biomarker variables) and optionally `subgroups` and `strat`.
#'   `groups_lists` optionally specifies groupings for `subgroups` variables.
#' @seealso [h_coxreg_mult_cont_df()] which is used internally.
#' @export
#' @examples
#' # Typical analysis of two continuous biomarkers `BMRKR1` and `AGE`,
#' # in multiple regression models containing one covariate `RACE`,
#' # as well as one stratification variable `STRATA1`. The subgroups
#' # are defined by the levels of `BMRKR2`.
#' df <- extract_survival_biomarkers(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     strata = "STRATA1",
#'     covariates = "SEX",
#'     subgroups = "BMRKR2"
#'   ),
#'   data = adtte_f
#' )
#' df
#'
#' # Here we group the levels of `BMRKR2` manually.
#' df_grouped <- extract_survival_biomarkers(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     strata = "STRATA1",
#'     covariates = "SEX",
#'     subgroups = "BMRKR2"
#'   ),
#'   data = adtte_f,
#'   groups_lists = list(
#'     BMRKR2 = list(
#'       "low" = "LOW",
#'       "low/medium" = c("LOW", "MEDIUM"),
#'       "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
#'     )
#'   )
#' )
#' df_grouped
extract_survival_biomarkers <- function(variables,
                                        data,
                                        groups_lists = list(),
                                        control = control_coxreg(),
                                        label_all = "All Patients") {
  assertthat::assert_that(
    is.list(variables),
    is.character(variables$subgroups) || is.null(variables$subgroups),
    assertthat::is.string(label_all)
  )
  # Start with all patients.
  result_all <- h_coxreg_mult_cont_df(
    variables = variables,
    data = data,
    control = control
  )
  result_all$subgroup <- label_all
  result_all$var <- "ALL"
  result_all$var_label <- label_all
  result_all$row_type <- "content"
  if (is.null(variables$subgroups)) {
    # Only return result for all patients.
    result_all
  } else {
    # Add subgroups results.
    l_data <- h_split_by_subgroups(
      data,
      variables$subgroups,
      groups_lists = groups_lists
    )
    l_result <- lapply(l_data, function(grp) {
      result <- h_coxreg_mult_cont_df(
        variables = variables,
        data = grp$df,
        control = control
      )
      result_labels <- grp$df_labels[rep(1, times = nrow(result)), ]
      cbind(result, result_labels)
    })
    result_subgroups <- do.call(rbind, args = c(l_result, make.row.names = FALSE))
    result_subgroups$row_type <- "analysis"
    rbind(
      result_all,
      result_subgroups
    )
  }
}

#' @describeIn survival_biomarkers_subgroups table creating function.
#' @param df (`data.frame`)\cr containing all analysis variables, as returned by
#'   [extract_survival_biomarkers()].
#' @param vars (`character`)\cr the name of statistics to be reported among
#'  `n_tot_events` (total number of events per group),
#'  `n_tot` (total number of observations per group),
#'  `median` (median survival time),
#'  `hr` (hazard ratio),
#'  `ci` (confidence interval of hazard ratio) and
#'  `pval` (p value of the effect).
#'  Note, one of the statistics `n_tot` and `n_tot_events`, as well as both `hr` and `ci`
#'  are required.
#' @seealso [h_tab_surv_one_biomarker()] which is used internally.
#' @note In contrast to [tabulate_survival_subgroups()] this tabulation function does
#'   not start from an input layout `lyt`. This is because internally the table is
#'   created by combining multiple subtables.
#' @export
#' @examples
#'
#' ## Table with default columns.
#' tabulate_survival_biomarkers(df)
#'
#' ## Table with a manually chosen set of columns: leave out "pval", reorder.
#' tab <- tabulate_survival_biomarkers(
#'   df = df,
#'   vars = c("n_tot_events", "ci", "n_tot", "median", "hr"),
#'   time_unit = as.character(adtte_f$AVALU[1])
#' )
#'
#' ## Finally produce the forest plot.
#' g_forest(tab, xlim = c(0.8, 1.2))
tabulate_survival_biomarkers <- function(df,
                                         vars = c("n_tot", "n_tot_events", "median", "hr", "ci", "pval"),
                                         time_unit = NULL) {
  assertthat::assert_that(
    is.data.frame(df),
    is.character(df$biomarker),
    is.character(df$biomarker_label),
    all(vars %in% c("n_tot", "n_tot_events", "median", "hr", "ci", "pval"))
  )
  df_subs <- split(df, f = df$biomarker)
  tabs <- lapply(df_subs, FUN = function(df_sub) {
    tab_sub <- h_tab_surv_one_biomarker(
      df = df_sub,
      vars = vars,
      time_unit = time_unit
    )
    # Insert label row as first row in table.
    label_at_path(tab_sub, path = row_paths(tab_sub)[[1]][1]) <- df_sub$biomarker_label[1]
    tab_sub
  })
  result <- do.call(rbind_fix, tabs)

  n_tot_ids <- grep("^n_tot", vars)
  hr_id <- match("hr", vars)
  ci_id <- match("ci", vars)
  structure(
    result,
    forest_header = paste0(c("Higher", "Lower"), "\nBetter"),
    col_x = hr_id,
    col_ci = ci_id,
    col_symbol_size = n_tot_ids[1]
  )
}
