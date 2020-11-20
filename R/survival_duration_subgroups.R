#' Tabulate Survival Duration by Subgroup
#'
#' Tabulate statistics such as median survival time and hazard ratio for population subgroups.
#'
#' @details These functions create a layout starting from a data frame which contains
#'   the required statistics. Tables typically used as part of forest plot.
#'
#' @inheritParams argument_convention
#' @inheritParams survival_coxph_pairwise
#' @param data (`data frame`)\cr the dataset containing the variables to summarize.
#' @param label_all (`string`)\cr label for the total population analysis.
#' @name survival_duration_subgroups
#' @order 1
#' @examples
#'
#' # Testing dataset.
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' adtte <- radtte(cached = TRUE)
#'
#' # Save variable labels before data processing steps.
#' adtte_labels <- var_labels(adtte)
#'
#' adtte <- adtte %>%
#'   filter(
#'     PARAMCD == "OS",
#'     ARM %in% c("B: Placebo", "A: Drug X"),
#'     SEX %in% c("M", "F")
#'   ) %>%
#'   mutate(
#'     # Reorder levels of ARM to display reference arm before treatment arm.
#'     ARM = droplevels(forcats::fct_relevel(ARM, "B: Placebo")),
#'     SEX = droplevels(SEX),
#'     is_event = CNSR == 0
#'   ) %>%
#'   var_relabel(
#'     ARM = adtte_labels["ARM"],
#'     SEX = adtte_labels["SEX"],
#'     is_event = "Event Flag"
#'   )
#'
NULL

#' @describeIn survival_duration_subgroups Prepares estimates of median survival times and treatment hazard ratios for
#'   population subgroups in data frames. Simple wrapper for [h_survtime_subgroups_df()] and [h_coxph_subgroups_df()].
#'   Result is a list of two data frames: `survtime` and `hr`.
#'   `variables` corresponds to the names of variables found in `data`, passed as a named list and requires elements
#'   `tte`, `is_event`, `arm`, `subgroups` and optionally `strat`.
#' @export
#' @examples
#'
#' df <- extract_survival_subgroups(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     arm = "ARM", subgroups = c("SEX", "BMRKR2")
#'   ),
#'   data = adtte
#' )
#' df
#'
extract_survival_subgroups <- function(variables, data, control = control_coxph(), label_all = "All Patients") {

  df_survtime <- h_survtime_subgroups_df(variables, data, label_all = label_all)
  df_hr <- h_coxph_subgroups_df(variables, data, control = control, label_all = label_all)

  list(survtime = df_survtime, hr = df_hr)
}

#' @describeIn survival_duration_subgroups Formatted Analysis function used to format the results of
#'   [extract_survival_subgroups()]. Returns is a list of Formatted Analysis functions with one element per statistic.
#' @export
#' @examples
#' a_survival_subgroups(.formats = list("n" = "xx", "median" = "xx.xx"))
#'
a_survival_subgroups <- function(.formats = list(
  n = "xx", n_events = "xx", median = "xx.x",
  n_tot = "xx", hr = list(format_extreme_values(2L)),
  ci = list(format_extreme_values_ci(2L)),
  pval = "x.xxxx | (<0.0001)")
) {

  assert_that(
    is.list(.formats),
    all_elements_in_ref(names(.formats), ref = c("n", "n_events", "median", "n_tot", "hr", "ci", "pval"))
  )

  afun_lst <- Map(function(stat, fmt){
    if (stat == "ci") {
      function(df, labelstr = "", ...) {
        in_rows(.list = combine_vectors(df$lcl, df$ucl), .labels = as.character(df$subgroup), .formats = fmt)
      }
    } else {
      function(df, labelstr = "", ...) {
        in_rows(.list = as.list(df[[stat]]), .labels = as.character(df$subgroup), .formats = fmt)
      }
    }
  },
  stat = names(.formats),
  fmt = .formats
  )

  afun_lst
}

#' @describeIn survival_duration_subgroups layout creating function.
#' @param vars (`character`)\cr the name of statistics to be reported among
#'  `n` (total number of observations per group), `n_events` (number of events per group),
#'  `median` (median survival time), `n_tot` (total number of observations),
#'  `hr` (hazard ratio), `ci` (confidence interval of hazard ratio) and `pvalue` (p value of the effect).
#' @export
#' @examples
#'
#' ## Table of survival times by subgroup.
#' basic_table() %>%
#'   tabulate_survival_subgroups(vars = c("n", "median")) %>%
#'   build_table(df$survtime)
#'
#' ## Table of hazard ratios by subgroup.
#' basic_table() %>%
#'   tabulate_survival_subgroups(vars = c("n_tot", "hr", "ci")) %>%
#'   build_table(df$hr)
#'
tabulate_survival_subgroups <- function(
  lyt,
  vars,
  control = control_coxph()) {

  colvars <- d_survival_subgroups_colvars(vars, control = control)
  afun_lst <- a_survival_subgroups()

  lyt <- split_cols_by(lyt = lyt, var = "arm")
  lyt <- split_rows_by(
    lyt = lyt,
    var = "row_type",
    split_fun = keep_split_levels("content"),
    nested = FALSE
  )
  lyt <- summarize_row_groups(lyt = lyt, var = "var_label", cfun = afun_lst[vars])
  lyt <- split_rows_by(
    lyt = lyt,
    var = "row_type",
    split_fun = keep_split_levels("analysis"),
    nested = FALSE,
    child_labels = "hidden"
  )
  lyt <- split_rows_by(lyt = lyt, var = "var_label", nested = TRUE)
  lyt <- split_cols_by_multivar(
    lyt = lyt,
    vars = colvars$vars,
    varlabels = colvars$labels
  )
  analyze_colvars(lyt = lyt, afun = afun_lst[vars])

}

#' Labels for Column Variables in Survival Duration by Subgroup Table
#'
#' Internal function to check variables included in
#' [tabulate_survival_subgroups()] and create column labels.
#'
#' @inheritParams tabulate_survival_subgroups
#' @return `list` of variables to tabulate and their labels.
#'
d_survival_subgroups_colvars <- function(vars, control = control_coxph()) {

  assert_that(
    is.character(vars),
    all_elements_in_ref(vars, ref = c("n", "n_events", "median", "n_tot", "hr", "ci", "pval"))
  )

  varlabels <- c(
    n = "n",
    n_events = "Events",
    median = "Median",
    n_tot = "Total n",
    hr = "Hazard Ratio",
    ci = paste0(100 * control[["conf_level"]], "% Wald CI"),
    pval = paste0("p-value (", control[["pval_method"]], ")")
  )

  colvars <- vars

  # The `lcl` variable is just a placeholder available in the analysis data,
  # it is not acutally used in the tabulation.
  # Variables used in the tabulation are lcl and ucl, see `a_survival_subgroups` for details.
  colvars[colvars == "ci"] <- "lcl"

  list(
    vars = colvars,
    labels = varlabels[vars]
  )
}
