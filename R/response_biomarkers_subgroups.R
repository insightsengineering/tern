#' Tabulate Biomarker Effects on Binary Response by Subgroup
#'
#' Tabulate the estimated effects of multiple continuous biomarker variables
#' on a binary response endpoint across population subgroups.
#'
#' @details These functions create a layout starting from a data frame which contains
#'   the required statistics. The tables are then typically used as input for forest plots.
#'
#' @inheritParams argument_convention
#' @inheritParams response_subgroups
#' @name response_biomarkers_subgroups
#' @order 1
#' @examples
#' # Testing dataset.
#' library(scda)
#' library(dplyr)
#' library(forcats)
#' library(rtables)
#'
#' adrs <- synthetic_cdisc_data("latest")$adrs
#' adrs_labels <- formatters::var_labels(adrs)
#'
#' adrs_f <- adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   mutate(rsp = AVALC == "CR")
#' formatters::var_labels(adrs_f) <- c(adrs_labels, "Response")
NULL

#' @describeIn response_biomarkers_subgroups prepares estimates for number of responses, patients and
#'   overall response rate, as well as odds ratio estimates, confidence intervals and p-values,
#'   for multiple biomarkers across population subgroups in a single data frame.
#'   `variables` corresponds to the names of variables found in `data`, passed as a
#'   named list and requires elements `rsp` and `biomarkers` (vector of continuous
#'   biomarker variables) and optionally `covariates`, `subgroups` and `strat`.
#'   `groups_lists` optionally specifies groupings for `subgroups` variables.
#' @param control (named `list`)\cr controls for the response definition and the
#'   confidence level produced by [control_logistic()].
#' @seealso [h_logistic_mult_cont_df()] which is used internally.
#' @note You can also specify a continuous variable in `rsp` and then use the
#'   `response_definition` control to convert that internally to a logical
#'   variable reflecting binary response.
#' @export
#' @examples
#' # Typical analysis of two continuous biomarkers `BMRKR1` and `AGE`,
#' # in logistic regression models with one covariate `RACE`. The subgroups
#' # are defined by the levels of `BMRKR2`.
#' df <- extract_rsp_biomarkers(
#'   variables = list(
#'     rsp = "rsp",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "SEX",
#'     subgroups = "BMRKR2"
#'   ),
#'   data = adrs_f
#' )
#' df
#'
#' # Here we group the levels of `BMRKR2` manually, and we add a stratification
#' # variable `STRATA1`. We also here use a continuous variable `EOSDY`
#' # which is then binarized internally (response is defined as this variable
#' # being larger than 500).
#' df_grouped <- extract_rsp_biomarkers(
#'   variables = list(
#'     rsp = "EOSDY",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "SEX",
#'     subgroups = "BMRKR2",
#'     strat = "STRATA1"
#'   ),
#'   data = adrs_f,
#'   groups_lists = list(
#'     BMRKR2 = list(
#'       "low" = "LOW",
#'       "low/medium" = c("LOW", "MEDIUM"),
#'       "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
#'     )
#'   ),
#'   control = control_logistic(
#'     response_definition = "I(response > 500)"
#'   )
#' )
#' df_grouped
extract_rsp_biomarkers <- function(variables,
                                   data,
                                   groups_lists = list(),
                                   control = control_logistic(),
                                   label_all = "All Patients") {
  assertthat::assert_that(
    is.list(variables),
    assertthat::is.string(variables$rsp),
    is.character(variables$subgroups) || is.null(variables$subgroups),
    assertthat::is.string(label_all)
  )
  # Start with all patients.
  result_all <- h_logistic_mult_cont_df(
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
      result <- h_logistic_mult_cont_df(
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

#' @describeIn response_biomarkers_subgroups table creating function.
#' @param df (`data.frame`)\cr containing all analysis variables, as returned by
#'   [extract_rsp_biomarkers()].
#' @param vars (`character`)\cr the name of statistics to be reported among
#'  `n_tot` (total number of patients per group),
#'  `n_rsp` (total number of responses per group),
#'  `prop` (total response proportion per group),
#'  `or` (odds ratio),
#'  `ci` (confidence interval of odds ratio) and
#'  `pval` (p value of the effect).
#'  Note, the statistics `n_tot`, `or` and `ci` are required.
#' @seealso [h_tab_rsp_one_biomarker()] which is used internally.
#' @note In contrast to [tabulate_rsp_subgroups()] this tabulation function does
#'   not start from an input layout `lyt`. This is because internally the table is
#'   created by combining multiple subtables.
#' @export
#' @examples
#'
#' ## Table with default columns.
#' # df <- <need_data_input_to_work>
#' tabulate_rsp_biomarkers(df)
#'
#' ## Table with a manually chosen set of columns: leave out "pval", reorder.
#' tab <- tabulate_rsp_biomarkers(
#'   df = df,
#'   vars = c("n_rsp", "ci", "n_tot", "prop", "or")
#' )
#'
#' ## Finally produce the forest plot.
#' \dontrun{
#' g_forest(tab, xlim = c(0.7, 1.4))
#' }
tabulate_rsp_biomarkers <- function(df,
                                    vars = c("n_tot", "n_rsp", "prop", "or", "ci", "pval")) {
  assertthat::assert_that(
    is.data.frame(df),
    is.character(df$biomarker),
    is.character(df$biomarker_label),
    all(vars %in% c("n_tot", "n_rsp", "prop", "or", "ci", "pval"))
  )
  df_subs <- split(df, f = df$biomarker)
  tabs <- lapply(df_subs, FUN = function(df_sub) {
    tab_sub <- h_tab_rsp_one_biomarker(
      df = df_sub,
      vars = vars
    )
    # Insert label row as first row in table.
    label_at_path(tab_sub, path = row_paths(tab_sub)[[1]][1]) <- df_sub$biomarker_label[1]
    tab_sub
  })
  result <- do.call(rbind, tabs)

  n_id <- grep("n_tot", vars)
  or_id <- match("or", vars)
  ci_id <- match("ci", vars)
  structure(
    result,
    forest_header = paste0(c("Lower", "Higher"), "\nBetter"),
    col_x = or_id,
    col_ci = ci_id,
    col_symbol_size = n_id
  )
}
