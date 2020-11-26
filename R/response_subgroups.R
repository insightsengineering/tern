#' Tabulate Binary Response by Subgroup
#'
#' Tabulate statistics such as response rate and odds ratio for population subgroups.
#'
#' @details These functions create a layout starting from a data frame which contains
#'   the required statistics. Tables typically used as part of forest plot.
#'
#' @inheritParams argument_convention
#' @param data (`data frame`)\cr the dataset containing the variables to summarize.
#' @param method (`string`)\cr
#'   specifies the test used to calculate the p-value for the difference between
#'   two proportions. For options, see [s_test_proportion_diff()]. Default is `NULL`
#'   so no test is performed.
#' @name response_subgroups
#' @order 1
#' @examples
#'
#' # Testing dataset.
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' adrs <- radrs(cached = TRUE)
#' adrs_labels <- var_labels(adrs)
#'
#' adrs_f <- adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
#'   droplevels() %>%
#'   mutate(
#'     # Reorder levels of factor to make the placebo group the reference arm.
#'     ARM = forcats::fct_relevel(ARM, "B: Placebo"),
#'     rsp = AVALC == "CR"
#'   )
#' var_labels(adrs_f) <- c(adrs_labels, "Response")
#'
NULL

#' @describeIn response_subgroups prepares response rates and odds ratios for
#'   population subgroups in data frames. Simple wrapper for [h_odds_ratio_subgroups_df()] and
#'   [h_proportion_subgroups_df()]. Result is a list of two data frames:
#'   `prop` and `or`. `variables` corresponds to the names of variables found in `data`, passed as a
#'   named list and requires elements `rsp`, `arm` and optionally `subgroups`.
#' @param label_all (`string`)\cr label for the total population analysis.
#' @export
#' @examples
#'
#' df <- extract_rsp_subgroups(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
#'   data = adrs_f
#' )
#' df
#'
extract_rsp_subgroups <- function(variables, data, conf_level = 0.95, method = NULL, label_all = "All Patients") {

  df_prop <- h_proportion_subgroups_df(variables, data, label_all = label_all)
  df_or <- h_odds_ratio_subgroups_df(variables, data, conf_level = conf_level, method = method, label_all = label_all)

  list(prop = df_prop, or = df_or)
}

#' @describeIn response_subgroups Formatted Analysis function used to format the results of [extract_rsp_subgroups()].
#'   Returns is a list of Formatted Analysis functions with one element per statistic.
#' @export
#' @examples
#' a_response_subgroups(.formats = list("n" = "xx", "prop" = "xx.xx%"))
#'
a_response_subgroups <- function(.formats = list(
  n = "xx", n_rsp = "xx", prop = "xx.x%",
  n_tot = "xx", or = list(format_extreme_values(2L)),
  ci = list(format_extreme_values_ci(2L)),
  pval = "x.xxxx | (<0.0001)")
) {

  assert_that(
    is.list(.formats),
    all_elements_in_ref(names(.formats), ref = c("n", "n_rsp", "prop", "n_tot", "or", "ci", "pval"))
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

#' @describeIn response_subgroups layout creating function.
#' @param vars (`character`)\cr the name of statistics to be reported among
#'  `n` (total number of observations per group), `n_rsp` (number of responders per group),
#'  `prop` (proportion of responders), `n_tot` (total number of observations),
#'  `or` (odds ratio), `ci` (confidence interval of odds ratio) and `pvalue` (p value of the effect).
#' @export
#' @examples
#'
#' ## Table of response rates by subgroup.
#' basic_table() %>%
#'   tabulate_rsp_subgroups(vars = c("n", "prop")) %>%
#'   build_table(df$prop)
#'
#' ## Table of odds ratios by subgroup.
#' basic_table() %>%
#'   tabulate_rsp_subgroups(vars = c("n_tot", "or", "ci"), conf_level = 0.95) %>%
#'   build_table(df$or)
#'
tabulate_rsp_subgroups <- function(lyt,
                                   vars,
                                   conf_level = NULL,
                                   method = NULL) {

  colvars <- d_rsp_subgroups_colvars(vars, conf_level = conf_level, method = method)

  afun_lst <- a_response_subgroups()

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

#' Labels for Column Variables in Binary Response by Subgroup Table
#'
#' Internal function to check variables included in
#' [tabulate_rsp_subgroups] and create column labels.
#'
#' @inheritParams tabulate_rsp_subgroups
#' @return `list` of variables to tabulate and their labels.
#'
d_rsp_subgroups_colvars <- function(vars, conf_level = NULL, method = NULL) {

  assert_that(
    is.character(vars),
    all_elements_in_ref(vars, ref = c("n", "n_rsp", "prop", "n_tot", "or", "ci", "pval"))
  )

  varlabels <- c(
    n = "n",
    n_rsp = "Responder n",
    prop = "Response (%)",
    n_tot = "Total n",
    or = "Odds Ratio"
  )
  colvars <- vars

  if ("ci" %in% colvars) {

    assert_that(!is.null(conf_level))

    varlabels <- c(
      varlabels,
      ci = paste0(100 * conf_level, "% CI")
    )

    # The `lcl`` variable is just a placeholder available in the analysis data,
    # it is not acutally used in the tabulation.
    # Variables used in the tabulation are lcl and ucl, see `a_response_subgroups` for details.
    colvars[colvars == "ci"] <- "lcl"

  }

  if ("pval" %in% colvars) {

    assert_that(!is.null(method))

    varlabels <- c(
      varlabels,
      pval = d_test_proportion_diff(method)
    )
  }

  list(
    vars = colvars,
    labels = varlabels[vars]
  )
}
