#' Tabulate Binary Response by Subgroup
#'
#' Tabulate statistics such as response rate and odds ratio for population subgroups.
#'
#' @details These functions create a layout starting from a data frame which contains
#'   the required statistics. Tables typically used as part of forest plot.
#'
#' @inheritParams argument_convention
#' @param data (`data frame`)\cr the dataset containing the variables to summarize.
#' @param groups_lists (named `list` of `list`)\cr optionally contains for each `subgroups` variable a
#'   list, which specifies the new group levels via the names and the
#'   levels that belong to it in the character vectors that are elements of the list.
#' @param method (`string`)\cr
#'   specifies the test used to calculate the p-value for the difference between
#'   two proportions. For options, see [s_test_proportion_diff()]. Default is `NULL`
#'   so no test is performed.
#' @name response_subgroups
#' @order 1
#' @examples
#'
#' # Testing dataset.
#' library(scda)
#' library(dplyr)
#' library(forcats)
#' library(rtables)
#'
#' adrs <- synthetic_cdisc_data("latest")$adrs
#' adrs_labels <- var_labels(adrs)
#'
#' adrs_f <- adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
#'   droplevels() %>%
#'   mutate(
#'     # Reorder levels of factor to make the placebo group the reference arm.
#'     ARM = fct_relevel(ARM, "B: Placebo"),
#'     rsp = AVALC == "CR"
#'   )
#'
#' var_labels(adrs_f) <- c(adrs_labels, "Response")
#'
NULL

#' @describeIn response_subgroups prepares response rates and odds ratios for
#'   population subgroups in data frames. Simple wrapper for [h_odds_ratio_subgroups_df()] and
#'   [h_proportion_subgroups_df()]. Result is a list of two data frames:
#'   `prop` and `or`. `variables` corresponds to the names of variables found in `data`, passed as a
#'   named list and requires elements `rsp`, `arm` and optionally `subgroups` and `strat`. `groups_lists`
#'   optionally specifies groupings for `subgroups` variables.
#' @param label_all (`string`)\cr label for the total population analysis.
#' @export
#' @examples
#'
#' # Unstratified analysis.
#' df <- extract_rsp_subgroups(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
#'   data = adrs_f
#' )
#' df
#'
#' # Stratified analysis.
#' df_strat <- extract_rsp_subgroups(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2"), strat = "STRATA1"),
#'   data = adrs_f
#' )
#' df_strat
#'
#' # Grouping of the BMRKR2 levels.
#' df_grouped <- extract_rsp_subgroups(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
#'   data = adrs_f,
#'   groups_lists = list(
#'     BMRKR2 = list(
#'       "low" = "LOW",
#'       "low/medium" = c("LOW", "MEDIUM"),
#'       "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
#'     )
#'   )
#' )
#' df_grouped
#'
extract_rsp_subgroups <- function(variables,
                                  data,
                                  groups_lists = list(),
                                  conf_level = 0.95,
                                  method = NULL,
                                  label_all = "All Patients") {

  df_prop <- h_proportion_subgroups_df(
    variables,
    data,
    groups_lists = groups_lists,
    label_all = label_all
  )
  df_or <- h_odds_ratio_subgroups_df(
    variables,
    data,
    groups_lists = groups_lists,
    conf_level = conf_level,
    method = method,
    label_all = label_all
  )

  list(prop = df_prop, or = df_or)
}

#' @describeIn response_subgroups Formatted Analysis function used to format the results of [extract_rsp_subgroups()].
#'   Returns is a list of Formatted Analysis functions with one element per statistic.
#' @importFrom rtables in_rows
#' @export
#' @examples
#' a_response_subgroups(.formats = list("n" = "xx", "prop" = "xx.xx%"))
#'
a_response_subgroups <- function(.formats = list(
  n = "xx",
  n_rsp = "xx",
  prop = "xx.x%",
  n_tot = "xx",
  or = list(format_extreme_values(2L)),
  ci = list(format_extreme_values_ci(2L)),
  pval = "x.xxxx | (<0.0001)")
) {

  assert_that(
    is.list(.formats),
    all_elements_in_ref(
      names(.formats),
      ref = c("n", "n_rsp", "prop", "n_tot", "or", "ci", "pval")
    )
  )

  afun_lst <- Map(function(stat, fmt) {
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

#' @describeIn response_subgroups table creating function.
#' @param df (`list`)\cr of data frames containing all analysis variables. List should be
#'   created using [extract_rsp_subgroups()].
#' @param vars (`character`)\cr the name of statistics to be reported among
#'  `n` (total number of observations per group),
#'  `n_rsp` (number of responders per group),
#'  `prop` (proportion of responders),
#'  `n_tot` (total number of observations),
#'  `or` (odds ratio),
#'  `ci` (confidence interval of odds ratio) and
#'  `pval` (p value of the effect).
#'  Note, the statistics `n_tot`, `or` and `ci` are required.
#' @export
#' @examples
#'
#' ## Table with default columns.
#' basic_table() %>%
#'   tabulate_rsp_subgroups(df)
#'
#' ## Table with selected columns.
#' basic_table() %>%
#'   tabulate_rsp_subgroups(
#'     df = df,
#'     vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci")
#'   )
#'
tabulate_rsp_subgroups <- function(lyt,
                                   df,
                                   vars = c("n_tot", "n", "prop", "or", "ci")) {

  conf_level <- df$or$conf_level[1]
  method <- if ("pval_label" %in% names(df$or)) {
    df$or$pval_label[1]
  } else {
    NULL
  }

  afun_lst <- a_response_subgroups()
  colvars <- d_rsp_subgroups_colvars(vars, conf_level = conf_level, method = method)

  colvars_prop <- list(
    vars = colvars$vars[names(colvars$labels) %in% c("n", "prop", "n_rsp")],
    labels = colvars$labels[names(colvars$labels) %in% c("n", "prop", "n_rsp")]
  )
  colvars_or <- list(
    vars = colvars$vars[names(colvars$labels) %in% c("n_tot", "or", "ci", "pval")],
    labels = colvars$labels[names(colvars$labels) %in% c("n_tot", "or", "ci", "pval")]
  )

  # Columns from table_prop are optional.
  if (length(colvars_prop$vars) > 0) {

    lyt_prop <- split_cols_by(lyt = lyt, var = "arm")
    lyt_prop <- split_rows_by(
      lyt = lyt_prop,
      var = "row_type",
      split_fun = keep_split_levels("content"),
      nested = FALSE
    )
    lyt_prop <- summarize_row_groups(
      lyt = lyt_prop,
      var = "var_label",
      cfun = afun_lst[names(colvars_prop$labels)]
    )
    lyt_prop <- split_cols_by_multivar(
      lyt = lyt_prop,
      vars = colvars_prop$vars,
      varlabels = colvars_prop$labels
    )

    if ("analysis" %in% df$prop$row_type) {

      lyt_prop <- split_rows_by(
        lyt = lyt_prop,
        var = "row_type",
        split_fun = keep_split_levels("analysis"),
        nested = FALSE,
        child_labels = "hidden"
      )
      lyt_prop <- split_rows_by(lyt = lyt_prop, var = "var_label", nested = TRUE)
      lyt_prop <- analyze_colvars(
        lyt = lyt_prop,
        afun = afun_lst[names(colvars_prop$labels)],
        inclNAs = TRUE
      )
    }

    table_prop <- build_table(lyt_prop, df = df$prop)

  } else {
    table_prop <- NULL
  }

  # Columns "n_tot", "or", "ci" in table_or are required.
  lyt_or <- split_cols_by(lyt = lyt, var = "arm")
  lyt_or <- split_rows_by(
    lyt = lyt_or,
    var = "row_type",
    split_fun = keep_split_levels("content"),
    nested = FALSE
  )
  lyt_or <- split_cols_by_multivar(
    lyt = lyt_or,
    vars = colvars_or$vars,
    varlabels = colvars_or$labels
  )
  lyt_or <- summarize_row_groups(
    lyt = lyt_or,
    var = "var_label",
    cfun = afun_lst[names(colvars_or$labels)]
  ) %>%
    append_topleft("Baseline Risk Factors")

  if ("analysis" %in% df$or$row_type) {
    lyt_or <- split_rows_by(
      lyt = lyt_or,
      var = "row_type",
      split_fun = keep_split_levels("analysis"),
      nested = FALSE,
      child_labels = "hidden"
    )
    lyt_or <- split_rows_by(lyt = lyt_or, var = "var_label", nested = TRUE)
    lyt_or <- analyze_colvars(
      lyt = lyt_or,
      afun = afun_lst[names(colvars_or$labels)],
      inclNAs = TRUE
    )
  }
  table_or <- build_table(lyt_or, df = df$or)

  n_tot_id <- match("n_tot", colvars_or$vars)
  if (is.null(table_prop)) {
    result <- table_or
    or_id <- match("or", colvars_or$vars)
    ci_id <- match("lcl", colvars_or$vars)
  } else {
    result <- cbind_rtables(table_or[, n_tot_id], table_prop, table_or[, -n_tot_id])
    or_id <- 1L + ncol(table_prop) + match("or", colvars_or$vars[-n_tot_id])
    ci_id <- 1L + ncol(table_prop) + match("lcl", colvars_or$vars[-n_tot_id])
    n_tot_id <- 1L
  }
  structure(
    result,
    forest_header = paste0(levels(df$prop$arm), "\nBetter"),
    col_x = or_id,
    col_ci = ci_id,
    col_symbol_size = n_tot_id
  )
}

#' Labels for Column Variables in Binary Response by Subgroup Table
#'
#' Internal function to check variables included in
#' [tabulate_rsp_subgroups] and create column labels.
#'
#' @inheritParams tabulate_rsp_subgroups
#' @return `list` of variables to tabulate and their labels.
#'
d_rsp_subgroups_colvars <- function(vars,
                                    conf_level = NULL,
                                    method = NULL) {

  assert_that(
    is.character(vars),
    all_elements_in_ref(c("n_tot", "or", "ci"), vars),
    all_elements_in_ref(vars, ref = c("n", "n_rsp", "prop", "n_tot", "or", "ci", "pval"))
  )

  varlabels <- c(
    n = "n",
    n_rsp = "Responders",
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
    varlabels <- c(
      varlabels,
      pval = method
    )
  }

  list(
    vars = colvars,
    labels = varlabels[vars]
  )
}
