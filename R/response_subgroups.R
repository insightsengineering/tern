#' Helper Functions for Tabulating Binary Response by Subgroup
#'
#' Helper functions that tabulate in a data frame statistics such as response rate
#' and odds ratio for population subgroups.
#'
#' @details Main functionality is to prepare data for use in a layout creating function.
#'
#' @inheritParams argument_convention
#' @param arm (`factor`)\cr the treatment group variable.
#' @param data (`data frame`)\cr the dataset containing the variables to summarize.
#' @param method (`string`)\cr
#'   specifies the test used to calculate the p-value for the difference between
#'   two proportions. For options, see [s_test_proportion_diff()]. Default is `NULL`
#'   so no test is performed.
#' @param label_all (`string`)\cr label for the total population analysis.
#' @name h_response_subgroups
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
#' adrs <- adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
#'   droplevels() %>%
#'   mutate(
#'     # Reorder levels of factor to make the placebo group the reference arm.
#'     ARM = forcats::fct_relevel(ARM, "B: Placebo"),
#'     rsp = AVALC == "CR"
#'   )
#' var_labels(adrs) <- c(adrs_labels, "Response")
#'
NULL

#' @describeIn h_response_subgroups Helper to prepare a data frame of binary responses by arm.
#' @inheritParams h_response_subgroups
#' @export
#' @examples
#'
#' h_proportion_df(
#'   c(TRUE, FALSE, FALSE),
#'   arm = factor(c("A", "A", "B"), levels = c("A", "B"))
#' )
#'
h_proportion_df <- function(rsp, arm) {

  assert_that(
    is_logical_vector(rsp),
    is_valid_factor(arm),
    is_equal_length(rsp, arm)
  )

  lst_rsp <- split(rsp, arm)
  lst_results <- Map(function(x, arm) {

    s_prop <- s_proportion(x)
    data.frame(
      arm = arm,
      n = length(x),
      n_rsp = unname(s_prop$n_prop[1]),
      prop = unname(s_prop$n_prop[2]),
      stringsAsFactors = FALSE
    )

  }, lst_rsp, names(lst_rsp))

  df <- do.call(rbind, args = c(lst_results, make.row.names = FALSE))
  df$arm <- factor(df$arm, levels = levels(arm))
  df
}

#' @describeIn h_response_subgroups Summarizes proportion of binary responses by arm and across subgroups
#'    in a data frame. `variables` corresponds to the names of variables found in `data`, passed as a named list and
#'    requires elements `rsp`, `arm` and `subgroups`.
#' @export
#' @examples
#'
#' h_proportion_subgroups_df(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
#'   data = adrs
#' )
#'
h_proportion_subgroups_df <- function(variables, data, label_all = "All Patients") {

  assert_that(
    is.character(variables$rsp),
    is.character(variables$arm),
    is.character(variables$subgroups),
    is_character_single(label_all),
    is_df_with_variables(data, as.list(unlist(variables))),
    is_valid_factor(data[[variables$arm]]),
    are_equal(nlevels(data[[variables$arm]]), 2)
  )

  subgroup_labels <- var_labels(data[, variables$subgroups, drop = FALSE], fill = TRUE)

  # Add All Patients.
  result_all <- h_proportion_df(data[[variables$rsp]], data[[variables$arm]])
  result_all$subgroup <- label_all
  result_all$var <- "ALL"
  result_all$var_label <- label_all
  result_all$row_type <- "content"

  # Add Subgroups.
  l_subgroups <- lapply(variables$subgroups, function(grp_i){

    l_df <- lapply(split(data, data[[grp_i]]), function(x) {
      h_proportion_df(x[[variables$rsp]], x[[variables$arm]])
    })

    df <- do.call(rbind, args = c(l_df, make.row.names = FALSE))
    df$subgroup <- rep(names(l_df), each = 2)
    df$var <- grp_i
    df$var_label <- subgroup_labels[grp_i]
    df$row_type <- "analysis"

    df
  })

  result_subgroups <- do.call(rbind, args = c(l_subgroups, make.row.names = FALSE))

  rbind(
    result_all,
    result_subgroups
  )

}

#' @describeIn h_response_subgroups Helper to prepare a data frame with estimates of
#'   the odds ratio between a treatment and a control arm.
#' @inheritParams response_subgroups
#' @export
#' @examples
#'
#' h_odds_ratio_df(
#'   c(TRUE, FALSE, FALSE, TRUE),
#'   arm = factor(c("A", "A", "B", "B"), levels = c("A", "B"))
#' )
#' # Include p-value.
#' h_odds_ratio_df(adrs$rsp, adrs$ARM, method = "chisq")
#'
h_odds_ratio_df <- function(rsp, arm, conf_level = 0.95, method = NULL) {

  assert_that(
    is_valid_factor(arm),
    is_equal_length(rsp, arm),
    are_equal(nlevels(arm), 2)
  )

  df_rsp <- data.frame(rsp = rsp)

  l_df <- split(df_rsp, arm)

  # Odds ratio and CI.
  result_odds_ratio <- s_odds_ratio(
    df = l_df[[2]],
    .var = "rsp",
    .ref_group = l_df[[1]],
    .in_ref_col = FALSE,
    conf_level = conf_level
  )

  df <- data.frame(
    # Dummy column needed downstream to create a nested header.
    arm = " ",
    n_tot = nrow(df_rsp),
    or = unname(result_odds_ratio$or_ci["est"]),
    lcl = unname(result_odds_ratio$or_ci["lcl"]),
    ucl = unname(result_odds_ratio$or_ci["ucl"]),
    conf_level = conf_level,
    stringsAsFactors = FALSE
  )

  if (!is.null(method)) {

    # Test for difference.
    result_test <- s_test_proportion_diff(
      df = l_df[[2]],
      .var = "rsp",
      .ref_group = l_df[[1]],
      .in_ref_col = FALSE,
      variables = list(strata = NULL),
      method = method
    )

    df$pval <- as.numeric(result_test$pval)
    df$pval_label <- obj_label(result_test$pval)
  }

  df

}

#' @describeIn h_response_subgroups Summarizes estimates of the odds ratio between a treatment and a control
#'   arm across subgroups in a data frame. `variables` corresponds to the names of variables found in
#'   `data`, passed as a named list and requires elements `rsp`, `arm` and `subgroups`.
#' @export
#' @examples
#'
#' h_odds_ratio_subgroups_df(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
#'   data = adrs
#' )
#'
h_odds_ratio_subgroups_df <- function(variables, data, conf_level = 0.95, method = NULL, label_all = "All Patients") {

  assert_that(
    is.character(variables$rsp),
    is.character(variables$arm),
    is.character(variables$subgroups),
    is_character_single(label_all),
    is_df_with_variables(data, as.list(unlist(variables))),
    is_valid_factor(data[[variables$arm]]),
    are_equal(nlevels(data[[variables$arm]]), 2)
  )

  subgroup_labels <- var_labels(data[, variables$subgroups, drop = FALSE], fill = TRUE)

  # Add All Patients.
  result_all <- h_odds_ratio_df(
    rsp = data[[variables$rsp]],
    arm = data[[variables$arm]],
    conf_level = conf_level,
    method = method
  )
  result_all$subgroup <- label_all
  result_all$var <- "ALL"
  result_all$var_label <- label_all
  result_all$row_type <- "content"

  l_subgroups <- lapply(variables$subgroups, function(grp_i){

    l_df <- lapply(split(data, data[[grp_i]]), function(x) {
      h_odds_ratio_df(
        rsp = x[[variables$rsp]],
        arm = x[[variables$arm]],
        conf_level = conf_level,
        method = method
      )
    })

    df <- do.call(rbind, args = c(l_df, make.row.names = FALSE))
    df$subgroup <- names(l_df)
    df$var <- grp_i
    df$var_label <- subgroup_labels[grp_i]
    df$row_type <- "analysis"

    df
  })

  result_subgroups <- do.call(rbind, args = c(l_subgroups, make.row.names = FALSE))

  rbind(
    result_all,
    result_subgroups
  )

}

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
#' adrs <- adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
#'   droplevels() %>%
#'   mutate(
#'     # Reorder levels of factor to make the placebo group the reference arm.
#'     ARM = forcats::fct_relevel(ARM, "B: Placebo"),
#'     rsp = AVALC == "CR"
#'   )
#' var_labels(adrs) <- c(adrs_labels, "Response")
#'
NULL

#' @describeIn response_subgroups Prepares response rates and odds ratios for
#'   population subgroups in data frames. Simple wrapper for [h_odds_ratio_subgroups_df()] and
#'   [h_proportion_subgroups_df()]. Result is a list of two data frames:
#'   `prop` and `or`. `variables` corresponds to the names of variables found in `data`, passed as a
#'   named list and requires elements `rsp`, `arm` and `subgroups`.
#' @param label_all (`string`)\cr label for the total population analysis.
#' @export
#' @examples
#'
#' df <- extract_rsp_subgroups(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
#'   data = adrs
#' )
#' df
#'
extract_rsp_subgroups <- function(variables, data, conf_level = 0.95, method = NULL, label_all = "All Patients") {

  df_prop <- h_proportion_subgroups_df(variables, data, label_all = label_all)
  df_or <- h_odds_ratio_subgroups_df(variables, data, conf_level = conf_level, method = method, label_all = label_all)

  list(prop = df_prop, or = df_or)
}

#' @describeIn response_subgroups Formatted Analysis function used to format the results of [h_response_subgroups()].
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
tabulate_rsp_subgroups <- function(
  lyt,
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
