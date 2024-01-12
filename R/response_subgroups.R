#' Tabulate Binary Response by Subgroup
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Tabulate statistics such as response rate and odds ratio for population subgroups.
#'
#' @inheritParams extract_rsp_subgroups
#' @inheritParams argument_convention
#'
#' @details These functions create a layout starting from a data frame which contains
#'   the required statistics. Tables typically used as part of forest plot.
#'
#' @seealso [extract_rsp_subgroups()]
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
#'   filter(ARM %in% c("A: Drug X", "B: Placebo")) %>%
#'   droplevels() %>%
#'   mutate(
#'     # Reorder levels of factor to make the placebo group the reference arm.
#'     ARM = fct_relevel(ARM, "B: Placebo"),
#'     rsp = AVALC == "CR"
#'   )
#' formatters::var_labels(adrs_f) <- c(adrs_labels, "Response")
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
#' @name response_subgroups
#' @order 1
NULL

#' Prepares Response Data for Population Subgroups in Data Frames
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Prepares response rates and odds ratios for population subgroups in data frames. Simple wrapper
#' for [h_odds_ratio_subgroups_df()] and [h_proportion_subgroups_df()]. Result is a list of two
#' `data.frames`: `prop` and `or`. `variables` corresponds to the names of variables found in `data`,
#' passed as a named `list` and requires elements `rsp`, `arm` and optionally `subgroups` and `strat`.
#' `groups_lists` optionally specifies groupings for `subgroups` variables.
#'
#' @inheritParams argument_convention
#' @inheritParams response_subgroups
#' @param label_all (`string`)\cr label for the total population analysis.
#'
#' @return A named list of two elements:
#'   * `prop`: A `data.frame` containing columns `arm`, `n`, `n_rsp`, `prop`, `subgroup`, `var`,
#'     `var_label`, and `row_type`.
#'   * `or`: A `data.frame` containing columns `arm`, `n_tot`, `or`, `lcl`, `ucl`, `conf_level`,
#'     `subgroup`, `var`, `var_label`, and `row_type`.
#'
#' @seealso [response_subgroups]
#'
#' @export
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

#' @describeIn response_subgroups Formatted analysis function which is used as `afun` in `tabulate_rsp_subgroups()`.
#'
#' @return
#' * `a_response_subgroups()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_response_subgroups <- function(.formats = list(
                                   n = "xx", # nolint start
                                   n_rsp = "xx",
                                   prop = "xx.x%",
                                   n_tot = "xx",
                                   or = list(format_extreme_values(2L)),
                                   ci = list(format_extreme_values_ci(2L)),
                                   pval = "x.xxxx | (<0.0001)" # nolint end
                                 ),
                                 na_str = default_na_str()) {
  checkmate::assert_list(.formats)
  checkmate::assert_subset(
    names(.formats),
    c("n", "n_rsp", "prop", "n_tot", "or", "ci", "pval")
  )

  afun_lst <- Map(
    function(stat, fmt, na_str) {
      if (stat == "ci") {
        function(df, labelstr = "", ...) {
          in_rows(
            .list = combine_vectors(df$lcl, df$ucl),
            .labels = as.character(df$subgroup),
            .formats = fmt,
            .format_na_strs = na_str
          )
        }
      } else {
        function(df, labelstr = "", ...) {
          in_rows(
            .list = as.list(df[[stat]]),
            .labels = as.character(df$subgroup),
            .formats = fmt,
            .format_na_strs = na_str
          )
        }
      }
    },
    stat = names(.formats),
    fmt = .formats,
    na_str = na_str
  )

  afun_lst
}

#' @describeIn response_subgroups Table-creating function which creates a table
#'   summarizing binary response by subgroup. This function is a wrapper for [rtables::analyze_colvars()]
#'   and [rtables::summarize_row_groups()].
#'
#' @param df (`list`)\cr of data frames containing all analysis variables. List should be
#'   created using [extract_rsp_subgroups()].
#' @param vars (`character`)\cr the names of statistics to be reported among:
#'   * `n`: Total number of observations per group.
#'   * `n_rsp`: Number of responders per group.
#'   * `prop`: Proportion of responders.
#'   * `n_tot`: Total number of observations.
#'   * `or`: Odds ratio.
#'   * `ci` : Confidence interval of odds ratio.
#'   * `pval`: p-value of the effect.
#'   Note, the statistics `n_tot`, `or` and `ci` are required.
#'
#' @return An `rtables` table summarizing binary response by subgroup.
#'
#' @examples
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
#' @export
#' @order 2
tabulate_rsp_subgroups <- function(lyt,
                                   df,
                                   vars = c("n_tot", "n", "prop", "or", "ci"),
                                   groups_lists = list(),
                                   label_all = "All Patients",
                                   na_str = default_na_str()) {
  conf_level <- df$or$conf_level[1]
  method <- if ("pval_label" %in% names(df$or)) {
    df$or$pval_label[1]
  } else {
    NULL
  }

  extra_args <- list(groups_lists = groups_lists, conf_level = conf_level, method = method, label_all = label_all)

  afun_lst <- a_response_subgroups(na_str = na_str)
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
    lyt_prop <- split_cols_by_multivar(
      lyt = lyt_prop,
      vars = colvars_prop$vars,
      varlabels = colvars_prop$labels
    )

    # "All Patients" row
    lyt_prop <- split_rows_by(
      lyt = lyt_prop,
      var = "row_type",
      split_fun = keep_split_levels("content"),
      nested = FALSE,
      child_labels = "hidden"
    )
    lyt_prop <- analyze_colvars(
      lyt = lyt_prop,
      afun = afun_lst[names(colvars_prop$labels)],
      na_str = na_str,
      extra_args = extra_args
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
        na_str = na_str,
        inclNAs = TRUE,
        extra_args = extra_args
      )
    }

    table_prop <- build_table(lyt_prop, df = df$prop)
  } else {
    table_prop <- NULL
  }

  # Columns "n_tot", "or", "ci" in table_or are required.
  lyt_or <- split_cols_by(lyt = lyt, var = "arm")
  lyt_or <- split_cols_by_multivar(
    lyt = lyt_or,
    vars = colvars_or$vars,
    varlabels = colvars_or$labels
  )

  # "All Patients" row
  lyt_or <- split_rows_by(
    lyt = lyt_or,
    var = "row_type",
    split_fun = keep_split_levels("content"),
    nested = FALSE,
    child_labels = "hidden"
  )
  lyt_or <- analyze_colvars(
    lyt = lyt_or,
    afun = afun_lst[names(colvars_or$labels)],
    na_str = na_str,
    extra_args = extra_args
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
      na_str = na_str,
      inclNAs = TRUE,
      extra_args = extra_args
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
#' @description `r lifecycle::badge("stable")`
#'
#' Internal function to check variables included in [tabulate_rsp_subgroups()] and create column labels.
#'
#' @inheritParams argument_convention
#' @inheritParams tabulate_rsp_subgroups
#'
#' @return A `list` of variables to tabulate and their labels.
#'
#' @export
d_rsp_subgroups_colvars <- function(vars,
                                    conf_level = NULL,
                                    method = NULL) {
  checkmate::assert_character(vars)
  checkmate::assert_subset(c("n_tot", "or", "ci"), vars)
  checkmate::assert_subset(
    vars,
    c("n", "n_rsp", "prop", "n_tot", "or", "ci", "pval")
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
    checkmate::assert_false(is.null(conf_level))

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
