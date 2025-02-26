#' Tabulate binary response by subgroup
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The [tabulate_rsp_subgroups()] function creates a layout element to tabulate binary response by subgroup, returning
#' statistics including response rate and odds ratio for each population subgroup. The table is created from `df`, a
#' list of data frames returned by [extract_rsp_subgroups()], with the statistics to include specified via the `vars`
#' parameter.
#'
#' A forest plot can be created from the resulting table using the [g_forest()] function.
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
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2"), strata = "STRATA1"),
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

#' Prepare response data for population subgroups in data frames
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Prepares response rates and odds ratios for population subgroups in data frames. Simple wrapper
#' for [h_odds_ratio_subgroups_df()] and [h_proportion_subgroups_df()]. Result is a list of two
#' `data.frames`: `prop` and `or`. `variables` corresponds to the names of variables found in `data`,
#' passed as a named `list` and requires elements `rsp`, `arm` and optionally `subgroups` and `strata`.
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
  if ("strat" %in% names(variables)) {
    warning(
      "Warning: the `strat` element name of the `variables` list argument to `extract_rsp_subgroups() ",
      "was deprecated in tern 0.9.4.\n  ",
      "Please use the name `strata` instead of `strat` in the `variables` argument."
    )
    variables[["strata"]] <- variables[["strat"]]
  }

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
a_response_subgroups <- function(df,
                                 labelstr = "",
                                 ...,
                                 .stats = NULL,
                                 .stat_names = NULL,
                                 .formats = NULL,
                                 .labels = NULL,
                                 .indent_mods = NULL) {
  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)
  extra_afun_params <- retrieve_extra_afun_params(names(dots_extra_args$.additional_fun_parameters))
  dots_extra_args$.additional_fun_parameters <- NULL
  cur_stat <- extra_afun_params$.var %||% .stats
  var_lvls <- if ("biomarker" %in% names(dots_extra_args) && "biomarker" %in% names(df)) {
    if ("overall" %in% names(dots_extra_args)) {
      as.character(df$biomarker)
    } else {
      paste(as.character(df$biomarker), as.character(df$subgroup), sep = ".")
    }
  } else {
    make.unique(as.character(df$subgroup))
  }

  # if empty, return NA
  if (nrow(df) == 0) {
    return(in_rows(.list = list(NA) %>% stats::setNames(cur_stat)))
  }

  # Main statistics taken from df
  x_stats <- as.list(df)

  # Fill in formatting defaults
  .stats <- get_stats("tabulate_rsp_subgroups", stats_in = cur_stat)
  levels_per_stats <- rep(list(var_lvls), length(.stats)) %>% setNames(.stats)
  .formats <- get_formats_from_stats(.stats, .formats, levels_per_stats)
  .labels <- get_labels_from_stats(
    .stats, .labels, levels_per_stats,
    tern_defaults = as.list(as.character(df$subgroup)) %>% setNames(var_lvls)
  )
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods, levels_per_stats)

  x_stats <- lapply(
    .stats,
    function(x) x_stats[[x]] %>% stats::setNames(var_lvls)
  ) %>%
    stats::setNames(.stats)

  .nms <- if ("biomarker" %in% names(dots_extra_args)) var_lvls else names(.labels)

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, extra_afun_params$.df_row, extra_afun_params$.var)

  # Get and check statistical names
  .stat_names <- get_stat_names(x_stats, .stat_names)

  in_rows(
    .list = x_stats %>% .unlist_keep_nulls(),
    .formats = .formats,
    .names = .nms,
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

#' @describeIn response_subgroups Table-creating function which creates a table
#'   summarizing binary response by subgroup. This function is a wrapper for [rtables::analyze_colvars()]
#'   and [rtables::summarize_row_groups()].
#'
#' @param df (`list`)\cr a list of data frames containing all analysis variables. List should be
#'   created using [extract_rsp_subgroups()].
#' @param vars (`character`)\cr the names of statistics to be reported among:
#'   * `n`: Total number of observations per group.
#'   * `n_rsp`: Number of responders per group.
#'   * `prop`: Proportion of responders.
#'   * `n_tot`: Total number of observations.
#'   * `or`: Odds ratio.
#'   * `ci` : Confidence interval of odds ratio.
#'   * `pval`: p-value of the effect.
#'   Note, the statistics `n_tot`, `or`, and `ci` are required.
#' @param riskdiff (`list`)\cr if a risk (proportion) difference column should be added, a list of settings to apply
#'   within the column. See [control_riskdiff()] for details. If `NULL`, no risk difference column will be added. If
#'   `riskdiff$arm_x` and `riskdiff$arm_y` are `NULL`, the first level of `df$prop$arm` will be used as `arm_x` and
#'   the second level as `arm_y`.
#'
#' @return An `rtables` table summarizing binary response by subgroup.
#'
#' @examples
#' # Table with default columns
#' basic_table() %>%
#'   tabulate_rsp_subgroups(df)
#'
#' # Table with selected columns
#' basic_table() %>%
#'   tabulate_rsp_subgroups(
#'     df = df,
#'     vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci")
#'   )
#'
#' # Table with risk difference column added
#' basic_table() %>%
#'   tabulate_rsp_subgroups(
#'     df,
#'     riskdiff = control_riskdiff(
#'       arm_x = levels(df$prop$arm)[1],
#'       arm_y = levels(df$prop$arm)[2]
#'     )
#'   )
#'
#' @export
#' @order 2
tabulate_rsp_subgroups <- function(lyt,
                                   df,
                                   vars = c("n_tot", "n", "prop", "or", "ci"),
                                   groups_lists = list(),
                                   label_all = lifecycle::deprecated(),
                                   riskdiff = NULL,
                                   na_str = default_na_str(),
                                   ...,
                                   .stat_names = NULL,
                                   .formats = NULL,
                                   .labels = NULL,
                                   .indent_mods = NULL) {
  checkmate::assert_list(riskdiff, null.ok = TRUE)
  checkmate::assert_true(all(c("n_tot", "or", "ci") %in% vars))
  if ("pval" %in% vars && !"pval" %in% names(df$or)) {
    warning(
      'The "pval" statistic has been selected but is not present in "df" so it will not be included in the output ',
      'table. To include the "pval" statistic, please specify a p-value test when generating "df" via ',
      'the "method" argument to `extract_rsp_subgroups()`. If method = "cmh", strata must also be specified via the ',
      '"variables" argument to `extract_rsp_subgroups()`.'
    )
  }

  if (lifecycle::is_present(label_all)) {
    lifecycle::deprecate_warn(
      "0.9.8", "tabulate_rsp_subgroups(label_all)",
      details =
        "Please assign the `label_all` parameter within the `extract_rsp_subgroups()` function when creating `df`."
    )
  }

  # Process standard extra arguments
  extra_args <- list(".stats" = vars)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Create "ci" column from "lcl" and "ucl"
  df$or$ci <- combine_vectors(df$or$lcl, df$or$ucl)

  # Extract additional parameters from df
  conf_level <- df$or$conf_level[1]
  method <- if ("pval_label" %in% names(df$or)) df$or$pval_label[1] else NULL
  colvars <- d_rsp_subgroups_colvars(vars, conf_level = conf_level, method = method)
  prop_vars <- intersect(colvars$vars, c("n", "prop", "n_rsp"))
  or_vars <- intersect(names(colvars$labels), c("n_tot", "or", "ci", "pval"))
  colvars_prop <- list(vars = prop_vars, labels = colvars$labels[prop_vars])
  colvars_or <- list(vars = or_vars, labels = colvars$labels[or_vars])

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    groups_lists = list(groups_lists), conf_level = conf_level, method = method,
    ...
  )

  # Adding additional info from layout to analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_response_subgroups) <- c(formals(a_response_subgroups), extra_args[[".additional_fun_parameters"]])

  # Add risk difference column
  if (!is.null(riskdiff)) {
    if (is.null(riskdiff$arm_x)) riskdiff$arm_x <- levels(df$prop$arm)[1]
    if (is.null(riskdiff$arm_y)) riskdiff$arm_y <- levels(df$prop$arm)[2]
    colvars_or$vars <- c(colvars_or$vars, "riskdiff")
    colvars_or$labels <- c(colvars_or$labels, riskdiff = riskdiff$col_label)
    arm_cols <- paste(rep(c("n_rsp", "n_rsp", "n", "n")), c(riskdiff$arm_x, riskdiff$arm_y), sep = "_")

    df_prop_diff <- df$prop %>%
      dplyr::select(-"prop") %>%
      tidyr::pivot_wider(
        id_cols = c("subgroup", "var", "var_label", "row_type"),
        names_from = "arm",
        values_from = c("n", "n_rsp")
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        riskdiff = stat_propdiff_ci(
          x = as.list(.data[[arm_cols[1]]]),
          y = as.list(.data[[arm_cols[2]]]),
          N_x = .data[[arm_cols[3]]],
          N_y = .data[[arm_cols[4]]],
          pct = riskdiff$pct
        )
      ) %>%
      dplyr::select(-dplyr::all_of(arm_cols))

    df$or <- df$or %>%
      dplyr::left_join(
        df_prop_diff,
        by = c("subgroup", "var", "var_label", "row_type")
      )
  }

  # Add columns from table_prop (optional)
  if (length(colvars_prop$vars) > 0) {
    lyt_prop <- split_cols_by(lyt = lyt, var = "arm")
    lyt_prop <- split_cols_by_multivar(
      lyt = lyt_prop,
      vars = colvars_prop$vars,
      varlabels = colvars_prop$labels
    )

    # Add "All Patients" row
    lyt_prop <- split_rows_by(
      lyt = lyt_prop,
      var = "row_type",
      split_fun = keep_split_levels("content"),
      nested = FALSE,
      child_labels = "hidden"
    )
    lyt_prop <- analyze_colvars(
      lyt = lyt_prop,
      afun = a_response_subgroups,
      na_str = na_str,
      extra_args = extra_args
    )

    # Add analysis rows
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
        afun = a_response_subgroups,
        na_str = na_str,
        inclNAs = TRUE,
        extra_args = extra_args
      )
    }

    table_prop <- build_table(lyt_prop, df = df$prop)
  } else {
    table_prop <- NULL
  }

  # Add columns from table_or ("n_tot", "or", and "ci" required)
  lyt_or <- split_cols_by(lyt = lyt, var = "arm")
  lyt_or <- split_cols_by_multivar(
    lyt = lyt_or,
    vars = colvars_or$vars,
    varlabels = colvars_or$labels
  )

  # Add "All Patients" row
  lyt_or <- split_rows_by(
    lyt = lyt_or,
    var = "row_type",
    split_fun = keep_split_levels("content"),
    nested = FALSE,
    child_labels = "hidden"
  )
  lyt_or <- analyze_colvars(
    lyt = lyt_or,
    afun = a_response_subgroups,
    na_str = na_str,
    extra_args = extra_args
  ) %>%
    append_topleft("Baseline Risk Factors")

  # Add analysis rows
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
      afun = a_response_subgroups,
      na_str = na_str,
      inclNAs = TRUE,
      extra_args = extra_args
    )
  }

  table_or <- build_table(lyt_or, df = df$or)

  # Join tables, add forest plot attributes
  n_tot_id <- match("n_tot", colvars_or$vars)
  if (is.null(table_prop)) {
    result <- table_or
    or_id <- match("or", colvars_or$vars)
    ci_id <- match("ci", colvars_or$vars)
  } else {
    result <- cbind_rtables(table_or[, n_tot_id], table_prop, table_or[, -n_tot_id])
    or_id <- 1L + ncol(table_prop) + match("or", colvars_or$vars[-n_tot_id])
    ci_id <- 1L + ncol(table_prop) + match("ci", colvars_or$vars[-n_tot_id])
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

#' Labels for column variables in binary response by subgroup table
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
