#' Tabulate survival duration by subgroup
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The [tabulate_survival_subgroups()] function creates a layout element to tabulate survival duration by subgroup,
#' returning statistics including median survival time and hazard ratio for each population subgroup. The table is
#' created from `df`, a list of data frames returned by [extract_survival_subgroups()], with the statistics to include
#' specified via the `vars` parameter.
#'
#' A forest plot can be created from the resulting table using the [g_forest()] function.
#'
#' @inheritParams argument_convention
#' @inheritParams survival_coxph_pairwise
#' @param df (`list`)\cr list of data frames containing all analysis variables. List should be
#'   created using [extract_survival_subgroups()].
#' @param vars (`character`)\cr the names of statistics to be reported among:
#'   * `n_tot_events`: Total number of events per group.
#'   * `n_events`: Number of events per group.
#'   * `n_tot`: Total number of observations per group.
#'   * `n`: Number of observations per group.
#'   * `median`: Median survival time.
#'   * `hr`: Hazard ratio.
#'   * `ci`: Confidence interval of hazard ratio.
#'   * `pval`: p-value of the effect.
#'   Note, one of the statistics `n_tot` and `n_tot_events`, as well as both `hr` and `ci`
#'   are required.
#' @param time_unit (`string`)\cr label with unit of median survival time. Default `NULL` skips displaying unit.
#'
#' @details These functions create a layout starting from a data frame which contains
#'   the required statistics. Tables typically used as part of forest plot.
#'
#' @seealso [extract_survival_subgroups()]
#'
#' @examples
#' library(dplyr)
#'
#' adtte <- tern_ex_adtte
#'
#' # Save variable labels before data processing steps.
#' adtte_labels <- formatters::var_labels(adtte)
#'
#' adtte_f <- adtte %>%
#'   filter(
#'     PARAMCD == "OS",
#'     ARM %in% c("B: Placebo", "A: Drug X"),
#'     SEX %in% c("M", "F")
#'   ) %>%
#'   mutate(
#'     # Reorder levels of ARM to display reference arm before treatment arm.
#'     ARM = droplevels(forcats::fct_relevel(ARM, "B: Placebo")),
#'     SEX = droplevels(SEX),
#'     AVALU = as.character(AVALU),
#'     is_event = CNSR == 0
#'   )
#' labels <- c(
#'   "ARM" = adtte_labels[["ARM"]],
#'   "SEX" = adtte_labels[["SEX"]],
#'   "AVALU" = adtte_labels[["AVALU"]],
#'   "is_event" = "Event Flag"
#' )
#' formatters::var_labels(adtte_f)[names(labels)] <- labels
#'
#' df <- extract_survival_subgroups(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     arm = "ARM", subgroups = c("SEX", "BMRKR2")
#'   ),
#'   label_all = "Total Patients",
#'   data = adtte_f
#' )
#' df
#'
#' df_grouped <- extract_survival_subgroups(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     arm = "ARM", subgroups = c("SEX", "BMRKR2")
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
#'
#' @name survival_duration_subgroups
#' @order 1
NULL

#' Prepare survival data for population subgroups in data frames
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Prepares estimates of median survival times and treatment hazard ratios for population subgroups in
#' data frames. Simple wrapper for [h_survtime_subgroups_df()] and [h_coxph_subgroups_df()]. Result is a `list`
#' of two `data.frame`s: `survtime` and `hr`. `variables` corresponds to the names of variables found in `data`,
#' passed as a named `list` and requires elements `tte`, `is_event`, `arm` and optionally `subgroups` and `strata`.
#' `groups_lists` optionally specifies groupings for `subgroups` variables.
#'
#' @inheritParams argument_convention
#' @inheritParams survival_duration_subgroups
#' @inheritParams survival_coxph_pairwise
#'
#' @return A named `list` of two elements:
#'   * `survtime`: A `data.frame` containing columns `arm`, `n`, `n_events`, `median`, `subgroup`, `var`,
#'     `var_label`, and `row_type`.
#'   * `hr`: A `data.frame` containing columns `arm`, `n_tot`, `n_tot_events`, `hr`, `lcl`, `ucl`, `conf_level`,
#'     `pval`, `pval_label`, `subgroup`, `var`, `var_label`, and `row_type`.
#'
#' @seealso [survival_duration_subgroups]
#'
#' @export
extract_survival_subgroups <- function(variables,
                                       data,
                                       groups_lists = list(),
                                       control = control_coxph(),
                                       label_all = "All Patients") {
  if ("strat" %in% names(variables)) {
    warning(
      "Warning: the `strat` element name of the `variables` list argument to `extract_survival_subgroups() ",
      "was deprecated in tern 0.9.4.\n  ",
      "Please use the name `strata` instead of `strat` in the `variables` argument."
    )
    variables[["strata"]] <- variables[["strat"]]
  }

  df_survtime <- h_survtime_subgroups_df(
    variables,
    data,
    groups_lists = groups_lists,
    label_all = label_all
  )
  df_hr <- h_coxph_subgroups_df(
    variables,
    data,
    groups_lists = groups_lists,
    control = control,
    label_all = label_all
  )

  list(survtime = df_survtime, hr = df_hr)
}

#' @describeIn survival_duration_subgroups  Formatted analysis function which is used as
#'   `afun` in `tabulate_survival_subgroups()`.
#'
#' @return
#' * `a_survival_subgroups()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_survival_subgroups <- function(df,
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
  cur_col_stat <- extra_afun_params$.var %||% .stats

  # Uniquely name & label rows
  var_lvls <- if ("biomarker" %in% names(dots_extra_args) && "biomarker" %in% names(df)) {
    if ("overall" %in% names(dots_extra_args)) { # label rows for (nested) biomarker tables - e.g. "AGE", "BMRKR1"
      as.character(df$biomarker)
    } else { # data rows for (nested) biomarker tables - e.g. "AGE.LOW", "BMRKR1.Total Patients"
      paste(as.character(df$biomarker), as.character(df$subgroup), sep = ".")
    }
  } else { # data rows for non-biomarker tables - e.g. "Total Patients", "F", "M"
    make.unique(as.character(df$subgroup))
  }

  # if empty, return NA
  if (nrow(df) == 0) {
    return(in_rows(.list = list(NA) %>% stats::setNames(cur_col_stat)))
  }

  # Main statistics taken from df
  x_stats <- as.list(df)

  # Fill in formatting defaults
  .stats <- get_stats("tabulate_survival_subgroups", stats_in = cur_col_stat)
  levels_per_stats <- rep(list(var_lvls), length(.stats)) %>% setNames(.stats)
  .formats <- get_formats_from_stats(.stats, .formats, levels_per_stats)
  .labels <- get_labels_from_stats(
    .stats, .labels, levels_per_stats,
    # default labels are pre-determined in extract_*() function
    tern_defaults = as.list(as.character(df$subgroup)) %>% setNames(var_lvls)
  )
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods, levels_per_stats)

  x_stats <- lapply(
    .stats,
    function(x) x_stats[[x]] %>% stats::setNames(var_lvls)
  ) %>%
    stats::setNames(.stats) %>%
    .unlist_keep_nulls()

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, extra_afun_params$.df_row, extra_afun_params$.var)

  # Get and check statistical names
  .stat_names <- get_stat_names(x_stats, .stat_names)

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = names(.labels),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

#' @describeIn survival_duration_subgroups Table-creating function which creates a table
#'   summarizing survival by subgroup. This function is a wrapper for [rtables::analyze_colvars()]
#'   and [rtables::summarize_row_groups()].
#'
#' @param label_all `r lifecycle::badge("deprecated")`\cr please assign the `label_all` parameter within the
#'   [extract_survival_subgroups()] function when creating `df`.
#' @param riskdiff (`list`)\cr if a risk (proportion) difference column should be added, a list of settings to apply
#'   within the column. See [control_riskdiff()] for details. If `NULL`, no risk difference column will be added. If
#'   `riskdiff$arm_x` and `riskdiff$arm_y` are `NULL`, the first level of `df$survtime$arm` will be used as `arm_x`
#'   and the second level as `arm_y`.
#'
#' @return An `rtables` table summarizing survival by subgroup.
#'
#' @examples
#' ## Table with default columns.
#' basic_table() %>%
#'   tabulate_survival_subgroups(df, time_unit = adtte_f$AVALU[1])
#'
#' ## Table with a manually chosen set of columns: adding "pval".
#' basic_table() %>%
#'   tabulate_survival_subgroups(
#'     df = df,
#'     vars = c("n_tot_events", "n_events", "median", "hr", "ci", "pval"),
#'     time_unit = adtte_f$AVALU[1]
#'   )
#'
#' @export
#' @order 2
tabulate_survival_subgroups <- function(lyt,
                                        df,
                                        vars = c("n_tot_events", "n_events", "median", "hr", "ci"),
                                        groups_lists = list(),
                                        label_all = lifecycle::deprecated(),
                                        time_unit = NULL,
                                        riskdiff = NULL,
                                        na_str = default_na_str(),
                                        ...,
                                        .stat_names = NULL,
                                        .formats = NULL,
                                        .labels = NULL,
                                        .indent_mods = NULL) {
  checkmate::assert_list(riskdiff, null.ok = TRUE)
  checkmate::assert_true(any(c("n_tot", "n_tot_events") %in% vars))
  checkmate::assert_true(all(c("hr", "ci") %in% vars))
  if ("pval" %in% vars && !"pval" %in% names(df$hr)) {
    warning(
      'The "pval" statistic has been selected but is not present in "df" so it will not be included in the output ',
      'table. To include the "pval" statistic, please specify a p-value test when generating "df" via ',
      'the "method" argument to `extract_survival_subgroups()`. If method = "cmh", strata must also be specified via ',
      'the "variables" argument to `extract_survival_subgroups()`.'
    )
  }

  if (lifecycle::is_present(label_all)) {
    lifecycle::deprecate_warn(
      "0.9.5", "tabulate_survival_subgroups(label_all)",
      details =
        "Please assign the `label_all` parameter within the `extract_survival_subgroups()` function when creating `df`."
    )
  }

  # Process standard extra arguments
  extra_args <- list(".stats" = vars)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Create "ci" column from "lcl" and "ucl"
  df$hr$ci <- combine_vectors(df$hr$lcl, df$hr$ucl)

  # Extract additional parameters from df
  conf_level <- df$hr$conf_level[1]
  method <- if ("pval_label" %in% names(df$hr)) df$hr$pval_label[1] else NULL
  colvars <- d_survival_subgroups_colvars(vars, conf_level = conf_level, method = method, time_unit = time_unit)
  survtime_vars <- intersect(colvars$vars, c("n", "n_events", "median"))
  hr_vars <- intersect(names(colvars$labels), c("n_tot", "n_tot_events", "hr", "ci", "pval"))
  colvars_survtime <- list(vars = survtime_vars, labels = colvars$labels[survtime_vars])
  colvars_hr <- list(vars = hr_vars, labels = colvars$labels[hr_vars])

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    groups_lists = list(groups_lists), conf_level = conf_level, method = method,
    ...
  )

  # Adding additional info from layout to analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_survival_subgroups) <- c(formals(a_survival_subgroups), extra_args[[".additional_fun_parameters"]])

  # Add risk difference column
  if (!is.null(riskdiff)) {
    if (is.null(riskdiff$arm_x)) riskdiff$arm_x <- levels(df$survtime$arm)[1]
    if (is.null(riskdiff$arm_y)) riskdiff$arm_y <- levels(df$survtime$arm)[2]
    colvars_hr$vars <- c(colvars_hr$vars, "riskdiff")
    colvars_hr$labels <- c(colvars_hr$labels, riskdiff = riskdiff$col_label)
    arm_cols <- paste(rep(c("n_events", "n_events", "n", "n")), c(riskdiff$arm_x, riskdiff$arm_y), sep = "_")
    extra_args[[".formats"]] <- c(extra_args[[".formats"]], list(riskdiff = riskdiff$format))

    df_prop_diff <- df$survtime %>%
      dplyr::select(-"median") %>%
      tidyr::pivot_wider(
        id_cols = c("subgroup", "var", "var_label", "row_type"),
        names_from = "arm",
        values_from = c("n", "n_events")
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

    df$hr <- df$hr %>%
      dplyr::left_join(
        df_prop_diff,
        by = c("subgroup", "var", "var_label", "row_type")
      )
  }

  # Add columns from table_survtime (optional)
  if (length(colvars_survtime$vars) > 0) {
    lyt_survtime <- split_cols_by(lyt = lyt, var = "arm")
    lyt_survtime <- split_cols_by_multivar(
      lyt = lyt_survtime,
      vars = colvars_survtime$vars,
      varlabels = colvars_survtime$labels
    )

    # Add "All Patients" row
    lyt_survtime <- split_rows_by(
      lyt = lyt_survtime,
      var = "row_type",
      split_fun = keep_split_levels("content"),
      nested = FALSE,
      child_labels = "hidden",
      parent_name = "All Patients"
    )
    lyt_survtime <- analyze_colvars(
      lyt = lyt_survtime,
      afun = a_survival_subgroups,
      na_str = na_str,
      extra_args = extra_args
    )

    # Add analysis rows
    if ("analysis" %in% df$survtime$row_type) {
      lyt_survtime <- split_rows_by(
        lyt = lyt_survtime,
        var = "row_type",
        split_fun = keep_split_levels("analysis"),
        nested = FALSE,
        child_labels = "hidden",
        parent_name = "analysis rows"
      )
      lyt_survtime <- split_rows_by(lyt = lyt_survtime, var = "var_label", nested = TRUE)
      lyt_survtime <- analyze_colvars(
        lyt = lyt_survtime,
        afun = a_survival_subgroups,
        na_str = na_str,
        inclNAs = TRUE,
        extra_args = extra_args
      )
    }

    table_survtime <- build_table(lyt_survtime, df = df$survtime)
  } else {
    table_survtime <- NULL
  }

  # Add columns from table_hr ("n_tot_events" or "n_tot", "hr" and "ci" required)
  lyt_hr <- split_cols_by(lyt = lyt, var = "arm")
  lyt_hr <- split_cols_by_multivar(
    lyt = lyt_hr,
    vars = colvars_hr$vars,
    varlabels = colvars_hr$labels
  )

  # Add "All Patients" row
  lyt_hr <- split_rows_by(
    lyt = lyt_hr,
    var = "row_type",
    split_fun = keep_split_levels("content"),
    nested = FALSE,
    child_labels = "hidden",
    parent_name = "All patient row"
  )
  lyt_hr <- analyze_colvars(
    lyt = lyt_hr,
    afun = a_survival_subgroups,
    na_str = na_str,
    extra_args = extra_args
  ) %>%
    append_topleft("Baseline Risk Factors")

  # Add analysis rows
  if ("analysis" %in% df$survtime$row_type) {
    lyt_hr <- split_rows_by(
      lyt = lyt_hr,
      var = "row_type",
      split_fun = keep_split_levels("analysis"),
      nested = FALSE,
      child_labels = "hidden",
      parent_name = "analysis rows"
    )
    lyt_hr <- split_rows_by(lyt = lyt_hr, var = "var_label", nested = TRUE)
    lyt_hr <- analyze_colvars(
      lyt = lyt_hr,
      afun = a_survival_subgroups,
      na_str = na_str,
      inclNAs = TRUE,
      extra_args = extra_args
    )
  }

  table_hr <- build_table(lyt_hr, df = df$hr)

  # Join tables, add forest plot attributes
  n_tot_ids <- grep("^n_tot", colvars_hr$vars)
  if (is.null(table_survtime)) {
    result <- table_hr
    hr_id <- match("hr", colvars_hr$vars)
    ci_id <- match("ci", colvars_hr$vars)
  } else {
    result <- cbind_rtables(table_hr[, n_tot_ids], table_survtime, table_hr[, -n_tot_ids])
    hr_id <- length(n_tot_ids) + ncol(table_survtime) + match("hr", colvars_hr$vars[-n_tot_ids])
    ci_id <- length(n_tot_ids) + ncol(table_survtime) + match("ci", colvars_hr$vars[-n_tot_ids])
    n_tot_ids <- seq_along(n_tot_ids)
  }
  structure(
    result,
    forest_header = paste0(rev(levels(df$survtime$arm)), "\nBetter"),
    col_x = hr_id,
    col_ci = ci_id,
    col_symbol_size = n_tot_ids[1] # for scaling the symbol sizes in forest plots
  )
}

#' Labels for column variables in survival duration by subgroup table
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Internal function to check variables included in [tabulate_survival_subgroups()] and create column labels.
#'
#' @inheritParams tabulate_survival_subgroups
#' @inheritParams argument_convention
#' @param method (`string`)\cr p-value method for testing hazard ratio = 1.
#'
#' @return A `list` of variables and their labels to tabulate.
#'
#' @note At least one of `n_tot` and `n_tot_events` must be provided in `vars`.
#'
#' @export
d_survival_subgroups_colvars <- function(vars,
                                         conf_level,
                                         method,
                                         time_unit = NULL) {
  checkmate::assert_character(vars)
  checkmate::assert_string(time_unit, null.ok = TRUE)
  checkmate::assert_subset(c("hr", "ci"), vars)
  checkmate::assert_true(any(c("n_tot", "n_tot_events") %in% vars))
  checkmate::assert_subset(
    vars,
    c("n", "n_events", "median", "n_tot", "n_tot_events", "hr", "ci", "pval")
  )

  propcase_time_label <- if (!is.null(time_unit)) {
    paste0("Median (", time_unit, ")")
  } else {
    "Median"
  }

  varlabels <- c(
    n = "n",
    n_events = "Events",
    median = propcase_time_label,
    n_tot = "Total n",
    n_tot_events = "Total Events",
    hr = "Hazard Ratio",
    ci = paste0(100 * conf_level, "% Wald CI"),
    pval = method
  )

  colvars <- vars

  list(
    vars = colvars,
    labels = varlabels[vars]
  )
}
