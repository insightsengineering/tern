#' Tabulate Survival Duration by Subgroup
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Tabulate statistics such as median survival time and hazard ratio for population subgroups.
#'
#' @details These functions create a layout starting from a data frame which contains
#'   the required statistics. Tables typically used as part of forest plot.
#'
#' @inheritParams argument_convention
#' @inheritParams survival_coxph_pairwise
#' @param data (`data frame`)\cr the dataset containing the variables to summarize.
#' @param groups_lists (named `list` of `list`)\cr optionally contains for each `subgroups` variable a
#'   list, which specifies the new group levels via the names and the
#'   levels that belong to it in the character vectors that are elements of the list.
#' @param label_all (`string`)\cr label for the total population analysis.
#' @param time_unit (`string`)\cr label with unit of median survival time. Default `NULL` skips
#'   displaying unit.
#' @name survival_duration_subgroups
#' @order 1
#' @examples
#'
#' # Testing dataset.
#' library(scda)
#' library(dplyr)
#' library(forcats)
#' library(rtables)
#'
#' adtte <- synthetic_cdisc_data("latest")$adtte
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
#'     ARM = droplevels(fct_relevel(ARM, "B: Placebo")),
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
NULL

#' @describeIn survival_duration_subgroups prepares estimates of median survival times and treatment hazard ratios for
#'   population subgroups in data frames. Simple wrapper for [h_survtime_subgroups_df()] and [h_coxph_subgroups_df()].
#'   Result is a list of two data frames: `survtime` and `hr`.
#'   `variables` corresponds to the names of variables found in `data`, passed as a named list and requires elements
#'   `tte`, `is_event`, `arm` and optionally `subgroups` and `strat`. `groups_lists` optionally specifies
#'   groupings for `subgroups` variables.
#' @export
#' @examples
#'
#' df <- extract_survival_subgroups(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     arm = "ARM", subgroups = c("SEX", "BMRKR2")
#'   ),
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
extract_survival_subgroups <- function(variables,
                                       data,
                                       groups_lists = list(),
                                       control = control_coxph(),
                                       label_all = "All Patients") {
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

#' @describeIn survival_duration_subgroups Formatted Analysis function used to format the results of
#'   [extract_survival_subgroups()]. Returns is a list of Formatted Analysis functions with one element per statistic.
#'
#' @examples
#' # Internal function - a_survival_subgroups
#' \dontrun{
#' a_survival_subgroups(.formats = list("n" = "xx", "median" = "xx.xx"))
#' }
#'
#' @keywords internal
a_survival_subgroups <- function(.formats = list(
                                   n = "xx",
                                   n_events = "xx",
                                   n_tot_events = "xx",
                                   median = "xx.x",
                                   n_tot = "xx",
                                   hr = list(format_extreme_values(2L)),
                                   ci = list(format_extreme_values_ci(2L)),
                                   pval = "x.xxxx | (<0.0001)"
                                 )) {
  checkmate::assert_list(.formats)
  checkmate::assert_subset(
    names(.formats),
    c("n", "n_events", "median", "n_tot", "n_tot_events", "hr", "ci", "pval")
  )

  afun_lst <- Map(function(stat, fmt) {
    if (stat == "ci") {
      function(df, labelstr = "", ...) {
        in_rows(
          .list = combine_vectors(df$lcl, df$ucl),
          .labels = as.character(df$subgroup),
          .formats = fmt
        )
      }
    } else {
      function(df, labelstr = "", ...) {
        in_rows(
          .list = as.list(df[[stat]]),
          .labels = as.character(df$subgroup),
          .formats = fmt
        )
      }
    }
  },
  stat = names(.formats),
  fmt = .formats
  )

  afun_lst
}

#' @describeIn survival_duration_subgroups table creating function.
#' @param df (`list`)\cr of data frames containing all analysis variables. List should be
#'   created using [extract_survival_subgroups()].
#' @param vars (`character`)\cr the name of statistics to be reported among
#'  `n_tot_events` (total number of events per group),
#'  `n_events` (number of events per group),
#'  `n_tot` (total number of observations per group),
#'  `n` (number of observations per group),
#'  `median` (median survival time),
#'  `hr` (hazard ratio),
#'  `ci` (confidence interval of hazard ratio) and
#'  `pval` (p value of the effect).
#'  Note, one of the statistics `n_tot` and `n_tot_events`, as well as both `hr` and `ci`
#'  are required.
#' @export
#' @examples
#'
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
tabulate_survival_subgroups <- function(lyt,
                                        df,
                                        vars = c("n_tot_events", "n_events", "median", "hr", "ci"),
                                        time_unit = NULL) {
  conf_level <- df$hr$conf_level[1]
  method <- df$hr$pval_label[1]

  afun_lst <- a_survival_subgroups()
  colvars <- d_survival_subgroups_colvars(
    vars,
    conf_level = conf_level,
    method = method,
    time_unit = time_unit
  )

  colvars_survtime <- list(
    vars = colvars$vars[names(colvars$labels) %in% c("n", "n_events", "median")],
    labels = colvars$labels[names(colvars$labels) %in% c("n", "n_events", "median")]
  )
  colvars_hr <- list(
    vars = colvars$vars[names(colvars$labels) %in% c("n_tot", "n_tot_events", "hr", "ci", "pval")],
    labels = colvars$labels[names(colvars$labels) %in% c("n_tot", "n_tot_events", "hr", "ci", "pval")]
  )

  # Columns from table_survtime are optional.
  if (length(colvars_survtime$vars) > 0) {
    lyt_survtime <- split_cols_by(lyt = lyt, var = "arm")
    lyt_survtime <- split_rows_by(
      lyt = lyt_survtime,
      var = "row_type",
      split_fun = keep_split_levels("content"),
      nested = FALSE
    )
    lyt_survtime <- summarize_row_groups(
      lyt = lyt_survtime,
      var = "var_label",
      cfun = afun_lst[names(colvars_survtime$labels)]
    )
    lyt_survtime <- split_cols_by_multivar(
      lyt = lyt_survtime,
      vars = colvars_survtime$vars,
      varlabels = colvars_survtime$labels
    )

    if ("analysis" %in% df$survtime$row_type) {
      lyt_survtime <- split_rows_by(
        lyt = lyt_survtime,
        var = "row_type",
        split_fun = keep_split_levels("analysis"),
        nested = FALSE,
        child_labels = "hidden"
      )
      lyt_survtime <- split_rows_by(lyt = lyt_survtime, var = "var_label", nested = TRUE)
      lyt_survtime <- analyze_colvars(
        lyt = lyt_survtime,
        afun = afun_lst[names(colvars_survtime$labels)],
        inclNAs = TRUE
      )
    }

    table_survtime <- build_table(lyt_survtime, df = df$survtime)
  } else {
    table_survtime <- NULL
  }

  # Columns "n_tot_events" or "n_tot", and "hr", "ci" in table_hr are required.
  lyt_hr <- split_cols_by(lyt = lyt, var = "arm")
  lyt_hr <- split_rows_by(
    lyt = lyt_hr,
    var = "row_type",
    split_fun = keep_split_levels("content"),
    nested = FALSE
  )
  lyt_hr <- summarize_row_groups(
    lyt = lyt_hr,
    var = "var_label",
    cfun = afun_lst[names(colvars_hr$labels)]
  )
  lyt_hr <- split_cols_by_multivar(
    lyt = lyt_hr,
    vars = colvars_hr$vars,
    varlabels = colvars_hr$labels
  ) %>%
    append_topleft("Baseline Risk Factors")

  if ("analysis" %in% df$survtime$row_type) {
    lyt_hr <- split_rows_by(
      lyt = lyt_hr,
      var = "row_type",
      split_fun = keep_split_levels("analysis"),
      nested = FALSE,
      child_labels = "hidden"
    )
    lyt_hr <- split_rows_by(lyt = lyt_hr, var = "var_label", nested = TRUE)
    lyt_hr <- analyze_colvars(
      lyt = lyt_hr,
      afun = afun_lst[names(colvars_hr$labels)],
      inclNAs = TRUE
    )
  }
  table_hr <- build_table(lyt_hr, df = df$hr)

  # There can be one or two vars starting with "n_tot".
  n_tot_ids <- grep("^n_tot", colvars_hr$vars)
  if (is.null(table_survtime)) {
    result <- table_hr
    hr_id <- match("hr", colvars_hr$vars)
    ci_id <- match("lcl", colvars_hr$vars)
  } else {
    # Reorder the table.
    result <- cbind_rtables(table_hr[, n_tot_ids], table_survtime, table_hr[, -n_tot_ids])
    # And then calculate column indices accordingly.
    hr_id <- length(n_tot_ids) + ncol(table_survtime) + match("hr", colvars_hr$vars[-n_tot_ids])
    ci_id <- length(n_tot_ids) + ncol(table_survtime) + match("lcl", colvars_hr$vars[-n_tot_ids])
    n_tot_ids <- seq_along(n_tot_ids)
  }

  structure(
    result,
    forest_header = paste0(rev(levels(df$survtime$arm)), "\nBetter"),
    col_x = hr_id,
    col_ci = ci_id,
    # Take the first one for scaling the symbol sizes in graph.
    col_symbol_size = n_tot_ids[1]
  )
}

#' Labels for Column Variables in Survival Duration by Subgroup Table
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Internal function to check variables included in
#' [tabulate_survival_subgroups()] and create column labels.
#' Note that at least one of `n_tot` and `n_tot_events` needs to be provided in `vars`.
#'
#' @inheritParams tabulate_survival_subgroups
#' @inheritParams argument_convention
#' @param method p-value method for testing hazard ratio = 1.
#' @return `list` of variables to tabulate and their labels.
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

  # The `lcl` variable is just a placeholder available in the analysis data,
  # it is not acutally used in the tabulation.
  # Variables used in the tabulation are lcl and ucl, see `a_survival_subgroups` for details.
  colvars[colvars == "ci"] <- "lcl"

  list(
    vars = colvars,
    labels = varlabels[vars]
  )
}
