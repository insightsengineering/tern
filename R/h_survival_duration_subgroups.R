#' Helper Functions for Tabulating Survival Duration by Subgroup
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper functions that tabulate in a data frame statistics such as median survival
#' time and hazard ratio for population subgroups.
#'
#' @details Main functionality is to prepare data for use in a layout creating function.
#'
#' @inheritParams argument_convention
#' @inheritParams survival_coxph_pairwise
#' @inheritParams survival_duration_subgroups
#' @param arm (`factor`)\cr the treatment group variable.
#' @name h_survival_duration_subgroups
#' @order 1
#'
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
#'     is_event = CNSR == 0
#'   )
#' labels <- c("ARM" = adtte_labels[["ARM"]], "SEX" = adtte_labels[["SEX"]], "is_event" = "Event Flag")
#' formatters::var_labels(adtte_f)[names(labels)] <- labels
#'
NULL

#' @describeIn h_survival_duration_subgroups helper to prepare a data frame of median survival times by arm.
#' @inheritParams h_survival_duration_subgroups
#'
#' @examples
#' # Extract median survival time for one group.
#' h_survtime_df(
#'   tte = adtte_f$AVAL,
#'   is_event = adtte_f$is_event,
#'   arm = adtte_f$ARM
#' )
#'
#' @export
h_survtime_df <- function(tte, is_event, arm) {
  checkmate::assert_numeric(tte)
  checkmate::assert_logical(is_event, len = length(tte))
  assert_valid_factor(arm, len = length(tte))

  df_tte <- data.frame(
    tte = tte,
    is_event = is_event,
    stringsAsFactors = FALSE
  )

  # Delete NAs
  non_missing_rows <- stats::complete.cases(df_tte)
  df_tte <- df_tte[non_missing_rows, ]
  arm <- arm[non_missing_rows]

  lst_tte <- split(df_tte, arm)
  lst_results <- Map(function(x, arm) {
    if (nrow(x) > 0) {
      s_surv <- s_surv_time(x, .var = "tte", is_event = "is_event")
      median_est <- unname(as.numeric(s_surv$median))
      n_events <- sum(x$is_event)
    } else {
      median_est <- NA
      n_events <- NA
    }

    data.frame(
      arm = arm,
      n = nrow(x),
      n_events = n_events,
      median = median_est,
      stringsAsFactors = FALSE
    )
  }, lst_tte, names(lst_tte))

  df <- do.call(rbind, args = c(lst_results, make.row.names = FALSE))
  df$arm <- factor(df$arm, levels = levels(arm))
  df
}

#' @describeIn h_survival_duration_subgroups summarizes median survival times by arm and across subgroups
#'    in a data frame. `variables` corresponds to the names of variables found in `data`, passed as a named list and
#'    requires elements `tte`, `is_event`, `arm` and optionally `subgroups`. `groups_lists` optionally specifies
#'    groupings for `subgroups` variables.
#'
#' @examples
#' # Extract median survival time for multiple groups.
#' h_survtime_subgroups_df(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     arm = "ARM",
#'     subgroups = c("SEX", "BMRKR2")
#'   ),
#'   data = adtte_f
#' )
#'
#' # Define groupings for BMRKR2 levels.
#' h_survtime_subgroups_df(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     arm = "ARM",
#'     subgroups = c("SEX", "BMRKR2")
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
#'
#' @export
h_survtime_subgroups_df <- function(variables,
                                    data,
                                    groups_lists = list(),
                                    label_all = "All Patients") {
  checkmate::assert_character(variables$tte)
  checkmate::assert_character(variables$is_event)
  checkmate::assert_character(variables$arm)
  checkmate::assert_character(variables$subgroups, null.ok = TRUE)

  assert_df_with_variables(data, variables)

  checkmate::assert_string(label_all)

  # Add All Patients.
  result_all <- h_survtime_df(data[[variables$tte]], data[[variables$is_event]], data[[variables$arm]])
  result_all$subgroup <- label_all
  result_all$var <- "ALL"
  result_all$var_label <- label_all
  result_all$row_type <- "content"

  # Add Subgroups.
  if (is.null(variables$subgroups)) {
    result_all
  } else {
    l_data <- h_split_by_subgroups(data, variables$subgroups, groups_lists = groups_lists)
    l_result <- lapply(l_data, function(grp) {
      result <- h_survtime_df(grp$df[[variables$tte]], grp$df[[variables$is_event]], grp$df[[variables$arm]])
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

#' @describeIn h_survival_duration_subgroups helper to prepare a data frame with estimates of
#'   treatment hazard ratio.
#' @param strata_data (`factor`, `data.frame` or `NULL`)\cr
#'   required if stratified analysis is performed.
#'
#' @examples
#' # Extract hazard ratio for one group.
#' h_coxph_df(adtte_f$AVAL, adtte_f$is_event, adtte_f$ARM)
#'
#' # Extract hazard ratio for one group with stratification factor.
#' h_coxph_df(adtte_f$AVAL, adtte_f$is_event, adtte_f$ARM, strata_data = adtte_f$STRATA1)
#'
#' @export
h_coxph_df <- function(tte, is_event, arm, strata_data = NULL, control = control_coxph()) {
  checkmate::assert_numeric(tte)
  checkmate::assert_logical(is_event, len = length(tte))
  assert_valid_factor(arm, n.levels = 2, len = length(tte))

  df_tte <- data.frame(tte = tte, is_event = is_event)
  strata_vars <- NULL

  if (!is.null(strata_data)) {
    if (is.data.frame(strata_data)) {
      strata_vars <- names(strata_data)
      checkmate::assert_data_frame(strata_data, nrows = nrow(df_tte))
      assert_df_with_factors(strata_data, as.list(stats::setNames(strata_vars, strata_vars)))
    } else {
      assert_valid_factor(strata_data, len = nrow(df_tte))
      strata_vars <- "strata_data"
    }
    df_tte[strata_vars] <- strata_data
  }

  l_df <- split(df_tte, arm)

  if (nrow(l_df[[1]]) > 0 && nrow(l_df[[2]]) > 0) {

    # Hazard ratio and CI.
    result <- s_coxph_pairwise(
      df = l_df[[2]],
      .ref_group = l_df[[1]],
      .in_ref_col = FALSE,
      .var = "tte",
      is_event = "is_event",
      strat = strata_vars,
      control = control
    )

    df <- data.frame(
      # Dummy column needed downstream to create a nested header.
      arm = " ",
      n_tot = unname(as.numeric(result$n_tot)),
      n_tot_events = unname(as.numeric(result$n_tot_events)),
      hr = unname(as.numeric(result$hr)),
      lcl = unname(result$hr_ci[1]),
      ucl = unname(result$hr_ci[2]),
      conf_level = control[["conf_level"]],
      pval = as.numeric(result$pvalue),
      pval_label = obj_label(result$pvalue),
      stringsAsFactors = FALSE
    )
  } else if (
    (nrow(l_df[[1]]) == 0 && nrow(l_df[[2]]) > 0) ||
      (nrow(l_df[[1]]) > 0 && nrow(l_df[[2]]) == 0)
  ) {
    df_tte_complete <- df_tte[stats::complete.cases(df_tte), ]
    df <- data.frame(
      # Dummy column needed downstream to create a nested header.
      arm = " ",
      n_tot = nrow(df_tte_complete),
      n_tot_events = sum(df_tte_complete$is_event),
      hr = NA,
      lcl = NA,
      ucl = NA,
      conf_level = control[["conf_level"]],
      pval = NA,
      pval_label = NA,
      stringsAsFactors = FALSE
    )
  } else {
    df <- data.frame(
      # Dummy column needed downstream to create a nested header.
      arm = " ",
      n_tot = 0L,
      n_tot_events = 0L,
      hr = NA,
      lcl = NA,
      ucl = NA,
      conf_level = control[["conf_level"]],
      pval = NA,
      pval_label = NA,
      stringsAsFactors = FALSE
    )
  }

  df
}

#' @describeIn h_survival_duration_subgroups summarizes estimates of the treatment hazard ratio
#'   across subgroups in a data frame. `variables` corresponds to the names of variables found in
#'   `data`, passed as a named list and requires elements `tte`, `is_event`, `arm` and
#'   optionally `subgroups` and `strat`. `groups_lists` optionally specifies
#'   groupings for `subgroups` variables.
#'
#' @examples
#' # Extract hazard ratio for multiple groups.
#' h_coxph_subgroups_df(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     arm = "ARM",
#'     subgroups = c("SEX", "BMRKR2")
#'   ),
#'   data = adtte_f
#' )
#'
#' # Define groupings of BMRKR2 levels.
#' h_coxph_subgroups_df(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     arm = "ARM",
#'     subgroups = c("SEX", "BMRKR2")
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
#'
#' # Extract hazard ratio for multiple groups with stratification factors.
#' h_coxph_subgroups_df(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     arm = "ARM",
#'     subgroups = c("SEX", "BMRKR2"),
#'     strat = c("STRATA1", "STRATA2")
#'   ),
#'   data = adtte_f
#' )
#'
#' @export
h_coxph_subgroups_df <- function(variables,
                                 data,
                                 groups_lists = list(),
                                 control = control_coxph(),
                                 label_all = "All Patients") {
  checkmate::assert_character(variables$tte)
  checkmate::assert_character(variables$is_event)
  checkmate::assert_character(variables$arm)
  if (!is.null(variables$subgroups)) checkmate::assert_character(variables$subgroups)
  if (!is.null(variables$strat)) checkmate::assert_character(variables$strat)
  assert_df_with_factors(data, list(val = variables$arm), min.levels = 2, max.levels = 2)
  assert_df_with_variables(data, variables)
  checkmate::assert_string(label_all)

  # Add All Patients.
  result_all <- h_coxph_df(
    tte = data[[variables$tte]],
    is_event = data[[variables$is_event]],
    arm = data[[variables$arm]],
    strata_data = if (is.null(variables$strat)) NULL else data[variables$strat],
    control = control
  )
  result_all$subgroup <- label_all
  result_all$var <- "ALL"
  result_all$var_label <- label_all
  result_all$row_type <- "content"

  # Add Subgroups.
  if (is.null(variables$subgroups)) {
    result_all
  } else {
    l_data <- h_split_by_subgroups(data, variables$subgroups, groups_lists = groups_lists)

    l_result <- lapply(l_data, function(grp) {
      result <- h_coxph_df(
        tte = grp$df[[variables$tte]],
        is_event = grp$df[[variables$is_event]],
        arm = grp$df[[variables$arm]],
        strata_data = if (is.null(variables$strat)) NULL else grp$df[variables$strat],
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

#' Split Dataframe by Subgroups
#'
#' Split a dataframe into a non-nested list of subsets.
#'
#' @details Main functionality is to prepare data for use in forest plot layouts.
#'
#' @inheritParams survival_duration_subgroups
#' @param data (`data frame`)\cr dataset to split.
#' @param subgroups (`character`)\cr names of factor variables from `data` used to create subsets.
#'   Unused levels not present in `data` are dropped. Note that the order in this vector
#'   determines the order in the downstream table.
#'
#' @return A list with subset data (`df`) and metadata about the subset (`df_labels`).
#'
#' @examples
#' library(rtables)
#'
#' df <- data.frame(
#'   x = c(1:5),
#'   y = factor(c("A", "B", "A", "B", "A"), levels = c("A", "B", "C")),
#'   z = factor(c("C", "C", "D", "D", "D"), levels = c("D", "C"))
#' )
#' formatters::var_labels(df) <- paste("label for", names(df))
#'
#' h_split_by_subgroups(
#'   data = df,
#'   subgroups = c("y", "z")
#' )
#'
#' h_split_by_subgroups(
#'   data = df,
#'   subgroups = c("y", "z"),
#'   groups_lists = list(
#'     y = list("AB" = c("A", "B"), "C" = "C")
#'   )
#' )
#'
#' @export
h_split_by_subgroups <- function(data,
                                 subgroups,
                                 groups_lists = list()) {
  checkmate::assert_character(subgroups, min.len = 1, any.missing = FALSE)
  checkmate::assert_list(groups_lists, names = "named")
  checkmate::assert_subset(names(groups_lists), subgroups)
  assert_df_with_factors(data, as.list(stats::setNames(subgroups, subgroups)))

  data_labels <- unname(formatters::var_labels(data))
  df_subgroups <- data[, subgroups, drop = FALSE]
  subgroup_labels <- formatters::var_labels(df_subgroups, fill = TRUE)

  l_labels <- Map(function(grp_i, name_i) {
    existing_levels <- levels(droplevels(grp_i))
    grp_levels <- if (name_i %in% names(groups_lists)) {
      # For this variable groupings are defined. We check which groups are contained in the data.
      group_list_i <- groups_lists[[name_i]]
      group_has_levels <- vapply(group_list_i, function(lvls) any(lvls %in% existing_levels), TRUE)
      names(which(group_has_levels))
    } else {
      existing_levels
    }
    df_labels <- data.frame(
      subgroup = grp_levels,
      var = name_i,
      var_label = unname(subgroup_labels[name_i]),
      stringsAsFactors = FALSE # Rationale is that subgroups may not be unique.
    )
  }, df_subgroups, names(df_subgroups))

  # Create a dataframe with one row per subgroup.
  df_labels <- do.call(rbind, args = c(l_labels, make.row.names = FALSE))
  row_label <- paste0(df_labels$var, ".", df_labels$subgroup)
  row_split_var <- factor(row_label, levels = row_label)

  # Create a list of data subsets.
  lapply(split(df_labels, row_split_var), function(row_i) {
    which_row <- if (row_i$var %in% names(groups_lists)) {
      data[[row_i$var]] %in% groups_lists[[row_i$var]][[row_i$subgroup]]
    } else {
      data[[row_i$var]] == row_i$subgroup
    }
    df <- data[which_row, ]
    rownames(df) <- NULL
    formatters::var_labels(df) <- data_labels

    list(
      df = df,
      df_labels = data.frame(row_i, row.names = NULL)
    )
  })
}
