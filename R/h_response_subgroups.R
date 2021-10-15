#' Helper Functions for Tabulating Binary Response by Subgroup
#'
#' Helper functions that tabulate in a data frame statistics such as response rate
#' and odds ratio for population subgroups.
#'
#' @details Main functionality is to prepare data for use in a layout creating function.
#'
#' @inheritParams argument_convention
#' @inheritParams response_subgroups
#' @param arm (`factor`)\cr the treatment group variable.
#' @name h_response_subgroups
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
#' var_labels(adrs_f) <- c(adrs_labels, "Response")
#'
NULL

#' @describeIn h_response_subgroups helper to prepare a data frame of binary responses by arm.
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
    is.logical(rsp),
    is_valid_factor(arm),
    is_equal_length(rsp, arm)
  )

  lst_rsp <- split(rsp, arm)
  lst_results <- Map(function(x, arm) {
  x <- x[!is.na(x)]
    if (length(x) > 0) {
      s_prop <- s_proportion(x)
      data.frame(
        arm = arm,
        n = length(x),
        n_rsp = unname(s_prop$n_prop[1]),
        prop = unname(s_prop$n_prop[2]),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        arm = arm,
        n = 0L,
        n_rsp = NA,
        prop = NA,
        stringsAsFactors = FALSE
      )
    }

  }, lst_rsp, names(lst_rsp))

  df <- do.call(rbind, args = c(lst_results, make.row.names = FALSE))
  df$arm <- factor(df$arm, levels = levels(arm))
  df
}

#' @describeIn h_response_subgroups summarizes proportion of binary responses by arm and across subgroups
#'    in a data frame. `variables` corresponds to the names of variables found in `data`, passed as a named list and
#'    requires elements `rsp`, `arm` and optionally `subgroups`. `groups_lists` optionally specifies
#'    groupings for `subgroups` variables.
#' @export
#' @examples
#'
#' h_proportion_subgroups_df(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
#'   data = adrs_f
#' )
#'
#' # Define groupings for BMRKR2 levels.
#' h_proportion_subgroups_df(
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
h_proportion_subgroups_df <- function(variables,
                                      data,
                                      groups_lists = list(),
                                      label_all = "All Patients") {

  assert_that(
    is.character(variables$rsp),
    is.character(variables$arm),
    is.character(variables$subgroups) || is.null(variables$subgroups),
    is_character_single(label_all),
    is_df_with_variables(data, as.list(unlist(variables))),
    is_df_with_nlevels_factor(data, variable = variables$arm, n_levels = 2)
  )

  # Add All Patients.
  result_all <- h_proportion_df(data[[variables$rsp]], data[[variables$arm]])
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
      result <- h_proportion_df(grp$df[[variables$rsp]], grp$df[[variables$arm]])
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

#' @describeIn h_response_subgroups helper to prepare a data frame with estimates of
#'   the odds ratio between a treatment and a control arm.
#' @inheritParams response_subgroups
#' @param strata_data (`factor`, `data.frame` or `NULL`)\cr
#'   required if stratified analysis is performed.
#' @export
#' @examples
#'
#' # Unstratatified analysis.
#' h_odds_ratio_df(
#'   c(TRUE, FALSE, FALSE, TRUE),
#'   arm = factor(c("A", "A", "B", "B"), levels = c("A", "B"))
#' )
#'
#' # Include p-value.
#' h_odds_ratio_df(adrs_f$rsp, adrs_f$ARM, method = "chisq")
#'
#' # Stratatified analysis.
#' h_odds_ratio_df(
#'   rsp = adrs_f$rsp,
#'   arm = adrs_f$ARM,
#'   strata_data = adrs_f[, c("STRATA1", "STRATA2")],
#'   method = "cmh"
#' )
#'
h_odds_ratio_df <- function(rsp, arm, strata_data = NULL, conf_level = 0.95, method = NULL) {

  assert_that(
    is_valid_factor(arm),
    is_equal_length(rsp, arm),
    are_equal(nlevels(arm), 2)
  )

  df_rsp <- data.frame(
    rsp = rsp,
    arm = arm
  )

  if (!is.null(strata_data)) {

    strata_var <- interaction(strata_data, drop = TRUE)
    strata_name <- "strata"

    assert_that(
      is_valid_factor(strata_var),
      are_equal(length(strata_var), nrow(df_rsp))
    )

    df_rsp[[strata_name]] <- strata_var
  } else {
    strata_name <- NULL
  }

  l_df <- split(df_rsp, arm)

  if (nrow(l_df[[1]]) > 0 && nrow(l_df[[2]]) > 0) {

    # Odds ratio and CI.
    result_odds_ratio <- s_odds_ratio(
      df = l_df[[2]],
      .var = "rsp",
      .ref_group = l_df[[1]],
      .in_ref_col = FALSE,
      .df_row = df_rsp,
      variables = list(arm = "arm", strata = strata_name),
      conf_level = conf_level
    )

    df <- data.frame(
      # Dummy column needed downstream to create a nested header.
      arm = " ",
      n_tot = unname(result_odds_ratio$n_tot["n_tot"]),
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
        variables = list(strata = strata_name),
        method = method
      )

      df$pval <- as.numeric(result_test$pval)
      df$pval_label <- obj_label(result_test$pval)
    }

  } else {

    df <- data.frame(
      # Dummy column needed downstream to create a nested header.
      arm = " ",
      n_tot = 0L,
      or = NA,
      lcl = NA,
      ucl = NA,
      conf_level = conf_level,
      stringsAsFactors = FALSE
    )
    if (!is.null(method)) {
      df$pval <- NA
      df$pval_label <- NA
    }

  }

  df

}

#' @describeIn h_response_subgroups summarizes estimates of the odds ratio between a treatment and a control
#'   arm across subgroups in a data frame. `variables` corresponds to the names of variables found in
#'   `data`, passed as a named list and requires elements `rsp`, `arm` and optionally `subgroups`
#'   and `strat`. `groups_lists` optionally specifies groupings for `subgroups` variables.
#' @export
#' @examples
#'
#' # Unstratified analysis.
#' h_odds_ratio_subgroups_df(
#'   variables = list(rsp = "rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2")),
#'   data = adrs_f
#' )
#'
#' # Stratified analysis.
#' h_odds_ratio_subgroups_df(
#'   variables = list(
#'     rsp = "rsp",
#'     arm = "ARM",
#'     subgroups = c("SEX", "BMRKR2"),
#'     strat = c("STRATA1", "STRATA2")
#'    ),
#'   data = adrs_f
#' )
#'
#' # Define groupings of BMRKR2 levels.
#' h_odds_ratio_subgroups_df(
#'   variables = list(
#'     rsp = "rsp",
#'     arm = "ARM",
#'     subgroups = c("SEX", "BMRKR2")
#'   ),
#'   data = adrs_f,
#'   groups_lists = list(
#'     BMRKR2 = list(
#'       "low" = "LOW",
#'       "low/medium" = c("LOW", "MEDIUM"),
#'       "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
#'     )
#'   )
#' )
#'
h_odds_ratio_subgroups_df <- function(variables,
                                      data,
                                      groups_lists = list(),
                                      conf_level = 0.95,
                                      method = NULL,
                                      label_all = "All Patients") {

  assert_that(
    is.character(variables$rsp),
    is.character(variables$arm),
    is.character(variables$subgroups) || is.null(variables$subgroups),
    is.character(variables$strat) || is.null(variables$strat),
    is_character_single(label_all),
    is_df_with_variables(data, as.list(unlist(variables))),
    is_df_with_nlevels_factor(data, variable = variables$arm, n_levels = 2)
  )

  strata_data <- if (is.null(variables$strat)) {
    NULL
  } else {
    data[, variables$strat, drop = FALSE]
  }

  # Add All Patients.
  result_all <- h_odds_ratio_df(
    rsp = data[[variables$rsp]],
    arm = data[[variables$arm]],
    strata_data = strata_data,
    conf_level = conf_level,
    method = method
  )
  result_all$subgroup <- label_all
  result_all$var <- "ALL"
  result_all$var_label <- label_all
  result_all$row_type <- "content"

  if (is.null(variables$subgroups)) {
    result_all
  } else {

    l_data <- h_split_by_subgroups(data, variables$subgroups, groups_lists = groups_lists)

    l_result <- lapply(l_data, function(grp) {

      grp_strata_data <- if (is.null(variables$strat)) {
        NULL
      } else {
        grp$df[, variables$strat, drop = FALSE]
      }

      result <- h_odds_ratio_df(
        rsp = grp$df[[variables$rsp]],
        arm = grp$df[[variables$arm]],
        strata_data = grp_strata_data,
        conf_level = conf_level,
        method = method
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
