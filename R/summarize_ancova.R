#' Summarize analysis of covariance (ANCOVA) results
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The analyze function [summarize_ancova()] creates a layout element to summarize ANCOVA results.
#'
#' This function can be used to analyze multiple endpoints and/or multiple timepoints within the response variable(s)
#' specified as `vars`.
#'
#' Additional variables for the analysis, namely an arm (grouping) variable and covariate variables, can be defined
#' via the `variables` argument. See below for more details on how to specify `variables`. An interaction term can
#' be implemented in the model if needed. The interaction variable that should interact with the arm variable is
#' specified via the `interaction_term` parameter, and the specific value of `interaction_term` for which to extract
#' the ANCOVA results via the `interaction_y` parameter.
#'
#' @inheritParams h_ancova
#' @inheritParams argument_convention
#' @param interaction_y (`string` or `flag`)\cr a selected item inside of the `interaction_item` variable which will be
#'   used to select the specific ANCOVA results. if the interaction is not needed, the default option is `FALSE`.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("summarize_ancova"), type = "sh")``
#'
#' @name summarize_ancova
#' @order 1
NULL

#' Helper function to return results of a linear model
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams argument_convention
#' @param .df_row (`data.frame`)\cr data set that includes all the variables that are called in `.var` and `variables`.
#' @param variables (named `list` of `string`)\cr list of additional analysis variables, with expected elements:
#'   * `arm` (`string`)\cr group variable, for which the covariate adjusted means of multiple groups will be
#'     summarized. Specifically, the first level of `arm` variable is taken as the reference group.
#'   * `covariates` (`character`)\cr a vector that can contain single variable names (such as `"X1"`), and/or
#'     interaction terms indicated by `"X1 * X2"`.
#' @param interaction_item (`string` or `NULL`)\cr name of the variable that should have interactions
#'   with arm. if the interaction is not needed, the default option is `NULL`.
#' @param weights_emmeans (`string` or `NULL`)\cr argument from [emmeans::emmeans()]
#'
#' @return The summary of a linear model.
#'
#' @examples
#' h_ancova(
#'   .var = "Sepal.Length",
#'   .df_row = iris,
#'   variables = list(arm = "Species", covariates = c("Petal.Length * Petal.Width", "Sepal.Width"))
#' )
#'
#' @export
h_ancova <- function(.var,
                     .df_row,
                     variables,
                     interaction_item = NULL,
                     weights_emmeans = NULL) {
  checkmate::assert_string(.var)
  checkmate::assert_list(variables)
  checkmate::assert_subset(names(variables), c("arm", "covariates"))
  assert_df_with_variables(.df_row, list(rsp = .var))

  arm <- variables$arm
  covariates <- variables$covariates
  if (!is.null(covariates) && length(covariates) > 0) {
    # Get all covariate variable names in the model.
    var_list <- get_covariates(covariates)
    assert_df_with_variables(.df_row, var_list)
  }

  covariates_part <- paste(covariates, collapse = " + ")
  if (covariates_part != "") {
    formula <- stats::as.formula(paste0(.var, " ~ ", covariates_part, " + ", arm))
  } else {
    formula <- stats::as.formula(paste0(.var, " ~ ", arm))
  }

  if (is.null(interaction_item)) {
    specs <- arm
  } else {
    specs <- c(arm, interaction_item)
  }

  lm_fit <- stats::lm(
    formula = formula,
    data = .df_row
  )
  emmeans_fit <- emmeans::emmeans(
    lm_fit,
    # Specify here the group variable over which EMM are desired.
    specs = specs,
    # Pass the data again so that the factor levels of the arm variable can be inferred.
    data = .df_row,
    weights = weights_emmeans
  )

  emmeans_fit
}

#' @describeIn summarize_ancova Statistics function that produces a named list of results
#'   of the investigated linear model.
#'
#' @return
#' * `s_ancova()` returns a named list of 5 statistics:
#'   * `n`: Count of complete sample size for the group.
#'   * `lsmean`: Estimated marginal means in the group.
#'   * `lsmean_diff`: Difference in estimated marginal means in comparison to the reference group.
#'     If working with the reference group, this will be empty.
#'   * `lsmean_diff_ci`: Confidence level for difference in estimated marginal means in comparison
#'     to the reference group.
#'   * `pval`: p-value (not adjusted for multiple comparisons).
#'
#' @keywords internal
s_ancova <- function(df,
                     .var,
                     .df_row,
                     .ref_group,
                     .in_ref_col,
                     variables,
                     conf_level,
                     interaction_y = FALSE,
                     interaction_item = NULL,
                     weights_emmeans = NULL,
                     ...) {
  emmeans_fit <- h_ancova(.var = .var, variables = variables, .df_row = .df_row, interaction_item = interaction_item, weights_emmeans = weights_emmeans)

  sum_fit <- summary(
    emmeans_fit,
    level = conf_level
  )

  arm <- variables$arm

  sum_level <- as.character(unique(df[[arm]]))

  # Ensure that there is only one element in sum_level.
  checkmate::assert_scalar(sum_level)

  sum_fit_level <- sum_fit[sum_fit[[arm]] == sum_level, ]

  # Get the index of the ref arm
  if (interaction_y != FALSE) {
    y <- unlist(df[(df[[interaction_item]] == interaction_y), .var])
    # convert characters selected in interaction_y into the numeric order
    interaction_y <- which(sum_fit_level[[interaction_item]] == interaction_y)
    sum_fit_level <- sum_fit_level[interaction_y, ]
    # if interaction is called, reset the index
    ref_key <- seq(sum_fit[[arm]][unique(.ref_group[[arm]])])
    ref_key <- tail(ref_key, n = 1)
    ref_key <- (interaction_y - 1) * length(unique(.df_row[[arm]])) + ref_key
  } else {
    y <- df[[.var]]
    # Get the index of the ref arm when interaction is not called
    ref_key <- seq(sum_fit[[arm]][unique(.ref_group[[arm]])])
    ref_key <- tail(ref_key, n = 1)
  }

  if (.in_ref_col) {
    list(
      n = length(y[!is.na(y)]),
      lsmean = formatters::with_label(sum_fit_level$emmean, "Adjusted Mean"),
      lsmean_diff = formatters::with_label(numeric(), "Difference in Adjusted Means"),
      lsmean_diff_ci = formatters::with_label(numeric(), f_conf_level(conf_level)),
      pval = formatters::with_label(numeric(), "p-value")
    )
  } else {
    # Estimate the differences between the marginal means.
    emmeans_contrasts <- emmeans::contrast(
      emmeans_fit,
      # Compare all arms versus the control arm.
      method = "trt.vs.ctrl",
      # Take the arm factor from .ref_group as the control arm.
      ref = ref_key,
      level = conf_level
    )
    sum_contrasts <- summary(
      emmeans_contrasts,
      # Derive confidence intervals, t-tests and p-values.
      infer = TRUE,
      # Do not adjust the p-values for multiplicity.
      adjust = "none"
    )

    contrast_lvls <- gsub(
      "^\\(|\\)$", "", gsub(paste0(" - \\(*", .ref_group[[arm]][1], ".*"), "", sum_contrasts$contrast)
    )
    if (!is.null(interaction_item)) {
      sum_contrasts_level <- sum_contrasts[grepl(sum_level, contrast_lvls, fixed = TRUE), ]
    } else {
      sum_contrasts_level <- sum_contrasts[sum_level == contrast_lvls, ]
    }
    if (interaction_y != FALSE) {
      sum_contrasts_level <- sum_contrasts_level[interaction_y, ]
    }

    list(
      n = length(y[!is.na(y)]),
      lsmean = formatters::with_label(sum_fit_level$emmean, "Adjusted Mean"),
      lsmean_diff = formatters::with_label(sum_contrasts_level$estimate, "Difference in Adjusted Means"),
      lsmean_diff_ci = formatters::with_label(
        c(sum_contrasts_level$lower.CL, sum_contrasts_level$upper.CL),
        f_conf_level(conf_level)
      ),
      pval = formatters::with_label(sum_contrasts_level$p.value, "p-value")
    )
  }
}

#' @describeIn summarize_ancova Formatted analysis function which is used as `afun` in `summarize_ancova()`.
#'
#' @return
#' * `a_ancova()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_ancova <- function(df,
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

  # Check for user-defined functions
  default_and_custom_stats_list <- .split_std_from_custom_stats(.stats)
  .stats <- default_and_custom_stats_list$all_stats
  custom_stat_functions <- default_and_custom_stats_list$custom_stats

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_ancova,
    custom_stat_fnc_list = custom_stat_functions,
    args_list = c(
      df = list(df),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in formatting defaults
  .stats <- get_stats("summarize_ancova",
    stats_in = .stats,
    custom_stats_in = names(custom_stat_functions)
  )
  x_stats <- x_stats[.stats]
  .formats <- get_formats_from_stats(.stats, .formats)
  .labels <- get_labels_from_stats(
    .stats, .labels,
    tern_defaults = c(lapply(x_stats[names(x_stats) != "n"], attr, "label"), tern_default_labels)
  )
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods)

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, extra_afun_params$.df_row, extra_afun_params$.var)

  # Get and check statistical names
  .stat_names <- get_stat_names(x_stats, .stat_names)

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = .labels %>% .unlist_keep_nulls(),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

#' @describeIn summarize_ancova Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `summarize_ancova()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_ancova()` to the table layout.
#'
#' @examples
#' basic_table() %>%
#'   split_cols_by("Species", ref_group = "setosa") %>%
#'   add_colcounts() %>%
#'   summarize_ancova(
#'     vars = "Petal.Length",
#'     variables = list(arm = "Species", covariates = NULL),
#'     table_names = "unadj",
#'     conf_level = 0.95, var_labels = "Unadjusted comparison",
#'     .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means")
#'   ) %>%
#'   summarize_ancova(
#'     vars = "Petal.Length",
#'     variables = list(arm = "Species", covariates = c("Sepal.Length", "Sepal.Width")),
#'     table_names = "adj",
#'     conf_level = 0.95, var_labels = "Adjusted comparison (covariates: Sepal.Length and Sepal.Width)"
#'   ) %>%
#'   build_table(iris)
#'
#' @export
#' @order 2
summarize_ancova <- function(lyt,
                             vars,
                             variables,
                             conf_level,
                             interaction_y = FALSE,
                             interaction_item = NULL,
                             weights_emmeans = NULL,
                             var_labels,
                             na_str = default_na_str(),
                             nested = TRUE,
                             ...,
                             show_labels = "visible",
                             table_names = vars,
                             .stats = c("n", "lsmean", "lsmean_diff", "lsmean_diff_ci", "pval"),
                             .stat_names = NULL,
                             .formats = NULL,
                             .labels = NULL,
                             .indent_mods = list("lsmean_diff_ci" = 1L, "pval" = 1L)) {
  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    variables = list(variables), conf_level = list(conf_level), interaction_y = list(interaction_y),
    interaction_item = list(interaction_item),
    weights_emmeans = weights_emmeans,
    ...
  )

  # Append additional info from layout to the analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_ancova) <- c(formals(a_ancova), extra_args[[".additional_fun_parameters"]])

  analyze(
    lyt = lyt,
    vars = vars,
    afun = a_ancova,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names
  )
}
