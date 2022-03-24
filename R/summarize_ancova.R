#' Summary for analysis of covariance (ANCOVA).
#'
#' Summarize results of ANCOVA. This can be used to analyze multiple endpoints and/or
#' multiple timepoints within the same response variable `.var`.
#'
#' @name summarize_ancova
#'
NULL

#' @describeIn summarize_ancova Helper function to return results of a linear model.
#' @inheritParams argument_convention
#' @param .df_row (`data frame`)\cr data set that includes all the variables that are called
#'   in `.var` and `variables`.
#' @param variables (named `list` of `strings`)\cr list of additional analysis variables, with
#'   expected elements:
#'   - `arm`: (`string`)\cr group variable, for which the covariate adjusted means of multiple
#'   groups will be summarized. Specifically, the first level of `arm` variable is taken as the
#'   reference group.
#'   - `covariates`: (`character`)\cr a vector that can contain single variable names (such as
#'   `"X1"`), and/or interaction terms indicated by `"X1 * X2"`.
#'
#' @export
#'
#' @examples
#' h_ancova(
#'   .var = "Sepal.Length",
#'   .df_row = iris,
#'   variables = list(arm = "Species", covariates = c("Petal.Length * Petal.Width", "Sepal.Width"))
#' )
h_ancova <- function(.var,
                     .df_row,
                     variables) {
  assertthat::assert_that(
    assertthat::is.string(.var),
    is.list(variables),
    all(names(variables) %in% c("arm", "covariates")),
    is_df_with_variables(.df_row, list(rsp = .var))
  )

  arm <- variables$arm
  covariates <- variables$covariates
  if (!is.null(covariates)) {
    # Get all covariate variable names in the model.
    var_list <- get_covariates(covariates)
    assertthat::assert_that(
      is_df_with_variables(.df_row, var_list)
    )
  }

  covariates_part <- paste(covariates, collapse = " + ")
  if (covariates_part != "") {
    formula <- stats::as.formula(paste0(.var, " ~ ", covariates_part, " + ", arm))
  } else {
    formula <- stats::as.formula(paste0(.var, " ~ ", arm))
  }

  lm_fit <- stats::lm(
    formula = formula,
    data = .df_row
  )
  emmeans_fit <- emmeans::emmeans(
    lm_fit,
    # Specify here the group variable over which EMM are desired.
    specs = arm,
    # Pass the data again so that the factor levels of the arm variable can be inferred.
    data = .df_row
  )

  emmeans_fit
}

#' @describeIn summarize_ancova Statistics function that produces a named list of results
#'   of the investigated linear model.
#' @inheritParams argument_convention
#' @inheritParams h_ancova
#' @return A named list of 5 statistics:
#'   - `n`: count of complete sample size for the group.
#'   - `lsmean`: estimated marginal means in the group.
#'   - `lsmean_diff`: difference in estimated marginal means in comparison to the reference
#'   group. If working with the reference group, this will be empty.
#'   - `lsmean_diff_ci`: confidence level for difference in estimated marginal means in
#'   comparison to the reference group.
#'   - `pval`: p-value (not adjusted for multiple comparisons).
#'
#' @export
#'
#' @examples
#' library(scda)
#' library(dplyr)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adqs <- synthetic_cdisc_data("latest")$adqs
#'
#' adqs_single <- adqs %>%
#'   filter(
#'     AVISIT == "WEEK 1 DAY 8", # single time point
#'     PARAMCD == "FKSI-FWB" # single end point
#'   ) %>%
#'   mutate(CHG = ifelse(BMEASIFL == "Y", CHG, NA)) # only analyze evaluable population
#'
#' df <- adqs_single %>%
#'   filter(ARMCD == "ARM B")
#' .var <- "CHG"
#' .df_row <- adqs_single
#' variables <- list(arm = "ARMCD", covariates = "SEX * AGE")
#' .ref_group <- adqs_single %>%
#'   filter(ARMCD == "ARM A")
#' conf_level <- 0.95
#'
#' s_ancova(df, .var, .df_row, variables, .ref_group, .in_ref_col = FALSE, conf_level)
s_ancova <- function(df,
                     .var,
                     .df_row,
                     variables,
                     .ref_group,
                     .in_ref_col,
                     conf_level) {
  emmeans_fit <- h_ancova(.var = .var, variables = variables, .df_row = .df_row)

  sum_fit <- summary(
    emmeans_fit,
    level = conf_level
  )

  arm <- variables$arm

  y <- df[[.var]]
  sum_level <- as.character(unique(df[[arm]]))
  # Ensure that there is only one element in sum_level.
  assertthat::assert_that(
    assertthat::is.scalar(sum_level)
  )
  sum_fit_level <- sum_fit[sum_fit[[arm]] == sum_level, ]

  if (.in_ref_col) {
    list(
      n = length(y[!is.na(y)]),
      lsmean = formatable::with_label(sum_fit_level$emmean, "Adjusted Mean"),
      lsmean_diff = formatable::with_label(character(), "Difference in Adjusted Means"),
      lsmean_diff_ci = formatable::with_label(character(), f_conf_level(conf_level)),
      pval = formatable::with_label(character(), "p-value")
    )
  } else {
    # Estimate the differences between the marginal means.
    emmeans_contrasts <- emmeans::contrast(
      emmeans_fit,
      # Compare all arms versus the control arm.
      method = "trt.vs.ctrl",
      # Take the first level of the arm factor as the control arm.
      ref = 1
    )
    sum_contrasts <- summary(
      emmeans_contrasts,
      # Derive confidence intervals, t-tests and p-values.
      infer = TRUE,
      # Do not adjust the p-values for multiplicity.
      adjust = "none"
    )

    sum_contrasts_level <- sum_contrasts[grepl(sum_level, sum_contrasts$contrast), ]

    list(
      n = length(y[!is.na(y)]),
      lsmean = formatable::with_label(sum_fit_level$emmean, "Adjusted Mean"),
      lsmean_diff = formatable::with_label(sum_contrasts_level$estimate, "Difference in Adjusted Means"),
      lsmean_diff_ci = formatable::with_label(
        c(sum_contrasts_level$lower.CL, sum_contrasts_level$upper.CL),
        f_conf_level(conf_level)
      ),
      pval = formatable::with_label(sum_contrasts_level$p.value, "p-value")
    )
  }
}

#' @describeIn summarize_ancova Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#' @export
#'
#' @examples
#' a_ancova(df, .var, .df_row, variables, .ref_group, .in_ref_col = FALSE, conf_level)
a_ancova <- make_afun(
  s_ancova,
  .indent_mods = c("n" = 0L, "lsmean" = 0L, "lsmean_diff" = 0L, "lsmean_diff_ci" = 1L, "pval" = 1L),
  .formats = c(
    "n" = "xx",
    "lsmean" = "xx.xx",
    "lsmean_diff" = "xx.xx",
    "lsmean_diff_ci" = "(xx.xx, xx.xx)",
    "pval" = "x.xxxx | (<0.0001)"
  ),
  .null_ref_cells = FALSE
)

#' @describeIn summarize_ancova Layout creating function which can be be used for creating
#'   summary tables for analysis of covariance (ANCOVA).
#' @inheritParams argument_convention
#' @export
#' @examples
#'
#' adqs_single <- adqs %>%
#'   filter(
#'     AVISIT == "WEEK 1 DAY 8", # single time point
#'     PARAMCD == "FKSI-FWB" # single end point
#'   ) %>%
#'   mutate(CHG = ifelse(BMEASIFL == "Y", CHG, NA)) # only analyze evaluable population
#' adqs_multi <- adqs %>%
#'   filter(AVISIT == "WEEK 1 DAY 8")
#'
#' basic_table() %>%
#'   split_cols_by("ARMCD", ref_group = "ARM A") %>%
#'   add_colcounts() %>%
#'   summarize_ancova(
#'     vars = "CHG",
#'     variables = list(arm = "ARMCD", covariates = NULL),
#'     table_names = "unadj",
#'     conf_level = 0.95, var_labels = "Unadjusted comparison",
#'     .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means")
#'   ) %>%
#'   summarize_ancova(
#'     vars = "CHG",
#'     variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
#'     table_names = "adj",
#'     conf_level = 0.95, var_labels = "Adjusted comparison (covariates BASE and STRATA1)"
#'   ) %>%
#'   build_table(adqs_single, alt_counts_df = adsl)
#' \dontrun{
#' basic_table() %>%
#'   split_cols_by("ARMCD", ref_group = "ARM A") %>%
#'   split_rows_by("PARAMCD") %>%
#'   summarize_ancova(
#'     vars = "CHG",
#'     variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
#'     conf_level = 0.95, var_labels = "Adjusted mean"
#'   ) %>%
#'   build_table(adqs_multi, alt_counts_df = adsl)
#' }
summarize_ancova <- function(lyt,
                             vars,
                             var_labels,
                             ...,
                             show_labels = "visible",
                             table_names = vars,
                             .stats = NULL,
                             .formats = NULL,
                             .labels = NULL,
                             .indent_mods = NULL) {
  afun <- make_afun(
    a_ancova,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )

  analyze(
    lyt,
    vars,
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names,
    afun = afun,
    extra_args = list(...)
  )
}
