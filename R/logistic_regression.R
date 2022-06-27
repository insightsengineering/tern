#' Multi-Variable Logistic Regression Table
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Layout creating function which summarizes a logistic variable regression for binary outcome with
#' categorical/continuous covariates in model statement. For each covariate category (if categorical)
#' or specified values (if continuous), present degrees of freedom, regression parameter estimate and
#' standard error (SE) relative to reference group or category. Report odds ratios for each covariate
#' category or specified values and corresponding Wald confidence intervals as default but allow user
#' to specify other confidence levels. Report p-value for Wald chi-square test of the null hypothesis
#' that covariate has no effect on response in model containing all specified covariates.
#' Allow option to include one two-way interaction and present similar output for
#' each interaction degree of freedom.
#' Note: For the formula, the variable names need to be standard dataframe column name without
#' special characters.
#'
#' @inheritParams argument_convention
#' @param drop_and_remove_str string to be dropped and removed
#'
#' @examples
#' # flagging empty strings with "_"
#' df <- tern:::replace_emptys_with_na(df, rep_str = "_")
#' df2 <- tern:::replace_emptys_with_na(df2, rep_str = "_")
#'
#' result1 <- basic_table() %>%
#'   summarize_logistic(
#'     conf_level = 0.95,
#'     drop_and_remove_str = "_"
#'   ) %>%
#'   build_table(df = df)
#' result1
#'
#' result2 <- basic_table() %>%
#'   summarize_logistic(
#'     conf_level = 0.95,
#'     drop_and_remove_str = "_"
#'   ) %>%
#'   build_table(df = df2)
#' result2
#'
#' @export
summarize_logistic <- function(lyt,
                               conf_level,
                               drop_and_remove_str = "") {

  # checks
  stopifnot(assertthat::is.string(drop_and_remove_str))

  sum_logistic_variable_test <- logistic_summary_by_flag("is_variable_summary")
  sum_logistic_term_estimates <- logistic_summary_by_flag("is_term_summary")
  sum_logistic_odds_ratios <- logistic_summary_by_flag("is_reference_summary")
  split_fun <- drop_and_remove_levels(drop_and_remove_str)

  lyt <- logistic_regression_cols(lyt, conf_level = conf_level)
  lyt <- split_rows_by(lyt, var = "variable", labels_var = "variable_label", split_fun = split_fun)
  lyt <- sum_logistic_variable_test(lyt)
  lyt <- split_rows_by(lyt, var = "term", labels_var = "term_label", split_fun = split_fun)
  lyt <- sum_logistic_term_estimates(lyt)
  lyt <- split_rows_by(lyt, var = "interaction", labels_var = "interaction_label", split_fun = split_fun)
  lyt <- split_rows_by(lyt, var = "reference", labels_var = "reference_label", split_fun = split_fun)
  lyt <- sum_logistic_odds_ratios(lyt)
  lyt
}

#' Fit for Logistic Regression
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Fit a (conditional) logistic regression model.
#'
#' @inheritParams argument_convention
#' @param data (`data frame`)\cr the data frame on which the model was fit.
#' @param response_definition (`string`)\cr the definition of what an event is in terms of `response`.
#'   This will be used when fitting the (conditional) logistic regression model on the left hand
#'   side of the formula.
#' @section Model Specification:
#'
#' The `variables` list needs to include the following elements:\cr
#' - `arm`: usual treatment arm variable name.
#' - `response`: the response arm variable name. Usually this is a 0/1 variable.
#' - `covariates`: this is either `NULL` (no covariates) or
#'      a character vector of covariate variable names.
#' - `interaction`: this is either `NULL` (no interaction) or a string of a single
#'      covariate variable name already included in `covariates`. Then the interaction
#'      with the treatment arm is included in the model.
#' @details Note this function may hang or error for certain datasets when an old version of the
#'   survival package (< 3.2-13) is used.
#'
#' @examples
#' library(scda)
#' library(dplyr)
#' library(rtables)
#'
#' adrs <- synthetic_cdisc_data("latest")$adrs
#' adrs_f <- adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   filter(RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")) %>%
#'   mutate(
#'     Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
#'     RACE = factor(RACE),
#'     SEX = factor(SEX)
#'   )
#' formatters::var_labels(adrs_f) <- c(formatters::var_labels(adrs), Response = "Response")
#' mod1 <- fit_logistic(
#'   data = adrs_f,
#'   variables = list(
#'     response = "Response",
#'     arm = "ARMCD",
#'     covariates = c("AGE", "RACE")
#'   )
#' )
#' mod2 <- fit_logistic(
#'   data = adrs_f,
#'   variables = list(
#'     response = "Response",
#'     arm = "ARMCD",
#'     covariates = c("AGE", "RACE"),
#'     interaction = "AGE"
#'   )
#' )
#'
#' @export
fit_logistic <- function(data,
                         variables = list(
                           response = "Response",
                           arm = "ARMCD",
                           covariates = NULL,
                           interaction = NULL,
                           strata = NULL
                         ),
                         response_definition = "response") {
  assertthat::assert_that(
    is.list(variables),
    all(names(variables) %in% c("response", "arm", "covariates", "interaction", "strata")),
    assertthat::is.string(response_definition),
    grepl("response", response_definition)
  )
  assert_df_with_variables(data, variables)

  response_definition <- sub(
    pattern = "response",
    replacement = variables$response,
    x = response_definition,
    fixed = TRUE
  )
  form <- paste0(response_definition, " ~ ", variables$arm)
  if (!is.null(variables$covariates)) {
    form <- paste0(form, " + ", paste(variables$covariates, collapse = " + "))
  }
  if (!is.null(variables$interaction)) {
    assertthat::assert_that(
      assertthat::is.string(variables$interaction),
      variables$interaction %in% variables$covariates
    )
    form <- paste0(form, " + ", variables$arm, ":", variables$interaction)
  }
  if (!is.null(variables$strata)) {
    strata_arg <- if (length(variables$strata) > 1) {
      paste0("I(interaction(", paste0(variables$strata, collapse = ", "), "))")
    } else {
      variables$strata
    }
    form <- paste0(form, "+ strata(", strata_arg, ")")
  }
  formula <- stats::as.formula(form)
  if (is.null(variables$strata)) {
    stats::glm(
      formula = formula,
      data = data,
      family = stats::binomial("logit")
    )
  } else {
    clogit_with_tryCatch(
      formula = formula,
      data = data,
      x = TRUE
    )
  }
}

#' Custom Tidy Method for Binomial GLM Results
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper method (for [broom::tidy()]) to prepare a data frame from a `glm` object
#' with `binomial` family.
#'
#' @inheritParams argument_convention
#' @param at (`NULL` or `numeric`)\cr optional values for the interaction variable. Otherwise
#'   the median is used.
#' @param fit_glm logistic regression model fitted by [stats::glm()] with "binomial" family.
#' @method tidy glm
#'
#' @seealso [h_logistic()] for relevant helper functions.
#'
#' @examples
#' library(broom)
#' df <- tidy(mod1, conf_level = 0.99)
#' df2 <- tidy(mod2, conf_level = 0.99)
#'
#' @export
tidy.glm <- function(fit_glm, # nolint
                     conf_level = 0.95,
                     at = NULL) {
  assertthat::assert_that(
    "glm" %in% class(fit_glm),
    fit_glm$family$family == "binomial"
  )
  terms_name <- attr(stats::terms(fit_glm), "term.labels")
  xs_class <- attr(fit_glm$terms, "dataClasses")
  interaction <- terms_name[which(!terms_name %in% names(xs_class))]
  df <- if (length(interaction) == 0) {
    h_logistic_simple_terms(
      x = terms_name,
      fit_glm = fit_glm,
      conf_level = conf_level
    )
  } else {
    h_logistic_inter_terms(
      x = terms_name,
      fit_glm = fit_glm,
      conf_level = conf_level,
      at = at
    )
  }
  for (var in c("variable", "term", "interaction", "reference")) {
    df[[var]] <- factor(df[[var]], levels = unique(df[[var]]))
  }
  df
}

#' Logistic Regression Multi-Variable Column Layout Function
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Layout creating function for a multi-variable column layout summarizing
#' logistic regression results.
#'
#' @inheritParams argument_convention
#'
#' @export
logistic_regression_cols <- function(lyt,
                                     conf_level = 0.95) {
  vars <- c("df", "estimate", "std_error", "odds_ratio", "ci", "pvalue")
  var_labels <- c(
    df = "Degrees of Freedom",
    estimate = "Parameter Estimate",
    std_error = "Standard Error",
    odds_ratio = "Odds Ratio",
    ci = paste("Wald", f_conf_level(conf_level)),
    pvalue = "p-value"
  )
  split_cols_by_multivar(
    lyt = lyt,
    vars = vars,
    varlabels = var_labels
  )
}

#' Logistic Regression Summary Table Constructor Function
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Constructor for content functions to be used to summarize
#' logistic regression results.
#'
#' @param flag_var (`string`)\cr variable name identifying which row should be used in this
#'   content function.
#'
#' @export
logistic_summary_by_flag <- function(flag_var) {
  assertthat::assert_that(assertthat::is.string(flag_var))
  function(lyt) {
    cfun_list <- list(
      df = cfun_by_flag("df", flag_var, format = "xx."),
      estimate = cfun_by_flag("estimate", flag_var, format = "xx.xxx"),
      std_error = cfun_by_flag("std_error", flag_var, format = "xx.xxx"),
      odds_ratio = cfun_by_flag("odds_ratio", flag_var, format = ">999.99"),
      ci = cfun_by_flag("ci", flag_var, format = format_extreme_values_ci(2L)),
      pvalue = cfun_by_flag("pvalue", flag_var, format = "x.xxxx | (<0.0001)")
    )
    summarize_row_groups(
      lyt = lyt,
      cfun = cfun_list
    )
  }
}
