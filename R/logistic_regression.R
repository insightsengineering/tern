#' Multivariate Logistic Regression Table
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Layout-creating function which summarizes a logistic variable regression for binary outcome with
#' categorical/continuous covariates in model statement. For each covariate category (if categorical)
#' or specified values (if continuous), present degrees of freedom, regression parameter estimate and
#' standard error (SE) relative to reference group or category. Report odds ratios for each covariate
#' category or specified values and corresponding Wald confidence intervals as default but allow user
#' to specify other confidence levels. Report p-value for Wald chi-square test of the null hypothesis
#' that covariate has no effect on response in model containing all specified covariates.
#' Allow option to include one two-way interaction and present similar output for
#' each interaction degree of freedom.
#'
#' @inheritParams argument_convention
#' @param drop_and_remove_str (`character`)\cr string to be dropped and removed.
#'
#' @return A layout object suitable for passing to further layouting functions, or to [rtables::build_table()].
#'   Adding this function to an `rtable` layout will add a logistic regression variable summary to the table layout.
#'
#' @note For the formula, the variable names need to be standard `data.frame` column names without
#'   special characters.
#'
#' @examples
#' library(dplyr)
#' library(broom)
#'
#' adrs_f <- tern_ex_adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   filter(RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")) %>%
#'   mutate(
#'     Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
#'     RACE = factor(RACE),
#'     SEX = factor(SEX)
#'   )
#' formatters::var_labels(adrs_f) <- c(formatters::var_labels(tern_ex_adrs), Response = "Response")
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
#' df <- tidy(mod1, conf_level = 0.99)
#' df2 <- tidy(mod2, conf_level = 0.99)
#'
#' # flagging empty strings with "_"
#' df <- df_explicit_na(df, na_level = "_")
#' df2 <- df_explicit_na(df2, na_level = "_")
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
#' @order 1
summarize_logistic <- function(lyt,
                               conf_level,
                               drop_and_remove_str = "",
                               .indent_mods = NULL) {
  # checks
  checkmate::assert_string(drop_and_remove_str)

  sum_logistic_variable_test <- logistic_summary_by_flag("is_variable_summary")
  sum_logistic_term_estimates <- logistic_summary_by_flag("is_term_summary", .indent_mods = .indent_mods)
  sum_logistic_odds_ratios <- logistic_summary_by_flag("is_reference_summary", .indent_mods = .indent_mods)
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
#' @param data (`data.frame`)\cr the data frame on which the model was fit.
#' @param response_definition (`string`)\cr the definition of what an event is in terms of `response`.
#'   This will be used when fitting the (conditional) logistic regression model on the left hand
#'   side of the formula.
#'
#' @return A fitted logistic regression model.
#'
#' @section Model Specification:
#'
#' The `variables` list needs to include the following elements:
#'   * `arm`: Treatment arm variable name.
#'   * `response`: The response arm variable name. Usually this is a 0/1 variable.
#'   * `covariates`: This is either `NULL` (no covariates) or a character vector of covariate variable names.
#'   * `interaction`: This is either `NULL` (no interaction) or a string of a single covariate variable name already
#'     included in `covariates`. Then the interaction with the treatment arm is included in the model.
#'
#' @examples
#' library(dplyr)
#'
#' adrs_f <- tern_ex_adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   filter(RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")) %>%
#'   mutate(
#'     Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
#'     RACE = factor(RACE),
#'     SEX = factor(SEX)
#'   )
#' formatters::var_labels(adrs_f) <- c(formatters::var_labels(tern_ex_adrs), Response = "Response")
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
  assert_df_with_variables(data, variables)
  checkmate::assert_subset(names(variables), c("response", "arm", "covariates", "interaction", "strata"))
  checkmate::assert_string(response_definition)
  checkmate::assert_true(grepl("response", response_definition))

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
    checkmate::assert_string(variables$interaction)
    checkmate::assert_subset(variables$interaction, variables$covariates)
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
#' @param at (`NULL` or `numeric`)\cr optional values for the interaction variable. Otherwise the median is used.
#' @param x logistic regression model fitted by [stats::glm()] with "binomial" family.
#'
#' @return A `data.frame` containing the tidied model.
#'
#' @method tidy glm
#'
#' @seealso [h_logistic_regression] for relevant helper functions.
#'
#' @examples
#' library(dplyr)
#' library(broom)
#'
#' adrs_f <- tern_ex_adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   filter(RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")) %>%
#'   mutate(
#'     Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
#'     RACE = factor(RACE),
#'     SEX = factor(SEX)
#'   )
#' formatters::var_labels(adrs_f) <- c(formatters::var_labels(tern_ex_adrs), Response = "Response")
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
#' df <- tidy(mod1, conf_level = 0.99)
#' df2 <- tidy(mod2, conf_level = 0.99)
#'
#' @export
tidy.glm <- function(x, # nolint
                     conf_level = 0.95,
                     at = NULL,
                     ...) {
  checkmate::assert_class(x, "glm")
  checkmate::assert_set_equal(x$family$family, "binomial")

  terms_name <- attr(stats::terms(x), "term.labels")
  xs_class <- attr(x$terms, "dataClasses")
  interaction <- terms_name[which(!terms_name %in% names(xs_class))]
  df <- if (length(interaction) == 0) {
    h_logistic_simple_terms(
      x = terms_name,
      fit_glm = x,
      conf_level = conf_level
    )
  } else {
    h_logistic_inter_terms(
      x = terms_name,
      fit_glm = x,
      conf_level = conf_level,
      at = at
    )
  }
  for (var in c("variable", "term", "interaction", "reference")) {
    df[[var]] <- factor(df[[var]], levels = unique(df[[var]]))
  }
  df
}

#' Logistic Regression Multivariate Column Layout Function
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Layout-creating function which creates a multivariate column layout summarizing logistic
#' regression results. This function is a wrapper for [rtables::split_cols_by_multivar()].
#'
#' @inheritParams argument_convention
#'
#' @return A layout object suitable for passing to further layouting functions. Adding this
#'   function to an `rtable` layout will split the table into columns corresponding to
#'   statistics `df`, `estimate`, `std_error`, `odds_ratio`, `ci`, and `pvalue`.
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
#' Constructor for content functions to be used in [`summarize_logistic()`] to summarize
#' logistic regression results. This function is a wrapper for [rtables::summarize_row_groups()].
#'
#' @inheritParams argument_convention
#' @param flag_var (`string`)\cr variable name identifying which row should be used in this
#'   content function.
#'
#' @return A content function.
#'
#' @export
logistic_summary_by_flag <- function(flag_var, na_str = default_na_str(), .indent_mods = NULL) {
  checkmate::assert_string(flag_var)
  function(lyt) {
    cfun_list <- list(
      df = cfun_by_flag("df", flag_var, format = "xx.", .indent_mods = .indent_mods),
      estimate = cfun_by_flag("estimate", flag_var, format = "xx.xxx", .indent_mods = .indent_mods),
      std_error = cfun_by_flag("std_error", flag_var, format = "xx.xxx", .indent_mods = .indent_mods),
      odds_ratio = cfun_by_flag("odds_ratio", flag_var, format = ">999.99", .indent_mods = .indent_mods),
      ci = cfun_by_flag("ci", flag_var, format = format_extreme_values_ci(2L), .indent_mods = .indent_mods),
      pvalue = cfun_by_flag("pvalue", flag_var, format = "x.xxxx | (<0.0001)", .indent_mods = .indent_mods)
    )
    summarize_row_groups(
      lyt = lyt,
      cfun = cfun_list,
      na_str = na_str
    )
  }
}
