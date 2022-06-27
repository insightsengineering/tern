#' Cox Proportional Hazards Regression
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Fits a Cox regression model and estimate hazard ratio to describe the effect
#' size in a survival analysis.
#'
#' @details
#' Cox models are the most commonly used methods to estimate the magnitude of
#' the effect in survival analysis. It assumes proportional hazards: the ratio
#' of the hazards between groups (e.g., two arms) is constant over time.
#' This ratio is referred to as the "hazard ratio" (HR) and is one of the
#' most commonly reported metrics to describe the effect size in survival
#' analysis (NEST Team, 2020).
#'
#' @note The usual formatting arguments for the _layout creating_ function
#'  `summarize_coxreg` are not yet accepted (`.stats`, `.indent_mod`, `.formats`,
#'  `.labels`).
#' @inheritParams argument_convention
#'
#' @examples
#' # Testing dataset [survival::bladder].
#'
#' library(survival)
#' library(rtables)
#' set.seed(1, kind = "Mersenne-Twister")
#' dta_bladder <- with(
#'   data = bladder[bladder$enum < 5, ],
#'   data.frame(
#'     time = stop,
#'     status = event,
#'     armcd = as.factor(rx),
#'     covar1 = as.factor(enum),
#'     covar2 = factor(
#'       sample(as.factor(enum)),
#'       levels = 1:4, labels = c("F", "F", "M", "M")
#'     )
#'   )
#' )
#' labels <- c("armcd" = "ARM", "covar1" = "A Covariate Label", "covar2" = "Sex (F/M)")
#' formatters::var_labels(dta_bladder)[names(labels)] <- labels
#' dta_bladder$age <- sample(20:60, size = nrow(dta_bladder), replace = TRUE)
#'
#' plot(
#'   survfit(Surv(time, status) ~ armcd + covar1, data = dta_bladder),
#'   lty = 2:4,
#'   xlab = "Months",
#'   col = c("blue1", "blue2", "blue3", "blue4", "red1", "red2", "red3", "red4")
#' )
#'
#' @name cox_regression
NULL

#' Controls for Cox Regression
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Sets a list of parameters for Cox regression fit. Used internally,
#' see [fit_coxreg_univar()] and [fit_coxreg_multivar()].
#'
#' @inheritParams argument_convention
#' @param pval_method (`string`)\cr the method used for estimation of p.values;
#'   `wald` (default) or `likelihood`.
#' @param interaction (`flag`)\cr if `TRUE`, the model includes the
#'   interaction between the studied treatment and candidate covariate. Note that
#'   for univariate models without treatment arm, and multivariate models, no
#'   interaction can be used so that this needs to be `FALSE`.
#' @param ties (`string`)\cr among `exact` (equivalent to `DISCRETE` in SAS),
#'   `efron` and `breslow`, see [survival::coxph()].
#'   Note: there is no equivalent of SAS `EXACT` method in R.
#' @return A `list` of item corresponding to the arguments.
#'
#' @examples
#' control_coxreg()
#'
#' @export
control_coxreg <- function(pval_method = c("wald", "likelihood"),
                           ties = c("exact", "efron", "breslow"),
                           conf_level = 0.95,
                           interaction = FALSE) {
  pval_method <- match.arg(pval_method)
  ties <- match.arg(ties)
  assertthat::assert_that(
    is_proportion(conf_level),
    assertthat::is.flag(interaction)
  )
  list(
    pval_method = pval_method,
    ties = ties,
    conf_level = conf_level,
    interaction = interaction
  )
}

#' @describeIn cox_regression transforms the tabulated results from [`fit_coxreg_univar()`]
#'  and [`fit_coxreg_multivar()`] into a list. Not much calculation is done here,
#'  it rather prepares the data to be used by the layout creating function.
#'
#' @examples
#' # s_coxreg
#'
#' univar_model <- fit_coxreg_univar(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder
#' )
#' df1 <- broom::tidy(univar_model)
#' s_coxreg(df = df1, .var = "hr")
#'
#' # Only covariates.
#' univar_covs_model <- fit_coxreg_univar(
#'   variables = list(
#'     time = "time", event = "status",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder
#' )
#' df1_covs <- broom::tidy(univar_covs_model)
#' s_coxreg(df = df1_covs, .var = "hr")
#'
#' # Multivariate.
#' multivar_model <- fit_coxreg_multivar(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder
#' )
#' df2 <- broom::tidy(multivar_model)
#' s_coxreg(df = df2, .var = "hr")
#'
#' # Multivariate without treatment arm.
#' multivar_covs_model <- fit_coxreg_multivar(
#'   variables = list(
#'     time = "time", event = "status",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder
#' )
#' df2_covs <- broom::tidy(multivar_covs_model)
#' s_coxreg(df = df2_covs, .var = "hr")
#'
#' @export
s_coxreg <- function(df, .var) {
  assert_df_with_variables(df, list(term = "term", var = .var))
  assert_character_or_factor(df$term)
  df$term <- as.character(df$term)
  # We need a list with names corresponding to the stats to display.
  # There can be several covariate to test, but the names of the items should
  # be constant and equal to the stats to display.
  y <- split(df, f = df$term, drop = FALSE)
  y <- stats::setNames(y, nm = rep(.var, length(y)))
  lapply(
    X = y,
    FUN = function(x) {
      z <- as.list(x[[.var]])
      stats::setNames(z, nm = x$term_label)
    }
  )
}

#' @describeIn cox_regression layout creating function.
#'
#' @inheritParams argument_convention
#' @inheritParams control_coxreg
#' @param multivar (`flag`)\cr if `TRUE`, the multi-variable Cox regression will run
#'   and no interaction will be considered between the studied treatment and c
#'   candidate covariate. Default is `FALSE` for univariate Cox regression including
#'   an arm variable. When no arm variable is included in the univariate Cox regression,
#'   then also `TRUE` should be used to tabulate the covariate effect estimates instead
#'   of the treatment arm effect estimate across models.
#' @param vars (`character`)\cr the name of statistics to be reported among
#'  `n` (number of observation),
#'  `hr` (Hazard Ratio),
#'  `ci` (confidence interval),
#'  `pval` (p.value of the treatment effect) and
#'  `pval_inter` (the p.value of the interaction effect between the treatment
#'  and the covariate).
#'
#' @examples
#' # summarize_coxreg
#' result_univar <- basic_table() %>%
#'   split_rows_by("effect") %>%
#'   split_rows_by("term", child_labels = "hidden") %>%
#'   summarize_coxreg(conf_level = 0.95) %>%
#'   build_table(df1)
#' result_univar
#'
#' result_multivar <- basic_table() %>%
#'   split_rows_by("term", child_labels = "hidden") %>%
#'   summarize_coxreg(multivar = TRUE, conf_level = .95) %>%
#'   build_table(df2)
#' result_multivar
#'
#' # When tabulating univariate models with only covariates, also `multivar = TRUE`
#' # is used.
#' result_univar_covs <- basic_table() %>%
#'   split_rows_by("term", child_labels = "hidden") %>%
#'   summarize_coxreg(multivar = TRUE, conf_level = 0.95) %>%
#'   build_table(df1_covs)
#' result_univar_covs
#'
#' # No change for the multivariate tabulation when no treatment arm is included.
#' result_multivar_covs <- basic_table() %>%
#'   split_rows_by("term", child_labels = "hidden") %>%
#'   summarize_coxreg(multivar = TRUE, conf_level = .95) %>%
#'   build_table(df2_covs)
#' result_multivar_covs
#'
#' @export
summarize_coxreg <- function(lyt,
                             conf_level,
                             multivar = FALSE,
                             vars = c("n", "hr", "ci", "pval")) {
  afun_list <- Map(
    function(stat, format) {
      make_afun(s_coxreg, .stats = stat, .formats = format, .ungroup_stats = stat)
    },
    stat = c("n", "hr", "ci", "pval", "pval_inter"),
    format = c(
      n = "xx",
      hr = "xx.xx",
      ci = "(xx.xx, xx.xx)",
      pval = "x.xxxx | (<0.0001)",
      pval_inter = "x.xxxx | (<0.0001)"
    )
  )

  if (multivar) {
    vars <- intersect(c("hr", "ci", "pval"), vars)
    lyt <- split_cols_by_multivar(
      lyt = lyt,
      vars = vars,
      varlabels = c(
        hr = "Hazard Ratio",
        ci = paste0(100 * conf_level, "% CI"),
        pval = "p-value"
      )[vars]
    )
  } else {
    lyt <- split_cols_by_multivar(
      lyt = lyt,
      vars = vars,
      varlabels = c(
        n = "n", hr = "Hazard Ratio",
        ci = paste0(100 * conf_level, "% CI"),
        pval = "p-value",
        pval_inter = "Interaction p-value"
      )[vars]
    )
  }

  analyze_colvars(lyt = lyt, afun = afun_list[vars])
}

#' Fits for Cox Proportional Hazards Regression
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Fitting functions for univariate and multivariate Cox regression models.
#'
#' @examples
#' # Testing dataset [survival::bladder].
#'
#' library(survival)
#' library(rtables)
#' set.seed(1, kind = "Mersenne-Twister")
#' dta_bladder <- with(
#'   data = bladder[bladder$enum < 5, ],
#'   data.frame(
#'     time = stop,
#'     status = event,
#'     armcd = as.factor(rx),
#'     covar1 = as.factor(enum),
#'     covar2 = factor(
#'       sample(as.factor(enum)),
#'       levels = 1:4, labels = c("F", "F", "M", "M")
#'     )
#'   )
#' )
#' labels <- c("armcd" = "ARM", "covar1" = "A Covariate Label", "covar2" = "Sex (F/M)")
#' formatters::var_labels(dta_bladder)[names(labels)] <- labels
#' dta_bladder$age <- sample(20:60, size = nrow(dta_bladder), replace = TRUE)
#'
#' plot(
#'   survfit(Surv(time, status) ~ armcd + covar1, data = dta_bladder),
#'   lty = 2:4,
#'   xlab = "Months",
#'   col = c("blue1", "blue2", "blue3", "blue4", "red1", "red2", "red3", "red4")
#' )
#'
#' @name fit_coxreg
NULL

#' @describeIn fit_coxreg Fit a series of univariate Cox regression models
#'   given the inputs.
#'
#' @param variables (`list`)\cr a named list corresponds to the names of variables found
#'   in `data`, passed as a named list and corresponding to `time`, `event`, `arm`,
#'   `strata`, and `covariates` terms. If `arm` is missing from `variables`, then
#'   only Cox model(s) including the `covariates` will be fitted and the corresponding
#'   effect estimates will be tabulated later.
#' @param data (`data frame`)\cr the dataset containing the variables to fit the
#'   models.
#' @param at (`list` of `numeric`)\cr when the candidate covariate is a
#'  `numeric`, use `at` to specify the value of the covariate at which the
#'  effect should be estimated.
#' @param control (`list`)\cr a list of parameters as returned by the
#'   helper function [control_coxreg()].
#' @return The function `fit_coxreg_univar` returns a `coxreg.univar` class object which is a named list
#' with 5 elements:
#'   - `mod`: Cox regression models fitted by [survival::coxph()].
#'   - `data`: The original data frame input.
#'   - `control`: The original control input.
#'   - `vars`: The variables used in the model.
#'   - `at`: Value of the covariate at which the effect should be estimated.
#' @note When using `fit_coxreg_univar` there should be two study arms.
#'
#' @examples
#' # fit_coxreg_univar
#'
#' ## Cox regression: arm + 1 covariate.
#' mod1 <- fit_coxreg_univar(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = "covar1"
#'   ),
#'   data = dta_bladder,
#'   control = control_coxreg(conf_level = 0.91)
#' )
#'
#' ## Cox regression: arm + 1 covariate + interaction, 2 candidate covariates.
#' mod2 <- fit_coxreg_univar(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder,
#'   control = control_coxreg(conf_level = 0.91, interaction = TRUE)
#' )
#'
#' ## Cox regression: arm + 1 covariate, stratified analysis.
#' mod3 <- fit_coxreg_univar(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd", strata = "covar2",
#'     covariates = c("covar1")
#'   ),
#'   data = dta_bladder,
#'   control = control_coxreg(conf_level = 0.91)
#' )
#'
#' ## Cox regression: no arm, only covariates.
#' mod4 <- fit_coxreg_univar(
#'   variables = list(
#'     time = "time", event = "status",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder
#' )
#'
#' @export
fit_coxreg_univar <- function(variables,
                              data,
                              at = list(),
                              control = control_coxreg()) {
  checkmate::assert_list(variables, names = "named")
  has_arm <- "arm" %in% names(variables)
  arm_name <- if (has_arm) "arm" else NULL

  if (!is.null(variables$covariates)) {
    assertthat::assert_that(is.character(variables$covariates))
  }

  assert_df_with_variables(data, variables)
  assert_list_of_variables(variables[c(arm_name, "event", "time")])

  if (!is.null(variables$strata)) {
    assertthat::assert_that(
      control$pval_method != "likelihood"
    )
  }
  if (has_arm) {
    assertthat::assert_that(is_df_with_nlevels_factor(data, variables$arm, 2L))
  }
  vars <- unlist(variables[c(arm_name, "covariates", "strata")], use.names = FALSE)
  for (i in vars) {
    if (is.factor(data[[i]])) {
      attr(data[[i]], "levels") <- levels(droplevels(data[[i]]))
    }
  }
  forms <- h_coxreg_univar_formulas(variables, interaction = control$interaction)
  mod <- lapply(
    forms, function(x) {
      survival::coxph(formula = stats::as.formula(x), data = data, ties = control$ties)
    }
  )
  structure(
    list(
      mod = mod,
      data = data,
      control = control,
      vars = variables,
      at = at
    ),
    class = "coxreg.univar"
  )
}

#' @describeIn fit_coxreg Fit a multi-variable Cox regression model.
#'
#' @return The function `fit_coxreg_multivar` returns a `coxreg.multivar` class object which is a named list
#'   with 4 elements:
#'   - `mod`: Cox regression model fitted by [survival::coxph()].
#'   - `data`: The original data frame input.
#'   - `control`: The original control input.
#'   - `vars`: The variables used in the model.
#'
#' @examples
#' # fit_coxreg_multivar
#'
#' ## Cox regression: multivariate Cox regression.
#' multivar_model <- fit_coxreg_multivar(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder
#' )
#'
#' # Example without treatment arm.
#' multivar_covs_model <- fit_coxreg_multivar(
#'   variables = list(
#'     time = "time", event = "status",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder
#' )
#'
#' @export
fit_coxreg_multivar <- function(variables,
                                data,
                                control = control_coxreg()) {
  checkmate::assert_list(variables, names = "named")
  has_arm <- "arm" %in% names(variables)
  arm_name <- if (has_arm) "arm" else NULL

  if (!is.null(variables$covariates)) {
    assertthat::assert_that(is.character(variables$covariates))
  }

  assertthat::assert_that(
    isFALSE(control$interaction)
  )
  assert_df_with_variables(data, variables)
  assert_list_of_variables(variables[c(arm_name, "event", "time")])

  if (!is.null(variables$strata)) {
    assertthat::assert_that(
      control$pval_method != "likelihood"
    )
  }

  form <- h_coxreg_multivar_formula(variables)
  mod <- survival::coxph(
    formula = stats::as.formula(form),
    data = data,
    ties = control$ties
  )
  structure(
    list(
      mod = mod,
      data = data,
      control = control,
      vars = variables
    ),
    class = "coxreg.multivar"
  )
}

#' Muffled `car::Anova`
#'
#' Applied on survival models, [car::Anova()] signal that the `strata` terms is dropped from the model formula when
#' present, this function deliberately muffles this message.
#'
#' @param mod (`coxph`)\cr Cox regression model fitted by [survival::coxph()].
#' @param test_statistic (`string`)\cr the method used for estimation of p.values;
#'   `wald` (default) or `likelihood`.
#'
#' @keywords internal
muffled_car_anova <- function(mod, test_statistic) {
  tryCatch(
    withCallingHandlers(
      expr = {
        car::Anova(
          mod,
          test.statistic = test_statistic,
          type = "III"
        )
      },
      message = function(m) invokeRestart("muffleMessage"),
      error = function(e) {
        stop(paste(
          "the model seems to have convergence problems, please try to change",
          "the configuration of covariates or strata variables, e.g.",
          "- original error:", e
        ))
      }
    )
  )
}
