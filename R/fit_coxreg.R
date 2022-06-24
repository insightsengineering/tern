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

