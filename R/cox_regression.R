#' Controls for Cox Regression
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Sets a list of parameters for Cox regression fit. Used internally.
#'
#' @inheritParams argument_convention
#' @param pval_method (`string`)\cr the method used for estimation of p.values; `wald` (default) or `likelihood`.
#' @param interaction (`flag`)\cr if `TRUE`, the model includes the interaction between the studied
#'   treatment and candidate covariate. Note that for univariate models without treatment arm, and
#'   multivariate models, no interaction can be used so that this needs to be `FALSE`.
#' @param ties (`string`)\cr among `exact` (equivalent to `DISCRETE` in SAS), `efron` and `breslow`,
#'   see [survival::coxph()]. Note: there is no equivalent of SAS `EXACT` method in R.
#'
#' @return A `list` of items with names corresponding to the arguments.
#'
#' @seealso [fit_coxreg_univar()] and [fit_coxreg_multivar()].
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
  checkmate::assert_flag(interaction)
  assert_proportion_value(conf_level)
  list(
    pval_method = pval_method,
    ties = ties,
    conf_level = conf_level,
    interaction = interaction
  )
}

#' Custom Tidy Methods for Cox Regression
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams argument_convention
#' @param x (`list`)\cr Result of the Cox regression model fitted by [fit_coxreg_univar()] (for univariate models)
#'   or [fit_coxreg_multivar()] (for multivariate models).
#'
#' @return [tidy()] returns:
#' * For `summary.coxph` objects,  a `data.frame` with columns: `Pr(>|z|)`, `exp(coef)`, `exp(-coef)`, `lower .95`,
#'   `upper .95`, `level`, and `n`.
#' * For `coxreg.univar` objects, a `data.frame` with columns: `effect`, `term`, `term_label`, `level`, `n`, `hr`,
#'   `lcl`, `ucl`, `pval`, and `ci`.
#' * For `coxreg.multivar` objects, a `data.frame` with columns: `term`, `pval`, `term_label`, `hr`, `lcl`, `ucl`,
#'   `level`, and `ci`.
#'
#' @seealso [cox_regression]
#'
#' @name tidy_coxreg
NULL

#' @describeIn tidy_coxreg Custom tidy method for [survival::coxph()] summary results.
#'
#' Tidy the [survival::coxph()] results into a `data.frame` to extract model results.
#'
#' @method tidy summary.coxph
#'
#' @examples
#' library(survival)
#' library(broom)
#'
#' set.seed(1, kind = "Mersenne-Twister")
#'
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
#' formula <- "survival::Surv(time, status) ~ armcd + covar1"
#' msum <- summary(coxph(stats::as.formula(formula), data = dta_bladder))
#' tidy(msum)
#'
#' @export
tidy.summary.coxph <- function(x, # nolint
                               ...) {
  checkmate::assert_class(x, "summary.coxph")
  pval <- x$coefficients
  confint <- x$conf.int
  levels <- rownames(pval)

  pval <- tibble::as_tibble(pval)
  confint <- tibble::as_tibble(confint)

  ret <- cbind(pval[, grepl("Pr", names(pval))], confint)
  ret$level <- levels
  ret$n <- x[["n"]]
  ret
}

#' @describeIn tidy_coxreg Custom tidy method for a univariate Cox regression.
#'
#' Tidy up the result of a Cox regression model fitted by [fit_coxreg_univar()].
#'
#' @method tidy coxreg.univar
#'
#' @examples
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
#' tidy(mod1)
#' tidy(mod2)
#'
#' @export
tidy.coxreg.univar <- function(x, # nolint
                               ...) {
  checkmate::assert_class(x, "coxreg.univar")
  mod <- x$mod
  vars <- c(x$vars$arm, x$vars$covariates)
  has_arm <- "arm" %in% names(x$vars)

  result <- if (!has_arm) {
    Map(
      mod = mod, vars = vars,
      f = function(mod, vars) {
        h_coxreg_multivar_extract(
          var = vars,
          data = x$data,
          mod = mod,
          control = x$control
        )
      }
    )
  } else if (x$control$interaction) {
    Map(
      mod = mod, covar = vars,
      f = function(mod, covar) {
        h_coxreg_extract_interaction(
          effect = x$vars$arm, covar = covar, mod = mod, data = x$data,
          at = x$at, control = x$control
        )
      }
    )
  } else {
    Map(
      mod = mod, vars = vars,
      f = function(mod, vars) {
        h_coxreg_univar_extract(
          effect = x$vars$arm, covar = vars, data = x$data, mod = mod,
          control = x$control
        )
      }
    )
  }
  result <- do.call(rbind, result)

  result$ci <- Map(lcl = result$lcl, ucl = result$ucl, f = function(lcl, ucl) c(lcl, ucl))
  result$n <- lapply(result$n, empty_vector_if_na)
  result$ci <- lapply(result$ci, empty_vector_if_na)
  result$hr <- lapply(result$hr, empty_vector_if_na)
  if (x$control$interaction) {
    result$pval_inter <- lapply(result$pval_inter, empty_vector_if_na)
    # Remove interaction p-values due to change in specifications.
    result$pval[result$effect != "Treatment:"] <- NA
  }
  result$pval <- lapply(result$pval, empty_vector_if_na)
  attr(result, "conf_level") <- x$control$conf_level
  result
}

#' @describeIn tidy_coxreg Custom tidy method for a multivariate Cox regression.
#'
#' Tidy up the result of a Cox regression model fitted by [fit_coxreg_multivar()].
#'
#' @method tidy coxreg.multivar
#'
#' @examples
#' multivar_model <- fit_coxreg_multivar(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder
#' )
#' broom::tidy(multivar_model)
#'
#' @export
tidy.coxreg.multivar <- function(x, # nolint
                                 ...) {
  checkmate::assert_class(x, "coxreg.multivar")
  vars <- c(x$vars$arm, x$vars$covariates)

  # Convert the model summaries to data.
  result <- Map(
    vars = vars,
    f = function(vars) {
      h_coxreg_multivar_extract(
        var = vars, data = x$data,
        mod = x$mod, control = x$control
      )
    }
  )
  result <- do.call(rbind, result)

  result$ci <- Map(lcl = result$lcl, ucl = result$ucl, f = function(lcl, ucl) c(lcl, ucl))
  result$ci <- lapply(result$ci, empty_vector_if_na)
  result$hr <- lapply(result$hr, empty_vector_if_na)
  result$pval <- lapply(result$pval, empty_vector_if_na)
  result <- result[, names(result) != "n"]
  attr(result, "conf_level") <- x$control$conf_level

  result
}

#' Fits for Cox Proportional Hazards Regression
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Fitting functions for univariate and multivariate Cox regression models.
#'
#' @param variables (`list`)\cr a named list corresponds to the names of variables found in `data`, passed as a named
#'   list and corresponding to `time`, `event`, `arm`, `strata`, and `covariates` terms. If `arm` is missing from
#'   `variables`, then only Cox model(s) including the `covariates` will be fitted and the corresponding effect
#'   estimates will be tabulated later.
#' @param data (`data.frame`)\cr the dataset containing the variables to fit the models.
#' @param at (`list` of `numeric`)\cr when the candidate covariate is a `numeric`, use `at` to specify
#'   the value of the covariate at which the effect should be estimated.
#' @param control (`list`)\cr a list of parameters as returned by the helper function [control_coxreg()].
#'
#' @seealso [h_cox_regression] for relevant helper functions, [cox_regression].
#'
#' @examples
#' library(survival)
#'
#' set.seed(1, kind = "Mersenne-Twister")
#'
#' # Testing dataset [survival::bladder].
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

#' @describeIn fit_coxreg Fit a series of univariate Cox regression models given the inputs.
#'
#' @return
#' * `fit_coxreg_univar()` returns a `coxreg.univar` class object which is a named `list`
#'   with 5 elements:
#'   * `mod`: Cox regression models fitted by [survival::coxph()].
#'   * `data`: The original data frame input.
#'   * `control`: The original control input.
#'   * `vars`: The variables used in the model.
#'   * `at`: Value of the covariate at which the effect should be estimated.
#'
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

  checkmate::assert_character(variables$covariates, null.ok = TRUE)

  assert_df_with_variables(data, variables)
  assert_list_of_variables(variables[c(arm_name, "event", "time")])

  if (!is.null(variables$strata)) {
    checkmate::assert_disjunct(control$pval_method, "likelihood")
  }
  if (has_arm) {
    assert_df_with_factors(data, list(val = variables$arm), min.levels = 2, max.levels = 2)
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

#' @describeIn fit_coxreg Fit a multivariate Cox regression model.
#'
#' @return
#' * `fit_coxreg_multivar()` returns a `coxreg.multivar` class object which is a named list
#'   with 4 elements:
#'   * `mod`: Cox regression model fitted by [survival::coxph()].
#'   * `data`: The original data frame input.
#'   * `control`: The original control input.
#'   * `vars`: The variables used in the model.
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
    checkmate::assert_character(variables$covariates)
  }

  checkmate::assert_false(control$interaction)
  assert_df_with_variables(data, variables)
  assert_list_of_variables(variables[c(arm_name, "event", "time")])

  if (!is.null(variables$strata)) {
    checkmate::assert_disjunct(control$pval_method, "likelihood")
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
#' @param test_statistic (`string`)\cr the method used for estimation of p.values; `wald` (default) or `likelihood`.
#'
#' @return Returns the output of [car::Anova()], with convergence message muffled.
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
