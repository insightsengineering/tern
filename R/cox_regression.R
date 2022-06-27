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
#' @name cox_regression
#' @order 1
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
NULL

#' @describeIn cox_regression Helper for Cox Regression Formula
#'
#' Creates a list of formulas. It is used internally by [fit_coxreg_univar()]
#' for the comparison of univariate Cox regression models.
#'
#' @inheritParams argument_convention
#' @inheritParams control_coxreg
#'
#' @return
#' The function `h_coxreg_univar_formulas` returns a `character` vector coercible
#' into formulas (e.g [stats::as.formula()]).
#'
#' @examples
#' # `h_coxreg_univar_formulas`
#'
#' ## Simple formulas.
#' h_coxreg_univar_formulas(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd", covariates = c("X", "y")
#'   )
#' )
#'
#' ## Addition of an optional strata.
#' h_coxreg_univar_formulas(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
#'     strata = "SITE"
#'   )
#' )
#'
#' ## Inclusion of the interaction term.
#' h_coxreg_univar_formulas(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
#'     strata = "SITE"
#'   ),
#'   interaction = TRUE
#' )
#'
#' ## Only covariates fitted in separate models.
#' h_coxreg_univar_formulas(
#'   variables = list(
#'     time = "time", event = "status", covariates = c("X", "y")
#'   )
#' )
#'
#' @export
h_coxreg_univar_formulas <- function(variables,
                                     interaction = FALSE) {
  checkmate::assert_list(variables, names = "named")
  has_arm <- "arm" %in% names(variables)
  arm_name <- if (has_arm) "arm" else NULL

  checkmate::assert_character(variables$covariates, null.ok = TRUE)

  checkmate::assert_flag(interaction)
  checkmate::assert_true((has_arm || (!interaction)))
  checkmate::assert_true((!is.null(variables$covariates) || (!interaction)))

  assert_list_of_variables(variables[c(arm_name, "event", "time")])

  if (!is.null(variables$covariates)) {
    forms <- paste0(
      "survival::Surv(", variables$time, ", ", variables$event, ") ~ ",
      ifelse(has_arm, variables$arm, "1"),
      ifelse(interaction, " * ", " + "),
      variables$covariates,
      ifelse(
        !is.null(variables$strata),
        paste0(" + strata(", paste0(variables$strata, collapse = ", "), ")"),
        ""
      )
    )
  } else {
    forms <- NULL
  }
  nams <- variables$covariates
  if (has_arm) {
    ref <- paste0(
      "survival::Surv(", variables$time, ", ", variables$event, ") ~ ",
      variables$arm,
      ifelse(
        !is.null(variables$strata),
        paste0(
          " + strata(", paste0(variables$strata, collapse = ", "), ")"
        ),
        ""
      )
    )
    forms <- c(ref, forms)
    nams <- c("ref", nams)
  }
  stats::setNames(forms, nams)
}

#' @describeIn cox_regression Helper for Multi-variable Cox Regression Formula
#'
#' Creates a formulas string. It is used internally by [fit_coxreg_multivar()]
#' for the comparison of multi-variable Cox regression models. Interactions will not
#' be included in multi-variable Cox regression model.
#'
#' @inheritParams argument_convention
#'
#' @return
#' The function `h_coxreg_univar_formulas` returns a `character` vector coercible
#' into formulas (e.g [stats::as.formula()]).
#'
#' @export
#'
#' @examples
#' # `h_coxreg_multivar_formula`
#'
#' h_coxreg_multivar_formula(
#'   variables = list(
#'     time = "AVAL", event = "event", arm = "ARMCD", covariates = c("RACE", "AGE")
#'   )
#' )
#'
#' # Addition of an optional strata.
#' h_coxreg_multivar_formula(
#'   variables = list(
#'     time = "AVAL", event = "event", arm = "ARMCD", covariates = c("RACE", "AGE"),
#'     strata = "SITE"
#'   )
#' )
#'
#' # Example without treatment arm.
#' h_coxreg_multivar_formula(
#'   variables = list(
#'     time = "AVAL", event = "event", covariates = c("RACE", "AGE"),
#'     strata = "SITE"
#'   )
#' )
h_coxreg_multivar_formula <- function(variables) {
  checkmate::assert_list(variables, names = "named")
  has_arm <- "arm" %in% names(variables)
  arm_name <- if (has_arm) "arm" else NULL

  checkmate::assert_character(variables$covariates, null.ok = TRUE)

  assert_list_of_variables(variables[c(arm_name, "event", "time")])

  y <- paste0(
    "survival::Surv(", variables$time, ", ", variables$event, ") ~ ",
    ifelse(has_arm, variables$arm, "1")
  )
  if (length(variables$covariates) > 0) {
    y <- paste(y, paste(variables$covariates, collapse = " + "), sep = " + ")
  }
  if (!is.null(variables$strata)) {
    y <- paste0(y, " + strata(", paste0(variables$strata, collapse = ", "), ")")
  }
  y
}

#' Controls for Cox regression
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
#'
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
  checkmate::assert_flag(interaction)
  assert_proportion_value(conf_level)
  list(
    pval_method = pval_method,
    ties = ties,
    conf_level = conf_level,
    interaction = interaction
  )
}

#' @describeIn cox_regression Fit a series of univariate Cox regression models
#'   given the inputs.
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
#' @export
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
    checkmate::assert_true(control$pval_method != "likelihood")
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

#' @describeIn cox_regression Custom tidy method for [survival::coxph()] summary results.
#'
#' Tidy the [survival::coxph()] results into a `data.frame` to extract model results.
#'
#' @inheritParams argument_convention
#' @method tidy summary.coxph
#' @export
#'
#' @examples
#' library(survival)
#' library(broom)
#' library(rtables)
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

#' @describeIn cox_regression Utility function to help tabulate the result of
#' a univariate Cox regression model.
#'
#' @inheritParams argument_convention
#' @inheritParams cox_regression_inter
#' @param effect (`string`)\cr the treatment variable.
#' @param mod (`coxph`)\cr Cox regression model fitted by [survival::coxph()].
#'
#' @examples
#' library(survival)
#'
#' dta_simple <- data.frame(
#'   time = c(5, 5, 10, 10, 5, 5, 10, 10),
#'   status = c(0, 0, 1, 0, 0, 1, 1, 1),
#'   armcd = factor(LETTERS[c(1, 1, 1, 1, 2, 2, 2, 2)], levels = c("A", "B")),
#'   var1 = c(45, 55, 65, 75, 55, 65, 85, 75),
#'   var2 = c("F", "M", "F", "M", "F", "M", "F", "U")
#' )
#' mod <- coxph(Surv(time, status) ~ armcd + var1, data = dta_simple)
#' result <- h_coxreg_univar_extract(
#'   effect = "armcd", covar = "armcd", mod = mod, data = dta_simple
#' )
#' result
#'
#' @export
h_coxreg_univar_extract <- function(effect,
                                    covar,
                                    data,
                                    mod,
                                    control = control_coxreg()) {
  checkmate::assert_string(covar)
  checkmate::assert_string(effect)
  checkmate::assert_class(mod, "coxph")
  test_statistic <- c(wald = "Wald", likelihood = "LR")[control$pval_method]

  mod_aov <- muffled_car_anova(mod, test_statistic)
  msum <- summary(mod, conf.int = control$conf_level)
  sum_cox <- broom::tidy(msum)

  # Combine results together.
  effect_aov <- mod_aov[effect, , drop = TRUE]
  pval <- effect_aov[[grep(pattern = "Pr", x = names(effect_aov)), drop = TRUE]]
  sum_main <- sum_cox[grepl(effect, sum_cox$level), ]

  term_label <- if (effect == covar) {
    paste0(
      levels(data[[covar]])[2],
      " vs control (",
      levels(data[[covar]])[1],
      ")"
    )
  } else {
    unname(labels_or_names(data[covar]))
  }
  data.frame(
    effect = ifelse(covar == effect, "Treatment:", "Covariate:"),
    term = covar,
    term_label = term_label,
    level = levels(data[[effect]])[2],
    n = mod[["n"]],
    hr = unname(sum_main["exp(coef)"]),
    lcl = unname(sum_main[grep("lower", names(sum_main))]),
    ucl = unname(sum_main[grep("upper", names(sum_main))]),
    pval = pval,
    stringsAsFactors = FALSE
  )
}

#' @describeIn cox_regression Custom tidy method for a Univariate Cox Regression
#'
#' Tidy up the result of a Cox regression model fitted by [`fit_coxreg_univar()`].
#'
#' @inheritParams argument_convention
#' @param x (`list`)\cr Result of the Cox regression model fitted by [`fit_coxreg_univar()`].
#'
#' @method tidy coxreg.univar
#' @export
#'
#' @examples
#' library(broom)
#' tidy(mod1)
#' tidy(mod2)
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

#' @describeIn cox_regression Fit a multi-variable Cox regression model.
#' @inheritParams fit_coxreg_univar
#' @return The function `fit_coxreg_multivar` returns a `coxreg.multivar` class object which is a named list
#'   with 4 elements:
#'   - `mod`: Cox regression model fitted by [survival::coxph()].
#'   - `data`: The original data frame input.
#'   - `control`: The original control input.
#'   - `vars`: The variables used in the model.
#' @export
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
    checkmate::assert_true(control$pval_method != "likelihood")
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

#' @describeIn cox_regression Tabulation of Multi-variable Cox Regressions
#'
#' Utility function to help tabulate the result of a multi-variable Cox regression model
#' for a treatment/covariate variable.
#'
#' @inheritParams argument_convention
#' @inheritParams h_coxreg_univar_extract
#' @export
#'
#' @examples
#' library(survival)
#'
#' mod <- coxph(Surv(time, status) ~ armcd + var1, data = dta_simple)
#' result <- h_coxreg_multivar_extract(
#'   var = "var1", mod = mod, data = dta_simple
#' )
#' result
h_coxreg_multivar_extract <- function(var,
                                      data,
                                      mod,
                                      control = control_coxreg()) {
  test_statistic <- c(wald = "Wald", likelihood = "LR")[control$pval_method]
  mod_aov <- muffled_car_anova(mod, test_statistic)

  msum <- summary(mod, conf.int = control$conf_level)
  sum_anova <- broom::tidy(mod_aov)
  sum_cox <- broom::tidy(msum)

  ret_anova <- sum_anova[sum_anova$term == var, c("term", "p.value")]
  names(ret_anova)[2] <- "pval"
  if (is.factor(data[[var]])) {
    ret_cox <- sum_cox[startsWith(prefix = var, x = sum_cox$level), !(names(sum_cox) %in% "exp(-coef)")]
  } else {
    ret_cox <- sum_cox[(var == sum_cox$level), !(names(sum_cox) %in% "exp(-coef)")]
  }
  names(ret_cox)[1:4] <- c("pval", "hr", "lcl", "ucl")
  varlab <- unname(labels_or_names(data[var]))
  ret_cox$term <- varlab

  if (is.numeric(data[[var]])) {
    ret <- ret_cox
    ret$term_label <- ret$term
  } else if (length(levels(data[[var]])) <= 2) {
    ret_anova$pval <- NA
    ret_anova$term_label <- paste0(varlab, " (reference = ", levels(data[[var]])[1], ")")
    ret_cox$level <- gsub(var, "", ret_cox$level)
    ret_cox$term_label <- ret_cox$level
    ret <- dplyr::bind_rows(ret_anova, ret_cox)
  } else {
    ret_anova$term_label <- paste0(varlab, " (reference = ", levels(data[[var]])[1], ")")
    ret_cox$level <- gsub(var, "", ret_cox$level)
    ret_cox$term_label <- ret_cox$level
    ret <- dplyr::bind_rows(ret_anova, ret_cox)
  }

  as.data.frame(ret)
}

#' @describeIn cox_regression Custom tidy method for a Multi-variable Cox Regression
#'
#' Tidy up the result of a Cox regression model fitted by [`fit_coxreg_multivar()`].
#'
#' @inheritParams argument_convention
#' @param x (`list`)\cr Result of the Cox regression model fitted by [`fit_coxreg_multivar()`].
#'
#' @method tidy coxreg.multivar
#' @export
#'
#' @examples
#' library(broom)
#' broom::tidy(multivar_model)
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

#' @describeIn cox_regression transforms the tabulated results from [`fit_coxreg_univar()`]
#'  and [`fit_coxreg_multivar()`] into a list. Not much calculation is done here,
#'  it rather prepares the data to be used by the layout creating function.
#' @export
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
s_coxreg <- function(df, .var) {
  assert_df_with_variables(df, list(term = "term", var = .var))
  checkmate::assert_multi_class(df$term, classes = c("factor", "character"))
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
#' @export
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
