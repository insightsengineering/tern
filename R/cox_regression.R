#' Cox Proportional Hazards Regression
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
#' analysis \insertCite{NESTTeam2020}{tern}.
#'
#' @note The usual formatting arguments for the _layout creating_ function
#'  `summarize_coxreg` are not yet accepted (`.stats`, `.indent_mod`, `.formats`,
#'  `.labels`).
#' @inheritParams argument_convention
#' @references
#' + \insertAllCited{}
#' + \insertRef{Hughes2020a}{tern}
#'
#' @name cox_regression
#' @order 1
#' @examples
#'
#' # Testing dataset [survival::bladder].
#'
#' library(survival)
#' set.seed(1, kind = "Mersenne-Twister")
#' dta_bladder <- with(
#'   data = bladder[bladder$enum < 5, ],
#'   data.frame(
#'     time = stop,
#'     status = event,
#'     armcd = as.factor(rx),
#'     covar1 = as.factor(enum),
#'     covar2 = factor(
#'       sample(as.factor(enum)), levels = 1:4, labels = c("F", "F", "M", "M")
#'     )
#'   )
#' )
#' attr(dta_bladder$armcd, "label") <- "ARM"
#' attr(dta_bladder$covar1, "label") <- "A Covariate Label"
#' attr(dta_bladder$covar2, "label") <- "Sex (F/M)"
#' dta_bladder$age <- sample(20:60, size = nrow(dta_bladder), replace = TRUE)
#'
#' plot(
#'   survfit(Surv(time, status) ~ armcd + covar1, data = dta_bladder),
#'   lty= 2:4, xlab="Months",
#'   col = c("blue1", "blue2", "blue3", "blue4", "red1", "red2", "red3", "red4")
#' )
#'
NULL

#' @describeIn cox_regression Helper for Cox Regression Formula
#'
#' Creates a list of formulas. It is used internally by [fit_coxreg_univar()]
#' for the comparison of univariate Cox regression models.
#'
#' @inheritParams argument_convention
#' @inheritParams control_coxreg
#'
#' @return It `character` vector coercible into formulas (e.g [as.formula()]).
#'
#' @importFrom stats setNames
#' @export
#' @examples
#'
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
h_coxreg_univar_formulas <- function(variables,
                                     interaction = FALSE) {
  assert_that(
    is.character(variables$covariates),
    is_variables(variables[c("arm", "event", "time")]),
    is.flag(interaction)
  )
  ref <- paste(
    "Surv(", variables$time, ",", variables$event, ") ~ ", variables$arm
  )
  covar <- paste(
    ref,
    ifelse(interaction, "*", "+"),
    variables$covariates,
    ifelse(
      !is.null(variables$strata),
      paste("+ strata(", variables$strata, ")"),
      ""
    )
  )
  stats::setNames(c(ref, covar), c("ref", variables$covariates))
}

#' @describeIn cox_regression Helper for Multi-variable Cox Regression Formula
#'
#' Creates a formulas string. It is used internally by [fit_coxreg_multivar()]
#' for the comparison of multi-variable Cox regression models. Interactions will not
#' be included in multi-variable Cox regression model.
#'
#' @inheritParams argument_convention
#'
#' @return A `string` coercible into formulas (e.g [as.formula()]).
#'
#' @export
#' @examples
#'
#' # `h_coxreg_multivar_formula`
#'
#' h_coxreg_multivar_formula(
#'   variables = list(
#'     time = "AVAL", event = "event", arm = "ARMCD", covariates = c("RACE", "AGE")
#'   )
#' )
#'
h_coxreg_multivar_formula <- function(variables) {
  assert_that(
    is.character(variables$covariates),
    is_variables(variables[c("arm", "event", "time")])
  )
  covariates_part <- paste(variables$covariates, collapse = " + ")
  paste0("Surv(", variables$time, ", ", variables$event, ") ~ ", variables$arm, " + ", covariates_part)
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
#'   interaction between the studied treatment and candidate covariate.
#' @param ties (`string`)\cr among `exact` (equivalent to `DISCRETE` in SAS),
#'   `efron` and `breslow`, see [survival::coxph()].
#'   Note: there is no equivalent of SAS `EXACT` method in R.
#'
#' @return A `list` of item corresponding to the arguments.
#' @export
#' @examples
#'
#' control_coxreg()
#'
control_coxreg <- function(pval_method = c("wald", "likelihood"),
                           ties = c("exact", "efron", "breslow"),
                           conf_level = 0.95,
                           interaction = FALSE) {
  pval_method <- match.arg(pval_method)
  ties <- match.arg(ties)
  assert_that(
    is_proportion(conf_level),
    is.flag(interaction)
  )
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
#'   and `covariates` terms.
#' @param data (`data frame`)\cr the dataset containing the variables to fit the
#'   models.
#' @param at (`list` of `numeric`)\cr when the candidate covariate is a
#'  `numeric`, use `at` to specify the value of the covariate at which the
#'  effect should be estimated.
#' @param control (`list`)\cr a list of parameters as returned by the
#'   helper function [control_coxreg()].
#' @return A `coxreg.univar` class object which is a named list with 5 elements:
#'   - `mod`: Cox regression model fitted by [survival::coxph()].
#'   - `data`: The original data frame input.
#'   - `control`: The original control input.
#'   - `vars`: The variables used in the model.
#'   - `at`: Value of the covariate at which the effect should be estimated.
#' @importFrom survival coxph
#' @importFrom stats as.formula
#' @importFrom car Anova
#' @export
#' @examples
#'
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
fit_coxreg_univar <- function(variables,
                              data,
                              at = list(),
                              control = control_coxreg()) {
  assert_that(
    is.character(variables$covariates),
    is_variables(variables[c("arm", "event", "time")]),
    is_df_with_variables(data, as.list(unlist(variables)))
  )

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

#' Custom tidy method for [survival::coxph()] summary results.
#'
#' Tidy the [survival::coxph()] results into a `tibble` to extract model results.
#'
#' @inheritParams argument_convention
#' @importFrom dplyr as_tibble
#' @method tidy summary.coxph
#' @export
#' @examples
#' library(survival)
#' set.seed(1, kind = "Mersenne-Twister")
#' dta_bladder <- with(
#'   data = bladder[bladder$enum < 5, ],
#'   data.frame(
#'     time = stop,
#'     status = event,
#'     armcd = as.factor(rx),
#'     covar1 = as.factor(enum),
#'     covar2 = factor(
#'       sample(as.factor(enum)), levels = 1:4, labels = c("F", "F", "M", "M")
#'     )
#'   )
#' )
#' attr(dta_bladder$armcd, "label") <- "ARM"
#' attr(dta_bladder$covar1, "label") <- "A Covariate Label"
#' attr(dta_bladder$covar2, "label") <- "Sex (F/M)"
#' dta_bladder$age <- sample(20:60, size = nrow(dta_bladder), replace = TRUE)
#'
#' formula <- "Surv(time, status) ~ armcd + covar1"
#' msum <- summary(survival::coxph(stats::as.formula(formula), data = dta_bladder))
#' broom::tidy(msum)
#'
tidy.summary.coxph <- function(x, # nousage # nolint
                               ...) {
  assert_that(
    class(x) == "summary.coxph"
  )

  pval <- x$coefficients
  confint <- x$conf.int
  levels <- rownames(pval)

  pval <- dplyr::as_tibble(pval)
  confint <- dplyr::as_tibble(confint)

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
#' @importFrom car Anova
#' @importFrom broom tidy
#' @export
#'
#' @examples
#'
#' library(survival)
#' dta_simple <-  data.frame(
#'   time = c(5, 5, 10, 10, 5, 5, 10, 10),
#'   status = c(0, 0, 1, 0, 0, 1, 1, 1),
#'   armcd  = factor(LETTERS[c(1, 1, 1, 1, 2, 2, 2, 2)], levels = c("A", "B")),
#'   var1 = c(45, 55, 65, 75, 55, 65, 85, 75),
#'   var2 = c("F", "M", "F", "M", "F", "M", "F", "U")
#' )
#' mod <- coxph(Surv(time, status) ~ armcd + var1, data = dta_simple)
#' result <- h_coxreg_univar_extract(
#'   effect = "armcd", covar = "armcd", mod = mod, data = dta_simple
#' )
#' result
#'
h_coxreg_univar_extract <- function(effect,
                                    covar,
                                    data,
                                    mod,
                                    control = control_coxreg()) {
  assert_that(
    is.string(covar),
    is.string(effect),
    class(mod) == "coxph"
  )
  test_statistic <- c(wald = "Wald", likelihood = "LR")[control$pval_method]
  mod_aov <- car::Anova(
    mod,
    test.statistic = test_statistic,
    type = "III"
  )
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

#' Cox Regression Helper: Interactions
#'
#' Test and estimate the effect of a treatment in interaction with a covariate.
#' The effect is estimated as the HR of the tested treatment for a given level
#' of the covariate, in comparison to the treatment control.
#'
#' @param x (`numeric` or `factor`)\cr the values of the effect to be tested.
#' @param effect (`string`)\cr the name of the effect to be tested and estimated.
#' @param covar (`string`)\cr the name of the covariate in the model.
#' @param mod (`coxph`)\cr the Cox regression model.
#' @param label (`string`)\cr the label to be return as `term_label`
#'   (see `return`).
#' @param control (`list`)\cr a list of controls as returned by
#'   [control_coxreg()].
#' @param ... see methods.
#' @export
#' @name cox_regression_inter
#' @examples
#'
#' # Testing dataset [survival::bladder].
#' library(survival)
#' set.seed(1, kind = "Mersenne-Twister")
#' dta_bladder <- with(
#'   data = bladder[bladder$enum < 5, ],
#'   data.frame(
#'     time = stop,
#'     status = event,
#'     armcd = as.factor(rx),
#'     covar1 = as.factor(enum),
#'     covar2 = factor(
#'       sample(as.factor(enum)), levels = 1:4, labels = c("F", "F", "M", "M")
#'     )
#'   )
#' )
#' attr(dta_bladder$armcd, "label") <- "ARM"
#' attr(dta_bladder$covar1, "label") <- "A Covariate Label"
#' attr(dta_bladder$covar2, "label") <- "Sex (F/M)"
#' dta_bladder$age <- sample(20:60, size = nrow(dta_bladder), replace = TRUE)
#'
#' plot(
#'   survfit(Surv(time, status) ~ armcd + covar1, data = dta_bladder),
#'   lty= 2:4, xlab="Months",
#'   col = c("blue1", "blue2", "blue3", "blue4", "red1", "red2", "red3", "red4")
#' )
#'
h_coxreg_inter_effect <- function(x,
                                  effect,
                                  covar,
                                  mod,
                                  label,
                                  control,
                                  ...) {
  UseMethod("h_coxreg_inter_effect", x)
}


#' @describeIn cox_regression_inter Estimate the interaction with a numerical
#'   covariate
#' @param at (`list`)\cr a list with items named after the covariate, every
#'   item is a vector of levels at which the interaction should be estimated.
#' @importFrom stats qnorm median terms
#' @export
h_coxreg_inter_effect.numeric <- function(x, # nousage # nolint
                                          effect,
                                          covar,
                                          mod,
                                          label,
                                          control,
                                          at,
                                          ...) {
  betas <- coef(mod)
  term_indices <- grep(
    pattern = effect,
    x = attr(stats::terms(mod), "term.labels")
  )
  assert_that(length(term_indices) == 2)
  betas <- betas[term_indices]
  betas_var <- diag(vcov(mod))[term_indices]
  betas_cov <- vcov(mod)[term_indices[1], term_indices[2]]
  xval <- if (is.null(at[[covar]])) {
    stats::median(x)
  } else {
    at[[covar]]
  }
  effect_index <- !grepl(":", names(betas))
  coef_hat <- betas[effect_index] + xval * betas[!effect_index]
  coef_se <- sqrt(
    betas_var[effect_index] +
      xval ^ 2 * betas_var[!effect_index] +
      2 * xval * betas_cov
  )
  q_norm  <- stats::qnorm((1 + control$conf_level) / 2)
  data.frame(
    effect = "Covariate:",
    term = rep(covar, length(xval)),
    term_label = paste0("  ", xval),
    level = as.character(xval),
    n = NA,
    hr = exp(coef_hat),
    lcl = exp(coef_hat - q_norm * coef_se),
    ucl = exp(coef_hat + q_norm * coef_se),
    pval = NA,
    pval_inter = NA,
    stringsAsFactors = FALSE
  )
}

#' @describeIn cox_regression_inter Estimate the interaction with a factor
#'   covariate.
#'
#' @param data (`data frame`)\cr the data frame on which the model was fit.
#' @importFrom stats coef model.matrix
#' @export
h_coxreg_inter_effect.factor <- function(x,  # nousage # nolint
                                         effect,
                                         covar,
                                         mod,
                                         label,
                                         control,
                                         data,
                                         ...) {

  y <- h_coxreg_inter_estimations(
    variable = effect, given = covar,
    lvl_var = levels(data[[effect]]),
    lvl_given = levels(data[[covar]]),
    mod = mod,
    conf_level = 0.95
  )[[1]]

  data.frame(
    effect = "Covariate:",
    term = rep(covar, nrow(y)),
    term_label = as.character(paste0("  ", levels(data[[covar]]))),
    level = as.character(levels(data[[covar]])),
    n = NA,
    hr = y[, "hr"],
    lcl = y[, "lcl"],
    ucl = y[, "ucl"],
    pval = NA,
    pval_inter = NA,
    stringsAsFactors = FALSE
  )
}

#' @describeIn cox_regression_inter a higher level function that returns
#'   the test of the interaction test and the estimated values. If
#'   no interaction, [h_coxreg_univar_extract()] is applied.
#' @importFrom car Anova
#' @importFrom broom tidy
#' @export
#' @examples
#'
#' mod <- coxph(Surv(time, status) ~ armcd * covar1, data = dta_bladder)
#' h_coxreg_extract_interaction(
#'   mod = mod, effect = "armcd", covar = "covar1", data = dta_bladder,
#'   control = control_coxreg()
#' )
#'
h_coxreg_extract_interaction <- function(effect,
                                         covar,
                                         mod,
                                         data,
                                         at,
                                         control) {
  if (!any(attr(stats::terms(mod), "order") == 2)) {
    y <- h_coxreg_univar_extract(
      effect = effect, covar = covar, mod = mod, data = data, control = control
    )
    y$pval_inter <- NA
    y
  } else {
    test_statistic <- c(wald = "Wald", likelihood = "LR")[control$pval_method]

    # Test the main treatment effect.
    mod_aov <- car::Anova(mod, test.statistic = test_statistic, type = "III")
    sum_anova <- broom::tidy(mod_aov)
    pval <- sum_anova[sum_anova$term == effect, ][["p.value"]]

    # Test the interaction effect.
    pval_inter <- sum_anova[grep(":", sum_anova$term), ][["p.value"]]
    covar_test <- data.frame(
      effect = "Covariate:",
      term = covar,
      term_label = unname(labels_or_names(data[covar])),
      level = "",
      n = mod$n, hr = NA, lcl = NA, ucl = NA, pval = pval,
      pval_inter = pval_inter,
      stringsAsFactors = FALSE
    )
    # Estimate the interaction.
    y <- h_coxreg_inter_effect(
      data[[covar]], covar = covar, effect = effect, mod = mod,
      label = unname(labels_or_names(data[covar])),
      at = at, control = control, data = data
    )
    rbind(covar_test, y)
  }
}

#' @describeIn cox_regression_inter Hazard ratio
#'
#' Hazard ratio estimation in interactions
#'
#' @param variable,given (`string`)\cr
#'   the name of variables in interaction. We seek the estimation of the levels
#'   of `variable` given the levels of `given`.
#' @param lvl_var,lvl_given (`character`)\cr
#'   corresponding levels has given by [levels()].
#' @param mod (`coxph`)\cr a fitted Cox regression model (see [survival::coxph()]).
#' @inheritParams argument_convention
#' @details Given the cox regression investigating the effect of Arm (A, B, C; reference A)
#'   and Sex (F, M; reference Female) and the model being abbreviated: y ~ Arm + Sex + Arm:Sex.
#'   The cox regression estimates the coefficients along with a variance-covariance matrix for:
#'
#'   - b1 (arm b), b2 (arm c),
#'   - b3 (sex m),
#'   - b4 (arm b: sex m), b5 (arm c: sex m)
#'
#'   The estimation of the Hazard Ratio for arm C/sex M is given in reference
#'   to arm A/Sex M by exp(b2 + b3 + b5)/ exp(b3) = exp(b2 + b5).
#'   The interaction coefficient is deduced by b2 + b5 while the standard error
#'   is obtained as $sqrt(Var b2 + Var b5 + 2 * covariance (b2,b5))$.
#'
#' @return A list of matrix (one per level of variable) with rows corresponding to the combinations of
#' `variable` and `given`, with columns:
#' \describe{
#'   \item{coef_hat}{Estimation of the coefficient}
#'   \item{coef_se}{Standard error of the estimation.}
#'   \item{hr}{Hazard ratio.}
#'   \item{lcl,ucl}{lower/upper confidence limit of the hazard ratio}
#' }
#'
#' @importFrom stats qnorm
#' @export
#' @examples
#'
#' # Testing dataset [survival::bladder].
#'
#' mod <- coxph(Surv(time, status) ~ armcd * covar1, data = dta_bladder)
#' result <- h_coxreg_inter_estimations(
#'   variable = "armcd", given = "covar1",
#'   lvl_var = levels(dta_bladder$armcd),
#'   lvl_given = levels(dta_bladder$covar1),
#'   mod = mod, conf_level = .95
#' )
#' result
#'
h_coxreg_inter_estimations <- function(variable, given,
                                       lvl_var, lvl_given,
                                       mod,
                                       conf_level = 0.95) {
  var_lvl <- paste0(variable, lvl_var[-1]) # [-1]: reference level
  giv_lvl <- paste0(given, lvl_given)
  design_mat <- expand.grid(variable = var_lvl, given = giv_lvl)
  design_mat <- design_mat[order(design_mat$variable, design_mat$given), ]
  design_mat <- within(
    data = design_mat,
    expr = {
      inter <- paste0(variable, ":", given) # nolint
      rev_inter <- paste0(given, ":", variable) # nolint
    }
  )
  split_by_variable <- design_mat$variable
  interaction_names <- paste(design_mat$variable, design_mat$given, sep = "/")

  mmat <- stats::model.matrix(mod)[1, ]
  mmat[!mmat == 0] <- 0

  design_mat <- apply(
    X = design_mat, MARGIN = 1, FUN = function(x) {
      mmat[names(mmat) %in% x[-which(names(x) == "given")]] <- 1
      mmat
    })
  colnames(design_mat) <- interaction_names

  coef <- stats::coef(mod)
  vcov <- vcov(mod)
  betas <- as.matrix(coef)
  coef_hat <- t(design_mat) %*% betas
  dimnames(coef_hat)[2] <- "coef"
  coef_se <- apply(
    design_mat, 2,
    function(x) {
      vcov_el <- as.logical(x)
      y <- vcov[vcov_el, vcov_el]
      y <- sum(y)
      y <- sqrt(y)
      return(y)
    })
  q_norm <- stats::qnorm((1 + conf_level) / 2)
  y <- cbind(coef_hat, `se(coef)` = coef_se)
  y <- apply(y, 1, function(x) {
    x["hr"] <- exp(x["coef"])
    x["lcl"] <- exp(x["coef"] - q_norm * x["se(coef)"])
    x["ucl"] <- exp(x["coef"] + q_norm * x["se(coef)"])
    x
  })
  y <- t(y)
  y <- by(y, split_by_variable, identity)
  y <- lapply(y, as.matrix)
  attr(y, "details") <- paste0(
    "Estimations of ", variable,
    " hazard ratio given the level of ", given, " compared to ",
    variable, " level ", lvl_var[1], "."
  )
  y
}

#' @describeIn cox_regression Custom tidy method for a Univariate Cox Regression
#'
#' Tidy up the result of a Cox regression model fitted by [`fit_coxreg_univar()`].
#'
#' @inheritParams argument_convention
#' @param x (`list`)\cr Result of the Cox regression model fitted by [`fit_coxreg_univar()`].
#' @importFrom dplyr bind_rows
#'
#' @method tidy coxreg.univar
#' @export
#'
#' @examples
#' broom::tidy(mod1)
#' broom::tidy(mod2)
#'
tidy.coxreg.univar <- function(x, # nousage # nolint
                               ...) {
  assert_that(
    class(x) == "coxreg.univar"
  )

  mod <- x$mod
  vars <- c(x$vars$arm, x$vars$covariates)

  result <- if (x$control$interaction) {
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
  result$pval <- lapply(result$pval, empty_vector_if_na)
  if (x$control$interaction) {
    result$pval_inter <- lapply(result$pval_inter, empty_vector_if_na)
  }
  attr(result, "conf_level") <- x$control$conf_level
  result
}

#' @describeIn cox_regression Fit a multi-variable Cox regression model.
#' @inheritParams fit_coxreg_univar
#' @return A `coxreg.multivar` class object which is a named list with 4 elements:
#'   - `mod`: Cox regression model fitted by [survival::coxph()].
#'   - `data`: The original data frame input.
#'   - `control`: The original control input.
#'   - `vars`: The variables used in the model.
#' @importFrom stats as.formula
#' @importFrom survival coxph
#' @importFrom car Anova
#' @export
#' @examples
#'
#' # fit_coxreg_multivar
#'
#' ## Cox regression: multivariate cox regression
#' multivar_model <- fit_coxreg_multivar(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder
#' )
#'
fit_coxreg_multivar <- function(variables,
                                data,
                                control = control_coxreg()) {

  assert_that(
    is.character(variables$covariates),
    is_variables(variables[c("arm", "event", "time")]),
    is_df_with_variables(data, as.list(unlist(variables)))
  )

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
#' @importFrom dplyr bind_rows
#' @importFrom broom tidy
#' @export
#'
#' @examples
#' mod <- coxph(Surv(time, status) ~ armcd + var1, data = dta_simple)
#' result <- h_coxreg_multivar_extract(
#'   var = "var1", mod = mod, data = dta_simple
#' )
#' result
#'
h_coxreg_multivar_extract <- function(var,
                                      data,
                                      mod,
                                      control = control_coxreg()) {

  test_statistic <- c(wald = "Wald", likelihood = "LR")[control$pval_method]
  mod_aov <- car::Anova(
    mod,
    test.statistic = test_statistic,
    type = "III"
  )
  msum <- summary(mod, conf.int = control$conf_level)
  sum_anova <- broom::tidy(mod_aov)
  sum_cox <- broom::tidy(msum)

  ret_anova <- sum_anova[sum_anova$term == var, c("term", "p.value")]
  names(ret_anova)[2] <- "pval"
  ret_cox <- sum_cox[grepl(var, sum_cox$level), !(names(sum_cox) %in% "exp(-coef)")]
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
#' @importFrom dplyr bind_rows
#'
#' @method tidy coxreg.multivar
#' @export
#'
#' @examples
#'
#' broom::tidy(multivar_model)
#'
tidy.coxreg.multivar <- function(x, # nousage # nolint
                                 ...) {
  assert_that(
    class(x) == "coxreg.multivar"
  )

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
#' @importFrom stats setNames
#' @export
#'
#' @examples
#'
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
s_coxreg <- function(df, .var) {
  assert_that(
    is_df_with_variables(df, list(term = "term", var = .var)),
    is_character_or_factor(df$term)
  )
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
#'   candidate covariate. Default is `FALSE` for univariate Cox regression.
#' @param vars (`character`)\cr the name of statistics to be reported among
#'  `n` (number of observation), `hr` (Hazard Ratio),
#'  `ci` (confidence interval), `pval` (p.value of the treatment effect) and
#'  `pval_inter` (the p.value of the interaction effect between the treatment
#'  and the covariate).
#' @export
#'
#' @examples
#'
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
    vars <- c("hr", "ci", "pval")
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
