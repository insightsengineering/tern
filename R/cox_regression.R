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
#'  `fit_coxreg` are not yet accepted (`.stats`, `.indent_mod`, `.formats`,
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
#' dta_bladder$AGE <- sample(20:60, size = nrow(dta_bladder), replace = TRUE)
#'
#' plot(
#'   survfit(Surv(time, status) ~ armcd + covar1, data = dta_bladder),
#'   lty= 2:4, xlab="Months",
#'   col = c("blue1", "blue2", "blue3", "blue4", "red1", "red2", "red3", "red4")
#' )
#'
NULL

#' Helper for Cox Regression Formula
#'
#' Creates a list of formulas. It is used internally by [h_coxreg()]
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
#' # `h_coxreg_univ_formulas`
#'
#' ## Simple formulas.
#' h_coxreg_univ_formulas(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd", covariates = c("X", "y")
#'   )
#' )
#'
#' ## Addition of an optional strata.
#' h_coxreg_univ_formulas(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
#'     strata = "SITE"
#'   )
#' )
#'
#' ## Inclusion of the interaction term.
#' h_coxreg_univ_formulas(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
#'     strata = "SITE"
#'   ),
#'   interaction = TRUE
#' )
#'
h_coxreg_univ_formulas <- function(variables,
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

#' Controls for Cox regression
#'
#' Sets a list of parameters for Cox regression fit. Used internally,
#' see [h_coxreg()].
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

#' Tabulation of Univariate Cox Regressions
#'
#' Tabulate the result of a Cox regression model.
#'
#' @inheritParams argument_convention
#' @inheritParams cox_regression_inter
#' @param effect (`string`)\cr the treatment variable.
#' @param mod (`coxph`)\cr Cox regression model fitted by [survival::coxph()].
#' @importFrom car Anova
#' @export
#'
#' @examples
#'
#' library(survival)
#' dta_simple <-  data.frame(
#'   time = c(5, 5, 10, 10, 5, 5, 10, 10),
#'   status = c(0, 0, 1, 0, 0, 1, 1, 1),
#'   armcd  = factor(LETTERS[c(1, 1, 1, 1, 2, 2, 2, 2)], levels = c("A", "B"))
#' )
#' mod <- coxph(Surv(time, status) ~ armcd, data = dta_simple)
#' result <- h_coxreg_univ_extract(
#'   effect = "armcd", covar = "armcd", mod = mod, data = dta_simple
#' )
#'
h_coxreg_univ_extract <- function(effect,
                                  covar,
                                  mod,
                                  data,
                                  control = control_coxreg()) {
  assert_that(
    class(mod) == "coxph",
    is.string(covar),
    is.string(effect)
  )
  test_statistic <- c(wald = "Wald", likelihood = "LR")[control$pval_method]
  mod_aov <- car::Anova(
    mod,
    test.statistic = test_statistic,
    type = "III"
  )

  effect_aov <- mod_aov[effect, , drop = TRUE]
  pval <- effect_aov[[grep(pattern = "Pr", x = names(effect_aov)), drop = TRUE]]
  msum <- summary(mod, conf.int = control$conf_level)
  confint <- msum$conf.int
  confint <- confint[grep(effect, rownames(confint)), ]

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
    hr = unname(confint["exp(coef)"]),
    lcl = unname(confint[grep("lower", names(confint))]),
    ucl = unname(confint[grep("upper", names(confint))]),
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
#' dta_bladder$AGE <- sample(20:60, size = nrow(dta_bladder), replace = TRUE)
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


#' @describeIn cox_regression_inter estimate the interaction with a numerical
#'   covariate
#' @param at (`list`)\cr a list with items named after the covariate, every
#'   item is a vector of levels at which the interaction should be estimated.
#' @importFrom stats qnorm median
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
    x = attr(terms(mod), "term.labels")
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

#' @describeIn cox_regression_inter estimates the interaction with a factor
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
#'   no interaction, [h_coxreg_univ_extract()] is applied.
#' @importFrom car Anova
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
  if (!any(attr(terms(mod), "order") == 2)) {
    y <- h_coxreg_univ_extract(
      mod = mod, covar = covar, data = data, control = control, effect = effect
    )
    y$pval_inter <- NA
    y
  } else {
    test_statistic <- c(wald = "Wald", likelihood = "LR")[control$pval_method]

    # Test the main treatment effect.
    mcar <- car::Anova(mod, test.statistic = test_statistic, type = "III")
    pval <- mcar[effect, grep("Pr", colnames(mcar))]

    # Test the interaction effect.
    mcar <- car::Anova(mod, test.statistic = test_statistic, type = "III")
    pval_inter <- mcar[grep(":", rownames(mcar)), grep("Pr", colnames(mcar))]
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

#' Hazard ratio
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
#' @importFrom  stats qnorm
#' @export
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
#' dta_bladder$AGE <- sample(20:60, size = nrow(dta_bladder), replace = TRUE)
#'
#' mod <- coxph(Surv(time, status) ~ armcd * covar1, data = dta_bladder)
#'
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

#' @describeIn cox_regression tabulates the results of a series of univariate Cox
#'   regression models. `variables` corresponds to the names of variables found
#'   in `data`, passed as a named list and corresponding to
#'   `time`, `event`, `arm`, `covariates` and `strata` terms.
#' @param data (`data frame`)\cr the dataset containing the variables to fit the
#'   models.
#' @param at (`list` of `numeric`)\cr when the candidate covariate is a
#'  `numeric`, use `at` to specify the value of the covariate at which the
#'  effect should be estimated.
#' @param control (`list`)\cr a list of parameters as returned by the
#'   helper function [control_coxreg()].
#'
#' @importFrom survival coxph
#' @importFrom stats as.formula
#' @export
#' @examples
#'
#' # h_coxreg
#'
#' ## Cox regression: arm + 1 covariate.
#' h_coxreg(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = "covar1"
#'   ),
#'   data = dta_bladder,
#'   control = control_coxreg(conf_level = 0.91)
#' )
#'
#' ## Cox regression: arm + 1 covariate + interaction, 2 candidate covariates.
#' h_coxreg(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder,
#'   control = control_coxreg(conf_level = 0.91, interaction = TRUE)
#' )
#'
#' ## Cox regression: arm + 1 covariate, stratified analysis.
#' h_coxreg(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd", strata = "covar2",
#'     covariates = c("covar1")
#'   ),
#'   data = dta_bladder,
#'   control = control_coxreg(conf_level = 0.91)
#' )
#'
h_coxreg <- function(variables,
                     data,
                     at = list(),
                     control = control_coxreg()) {
  assert_that(
    is.character(variables$covariates),
    is_variables(variables[c("arm", "event", "time")]),
    is_df_with_variables(data, as.list(unlist(variables)))
  )
  vars <- c(variables$arm, variables$covariates)
  forms <- h_coxreg_univ_formulas(variables, interaction = control$interaction)
  mod <- lapply(
    forms, function(x) {
      survival::coxph(formula = stats::as.formula(x), data = data, ties = control$ties)
    }
  )
  result <- if (control$interaction) {
    Map(
      mod = mod, covar = vars,
      f = function(mod, covar) h_coxreg_extract_interaction(
        effect = variables$arm,
        covar = covar,
        mod = mod,
        data = data,
        at = at,
        control = control
      )
    )
  } else {
    Map(
      mod = mod, vars = vars,
      f = function(mod, vars) {
        h_coxreg_univ_extract(
          mod = mod, control = control, covar = vars, effect = variables$arm,
          data = data
        )
      }
    )
  }
  result <- do.call(rbind, result)

  empty_vector_if_na <- function(x) {
    if (all(is.na(x))) {
      numeric()
    } else {
      x
    }
  }
  result$ci <- Map(lcl = result$lcl, ucl = result$ucl, f = function(lcl, ucl) c(lcl, ucl))
  result$n <- lapply(result$n, empty_vector_if_na)
  result$ci <- lapply(result$ci, empty_vector_if_na)
  result$hr <- lapply(result$hr, empty_vector_if_na)
  result$pval <- lapply(result$pval, empty_vector_if_na)
  if (control$interaction) {
    result$pval_inter <- lapply(result$pval_inter, empty_vector_if_na)
  }
  attr(result, "conf_level") <- control$conf_level
  result
}

#' @describeIn cox_regression transforms the tabulated results from `h_coxreg`
#'  into a list. Not much calculation is done here, it rather prepares the data to
#'  be used by the layout creating function.
#' @importFrom stats setNames
#' @export
#'
#' @examples
#'
#' # s_coxreg
#'
#' df <- h_coxreg(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder
#' )
#' s_coxreg(df = df, .var = "hr")
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
#' @param vars (`character`)\cr the name of statistics to be reported among
#'  `n` (number of observation), `hr` (Hazard Ratio),
#'  `ci` (confidence interval), `pval` (p.value of the treatment effect) and
#'  `pval_inter` (the p.value of the interaction effect between the treatment
#'  and the covariate).
#' @export
#'
#' @examples
#'
#' # fit_coxreg
#'
#' df <- h_coxreg(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder,
#'   control = control_coxreg(interaction = FALSE)
#' )
#' result <- split_rows_by(lyt = NULL, "effect") %>%
#'   split_rows_by("term", child_labels = "hidden") %>%
#'   fit_coxreg(conf_level = .95) %>%
#'   build_table(df = df)
#' result
#'
fit_coxreg <- function(lyt,
                       conf_level,
                       vars = c("n", "hr", "ci", "pval")) {
  afun <- format_wrap_df(
    sfun = s_coxreg,
    formats = c(
      n = "xx", hr = "xx.xx", ci = "(xx.xx, xx.xx)",
      pval =  "xx.xxxx", pval_inter = "xx.xxxx"
    ),
    indent_mods = c(n = 0L, hr = 0L, ci = 0L, pval = 0L, pval_inter = 0L)
  )
  lyt <- split_cols_by_multivar(
    lyt = lyt,
    vars = vars,
    varlabels = c(
      n = "n", hr = "HR",
      ci = paste0(100 * conf_level, "% CI"),
      pval = "pval",
      pval_inter = "pval_inter"
    )[vars]
  )
  analyze_colvars(lyt = lyt, afun = afun)
}
