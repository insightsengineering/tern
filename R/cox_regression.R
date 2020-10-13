#' Cox Proportional Hazards Regression
#'
#' Fits a Cox regression model and estimate hazard ratio to describe the effect
#' size in a survival analysis.
#'
#' @details
#' Cox models are the most commonly used methods to estimate the magnitude of
#' the effect in survival analysis. It assumes proportional hazards: the ratio
#' of the hazards of between groups (e.g., two arms) is constant over time.
#' This ratio is referred to as the "hazard ratio" (HR) and is one of the
#' most commonly reported metrics to describe the effect size in survival
#' analysis \insertCite{NESTTeam2020}{tern}.
#' @template formatting_arguments
#'
#' @inheritParams argument_convention
#' @references
#' + \insertAllCited{}
#' + \insertRef{Hughes2020a}{tern}
#'
#' @name cox_regression
#' @order 1
#'
NULL


#' Helper for Cox Regression Formula
#'
#' Creates a list of formulas. It is used internally in [h_coxreg()]
#' for the comparison of univariate Cox regression models.
#'
#' @inheritParams argument_convention
#' @inheritParams control_coxreg
#'
#' @return It actually returns a vector of
#'   character strings, coercible into formulas with
#'   [as.formula()].
#'
#' @export
#' @examples
#'
#' h_coxreg_univ_formulas(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd", covariates = c("X", "y")
#'   )
#' )
#' h_coxreg_univ_formulas(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
#'     strata = "SITE"
#'   )
#' )
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
  setNames(c(ref, covar), c("ref", variables$covariates))
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
#' @return A `list` of item corresponding to the provided arguments.
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
#' @inheritParams h_coxreg
#' @param mod model
#' @param effect (`string`)\cr the treatment variable.
#' @export
#'
#' @examples
#'
#' # Example, see `?survival::bladder`.
#' library(survival)
#' dta_test <- with(
#'   data = bladder[bladder$enum < 5, ],
#'   data.frame(
#'     time = stop,
#'     status = event,
#'     armcd = as.factor(rx),
#'     covar1 = as.factor(enum)
#'   )
#' )
#' mod <- coxph(Surv(time, status) ~ armcd + covar1, data = dta_test)
#' h_coxreg_univ_extract(mod, var = "covar1", data = dta_test)
#'
#' # Example, see `?survival::aml`.
#' plot(
#'   survfit(Surv(time, status) ~ x, data = aml),
#'   lty=2:3, xlab="Months"
#' )
#' mod <- coxph(Surv(time, status) ~ x, data = aml)
#' h_coxreg_univ_extract(mod, effect = "x", var = "x", data = aml)
#'
h_coxreg_univ_extract <- function(mod,
                                  var,
                                  data,
                                  effect = "armcd",
                                  control = control_coxreg()) {
  assert_that(
    class(mod) == "coxph",
    is.string(var),
    is.string(effect)
  )
  test_statistic <- c(wald = "Wald", likelihood = "LR")[control$pval_method]
  mod_aov <- car::Anova(
    mod,
    test.statistic = test_statistic,
    type = "III"
  )
  # Because the number of columns and rows depends on the number of effect
  # and pval method.
  effect_aov <- mod_aov[effect, , drop = TRUE]
  pval <- effect_aov[[grep(pattern = "Pr", x = names(effect_aov)), drop = TRUE]]
  msum <- summary(mod, conf.int = control$conf_level)
  confint <- msum$conf.int
  confint <- confint[grep(effect, rownames(confint)), ]
  data.frame(
    term = var,
    term_label = unname(labels_or_names(data[var])),
    n = mod[["n"]],
    hr = unname(confint["exp(coef)"]),
    lcl = unname(confint[grep("lower", names(confint))]),
    ucl = unname(confint[grep("upper", names(confint))]),
    pval = pval
  )
}

#' @describeIn cox_regression tabulates the result of a series of univariate Cox
#'   regression models. `variables` corresponds to the names of variables found
#'   in `data` includes, passed as a named list and corresponding to
#'   `time`, `event`, `arm`, `covariates` and `strata` terms.
#' @param data (`data frame`)\cr the dataset containing the variables to fit the
#'   models.
#' @param control (`list`)\cr a list of parameters as returned by the
#'   helper function [control_coxreg()].
#'
#' @export
#' @examples
#'
#' set.seed(1, kind = "Mersenne-Twister")
#' bladder <- survival::bladder
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
#' attr(dta_bladder$covar1, "label") <- "A Covar"
#' attr(dta_bladder$covar2, "label") <- "Sex (F/M)"
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
h_coxreg <- function(variables,
                     data,
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
      coxph(formula = as.formula(x), data = data, ties = control$ties)
    }
  )
  result <- Map(
    mod = mod, vars = vars,
    f = function(mod, vars) {
      h_coxreg_univ_extract(
        mod = mod, control = control, var = vars, effect = variables$arm,
        data = data
      )
    }
  )

  result <- do.call(rbind, result)
  result <- cbind(
    data.frame(
      mod = c("Treatment", rep("Covariate", length(variables$covariates))),
      stringsAsFactors = FALSE
    ),
    result
  )

  result$ci <- Map(lcl = result$lcl, ucl = result$ucl, f = function(lcl, ucl) c(lcl, ucl))
  attr(result, "conf_level") <- control$conf_level
  result
}

#' @describeIn cox_regression transforms the tabulated results from `h_coxreg`
#'  into a list. Not much calculation is done here, it rather prepares the data to
#'  be used by the layout creating function.
#' @export
#'
#' @examples
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
  y <- setNames(y, nm = rep(.var, length(y)))
  lapply(
    X = y,
    FUN = function(x) {
      z <- as.list(x[[.var]])
      setNames(z, nm = x$term_label)
    }
  )
}

#' @describeIn cox_regression layout creating function.
#' @export
#'
#' @examples
#'
#' df <- h_coxreg(
#'   variables = list(
#'     time = "time", event = "status", arm = "armcd",
#'     covariates = c("covar1", "covar2")
#'   ),
#'   data = dta_bladder
#' )
#' result <- split_rows_by(lyt = NULL, "mod", child_labels = "visible") %>%
#'   fit_coxreg(conf_level = .95) %>%
#'   build_table(df = df)
#' result
#'
fit_coxreg <- function(lyt, conf_level) {
  afun <- format_wrap_df(
    sfun = s_coxreg,
    formats = c(n = "xx", hr = "xx.xx", ci = "(xx.xx, xx.xx)", pval = "xx.xxxx"),
    indent_mods = c(n = 0L, hr = 0L, ci = 0L, pval = 0L)
  )
  split_cols_by_multivar(
    lyt = lyt, c("n", "hr", "ci", "pval"),
    varlabels = c("n", "HR", paste0(100 * conf_level, "% CI"), "pval")
  ) %>%
    analyze_colvars(afun = afun)
}
