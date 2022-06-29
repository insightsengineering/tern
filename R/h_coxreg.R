#' Helper Functions for Cox Proportional Hazards Regression
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper functions used in [fit_coxreg_univar()] and [fit_coxreg_multivar()].
#'
#' @inheritParams argument_convention
#'
#' @name h_coxreg
NULL

#' @describeIn h_coxreg Helper for Cox Regression Formula
#'
#' Creates a list of formulas. It is used internally by [fit_coxreg_univar()]
#' for the comparison of univariate Cox regression models.
#'
#' @inheritParams argument_convention
#' @inheritParams control_coxreg
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

#' @describeIn h_coxreg Helper for Multi-variable Cox Regression Formula
#'
#' @description Creates a formulas string. It is used internally by [fit_coxreg_multivar()]
#' for the comparison of multi-variable Cox regression models. Interactions will not
#' be included in multi-variable Cox regression model.
#'
#' @inheritParams argument_convention
#' @return
#' The function `h_coxreg_univar_formulas` returns a `character` vector coercible
#' into formulas (e.g [stats::as.formula()]).
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
#'
#' @export
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

#' @describeIn h_coxreg Utility function to help tabulate the result of
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

#' @describeIn h_coxreg Tabulation of Multi-variable Cox Regressions
#'
#' Utility function to help tabulate the result of a multi-variable Cox regression model
#' for a treatment/covariate variable.
#'
#' @inheritParams argument_convention
#' @inheritParams h_coxreg_univar_extract
#'
#' @examples
#' library(survival)
#'
#' mod <- coxph(Surv(time, status) ~ armcd + var1, data = dta_simple)
#' result <- h_coxreg_multivar_extract(
#'   var = "var1", mod = mod, data = dta_simple
#' )
#' result
#'
#' @export
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
