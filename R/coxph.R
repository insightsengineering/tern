#' Univariate Formula Special Term
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The special term `univariate` indicate that the model should be fitted individually for
#' every variable included in univariate.
#'
#' @param x A vector of variable name separated by commas.
#'
#' @return When used within a model formula, produces univariate models for each variable provided.
#'
#' @details
#' If provided alongside with pairwise specification, the model
#' `y ~ ARM + univariate(SEX, AGE, RACE)` lead to the study and comparison of the models
#' + `y ~ ARM`
#' + `y ~ ARM + SEX`
#' + `y ~ ARM + AGE`
#' + `y ~ ARM + RACE`
#'
#' @export
univariate <- function(x) {
  structure(x, varname = deparse(substitute(x)))
}

# Get the right-hand-term of a formula
rht <- function(x) {
  checkmate::assert_formula(x)
  y <- as.character(rev(x)[[1]])
  return(y)
}

#' Hazard Ratio Estimation in Interactions
#'
#' This function estimates the hazard ratios between arms when an interaction variable is given with
#' specific values.
#'
#' @param variable,given Names of two variable in interaction. We seek the estimation of the levels of `variable`
#'   given the levels of `given`.
#' @param lvl_var,lvl_given corresponding levels has given by `levels`.
#' @param mmat A name numeric filled with 0 used as template to obtain the design matrix.
#' @param coef Numeric of estimated coefficients.
#' @param vcov Variance-covariance matrix of underlying model.
#' @param conf_level Single numeric for the confidence level of estimate intervals.
#'
#' @details Given the cox regression investigating the effect of Arm (A, B, C; reference A)
#'   and Sex (F, M; reference Female). The model is abbreviated: y ~ Arm + Sex + Arm x Sex.
#'   The cox regression estimates the coefficients along with a variance-covariance matrix for:
#'
#'   - b1 (arm b), b2 (arm c)
#'   - b3 (sex m)
#'   - b4 (arm b: sex m), b5 (arm c: sex m)
#'
#'   Given that I want an estimation of the Hazard Ratio for arm C/sex M, the estimation
#'   will be given in reference to arm A/Sex M by exp(b2 + b3 + b5)/ exp(b3) = exp(b2 + b5),
#'   therefore the interaction coefficient is given by b2 + b5 while the standard error is obtained
#'   as $1.96 * sqrt(Var b2 + Var b5 + 2 * covariance (b2,b5))$ for a confidence level of 0.95.
#'
#' @return A list of matrix (one per level of variable) with rows corresponding to the combinations of
#'   `variable` and `given`, with columns:
#'   * `coef_hat`: Estimation of the coefficient.
#'   * `coef_se`: Standard error of the estimation.
#'   * `hr`: Hazard ratio.
#'   * `lcl, ucl`: Lower/upper confidence limit of the hazard ratio.
#'
#' @seealso [s_cox_multivariate()].
#'
#' @examples
#' library(dplyr)
#' library(survival)
#'
#' ADSL <- tern_ex_adsl %>%
#'   filter(SEX %in% c("F", "M"))
#'
#' adtte <- tern_ex_adtte %>% filter(PARAMCD == "PFS")
#' adtte$ARMCD <- droplevels(adtte$ARMCD)
#' adtte$SEX <- droplevels(adtte$SEX)
#'
#' mod <- coxph(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ (SEX + ARMCD)^2,
#'   data = adtte
#' )
#'
#' mmat <- stats::model.matrix(mod)[1, ]
#' mmat[!mmat == 0] <- 0
#'
#' @keywords internal
estimate_coef <- function(variable, given,
                          lvl_var, lvl_given,
                          coef,
                          mmat,
                          vcov,
                          conf_level = 0.95) {
  var_lvl <- paste0(variable, lvl_var[-1]) # [-1]: reference level
  giv_lvl <- paste0(given, lvl_given)

  design_mat <- expand.grid(variable = var_lvl, given = giv_lvl)
  design_mat <- design_mat[order(design_mat$variable, design_mat$given), ]
  design_mat <- within(
    data = design_mat,
    expr = {
      inter <- paste0(variable, ":", given)
      rev_inter <- paste0(given, ":", variable)
    }
  )

  split_by_variable <- design_mat$variable
  interaction_names <- paste(design_mat$variable, design_mat$given, sep = "/")

  design_mat <- apply(
    X = design_mat, MARGIN = 1, FUN = function(x) {
      mmat[names(mmat) %in% x[-which(names(x) == "given")]] <- 1
      return(mmat)
    }
  )
  colnames(design_mat) <- interaction_names

  betas <- as.matrix(coef)

  coef_hat <- t(design_mat) %*% betas
  dimnames(coef_hat)[2] <- "coef"

  coef_se <- apply(design_mat, 2, function(x) {
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

    return(x)
  })

  y <- t(y)
  y <- by(y, split_by_variable, identity)
  y <- lapply(y, as.matrix)

  attr(y, "details") <- paste0(
    "Estimations of ", variable,
    " hazard ratio given the level of ", given, " compared to ",
    variable, " level ", lvl_var[1], "."
  )
  return(y)
}

#' `tryCatch` around `car::Anova`
#'
#' Captures warnings when executing [car::Anova].
#'
#' @inheritParams car::Anova
#'
#' @return A list with item `aov` for the result of the model and `error_text` for the captured warnings.
#'
#' @examples
#' # `car::Anova` on cox regression model including strata and expected
#' # a likelihood ratio test triggers a warning as only `Wald` method is
#' # accepted.
#'
#' library(survival)
#'
#' mod <- coxph(
#'   formula = Surv(time = futime, event = fustat) ~ factor(rx) + strata(ecog.ps),
#'   data = ovarian
#' )
#'
#' @keywords internal
try_car_anova <- function(mod,
                          test.statistic) { # nolint
  y <- tryCatch(
    withCallingHandlers(
      expr = {
        warn_text <- c()
        list(
          aov = car::Anova(
            mod,
            test.statistic = test.statistic,
            type = "III"
          ),
          warn_text = warn_text
        )
      },
      warning = function(w) {
        # If a warning is detected it is handled as "w".
        warn_text <<- trimws(paste0("Warning in `try_car_anova`: ", w))

        # A warning is sometimes expected, then, we want to restart
        # the execution while ignoring the warning.
        invokeRestart("muffleWarning")
      }
    ),
    finally = {
    }
  )

  return(y)
}

#' Fit the Cox Regression Model and `Anova`
#'
#' The functions allows to derive from the [survival::coxph()] results the effect p.values using [car::Anova()].
#' This last package introduces more flexibility to get the effect p.values.
#'
#' @inheritParams t_coxreg
#'
#' @return A list with items `mod` (results of [survival::coxph()]), `msum` (result of `summary`) and
#'   `aov` (result of [car::Anova()]).
#'
#' @noRd
fit_n_aov <- function(formula,
                      data = data,
                      conf_level = conf_level,
                      pval_method = c("wald", "likelihood"),
                      ...) {
  pval_method <- match.arg(pval_method)

  environment(formula) <- environment()
  suppressWarnings({
    # We expect some warnings due to coxph which fails strict programming.
    mod <- survival::coxph(formula, data = data, ...)
    msum <- summary(mod, conf.int = conf_level)
  })

  aov <- try_car_anova(
    mod,
    test.statistic = switch(pval_method,
      "wald" = "Wald",
      "likelihood" = "LR"
    )
  )

  warn_attr <- aov$warn_text
  if (!is.null(aov$warn_text)) message(warn_attr)

  aov <- aov$aov
  y <- list(mod = mod, msum = msum, aov = aov)
  attr(y, "message") <- warn_attr

  return(y)
}

# argument_checks
check_formula <- function(formula) {
  if (!(inherits(formula, "formula"))) {
    stop("Check `formula`. A formula should resemble `Surv(time = AVAL, event = 1 - CNSR) ~ study_arm(ARMCD)`.")
  }

  invisible()
}

check_covariate_formulas <- function(covariates) {
  if (!all(vapply(X = covariates, FUN = inherits, what = "formula", FUN.VALUE = TRUE)) || is.null(covariates)) {
    stop("Check `covariates`, it should be a list of right-hand-term formulas, e.g. list(Age = ~AGE).")
  }

  invisible()
}

name_covariate_names <- function(covariates) {
  miss_names <- names(covariates) == ""
  no_names <- is.null(names(covariates))
  if (any(miss_names)) names(covariates)[miss_names] <- vapply(covariates[miss_names], FUN = rht, FUN.VALUE = "name")
  if (no_names) names(covariates) <- vapply(covariates, FUN = rht, FUN.VALUE = "name")
  return(covariates)
}

check_increments <- function(increments, covariates) {
  if (!is.null(increments)) {
    covariates <- vapply(covariates, FUN = rht, FUN.VALUE = "name")
    lapply(
      X = names(increments), FUN = function(x) {
        if (!x %in% covariates) {
          warning(
            paste(
              "Check `increments`, the `increment` for ", x,
              "doesn't match any names in investigated covariate(s)."
            )
          )
        }
      }
    )
  }

  invisible()
}

#' Multivariate Cox Model - Summarized Results
#'
#' Analyses based on multivariate Cox model are usually not performed for the Controlled Substance Reporting or
#' regulatory documents but serve exploratory purposes only (e.g., for publication). In practice, the model usually
#' includes only the main effects (without interaction terms). It produces the hazard ratio estimates for each of the
#' covariates included in the model.
#' The analysis follows the same principles (e.g., stratified vs. unstratified analysis and tie handling) as the
#' usual Cox model analysis. Since there is usually no pre-specified hypothesis testing for such analysis,
#' the p.values need to be interpreted with caution. (**Statistical Analysis of Clinical Trials Data with R**,
#' `NEST's bookdown`)
#'
#' @param formula (`formula`)\cr A formula corresponding to the investigated [survival::Surv()] survival model
#'   including covariates.
#' @param data (`data.frame`)\cr A data frame which includes the variable in formula and covariates.
#' @param conf_level (`proportion`)\cr The confidence level for the hazard ratio interval estimations. Default is 0.95.
#' @param pval_method (`character`)\cr The method used for the estimation of p-values, should be one of
#'   `"wald"` (default) or `"likelihood"`.
#' @param ... Optional parameters passed to [survival::coxph()]. Can include `ties`, a character string specifying the
#'   method for tie handling, one of `exact` (default), `efron`, `breslow`.
#'
#' @return A `list` with elements `mod`, `msum`, `aov`, and `coef_inter`.
#'
#' @details The output is limited to single effect terms. Work in ongoing for estimation of interaction terms
#'   but is out of scope as defined by the  Global Data Standards Repository
#'   (**`GDS_Standard_TLG_Specs_Tables_2.doc`**).
#'
#' @seealso [estimate_coef()].
#'
#' @examples
#' library(dplyr)
#'
#' adtte <- tern_ex_adtte
#' adtte_f <- subset(adtte, PARAMCD == "OS") # _f: filtered
#' adtte_f <- filter(
#'   adtte_f,
#'   PARAMCD == "OS" &
#'     SEX %in% c("F", "M") &
#'     RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
#' )
#' adtte_f$SEX <- droplevels(adtte_f$SEX)
#' adtte_f$RACE <- droplevels(adtte_f$RACE)
#'
#' @keywords internal
s_cox_multivariate <- function(formula, data,
                               conf_level = 0.95,
                               pval_method = c("wald", "likelihood"),
                               ...) {
  tf <- stats::terms(formula, specials = c("strata"))
  covariates <- rownames(attr(tf, "factors"))[-c(1, unlist(attr(tf, "specials")))]
  lapply(
    X = covariates,
    FUN = function(x) {
      if (is.character(data[[x]])) {
        data[[x]] <<- as.factor(data[[x]])
      }
      invisible()
    }
  )
  pval_method <- match.arg(pval_method)

  # Results directly exported from environment(fit_n_aov) to environment(s_function_draft)
  y <- fit_n_aov(
    formula = formula,
    data = data,
    conf_level = conf_level,
    pval_method = pval_method,
    ...
  )
  mod <- y$mod
  aov <- y$aov
  msum <- y$msum
  list2env(as.list(y), environment())

  all_term_labs <- attr(mod$terms, "term.labels")
  term_labs <- all_term_labs[which(attr(mod$terms, "order") == 1)]
  names(term_labs) <- term_labs

  coef_inter <- NULL
  if (any(attr(mod$terms, "order") > 1)) {
    for_inter <- all_term_labs[attr(mod$terms, "order") > 1]
    names(for_inter) <- for_inter
    mmat <- stats::model.matrix(mod)[1, ]
    mmat[!mmat == 0] <- 0
    mcoef <- stats::coef(mod)
    mvcov <- stats::vcov(mod)

    estimate_coef_local <- function(variable, given) {
      estimate_coef(
        variable, given,
        coef = mcoef, mmat = mmat, vcov = mvcov, conf_level = conf_level,
        lvl_var = levels(data[[variable]]), lvl_given = levels(data[[given]])
      )
    }

    coef_inter <- lapply(
      for_inter, function(x) {
        y <- attr(mod$terms, "factor")[, x]
        y <- names(y[y > 0])
        Map(estimate_coef_local, variable = y, given = rev(y))
      }
    )
  }

  list(mod = mod, msum = msum, aov = aov, coef_inter = coef_inter)
}
