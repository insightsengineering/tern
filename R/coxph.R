#' Pairwise formula special term
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The special term `pairwise` indicate that the model should be fitted individually for
#' every tested level in comparison to the reference level.
#'
#' @param x the variable for which pairwise result is expected
#'
#' @details Let's `ARM` being a factor with level A, B, C; let's be B the reference level,
#'  a model calling the formula including `pairwise(ARM)` will result in two models
#'  * a model including only levels A and B, and effect of A estimated in reference to B.
#'  * a model including only levels C and B, the effect of C estimated in reference to B.
#'
#' @export
#'
#' @md
pairwise <- function(x) {
  structure(x, varname = deparse(substitute(x)))
}

#' Univariate formula special term
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The special term `univariate` indicate that the model should be fitted individually for
#' every variable included in univariate.
#'
#' @param x A vector of variable name separated by comas.
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
#'
#' @md
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
#' @param variable,given Names of two variable in interaction. We seek the estimation of the levels of \code{variable}
#'   given the levels of \code{given}
#' @param lvl_var,lvl_given corresponding levels has given by \code{levels}.
#' @param mmat A name numeric filled with 0 used as template to obtain the design matrix.
#' @param coef Numeric of estimated coefficients.
#' @param vcov Variance-covariance matrix of underlying model.
#' @param conf_level Single numeric for the confidence level of estimate intervals.
#'
#' @details Given the cox regression investigating the effect of Arm (A, B, C; reference A)
#'   and Sex (F, M; reference Female). The model is abbreviated: y ~ Arm + Sex + Arm x Sex.
#'   The cox regression estimates the coefficients along with a variance-covariance matrix for:
#'
#'   - b1 (arm b), b2 (arm c),
#'   - b3 (sex m),
#'   - b4 (arm b: sex m), b5 (arm c: sex m)
#'
#'   Given that I want an estimation of the Hazard Ratio for arm C/sex M, the estimation
#'   will be given in reference to arm A/Sex M by exp(b2 + b3 + b5)/ exp(b3) = exp(b2 + b5),
#'   therefore the interaction coefficient is given by b2 + b5 while the standard error is obtained
#'   as $1.96 * sqrt(Var b2 + Var b5 + 2 * covariance (b2,b5))$ for a confidence level of 0.95.
#'
#' @return A list of matrix (one per level of variable) with rows corresponding to the combinations of
#' \code{variable} and \code{given}, with columns:
#' \describe{
#'   \item{coef_hat}{Estimation of the coefficient}
#'   \item{coef_se}{Standard error of the estimation.}
#'   \item{hr}{Hazard ratio.}
#'   \item{lcl,ucl}{lower/upper confidence limit of the hazard ratio}
#' }
#'
#' @examples
#' library(dplyr)
#' library(scda)
#' library(survival)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADSL <- ADSL %>%
#'   filter(SEX %in% c("F", "M"))
#'
#' ADTTE <- synthetic_cdisc_data("latest")$adtte %>%
#'   filter(PARAMCD == "PFS")
#' ADTTE$ARMCD <- droplevels(ADTTE$ARMCD)
#' ADTTE$SEX <- droplevels(ADTTE$SEX)
#'
#' mod <- coxph(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ (SEX + ARMCD)^2,
#'   data = ADTTE
#' )
#'
#' mmat <- stats::model.matrix(mod)[1, ]
#' mmat[!mmat == 0] <- 0
#'
#' # Internal function - estimate_coef
#' \dontrun{
#' estimate_coef(
#'   variable = "ARMCD", given = "SEX", lvl_var = "ARM A", lvl_given = "M",
#'   coef = stats::coef(mod), mmat = mmat, vcov = stats::vcov(mod), conf_level = .95
#' )
#' }
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
      inter <- paste0(variable, ":", given) # nolint
      rev_inter <- paste0(given, ":", variable) # nolint
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
#' @return A list with item `aov` for the result of the model and
#'   `error_text` for the captured warnings.
#'
#' @examples
#' # `car::Anova` on cox regression model including strata and expected
#' # a likelihood ratio test triggers a warning as only Wald method is
#' # accepted.
#'
#' library(survival)
#'
#' mod <- coxph(
#'   formula = Surv(time = futime, event = fustat) ~ factor(rx) + strata(ecog.ps),
#'   data = ovarian
#' )
#'
#' # Internal function - try_car_anova
#' \dontrun{
#' with_wald <- try_car_anova(mod = mod, test.statistic = "Wald")
#' with_lr <- try_car_anova(mod = mod, test.statistic = "LR")
#' }
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

#' Fit the Cox regression model and Anova
#'
#' The functions allows to derive from the [survival::coxph()] results
#' the effect p.values using [car::Anova()]. This last package introduces
#' more flexibility to get the effect p.values.
#'
#' @inheritParams t_coxreg
#' @noRd
#' @md
#'
#' @return A list with items `mod` (results of [survival::coxph()]),
#'   `msum` (result of `summary`) and `aov` (result of [car::Anova]).
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
  if (!all(vapply(X = covariates, FUN = inherits, what = "formula", FUN.VALUE = TRUE)) | is.null(covariates)) {
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


#' Multivariate Cox Model - summarized results
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
#' @param formula A \code{formula} corresponding to the investigated \code{\link[survival:Surv]{survival model}}
#'     including covariates.
#' @param data A \code{data.frame} which includes the variable in formula and covariates.
#' @param conf_level The level of confidence for the hazard ration interval estimations. Default is 0.95.
#' @param pval_method The method used for the estimation of p.values, should be one of \code{"wald"} (default) or
#'   \code{"likelihood"}.
#' @param ... Optional parameters passed to \code{\link[survival:coxph]{coxph()}}
#' + `ties` a character string specifying the method for tie handling, one of `exact` (default), `efron`, `breslow`.
#'
#' @details The output is limited to single effect terms. Work in ongoing for estimation of interaction terms
#'     but is out of scope as defined by the  Global Data Standards Repository
#'     (**`GDS_Standard_TLG_Specs_Tables_2.doc`**).
#'
#' @examples
#' library(scda)
#' library(dplyr)
#'
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS") # _f: filtered
#' ADTTE_f <- filter(
#'   ADTTE_f,
#'   PARAMCD == "OS" &
#'     SEX %in% c("F", "M") &
#'     RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
#' )
#' ADTTE_f$SEX <- droplevels(ADTTE_f$SEX)
#' ADTTE_f$RACE <- droplevels(ADTTE_f$RACE)
#'
#' # Internal function - s_cox_multivariate
#' \dontrun{
#' s_cox_multivariate(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ (ARMCD + RACE + AGE)^2, data = ADTTE_f
#' )
#' }
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
    conf_level  = conf_level,
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
