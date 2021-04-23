#' Pairwise formula special term
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

#' Cox regression including a single covariate - summarized results
#'
#' Fit cox (proportional hazard) regression models including the treatment and a single covariate.
#' Starting from a univariate model (e.g. survival model including an two-level arm predictor), a list of candidate
#' alternative models including an additional covariate (optionally including the interaction terms) is tested.
#'
#' @param formula (`formula`) \cr
#'   Specifies \code{\link[survival:Surv]{survival model}}.
#'   The arm variable needs to be wrapped in \code{\link{arm}}. The
#'   \code{\link[survival]{strata}} special will only be used for the stratified analysis. If there is not
#'   \code{\link[survival]{strata}} specification then the stratified analysis is omitted.
#' @param covariates a list of single right-hand-term formulas, if named, named will be used in the output
#' @param data A \code{data.frame} which includes the variable in formula and covariate
#' @param interactions The interaction term should be included, default is \code{FALSE}.
#' @param conf_level The level of confidence for the hazard ration interval estimations. Default is 0.95.
#' @param pval_method The method used for estimation of p.values, should be one of \code{"wald"} (default) or
#'   \code{"likelihood"}.
#' @param increments If a quantitative variable is included, it is possible to provide the expected level of estimation
#'   for the interaction. Should then be list, item are vector specifying levels, the item are named after the
#'   covariate name as it appears in covariate formula.
#' @param ... parameters passed down to \code{\link[survival:coxph]{coxph()}}
#' + `ties` a character string specifying the method for tie handling, one of `exact` (default), `efron`, `breslow`.
#'
#' @details The estimation of the coefficient and confidence interval follows four methods depending on the
#' inclusion of the interaction terms and specified level of a quantitative variable and follows. Four case
#' are therefore discriminated: no interaction with the covariate (i), interaction with a qualitative variable (ii),
#' interaction with a quantitative variable without (iii) or with a specified level (iv) for the estimation.
#'
#' @return A list with items:
#' \describe{
#'   \item{n}{the number of observations used for cox regression fit.}
#'   \item{hr}{hazard ratios of the arm.}
#'   \item{ci}{confidence interval of the estimated arm hazard ratio.}
#'   \item{pval}{p.value of the arm depending on the covariate included in the model.}
#'   \item{lrt}{p.value of the likelihood ratio test testing the interaction with the covariate.}
#'   \item{covariates}{the names of the covariate as provided or derived from variable name.}
#'   \item{tstr}{the strata term if included in the model.}
#' }
#'
#' @section Warning:
#' Note that `s_cox_univariate()` function is deprecated and will be removed in the coming releases.
#' Please use the function `fit_coxreg_univar()` instead.
#'
#' @importFrom stats anova coef median model.matrix qnorm setNames terms update vcov
#' @import survival
#'
#' @export
#'
#' @md
#'
#' @examples
#' library(random.cdisc.data)
#' library(survival)
#'
#' ADTTE <- radtte(cached = TRUE)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS") # _f: filtered
#' ADTTE_f <- within( # nolint
#'   data = subset(
#'     ADTTE_f,
#'     PARAMCD == "OS"
#'     & ARMCD %in% c("ARM A", "ARM B")
#'     & SEX %in% c("F", "M")
#'     & RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
#'   ),
#'   expr = { # nolint start
#'     set.seed(1)
#'     ARMCD <- droplevels(ARMCD)
#'     ARMCD <- relevel(ARMCD, "ARM B")
#'     SEX <- droplevels(SEX)
#'     RACE <- droplevels(RACE)
#'     X <- rnorm(n = length(ARM))
#'   } # nolint end
#' )
#'
#' s_cox_univariate(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD),
#'   data = ADTTE_f,
#'   covariates = list(~SEX)
#' )
#' \dontrun{
#' s_cox_univariate(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD),
#'   data = ADTTE_f,
#'   covariates = list("Race" = ~RACE, ~AGE, "a rand. quant var with increments" = ~X),
#'   interactions = TRUE,
#'   increments = list(X = c(-1, 1)),
#'   conf_level = 0.95,
#'   pval_method = c("wald", "log-rank", "likelihood")[1]
#' )
#' }
s_cox_univariate <- function(formula,
                             data,
                             covariates,
                             interactions = FALSE,
                             conf_level = 0.95,
                             pval_method = c("wald", "likelihood"),
                             increments = NULL,
                             ...) {

  ## Add deprecate warning
  .Deprecated(new = "fit_coxreg_univar")

  ## Argument checks
  check_formula(formula)
  if (is.null(covariates)) stop("Check `covariates`, provide a list of candidate covariates.")

  ## Character covariate are factors
  lapply(
    X = covariates,
    FUN = function(x) {
      if (is.character(data[[rht(x)]])) {
        data[[rht(x)]] <<- as.factor(data[[rht(x)]])
      }
      invisible()
    }
  )

  check_covariate_formulas(covariates)
  assert_that(is_proportion(conf_level, include_boundaries = TRUE))
  pval_method <- match.arg(pval_method)

  covariates <- name_covariate_names(covariates)
  check_increments(increments, covariates)

  # Formula univariate survival model, terms (t) and term index (i) for conveniency
  tf <- terms(formula, specials = c("arm", "strata"))
  iarm <- attr(tf, "specials")$arm
  tarm <- rownames(attr(tf, "factors"))[iarm]
  istr <- attr(tf, "specials")$strata
  tstr <- rownames(attr(tf, "factors"))[istr]

  if (is.null(iarm)) stop("Check `formula`, the arm variable needs to be wrapped in arm()")

  arms <- levels(with(data, eval(parse(text = tarm))))
  if (length(arms) != 2) stop("Check `formula`, the arm variable needs 2 levels.")

  ## List all models to be fitted ---
  formulas <- c(
    ref_mod = formula,
    lapply(covariates, function(x) update(formula, paste(" ~ . +", rht(x))))
  )

  if (interactions) {
    f_cov_x <- lapply(covariates, function(x) update(formula, paste(" ~ . +", rht(x), " + ", tarm, ":", rht(x))))
    names(f_cov_x) <- paste0(tarm, "|(", tarm, " * ", names(f_cov_x), ")")
    formulas <- c(formulas, f_cov_x)
  }

  # Fit the cox regression and return the model, summary and anova results
  fit <- lapply(formulas, fit_n_aov, data = data, conf_level = conf_level, pval_method = pval_method, ...)

  # Coef and SE estimations varies depending on:
  #    - interactions TRUE:
  #        + the covariate is a numeric TRUE:
  #            * there is a level for the interaction.  [^1]
  #            * there is no level for the interaction. [^2]
  #        + the covariate is a numeric FALSE.          [^3]
  #    - interactions FALSE.                            [^4]

  coef <- list()
  coef$ref_mod <- matrix(
    fit$ref_mod$msum$coefficients[paste0(tarm, arms[-1]), c("coef", "se(coef)")],
    ncol = 2,
    dimnames = list(NULL, c("coef", "se(coef)"))
  )
  rownames(coef$ref_mod) <- "ref_mod"


  if (interactions) {
    is_covar_num <- lapply(covariates, function(x) is.numeric(data[[rht(x)]]))
    coef_cov <- Map(
      # work on model fits with interaction
      fit = fit[- (1:(length(covariates) + 1))], is_covar_num = is_covar_num, covariates = covariates,
      f = function(fit, is_covar_num, covariates) {
        if (is_covar_num & rht(covariates) %in% names(increments)) {
          # [^1]: If covar is a numeric and increments are specified,
          # SE and COEF must be estimated for every level

          coef_narm <- paste0(tarm, levels(with(data, eval(parse(text = tarm))))[2])
          coef_ninter <- paste0(coef_narm, ":", rht(covariates))

          betas <- coef(fit$mod)[c(coef_narm, coef_ninter)]
          var_betas <- diag(vcov(fit$mod))[c(coef_narm, coef_ninter)]
          cov_betas <- vcov(fit$mod)[coef_narm, coef_ninter]

          xvals <- increments[[rht(covariates)]]
          names(xvals) <- xvals

          coef_hat <- betas[coef_narm] + xvals * betas[coef_ninter]
          coef_se <- sqrt(var_betas[coef_narm] + xvals^2 * var_betas[coef_ninter] + 2 * xvals * cov_betas)
          y <- cbind(coef_hat, coef_se)
          colnames(y) <- c("coef", "se(coef)")
          rownames(y) <- names(xvals)
        } else if (is_covar_num) {
          # [^2]: if the covariate is a numeric without specified increments
          y <- matrix(
            fit$msum$coefficients[paste0(tarm, arms[-1], ":", rht(covariates)), c("coef", "se(coef)")],
            ncol = 2,
            dimnames = list(median(data[[rht(covariates)]]), c("coef", "se(coef)"))
          )
        } else {
          # [^3]: If not a numeric: build contrasts

          mmat <- model.matrix(fit$mod)[1, ]
          mmat[!mmat == 0] <- 0
          y <- estimate_coef(
            variable = tarm, given = rht(covariates),
            coef = coef(fit$mod), mmat = mmat, vcov = vcov(fit$mod),
            lvl_var = levels(data[[gsub(".*\\((.*)\\).*", "\\1", tarm)]]),
            lvl_given = levels(data[[rht(covariates)]]),
            conf_level = 0.95
          )
          y <- y[[1]]
          rownames(y) <- levels(data[[rht(covariates)]])
        }
        return(y)
      }
    )
  } else if (!interactions) {
    # [^4]: no interactions
    coef_cov <- lapply(
      X = fit[-1], FUN = function(x) {
        coef <- matrix(
          x$msum$coefficients[paste0(tarm, arms[-1]), c("coef", "se(coef)")],
          ncol = 2,
          dimnames = list(NULL, c("coef", "se(coef)"))
        )
      }
    )

    coef_cov <- Map(
      f = function(x, name) {
        rownames(x) <- name
        return(x)
      },
      x = coef_cov,
      names(covariates)
    )
  }

  coef <- c(coef, coef_cov)

  ## Select/extract results
  # Extract number of observations used for each covariate
  n <- vapply(X = fit[1:(1 + length(covariates))], FUN = function(x) x$msum$n, FUN.VALUE = 1)

  # Hazard Ratio for treatment
  hr <- lapply(coef, function(x) exp(x[, "coef"]))

  # Confidence interval of Hazard ratio for treatment
  ci <- lapply(
    coef, function(x) {
      q_norm <- qnorm((1 + conf_level) / 2)
      y <- t(apply(x, 1, function(y) exp(y["coef"] + c(-1, +1) * q_norm * y["se(coef)"])))
      return(y)
    }
  )
  attr(ci, "conf_level") <- conf_level

  # Extract arm local p.value
  pval <- c(
    ref_mod = with(
      fit$ref_mod,
      unname(switch(pval_method, "wald" = msum$waldtest["pvalue"], "likelihood" = msum$logtest["pvalue"]))
    ),
    lapply(X = fit[-1], FUN = function(x) x$aov[tarm, "Pr(>Chisq)"])
  )

  # Likelihood ratio test for the interaction
  if (interactions) {
    lrt <- Map(
      f = function(
                   without_interaction,
                   with_interaction) {
        anova(without_interaction$mod, with_interaction$mod)[2, "P(>|Chi|)"]
      },
      without_interaction = fit[2:(length(covariates) + 1)],
      with_interaction = fit[- (1:(length(covariates) + 1))]
    )
    names(lrt) <- names(fit[- (1:(length(covariates) + 1))])
  } else if (!interactions) {
    lrt <- NULL
  }

  y <- list(
    n = n,
    hr = hr,
    ci = ci,
    pval = pval,
    lrt = lrt,
    covariates = names(covariates),
    tstr = tstr,
    pval_method = pval_method,
    treatment = setNames(arms, c("ref", "tested")),
    method = fit$ref_mod$mod$method
  )

  return(y)
}


# Get the right-hand-term of a formula
rht <- function(x) {
  stopifnot(is(x, "formula"))
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
#' @importFrom stats qnorm
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#' library(survival)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADSL <- ADSL %>%
#'   filter(SEX %in% c("F", "M"))
#' \dontrun{
#' ADTTE <- radtte(ADSL, seed = 2) %>%
#'   filter(PARAMCD == "PFS")
#' ADTTE$ARMCD <- droplevels(ADTTE$ARMCD)
#' ADTTE$SEX <- droplevels(ADTTE$SEX)
#'
#' mod <- coxph(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ (SEX + ARMCD)^2,
#'   data = ADTTE
#' )
#'
#' mmat <- model.matrix(mod)[1, ]
#' mmat[!mmat == 0] <- 0
#'
#' estimate_coef(
#'   variable = "ARMCD", given = "SEX",
#'   coef = coef(mod), mmat = mmat, vcov = vcov(mod), data = ADTTE, conf_level = .95
#' )
#' }
#'
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

  q_norm <- qnorm((1 + conf_level) / 2)
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
#' @importFrom car Anova
#'
#' @md
#' @return A list with item `aov` for the result of the model and
#'   `error_text` for the captured warnings.
#'
#' @examples
#' # `car::Anova` on cox regression model including strata and expected
#' # a likelihood ratio test triggers a warning as only Wald method is
#' # accepted.
#' \dontrun{
#' library(survival)
#' mod <- coxph(
#'   formula = Surv(time = futime, event = fustat) ~ factor(rx) + strata(ecog.ps),
#'   data = ovarian
#' )
#'
#' with_wald <- tern:::try_car_anova(mod = mod, test.statistic = "Wald")
#' with_lr <- tern:::try_car_anova(mod = mod, test.statistic = "LR")
#' }
try_car_anova <- function(mod,
                          test.statistic) { # nolint

  y <- tryCatch(
    withCallingHandlers(
      expr = {
        warn_text <- c()
        list(
          aov = Anova(
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
#' @importFrom survival coxph
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
    mod <- coxph(formula, data = data, ...)
    msum <- summary(mod, conf.int = conf_level)
  })

  aov <- try_car_anova(
    mod,
    test.statistic = switch(pval_method, "wald" = "Wald", "likelihood" = "LR")
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
    stop("Check `formula`. A formula should resemble `Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD)`.")
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
#' @md
#'
#' @export
#'
#' @importFrom stats model.matrix coef terms vcov
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADTTE <- radtte(cached = TRUE)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS") # _f: filtered
#' ADTTE_f <- filter(
#'   ADTTE_f,
#'   PARAMCD == "OS"
#'   & SEX %in% c("F", "M")
#'   & RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
#' )
#' ADTTE_f$SEX <- droplevels(ADTTE_f$SEX)
#' ADTTE_f$RACE <- droplevels(ADTTE_f$RACE)
#' \dontrun{
#' s_cox_multivariate(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ (ARMCD + RACE + AGE)^2, data = ADTTE_f
#' )
#' }
s_cox_multivariate <- function(formula, data,
                               conf_level = 0.95,
                               pval_method = c("wald", "likelihood"),
                               ...) {
  tf <- terms(formula, specials = c("strata"))
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
    mmat <- model.matrix(mod)[1, ]
    mmat[!mmat == 0] <- 0
    mcoef <- coef(mod)
    mvcov <- vcov(mod)

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
