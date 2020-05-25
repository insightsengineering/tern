#' Cox regression for standard outputs
#'
#' Cox models are the most commonly used methods to estimate the magnitude of the effect in survival analysis.
#' It assumes proportional hazards; that is, it assumes that the ratio of the hazards of the two groups
#' (e.g., two arms) is constant over time. This ratio is referred to as the "hazard ratio" and is one of the most
#' commonly reported metrics to describe the effect size in survival analysis.
#'
#' @param formula (\code{formula}) Specifies \code{\link[survival:Surv]{survival model}}.
#' + the left-hand side must include `Surv(time, event)` with time and event describing occurrence
#'  and censoring (see  \code{\link[survival:Surv]{Surv()}}).
#' + the right-hand side includes predictors and *specials* which can be used to specify
#'   a pairwise model or a range of candidate univariate models. See `details`.
#' @param data (\code{data.frame})\cr
#'   Contains all the variables that are used in \code{formula}
#' @param ... (optional) other argument passed to cox regression:
#' + `conf_level`   The level of confidence for the hazard ration interval estimations. Default is 0.95.
#' + `pval_method`  The method used for estimation of p.values, should be one of `wald` (default) or
#'   `likelihood`.
#' + `ties` a character string specifying the method for tie handling, one of `exact` (default), `efron`, `breslow`.
#' + `increments` If a quantitative variable is included, it is possible to provide the expected level
#'   of estimation for the interaction. If provided, it should then be list where
#'   each item is vector giving expected levels and is named after the variable name as it appears
#'   in the formula.
#'
#' @details
#'   Possible model specifications:
#'
#'   * `Surv(time, event) ~ pairwise(Pred)`, used to get hazard ratio of each tested level of
#'   treatment `Pred` independently, given in reference to the control treatment
#'   (see \link[tern:pairwise]{pairwise}).
#'   * `Surv(time, event) ~ pairwise(Pred) + univariate(Cov1, Cov2, ...)`, add candidate covariates
#'   separated by comas in special `univariate()` to test all bivariate combinations of `Pred` with
#'   each `Cov`. Replacing the symbol `+` by `*` will result in the additional estimation of the interaction
#'   terms. This is similar to `COXT01` standards.
#'   * `Surv(time, event) ~ Pred + Cov1 + ... ` estimate the simple effect of multiple Cox regression.
#'   This provides the `COXT02` standard output.
#'
#'   Known limits:
#'
#'   + the *special* `univariate()` requires `pairwise()` being also specified.
#'   + the interaction terms in multiple regression are not estimated.
#'   + the third-order interactions are not estimated.
#'
#'   For further information about the Cox Proportional Hazards Model, check
#'   "Statistical Analysis of Clinical Trials Data with R", NEST team.
#'
#' @return Depending on `formula`, returns an object of class `tbl` for every level of `pairwise()`
#'   if included in `COXT01`, a single `tbl` otherwise.
#'
#' @seealso \code{\link{t_coxph_pairwise}},
#'   \code{\link{t_cox_univariate}},
#'   \code{\link{t_cox_multivariate}}
#'
#' @md
#'
#' @examples
#' library(tern)
#' library(random.cdisc.data)
#' ADTTE   <- radtte(cached = TRUE)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")   # _f: filtered
#' ADTTE_f <- within( # nolint
#'   data = subset(
#'     ADTTE_f,
#'     PARAMCD == "OS"
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
#'   }  # nolint end
#' )
#'
#' # For annotation on top of Kaplan-Meier grob.
#' tern:::t_coxreg(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ pairwise(ARMCD), data = ADTTE_f,
#'   conf_level = 0.8, pval_method = "likelihood",  ties = "breslow"
#' )
#'
#' ## COXT01 - Standard output, no interactions
#' tern:::t_coxreg(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ pairwise(ARMCD) + univariate(SEX, RACE, AGE),
#'   data = ADTTE_f
#' )
#'
#' \dontrun{
#'   # For comparison
#'   mod10 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD), data = ADTTE_f)
#'   mod11 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + SEX, data = ADTTE_f)
#'   mod12 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + AGE, data = ADTTE_f)
#'   mod13 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + RACE, data = ADTTE_f)
#'
#'   car::Anova(mod10,  test.statistic = "Wald", type = 'III')
#'   car::Anova(mod11, test.statistic = "Wald", type = 'III')
#'   car::Anova(mod12, test.statistic = "Wald", type = 'III')
#'   car::Anova(mod13, test.statistic = "Wald", type = 'III')
#'
#'   summary(mod13)$coefficients
#' }
#'
#' ## COXT01 - options: control ties, set confidence interval level, choose a strata,
#' ## modify pval_method
#' tern:::t_coxreg(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ pairwise(ARMCD) + strata(SEX) +
#'     univariate(RACE, AGE),
#'   data = ADTTE_f, conf_level = 0.80,
#'   ties = c("exact", "efron", "breslow")[3], pval_method  = c("wald", "likelihood")[2]
#' )
#'
#' \dontrun{
#'   # For comparison
#'   mod20 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + strata(SEX), data = ADTTE_f)
#'   mod21 <- coxph(
#'     Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + strata(SEX) + AGE, data = ADTTE_f
#'   )
#'   mod22 <- coxph(
#'     Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + strata(SEX) + RACE,
#'     data = ADTTE_f
#'   )
#'
#'   car::Anova(mod20, test.statistic = "Wald", type = 'III')
#'   car::Anova(mod21, test.statistic = "Wald", type = 'III')
#'   car::Anova(mod22, test.statistic = "Wald", type = 'III')
#'
#'   summary(mod22)$coefficients
#' }
#'
#' \dontrun{
#'   ## COXT01 - Addition of the interaction and increments to choose estimated levels
#'   tern:::t_coxreg(
#'     formula =  Surv(time = AVAL, event = 1 - CNSR) ~
#'       pairwise(ARMCD) * univariate(RACE, SEX, AGE, X),
#'     data = ADTTE_f, conf_level = 0.74, increments = list(X = c(-1, 0, 2))
#'   )
#'
#'   ## COXT01: Other example, other data
#'   library(dplyr)
#'   ADSL <- radsl(cached = TRUE)
#'   ADSL <- ADSL %>% filter(SEX %in% c("F", "M"))
#'
#'   ADTTE <- radtte(ADSL, seed = 2) %>%
#'     filter(PARAMCD == "PFS" & ARMCD != "ARM C")
#'   ADTTE$ARMCD <- droplevels(ADTTE$ARMCD)
#'   ADTTE$SEX <- droplevels(ADTTE$SEX)
#'   tern:::t_coxreg(
#'     formula = Surv(time = AVAL, event = 1 - CNSR) ~
#'       pairwise(ARMCD) * univariate(SEX, BMRKR2, AGE),
#'     data = ADTTE,
#'     increments = list(AGE = mean(ADTTE$AGE)),
#'     conf_level = 0.975)
#'
#'   # check HRs
#'   m2 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ ARMCD*BMRKR2, data = ADTTE)
#'   coef(m2)
#'
#'   # For COXT02
#'   tern:::t_coxreg(
#'     formula = Surv(time = AVAL, event = 1 - CNSR) ~ ARMCD + RACE * AGE, data = ADTTE_f,
#'     conf_level = 0.8, pval_method = "likelihood",  ties = "breslow"
#'   )
#' }

t_coxreg <- function(formula, data, ...) {

  check_formula(formula)
  tf        <- terms(formula, specials = c("strata", "pairwise", "univariate"))
  tf_factor <- attr(tf, "factors")

  form_order <- colSums(tf_factor)
  if (max(form_order) > 2) stop("Check formula.
  The integration of 3-way interaction in Cox regression is discouraged,
  use `survival::coxph()` if as required by the scope of the study.")

  pairwise <- explicit_special(formula_terms = tf, special = "pairwise")
  strat    <- explicit_special(formula_terms = tf, special = "strata")
  univ     <- explicit_special(formula_terms = tf, special = "univariate")

  covariates <- rownames(tf_factor)[-c(1, unlist(attr(tf, "specials")))]
  if (length(covariates) == 0) covariates <- NULL

  # Accounting for covariates and arm, cases:
  #     - [^1]: if pairwise AND no covariate AND no univ spec: t_coxph_pairwise
  #     - [^2]: if pairwise AND univ spec: analysis one covariate at a time (+opt interaction)
  #     - [^3]: if no pairwise, simple effect all variable at once

  # [^1]: if pairwise AND no covariate AND no univ spec: t_coxph_pairwise
  if (!is.null(pairwise) & is.null(covariates) & is.null(univ)) {

    formula <- update.formula(
      old = formula, new = ifelse(
        test = is.null(strat),
        yes = paste0("~ arm(", pairwise$v, ")"),
        no  = paste0("~ arm(", pairwise$v, ") + ", strat$t)
      )
    )

    suppressWarnings({
      y <- t_coxph_pairwise(formula = formula, data = data, ...)
    })
    return(y)
  }

  # [^2]: if pairwise AND univ spec: analysis one covariate at a time (+opt interaction)
  if (!is.null(pairwise) & !is.null(univ)) {

    covariates <- gsub(pattern = " ", replacement = "",  univ$v)
    covariates <- unlist(strsplit(x = covariates, split = ","))
    covariates <- setNames(nm = covariates)
    covariates <- lapply(covariates, function(x) as.formula(paste("~", x)))

    form_ref <- update.formula(
      old = formula, new = ifelse(
        test = is.null(strat),
        yes = paste0("~ arm(", pairwise$v, ")"),
        no  = paste0("~ arm(", pairwise$v, ") + ", strat$t)
      )
    )

    pairwise$lvl_ref <- levels(data[[pairwise$v]])[1]
    pairwise$lvl <- levels(data[[pairwise$v]])[-1]
    names(pairwise$lvl) <- pairwise$lvl
    data <- lapply(
      pairwise$lvl, function(x) {

        y <- paste0("dplyr::filter(data, ", pairwise$v, " %in% c(pairwise$lvl_ref, x))");
        y <- eval(parse(text = y));
        y[[pairwise$v]] <- droplevels(y[[pairwise$v]])

        return(y)

      }
    )

    y <- lapply(
      X = data, FUN = function(x) {
        t_cox_univariate(
          formula = form_ref, data = x, covariates = covariates,
          interactions = max(form_order) == 2, ...)
      }
    )

    return(y)

  }

  # [^3]: if no pairwise, simple effect all variable at once
  if (is.null(pairwise)) {

    if (!is.null(univ)) stop("The pairwise specification was not found in model scope.")
    if (max(form_order) == 2) {
      message("Analysis based on multiplivariate Cox model is usually not
    performed for the CSR or regulatory documents and is for exploratory purposes only
    (e.g., for publication). In practice, the model usually includes only the main effects
    (without interaction terms).")
      stop("The second order interactions are not yet integrated in `t_coxreg`.
      Remove interaction terms or, if required by the study, refers to `survival::coxph()`.")
    }
    y <- t_cox_multivariate(formula = formula, data = data, ...)

    return(y)

  }

}


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
pairwise <- function(x) structure(x, varname = deparse(substitute(x)))


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
univariate <- function(x) structure(x, varname = deparse(substitute(x)))



explicit_special <- function(formula_terms, special){

  y <- attr(formula_terms, "specials")[[special]]
  if (!is.null(attr(formula_terms, "specials")[[special]])) {
    y <- list()
    # i, index; t, term; v, variable
    y$i <- attr(formula_terms, "specials")[[special]]
    y$t <- rownames(attr(formula_terms, "factors"))[y$i]
    y$v <- gsub(".*\\((.*)\\).*", "\\1", y$t)
  } else {
    y <- NULL
  }
  return(y)
}


#' Proportional Hazards Regression Model Fit Summary Table (pair-wise for multiple arms)
#'
#' An \code{\link[rtables]{rtable}} format of \code{\link[tern]{s_coxph_pairwise}}
#' results for further annotation on top of Kaplan-Meier grob.
#'
#' @inheritParams s_coxph_pairwise
#'
#' @details
#' The Cox PH model is evaluated pair-wise (reference to
#' comparison) and \code{\link[survival]{coxph}} is used to get the p-value,
#' calculate the hazard ratio and confidence interval.
#' For example, given an ARM variable has reference group "ARM A" and
#' two comparison groups "ARM B" and "ARM C",
#' t_coxph_pairwise will conduct two Cox PH models for the specified \code{formula},
#' one is for the subset data with only "ARM A" and "ARM B",
#' the other is for the subset data with only "ARM A" and "ARM C".
#'
#'
#' @import survival
#' @import rtables
#'
#' @export
#'
#' @template author_wangh107
#'
#' @examples
#' library(random.cdisc.data)
#' ADSL <- cadsl
#' ADTTE <- cadtte
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")
#'
#' tbl <- t_coxph_pairwise(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARM) + strata(SEX),
#'   data = ADTTE_f
#' )
#' tbl
#'
#' tbl <- t_coxph_pairwise(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + strata(SEX),
#'   data = ADTTE_f,
#'   conf_level = 0.8, pval_method = "wald", ties = "exact"
#' )
#' tbl
#'
#' # If the event never happened in selection, a note is returned in the tabulation
#' tbl <- t_coxph_pairwise(
#'   formula = Surv(time = AVAL, event = CNSR - CNSR) ~ arm(ARMCD) + strata(SEX),
#'   data = ADTTE_f,
#'   conf_level = 0.8, pval_method = "wald", ties = "exact"
#' )
#' tbl
t_coxph_pairwise <- function(formula,
                             data,
                             conf_level = 0.95, # nolint
                             pval_method = c("log-rank", "wald",  "likelihood"),
                             ...) {

  pval_method <- match.arg(pval_method)
  pval_method_str <- capitalize(pval_method)

  header <- c("HR", paste0(conf_level * 100, "% CI of HR"), paste0(pval_method_str, " p-value"))

  nevents <- sum(with(data, eval(attr(terms(formula), "variables")[[2]])[, "status"]))

  if (nevents == 0) {

    rrows <- list(rrow("*NOTE: ", rcell("No estimates as the event was not observed in selection.", colspan = 3)))
    footnote <- "nevent = 0"

  } else {
    # this runs both stratified and unstratified if there is a strata special
    coxph_values <- s_coxph_pairwise(formula, data, conf_level, pval_method, ...)

    sel <- ifelse(has_special_strata(formula), "stratified", "unstratified")

    rrows <- lapply(coxph_values[-1], function(xi) {
      # don't want row for reference arm
      vals <- xi[[sel]]
      rrow(
        row.name = xi$compare["comparison"],
        rcell(vals$hr, format = "xx.xxxx"),
        rcell(vals$hr_ci, format = "(xx.xxxx, xx.xxxx)"),
        rcell(vals$pvalue, format = "xx.xxxx")
      )
    })

    footnote <- paste0("pairwise comparison to \"", names(coxph_values)[1], "\"")
  }

  tbl <- rtablel(
    header,
    rrows
  )

  attr(tbl, "footnotes") <- footnote
  tbl
}


#' Run Pairwise (ARM) \code{CoxPH} model for unstratified and stratified analysis
#'
#'
#' @param formula (\code{formula})\cr
#'   Specifies \code{\link[survival:Surv]{survival model}}.
#'   The arm variable needs to be wrapped in \code{\link{arm}}. The
#'   \code{\link[survival]{strata}} special will only be used for the stratified analysis. If there is not
#'   \code{\link[survival]{strata}} specification then the stratified analysis is omitted.
#' @param data (\code{data.frame})\cr
#'   Contains all the variable that are used in \code{formula}
#' @param conf_level (\code{numeric} value)\cr
#'   level for computation of the confidence intervals.
#'   If set to \code{FALSE} no confidence intervals are printed
#' @param pval_method (one of (\code{"log-rank", "likelihood", "wald"}))\cr
#'   Specifies the method used to calculate the p-value. Default value is \code{"log-rank"}.
#' @param ... (optional)\cr
#'   Other arguments will be passed to \code{\link[survival]{coxph}}
#'
#' @return a list of dataframes with unstratified and/or stratified analysis results
#'
#' @export
#'
#' @importFrom stats setNames
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")
#'
#' cox_res <-  s_coxph_pairwise(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARM) + strata(RACE),
#'   data = ADTTE_f,
#'   pval_method = "wald",  ties = "exact"
#' )
#' names(cox_res)
#' cox_res[[2]]$stratified
#'
#' cox_res2 <- s_coxph_pairwise(
#'   formula =  Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARM),
#'   data = ADTTE_f,
#'   conf_level = 0.8, pval_method = "likelihood",  ties = "breslow"
#' )
#' cox_res2[[2]]$stratified
#' cox_res2[[2]]$unstratified
s_coxph_pairwise <- function(formula,
                             data,
                             conf_level = 0.95, # nolint
                             pval_method = c("log-rank", "wald",  "likelihood"),
                             ...) {

  # extracted data
  cl <- match.call()
  stopifnot(is.data.frame(data))

  tm <- t_tte_items(formula, cl, data, parent.frame())

  arm <- tm$arm
  if (nlevels(arm) < 2) {
    stop("at least to arms expected to calculate the pairwise comparisons")
  }

  arm_var <- attr(arm, "varname")
  form_unstr <- tm$formula_nostrata
  form_str <- tm$formula_strata

  reference_lvl <- levels(arm)[1]
  comparison_lvls <- levels(arm)[-1]

  coxph_results <- lapply(comparison_lvls, function(lvl) {

    df_lvl <- data[arm %in% c(reference_lvl, lvl), , drop = FALSE]
    df_lvl[[arm_var]] <- droplevels(df_lvl[[arm_var]])


    fit_unstratified <- coxph(formula = form_unstr, data = df_lvl, ...)
    unstratified <- coxph_extract(
      fit_unstratified,
      conf_level = conf_level,
      pval_method = pval_method)

    stratified <- if (is.null(form_str)) {
      NULL
    } else {
      fit_stratified <- coxph(formula = form_str, data = df_lvl, ...)
      coxph_extract(fit_stratified,
                    conf_level = conf_level,
                    pval_method = pval_method)
    }

    list(
      compare = c(reference = reference_lvl, comparison = lvl),
      unstratified = unstratified,
      stratified = stratified,
      conf_level = conf_level,
      pval_method = pval_method
    )
  })

  coxph_results <- setNames(c(list(NULL), coxph_results), levels(arm))

  coxph_results
}


#' Extract Info from coxph object
#'
#' @param fit an object of {\link{\code{coxph}}}
#' @inheritParams s_coxph_pairwise
#' @noRd
#'
#'
#' @examples
#'
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")
#'
#' fit <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARM) + strata(RACE), data = ADTTE_f)
#'
#' tern:::coxph_extract(fit)
coxph_extract <- function(fit,
                          conf_level = 0.95, # nolint
                          pval_method = c("log-rank", "wald",  "likelihood")) {

  stopifnot(is(fit, "coxph"))
  pval_method <- match.arg(pval_method)

  msum <- summary(fit, conf.int = conf_level)

  pval <- switch(pval_method,
                 "wald" = msum$waldtest["pvalue"],
                 "log-rank" = msum$sctest["pvalue"], # Score (logrank) test,
                 "likelihood" = msum$logtest["pvalue"]
  )

  if (all(is.na(fit$coefficients))) {
    hr <- NA_real_
    hr_ci <- c(NA_real_, NA_real_)
  } else {
    hr <- msum$conf.int[1, 1]
    hr_ci <- msum$conf.int[1, 3:4]
  }

  list(
    pvalue = as.vector(pval),
    hr =  hr,
    hr_ci = hr_ci,
    conf_level = conf_level,
    pval_method = pval_method
  )

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
#' @importFrom car Anova
#' @importFrom stats anova update model.matrix
#' @import survival
#'
#' @export
#'
#' @md
#'
#' @examples
#' library(tern)
#' library(random.cdisc.data)
#' ADTTE   <- radtte(cached = TRUE)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")   # _f: filtered
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
#'   }  # nolint end
#' )
#'
#' s_cox_univariate(
#' formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD),
#' data = ADTTE_f,
#' covariates = list(~ SEX)
#' )
#' \dontrun{
#' s_cox_univariate(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD),
#'   data = ADTTE_f,
#'   covariates = list("Race" = ~ RACE, ~ AGE, "a rand. quant var with increments" = ~ X),
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

  ## Argument checks
  check_formula(formula)
  if (is.null(covariates)) stop("Check `covariates`, provide a list of candidate covariates.")
  check_covariate_formulas(covariates)
  check_numeric_range(conf_level, min = 0, max = 1)
  pval_method <- match.arg(pval_method)

  covariates <- name_covariate_names(covariates)
  check_increments(increments, covariates)

  # Formula univariate survival model, terms (t) and term index (i) for conveniency
  tf   <- terms(formula, specials = c("arm", "strata"))
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

  coef <- list();
  coef$ref_mod <- matrix(
    fit$ref_mod$msum$coefficients[paste0(tarm, arms[-1]), c("coef", "se(coef)")],
    ncol = 2,
    dimnames = list(NULL, c("coef", "se(coef)"))
  )
  rownames(coef$ref_mod) <- "ref_mod"

  if (interactions) {

    is_covar_num <- lapply(covariates, function(x) is.numeric(data[[rht(x)]]));
    coef_cov <- Map(
      # work on model fits with interaction
      fit = fit[- (1:(length(covariates) + 1))], is_covar_num = is_covar_num, covariates = covariates,
      f = function(fit, is_covar_num, covariates) {

        if (is_covar_num & rht(covariates) %in% names(increments)) {
          # [^1]: If covar is a numeric and increments are specified,
          # SE and COEF must be estimated for every level

          coef_narm   <- paste0(tarm, levels(with(data, eval(parse(text = tarm))))[2])
          coef_ninter <- paste0(coef_narm, ":", rht(covariates))

          betas     <- coef(fit$mod)[c(coef_narm, coef_ninter)]
          var_betas <- diag(vcov(fit$mod))[c(coef_narm, coef_ninter)]
          cov_betas <- vcov(fit$mod)[coef_narm, coef_ninter]

          xvals        <- increments[[rht(covariates)]]
          names(xvals) <- xvals

          coef_hat <- betas[coef_narm] + xvals * betas[coef_ninter]
          coef_se  <- sqrt(var_betas[coef_narm] + xvals^2 * var_betas[coef_ninter] + 2 * xvals * cov_betas)
          y <- cbind(coef_hat, coef_se)
          colnames(y) <- c("coef", "se(coef)");
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

          mmat <- stats::model.matrix(fit$mod)[1, ]
          mmat[!mmat == 0] <- 0
          y <- estimate_coef(
            variable = tarm, given = rht(covariates),
            coef = coef(fit$mod), mmat = mmat, vcov = vcov(fit$mod),
            lvl_var = levels(data[[gsub(".*\\((.*)\\).*", "\\1", tarm)]]),
            lvl_given =  levels(data[[rht(covariates)]]),
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
  n <- vapply(X = fit[1 : (1 + length(covariates))], FUN = function(x) x$msum$n, FUN.VALUE = 1)

  # Hazard Ratio for treatment
  hr <- lapply(coef, function(x) exp(x[, "coef"]))

  # Confidence interval of Hazard ratio for treatment
  ci <- lapply(
    coef, function(x) {
      q_norm  <- qnorm((1 + conf_level) / 2)
      y  <- t(apply(x, 1, function(y) exp(y["coef"] + c(-1, +1) * q_norm * y["se(coef)"])))
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

  #Likelihood ratio test for the interaction
  if (interactions) {

    lrt <- Map(
      f = function(
        without_interaction,
        with_interaction
      ) {
        anova(without_interaction$mod, with_interaction$mod)[2, "P(>|Chi|)"]
      },
      without_interaction = fit[2:(length(covariates) + 1)],
      with_interaction    = fit[- (1:(length(covariates) + 1))]
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
    lrt  = lrt,
    covariates = names(covariates),
    tstr = tstr,
    pval_method = pval_method,
    treatment = setNames(arms, c("ref", "tested"))
  )

  return(y)

}


# Get the right-hand-term of a formula
rht <- function(x) {
  stopifnot(is(x, "formula"))
  y <- as.character(rev(x)[[1]])
  return(y)
}


#' Hazard ratio estimation in interactions
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
#' @export
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADSL <- ADSL %>% dplyr::filter(SEX %in% c("F", "M"))
#' \dontrun{
#' ADTTE <- radtte(ADSL, seed = 2) %>%  dplyr::filter(PARAMCD == "PFS")
#' ADTTE$ARMCD <- droplevels(ADTTE$ARMCD)
#' ADTTE$SEX <- droplevels(ADTTE$SEX)
#'
#' mod <- coxph(formula = Surv(time = AVAL, event = 1 - CNSR) ~ (SEX + ARMCD)^2, data = ADTTE)
#'
#' mmat <- model.matrix(mod)[1, ]
#' mmat[!mmat == 0] <- 0
#'
#' estimate_coef(
#'   variable = "ARMCD", given = "SEX",
#'   coef = coef(mod), mmat = mmat, vcov = vcov(mod), data = ADTTE, conf_level = .95
#' )
#' }
estimate_coef <- function(variable, given,
                          lvl_var, lvl_given,
                          coef,
                          mmat,
                          vcov,
                          conf_level = 0.95
) {

  var_lvl <- paste0(variable, lvl_var[-1]) # [-1]: reference level
  giv_lvl <- paste0(given,    lvl_given)

  design_mat <- expand.grid(variable = var_lvl, given = giv_lvl)
  design_mat <- design_mat[order(design_mat$variable, design_mat$given), ]
  design_mat <- within(
    data = design_mat,
    expr = {
      inter     <- paste0(variable, ":", given)    # nolint
      rev_inter <- paste0(given,    ":", variable) # nolint
    }
  )

  split_by_variable <- design_mat$variable
  interaction_names <- paste(design_mat$variable, design_mat$given, sep = "/")

  design_mat <- apply(
    X = design_mat, MARGIN = 1, FUN = function(x) {
      mmat[names(mmat) %in% x[-which(names(x) == "given")]] <- 1
      return(mmat)
    })
  colnames(design_mat) <- interaction_names

  betas <- as.matrix(coef)

  coef_hat <- t(design_mat) %*% betas
  dimnames(coef_hat)[2] <- "coef"

  coef_se <- apply(design_mat, 2, function(x){
    vcov_el <- as.logical(x)
    y <- vcov[vcov_el, vcov_el]
    y <- sum(y)
    y <- sqrt(y)
    return(y)
  })

  q_norm  <- qnorm((1 + conf_level) / 2)
  y <- cbind(coef_hat, `se(coef)` = coef_se)

  y <- apply(y, 1, function(x){

    x["hr"]  <- exp(x["coef"])
    x["lcl"]  <- exp(x["coef"] - q_norm * x["se(coef)"])
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



# Fit the cox regression model and compute anova ---
fit_n_aov <- function(formula,
                      data = data,
                      conf_level  = conf_level,
                      pval_method = c("wald", "likelihood"),
                      ...
) {

  pval_method <- match.arg(pval_method)

  environment(formula) <- environment()
  suppressWarnings({
    mod  <- coxph(formula, data = data, ...)
    msum <- summary(mod, conf.int = conf_level)
    suppressMessages({
      aov  <- car::Anova(
        mod,
        test.statistic = switch(pval_method, "wald" = "Wald", "likelihood" = "LR"),
        type = "III"
      )
    })
  })

  y <- list(mod = mod, msum = msum, aov = aov)
  return(y)

}


# argument_checks
check_formula <- function(formula){

  if (!(inherits(formula, "formula")))
    stop("Check `formula`. A formula should resemble `Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD)`.")

  invisible()
}


check_covariate_formulas <- function(covariates) {

  if (!all(vapply(X = covariates, FUN = inherits, what = "formula", FUN.VALUE = TRUE)) | is.null(covariates))
    stop("Check `covariates`, it should be a list of right-hand-term formulas, e.g. list(Age = ~AGE).")

  invisible()
}

name_covariate_names <- function(covariates) {

  miss_names <- names(covariates) == ""
  no_names   <- is.null(names(covariates))
  if (any(miss_names))  names(covariates)[miss_names] <- vapply(covariates[miss_names], FUN = rht, FUN.VALUE = "name")
  if (no_names)         names(covariates)             <- vapply(covariates,             FUN = rht, FUN.VALUE = "name")
  return(covariates)

}

check_increments <- function(increments, covariates) {

  if (!is.null(increments)) {
    covariates <- vapply(covariates, FUN = rht, FUN.VALUE = "name")
    lapply(
      X = names(increments), FUN = function(x) {
        if (!x %in% covariates)
          warning(
            paste("Check `increments`, the `increment` for ", x,
                  "doesn't match any names in investigated covariate(s).")
          )
      }
    )

  }

  invisible()
}

#' Cox regression including a single covariate - tabulated results
#'
#' Fit cox (proportional hazard) regression models including the treatment and a single covariate.
#' Starting from a univariate model (e.g. survival model including an two-level arm predictor), a list of candidate
#' alternative models including an additional covariate (optionally including the interaction terms) is tested.
#'
#' @inheritParams s_cox_univariate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(tern)
#' library(random.cdisc.data)
#' ADTTE   <- radtte(cached = TRUE) # nolint
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")   # _f: filtered # nolint
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
#'   }  # nolint end
#' )
#'
#' ## Standard output - no interactions - comparison -----------------------------
#' t_cox_univariate(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD),
#'   data = ADTTE_f,
#'   covariates = list(~ SEX, "Race" = ~ RACE, Age = ~ AGE, ~ X),
#'   interactions = FALSE
#' )
#'
#' # For comparison
#' mod10 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD), data = ADTTE_f)
#' mod11 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + SEX, data = ADTTE_f)
#' mod12 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + AGE, data = ADTTE_f)
#' mod13 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + RACE, data = ADTTE_f)
#'
#' car::Anova(mod10,  test.statistic = "Wald", type = 'III')
#' car::Anova(mod11, test.statistic = "Wald", type = 'III')
#' car::Anova(mod12, test.statistic = "Wald", type = 'III')
#' car::Anova(mod13, test.statistic = "Wald", type = 'III')
#'
#' summary(mod13)$coefficients
#'
#' ## Standard output - no interactions - but strata -----------------------------
#' t_cox_univariate(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + strata(SEX),
#'   data = ADTTE_f,
#'   covariates = list("Race" = ~ RACE, Age = ~ AGE, ~ X),
#'   interactions = FALSE
#' )
#'
#' # For comparison
#' mod20 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + strata(SEX), data = ADTTE_f)
#' mod21 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + strata(SEX) + AGE, data = ADTTE_f)
#' mod22 <- coxph(
#'   Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + strata(SEX) + RACE,
#'   data = ADTTE_f
#' )
#'
#' car::Anova(mod20, test.statistic = "Wald", type = 'III')
#' car::Anova(mod21, test.statistic = "Wald", type = 'III')
#' car::Anova(mod22, test.statistic = "Wald", type = 'III')
#'
#' summary(mod22)$coefficients
#'
#' ## Standard output - interactions ----------------------
#' t_cox_univariate(
#'   formula =  Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD),
#'   data = ADTTE_f,
#'   covariates = list(~ RACE, ~SEX, Age = ~ AGE, ~X),
#'   interactions = TRUE, conf_level = 0.8
#' )
#'
#' # For comparison
#' mod30 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD), data = ADTTE_f)
#' mod31 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) * AGE, data = ADTTE_f)
#' mod32 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) * RACE, data = ADTTE_f)
#'
#' car::Anova(mod30, test.statistic = "Wald", type = 'III')
#' car::Anova(mod31, test.statistic = "Wald", type = 'III')
#' car::Anova(mod32, test.statistic = "Wald", type = 'III')
#'
#' ## Standard output - interactions ----------------------
#' t_cox_univariate(
#'   formula =  Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD),
#'   data = ADTTE_f,
#'   covariates = list(~ RACE, ~SEX, Age = ~ AGE, ~X),
#'   interactions = TRUE, conf_level = 0.8
#' )
#'
#' ## Standard output - interactions + increments -----------------
#' t_cox_univariate(
#'   formula =  Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD),
#'   data = ADTTE_f,
#'   covariates = list(~ RACE, ~SEX, Age = ~ AGE, ~X),
#'   interactions = TRUE, conf_level = 0.8,
#'   increments = list(AGE = c(50), X = c(-0.5, 0, 0.5, 1))
#' )
#'
#' ## Other example, other data
#' library(dplyr)
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' ADSL <- ADSL %>% filter(SEX %in% c("F", "M"))
#'
#' ADTTE <- radtte(ADSL, seed = 2) %>%
#'   filter(PARAMCD == "PFS" & ARMCD != "ARM C")
#' ADTTE$ARMCD <- droplevels(ADTTE$ARMCD)
#' ADTTE$SEX <- droplevels(ADTTE$SEX)
#' t_cox_univariate(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD),
#'   data = ADTTE,
#'   covariates  = list(~ SEX, ~ BMRKR2, Age =~AGE),
#'   interactions = TRUE,
#'   increments = list(AGE = mean(ADTTE$AGE)),
#'   conf_level = 0.975)
#'
#' # check HRs
#' m2 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ ARMCD*BMRKR2, data = ADTTE)
#' coef(m2)
#' }

t_cox_univariate <- function(formula,
                             data,
                             covariates = NULL,
                             interactions = FALSE,
                             conf_level = 0.95,
                             pval_method = c("wald", "likelihood"),
                             increments = NULL,
                             ...) {

  y <- s_cox_univariate(
    formula = formula,
    data = data,
    covariates = covariates,
    interactions = interactions,
    conf_level = conf_level,
    pval_method = pval_method,
    increments = increments,
    ...
  )

  treatment_comparison <- paste0(
    "Treatment ", y$treatment["tested"], " vs control (", y$treatment["ref"], ")"
  )

  # Result output differs widely depending on the inclusion or not of the interaction term.
  if (!interactions) {

    my_header <- rheader(
      rrow("", "", rcell("Treatment effect adjusted for covariate #", colspan = 3)),
      rrow("Effect/ ",           "", "Hazard", "", ""),
      rrow("Covariate included", "", "Ratio for", "", ""),
      rrow("in the model",       "n", "Treatment",  paste0(conf_level * 100, "% CI*"), "p-value*")
    )

    tbl <- rbind(
      rtable(
        header = my_header,
        make_a_rrow(treatment_comparison, x = y, item = "ref_mod")
      ),
      rrow(),
      rrow("Covariate:"),
      indent(rtablel(header = NULL, lapply(y$covariates, make_a_rrow, x = y))),
      rrow(),
      rrow("# Model including treatment and covariate"),
      rrow("    without interaction"),
      rrow(paste0("* ", y$pval_method, " confidence interval/test"))
    )

  } else if (interactions) {

    my_header <- rheader(
      rrow("",                   "",   rcell("Treatment effect adjusted for covariate #", colspan = 4)),
      rrow("",                   "",   "",          "", "", "Treatment by"),
      rrow("Effect/ ",           "",   "Hazard",    "", "", "covariate"),
      rrow("Covariate included", "",   "Ratio for", "", "", "interaction"),
      rrow("in the model",       "n",  "Treatment",  paste0(conf_level * 100, "% CI*"),
           "p-value*", "p-value**")
    )

    ref_row <- rtable(
      header = my_header,
      rrowl(
        treatment_comparison,
        list(
          rcell(y$n["ref_mod"], format = "xx"),
          rcell(y$hr$ref_mod, format = "xx.xx"),
          rcell(y$ci$ref_mod, format = "(xx.xx, xx.xx)"),
          rcell(y$pval$ref_mod, format = "xx.xxxx"),
          NULL
        )
      )
    )

    covar_rows <- do.call(rbind, lapply(y$covariates, make_a_cov_chunk, y = y, header = my_header))

    tbl <- rbind(
      ref_row,
      rrow(),
      covar_rows,
      rrow(),
      rrow("# Model including treatment and covariate"),
      rrow("    and interaction"),
      rrow(paste0("* ", y$pval_method, " confidence interval/test")),
      rrow("** Likelihood-ratio test")
    )

  }

  return(tbl)
}


## When no-interaction tested, every row provides a similar information.
make_a_rrow <- function(name, x, item = name) {
  rrowl(
    name,
    list(
      rcell(x$n[item], format = "xx"),
      rcell(x$hr[[item]], format = "xx.xx"),
      rcell(x$ci[[item]], format = "(xx.xx, xx.xx)"),
      rcell(x$pval[[item]], format = "xx.xxxx")
    )
  )
}

# In case of interaction, every covariate results should be returned as a chunk of rows:
# the first row for the covariate related test then a row per covariate level.
make_a_cov_chunk <- function(item, y, header) {

  i_item <- which(y$covariates == item)
  cov_row <- rtable(
    header = header,
    rrowl(
      item,
      list(
        rcell(y$n[item]),
        NULL,
        NULL,
        rcell(y$pval[[1 + length(y$covariates) + i_item]], format = "xx.xxxx"),
        rcell(y$lrt[[i_item]], format = "xx.xxxx")
      )
    )
  )

  beta_ci <- cbind(hr = y$hr[[1 + i_item]], y$ci[[1 + i_item]])
  rnames  <- rownames(beta_ci)
  beta_ci <- split(beta_ci, rnames)[rnames]
  lvls <- rtablel(
    header = NULL,
    Map(
      f = function(values, name) {
        rrow(
          name, NULL,
          rcell(values[1], format = "xx.xx"), rcell(values[2:3], format = "(xx.xx, xx.xx)"),
          NULL, NULL
        )
      },
      values = beta_ci, name = names(beta_ci)
    )
  )

  z <- rbind(
    cov_row,
    indent(lvls),
    rrow()
  )

  return(z)

}


#' Multivariate Cox Model - summarized results
#'
#' Analysis based on multivariate Cox model is usually not performed for the `CSR` or regulatory documents and
#' is for exploratory purposes only (e.g., for publication). In practice, the model usually includes only the main
#' effects (without interaction terms). It produces the estimates for each of the covariates included in the model.
#' The analysis follows the same principles (e.g., stratified vs. unstratified analysis and tie handling) as the
#' usual Cox model analysis. Since there is usually no pre-specified hypothesis testing for such analysis,
#' the p values need to be interpreted with caution. (**Statistical Analysis of Clinical Trials Data with R**,
#' `NEST's bookdown`)
#'
#' @param formula (\code{formula})\cr
#'   Specifies \code{\link[survival:Surv]{survival model}} including covariate and interactions.
#' @param data A \code{data.frame} which includes the variable in formula and covariate
#' @param conf_level The level of confidence for the hazard ration interval estimations. Default is 0.95.
#' @param pval_method The method used for the estimation of p.values, should be one of \code{"wald"} (default) or
#'   \code{"likelihood"}.
#' @param ... (optional) parameters passed down to \code{\link[survival:coxph]{coxph()}}
#' + `ties` a character string specifying the method for tie handling, one of `exact` (default), `efron`, `breslow`.
#'
#' @details The output is limited to single effect terms. Work in ongoing for estimation of interaction terms
#' but is out of scope as defined by the `GDSR` (**`GDS_Standard_TLG_Specs_Tables_2.doc`**).
#'
#' @md
#'
#' @export
#'
#' @importFrom stats model.matrix coef
#'
#' @examples
#' library(tern)
#' library(random.cdisc.data)
#' ADTTE   <- radtte(cached = TRUE)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")   # _f: filtered
#' ADTTE_f <- dplyr::filter (
#'   ADTTE_f,
#'   PARAMCD == "OS"
#'   & SEX %in% c("F", "M")
#'   & RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
#' );
#' ADTTE_f$SEX  <- droplevels(ADTTE_f$SEX)
#' ADTTE_f$RACE <- droplevels(ADTTE_f$RACE)
#' \dontrun{
#' s_cox_multivariate(
#' formula = Surv(time = AVAL, event = 1 - CNSR) ~ (ARMCD + RACE + AGE)^2, data = ADTTE_f
#' )
#' }
s_cox_multivariate <- function(formula, data,
                               conf_level = 0.95,
                               pval_method = c("wald", "likelihood"),
                               ...
) {

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

  all_term_labs <-  attr(mod$terms, "term.labels")
  term_labs <- all_term_labs[which(attr(mod$terms, "order") == 1)]
  names(term_labs) <- term_labs

  coef_inter <- NULL
  if (any(attr(mod$terms, "order") > 1)) {

    for_inter <- all_term_labs[attr(mod$terms, "order") > 1]
    names(for_inter) <- for_inter
    mmat <- stats::model.matrix(mod)[1, ]
    mmat[!mmat == 0] <- 0
    mcoef <- coef(mod)
    mvcov <- vcov(mod)

    estimate_coef_local <- function(variable, given){
      estimate_coef(
        variable, given, coef = mcoef, mmat = mmat, vcov = mvcov, conf_level = conf_level,
        lvl_var = levels(data[[variable]]), lvl_given = levels(data[[given]])
      )
    }

    coef_inter <- lapply(
      for_inter, function(x){
        y <- attr(mod$terms, "factor")[, x]
        y <- names(y[y > 0])
        Map(estimate_coef_local, variable = y, given = rev(y))
      }
    )

  }

  list(mod = mod, msum = msum, aov = aov, coef_inter = coef_inter)

}


#' Multivariate Cox Model - tabulated results
#'
#' Analysis based on multivariate Cox model is usually not performed for the `CSR` or regulatory documents and
#' is for exploratory purposes only (e.g., for publication). In practice, the model usually includes only the main
#' effects (without interaction terms). It produces the estimates for each of the covariates included in the model.
#' The analysis follows the same principles (e.g., stratified vs. unstratified analysis and tie handling) as the
#' usual Cox model analysis. Since there is usually no pre-specified hypothesis testing for such analysis,
#' the p values need to be interpreted with caution. (**Statistical Analysis of Clinical Trials Data with R**,
#' `NEST's bookdown`)
#'
#' @inheritParams s_cox_multivariate
#'
#' @export
#'
#' @md
#'
#' @examples
#' library(tern)
#' library(random.cdisc.data)
#' ADTTE   <- radtte(cached = TRUE)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")   # _f: filtered
#' ADTTE_f <- dplyr::filter (
#'   ADTTE_f,
#'   PARAMCD == "OS"
#'   & SEX %in% c("F", "M")
#'   & RACE %in% c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
#' );
#' ADTTE_f$SEX  <- droplevels(ADTTE_f$SEX)
#' ADTTE_f$RACE <- droplevels(ADTTE_f$RACE)
#'
#' \dontrun{
#' t_cox_multivariate(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ ARMCD + AGE + RACE,
#'   data = ADTTE_f, conf_level = 0.95
#' )
#'
#' t_cox_multivariate(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ (ARMCD + RACE + AGE)^2,
#'   data = ADTTE_f
#' ) # not quite there yet.
#' }
t_cox_multivariate <- function(formula, data,
                               conf_level = 0.95,
                               pval_method = c("wald", "likelihood"),
                               ...
) {

  y <- s_cox_multivariate(
    formula = formula, data = data,
    conf_level = conf_level,
    pval_method = pval_method,
    ...
    )

  mod <- y$mod
  msum <- y$msum
  aov <- y$aov
  coef_inter <- y$coef_inter

  all_term_labs <-  attr(mod$terms, "term.labels")
  term_labs <- all_term_labs[which(attr(mod$terms, "order") == 1)]
  spe_term  <- unlist(attr(mod$terms, "specials"))
  if (any(spe_term)) term_labs <- term_labs[- (spe_term - 1)]
  term_class <- attr(mod$terms, "dataClasses")[term_labs]
  is_term_factors <- term_class == "factor"

  tbl_header <- rheader(
    rrow("Effect/ ",            "Hazard", "", ""),
    rrow("Covariate included",  "Ratio for", "", ""),
    rrow("in the model",        "Treatment",  paste0(round(conf_level * 100, 0), "% CI*"), "p-value*")
  );

  make_a_headline <- function(label, is_term_factor){

    if (is_term_factor) {
      y <- paste0(label, " (Reference = ", levels(data[[label]])[1], ")")
    } else {
      y <- label
    }
    return(y)

  }
  headline <- Map(f = make_a_headline, label = term_labs, is_term_factor = is_term_factors)

  get_ref_pval <- function(term_labs, aov = aov) aov[term_labs, ncol(aov)]

  get_coefs <- function(term_labs, mod = mod, msum = msum){

    coef_rows <- mod$assign[[term_labs]]
    coefs <- matrix(
      cbind(msum$coefficients, msum$conf.int)[coef_rows, c(2, 8, 9, 5)],
      ncol = 4,
      dimnames = list(NULL, c("hr", "lcl", "ucl", "pval"))
    )
    return(coefs)

  }

  give_coef_names <- function(coefs, term_labs, is_term_factors){

    if (is_term_factors) rownames(coefs) <- levels(data[[term_labs]])[-1]
    else rownames(coefs) <- median(data[[term_labs]])
    return(coefs)

  }

  pval_cov <- vapply(term_labs, get_ref_pval, aov = aov, FUN.VALUE = 0.001)
  coefs <- lapply(term_labs, get_coefs, mod = mod, msum)
  coefs <- Map(f = give_coef_names, coefs = coefs, term_labs = term_labs, is_term_factors = is_term_factors)
  names(coefs) <- term_labs

  if (is.null(coef_inter)) {

    make_a_cov_chunk <- function(term_labs, pval){

      if (nrow(coefs[[term_labs]]) > 1) {
        effect_row <- rtable(
          header = tbl_header,
          rrow(headline[term_labs], NULL, NULL, rcell(pval_cov[term_labs], format = "xx.xxxx"))
        )
      } else {
        effect_row <- rtable(
          header = tbl_header,       rrow(headline[term_labs])
        )
      }

      row_content <- split(coefs[[term_labs]], rownames(coefs[[term_labs]]))
      row_content <- rtablel(
        header = tbl_header,
        Map(
          f = function(row, name) {
            rrow(
              name,
              rcell(row[1], format = "xx.xx"),             # Hazard Ratio
              rcell(row[c(2, 3)], format = "(xx.xx, xx.xx)"), # Confidence Interval
              rcell(row[4], format = "xx.xxxx")              # pval
            )
          }, row = row_content, name = names(row_content))
      )

      tbl <- rbind(effect_row, indent(row_content), rrow())
      return(tbl)

    }

    tbl <- do.call(rbind, lapply(term_labs, make_a_cov_chunk))
    tbl <- rbind(
      tbl,
      rrow("* Wald confidence interval/test")
    )
    return(tbl)

  } else {
    stop(
    "The tabulation for Multivariate Cox Model including interaction term estimations is not
    available. Alternatively, the results of such analysis are summarised if using
    `s_cox_multivariate()` instead of `t_cox_multivariate()`"
    )
    invisible()
  }
}
