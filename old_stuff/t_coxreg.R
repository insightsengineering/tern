#' Cox proportional hazards regression for standard outputs
#'
#' Cox models are the most commonly used methods to estimate the magnitude of
#' the effect in survival analysis. It assumes proportional hazards: the ratio
#' of the hazards of between groups (e.g., two arms) is constant over time.
#' This ratio is referred to as the "hazard ratio" (HR) and is one of the
#' most commonly reported metrics to describe the effect size in survival
#' analysis.
#'
#' @param formula A `formula` corresponding to the
#' [survival model][survival::Surv()]:
#' + the left-hand side must include `Surv(time, event)` with time and event
#'   describing occurrence and censoring (see [Surv()][survival::Surv()]).
#' + the right-hand side includes predictors and *specials* which can be used
#'   to specify a pairwise model or a range of candidate univariate models.
#'   See `details`.
#' @param data A `data.frame` which includes all the variables that are called
#'   in `formula`.
#' @param simplify If `TRUE`, returns a `rtables` object instead of a list of
#'   `rtables object` when the variable contained in `pairwise` has only two
#'   levels to compare.
#' @param conf_level The confidence level of the interval.
#' @param pval_method The method used for the estimation of p.values, should be
#'   one of `wald` (default) or `likelihood`. The `log-rank` is accepted for
#'   models including a single variable, specified through [pairwise()].
#' @param increments If a quantitative variable is included, it is possible to
#'   provide the expected level of estimation for the interaction. If provided,
#'   it should be list where each item is a vector giving expected levels and is
#'   named after the variable name as it appears in the `formula`.
#' @param ... Optional other arguments passed to the Cox regression:
#' + `ties` a character string specifying the method for tie handling, one of
#'   `exact` (default), `efron`, `breslow`.
#' + see [coxph()][survival::coxph()] for additional settings.
#'
#' @details
#'   Possible model specifications:
#'
#'   * `Surv(time, event) ~ pairwise(Pred)`, used to get hazard ratio of each
#'     tested level of treatment `Pred` independently, given in reference to
#'     the control treatment (see [pairwise()]).
#'   * `Surv(time, event) ~ pairwise(Pred) + univariate(Cov1, Cov2, ...)`, add
#'     candidate covariates separated by comas in special [univariate()] to
#'     test all bivariate combinations of `Pred` with each `Cov`. Replacing the
#'     symbol `+` by `*` will result in the additional estimation of the
#'     interaction terms. This is similar to `COXT01` standards.
#'   * `Surv(time, event) ~ Pred + Cov1 + ... ` estimates the simple effect of
#'     multiple Cox regression. This provides the `COXT02` standard output.
#'     Note that the variables in returned tabulation are matched by position
#'     in `formula`.
#'
#'   Known limits:
#'
#'   + the *special* [univariate()] requires [pairwise()] being also specified.
#'   + the interaction terms in multiple regression are not estimated.
#'   + the third-order interactions are not estimated.
#'
#'   For further information about the Cox Proportional Hazards Model, check
#'   "Statistical Analysis of Clinical Trials Data with R", NEST team.
#'
#' @return Depending on `formula`, returns an `rtable` object for every
#'   level of [pairwise()] if included in `COXT01`; a single `rtable` otherwise.
#'
#' @export
#'
#' @seealso [t_coxph_pairwise()], [t_cox_univariate()], [t_cox_multivariate()].
#'
#' @md
#'
#' @examples
#' library(tern)
#' library(random.cdisc.data)
#' ADTTE   <- radtte(cached = TRUE)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS") # _f: filtered
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
#' t_coxreg(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ pairwise(ARMCD),
#'   data = ADTTE_f, conf_level = 0.8, pval_method = "likelihood",
#'   ties = "breslow"
#' )
#'
#' ## COXT01 - Standard output, no interactions
#' t_coxreg(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ pairwise(ARMCD) +
#'   univariate(SEX, RACE, AGE),
#'   data = ADTTE_f
#' )
#'
#' \dontrun{
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
#' }
#'
#' ## COXT01 - options: control ties, set confidence interval level,
#' ##   choose a strata, modify pval_method
#' t_coxreg(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ pairwise(ARMCD) + strata(SEX) +
#'     univariate(RACE, AGE),
#'   data = ADTTE_f, conf_level = 0.80,
#'   ties = c("exact", "efron", "breslow")[3], pval_method  = c("wald", "likelihood")[2]
#' )
#'
#' \dontrun{
#' # For comparison
#' mod20 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + strata(SEX), data = ADTTE_f)
#' mod21 <- coxph(
#'   Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + strata(SEX) + AGE, data = ADTTE_f
#' )
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
#' }
#'
#' \dontrun{
#' ## COXT01 - Addition of the interaction and increments to choose estimated levels
#' t_coxreg(
#'     formula =  Surv(time = AVAL, event = 1 - CNSR) ~
#'       pairwise(ARMCD) * univariate(RACE, SEX, AGE, X),
#'     data = ADTTE_f, conf_level = 0.74, increments = list(X = c(-1, 0, 2))
#' )
#'
#' ## COXT01: Other example, other data
#' library(dplyr)
#' ADSL <- radsl(cached = TRUE)
#' ADSL <- ADSL %>% dplyr::filter(SEX %in% c("F", "M"))
#'
#' ADTTE <- radtte(ADSL, seed = 2) %>%
#'     dplyr::filter(PARAMCD == "PFS" & ARMCD != "ARM C")
#' ADTTE$ARMCD <- droplevels(ADTTE$ARMCD)
#' ADTTE$SEX <- droplevels(ADTTE$SEX)
#' t_coxreg(
#'     formula = Surv(time = AVAL, event = 1 - CNSR) ~
#'       pairwise(ARMCD) * univariate(SEX, BMRKR2, AGE),
#'     data = ADTTE,
#'     increments = list(AGE = mean(ADTTE$AGE)),
#'     conf_level = 0.975)
#'
#' # check HRs
#' m2 <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ ARMCD*BMRKR2, data = ADTTE)
#' coef(m2)
#'
#' # For COXT02
#' t_coxreg(
#'     formula = Surv(time = AVAL, event = 1 - CNSR) ~ ARMCD + RACE + AGE +
#'     strata(SEX), data = ADTTE_f,
#'     conf_level = 0.8, pval_method = "likelihood",  ties = "breslow"
#'  )
#' }
t_coxreg <- function(formula, data,
                     simplify = TRUE, conf_level = 0.95,
                     pval_method = c("wald",  "likelihood", "log-rank"),
                     increments = NULL, ...) {

  check_formula(formula)
  pval_method <- match.arg(pval_method)

  if (!is.logical(simplify)) stop("Simplify should be either TRUE or FALSE.")

  tf <- terms(formula, specials = c("strata", "pairwise", "univariate"))
  tf_factor <- attr(tf, "factors")

  form_order <- colSums(tf_factor)
  if (max(form_order) > 2) stop("Check formula.
  The integration of 3-way interaction in Cox regression is discouraged,
  use `survival::coxph()` if as required by the scope of the study.")

  pairwise <- explicit_special(formula_terms = tf, special = "pairwise")
  strat    <- explicit_special(formula_terms = tf, special = "strata")
  univ     <- explicit_special(formula_terms = tf, special = "univariate")

  if (!is.null(pairwise)) {
    n_lvl_pairwise <- nlevels(data[[pairwise$v]])
    if (n_lvl_pairwise < 2) stop("The variable in `pairwise` needs at least two levels.")
  }

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
      y <- t_coxph_pairwise(
        formula = formula, data = data, conf_level = conf_level,
        pval_method = pval_method, ...
      )
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
          conf_level = conf_level, pval_method = pval_method,
          interactions = max(form_order) == 2,
          increments = increments,
          ...
        )
      }
    )

    if (simplify & length(y) == 1) {
      y <- y[[1]]
    }

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
    y <- t_cox_multivariate(
      formula = formula, data = data, conf_level = conf_level,
      pval_method = pval_method,
      ...
    )

    return(y)

  }

  stop("The arguments don't match a standard use case. Please, refers to `?t_coxreg`.")

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
#'   formula = Sur   values = beta_ci, name = names(beta_ci)
)v(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + strata(SEX),
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
#' ADSL <- ADSL %>% dplyr::filter(SEX %in% c("F", "M"))
#'
#' ADTTE <- radtte(ADSL, seed = 2) %>%
#'   dplyr::filter(PARAMCD == "PFS" & ARMCD != "ARM C")
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

  # The result output differs widely depending on the inclusion of the
  # interaction term.
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
      rrow("# Model including treatment, "),
      rrow("  covariate, no interaction."),
      rrow(paste0("* ", capitalize(y$pval_method), " CI/test")),
      rrow(paste("Ties:", capitalize(y$method)))
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
      rrow("# Model including treatment,"),
      rrow("  covariate, interaction"),
      rrow(paste0("* ", capitalize(y$pval_method), " CI/test")),
      rrow("** Likelihood-ratio test"),
      rrow(paste("Ties:", capitalize(y$method)))
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
#' )
#' }
t_cox_multivariate <- function(formula, data,
                               conf_level = 0.95,
                               pval_method = c("wald", "likelihood"),
                               ...
) {

  tf         <- terms(formula, specials = c("strata"))
  covariates <- rownames(attr(tf, "factors"))[-c(1, unlist(attr(tf, "specials")))]
  lapply(
    X   = covariates,
    FUN = function(x) {
      if (is.character(data[[x]]))
        data[[x]] <<- as.factor(data[[x]])
      invisible()
    }
  )
  pval_method <- match.arg(pval_method)

  y <- s_cox_multivariate(
    formula = formula, data = data,
    conf_level = conf_level,
    pval_method = pval_method,
    ...
  )

  mod  <- y$mod
  msum <- y$msum
  aov  <- y$aov
  coef_inter <- y$coef_inter

  all_term_labs <-  attr(mod$terms, "term.labels")
  term_labs <- all_term_labs[which(attr(mod$terms, "order") == 1)]
  spe_term  <- unlist(attr(mod$terms, "specials"))
  if (any(spe_term)) term_labs <- term_labs[- (spe_term - 1)]
  term_class <- attr(mod$terms, "dataClasses")[term_labs]
  is_term_factors <- term_class == "factor"

  strat    <- explicit_special(
    formula_terms = terms(formula, specials = c("strata")),
    special = "strata"
  )

  tbl_header <- rheader(
    rrow("Effect/ ", " ", "", ""),
    rrow("Covariate included", "Hazard", "", ""),
    rrow(
      "in the model", "Ratio",
      paste0(round(conf_level * 100, 0), "% CI*"),
      "p-value*"
    )
  )

  make_a_headline <- function(label, is_term_factor) {
    y <- if (is_term_factor) {
      paste0(label, " (Reference = ", levels(data[[label]])[1], ")")
    } else {
      label
    }
    return(y)
  }

  headline <- Map(
    f = make_a_headline,
    label = term_labs,
    is_term_factor = is_term_factors
  )

  get_ref_pval <- function(term_labs, aov = aov) aov[term_labs, ncol(aov)]

  get_coefs <- function(term_labs, mod = mod, msum = msum) {
    coef_rows <- mod$assign[[term_labs]]
    coefs <- matrix(
      cbind(msum$coefficients, msum$conf.int)[coef_rows, c(2, 8, 9, 5)],
      ncol = 4,
      dimnames = list(NULL, c("hr", "lcl", "ucl", "pval"))
    )
    return(coefs)
  }

  give_coef_names <- function(coefs, term_labs, is_term_factors) {
    if (is_term_factors) rownames(coefs) <- levels(data[[term_labs]])[-1]
    else rownames(coefs) <- term_labs
    return(coefs)
  }

  pval_cov <- vapply(term_labs, get_ref_pval, aov = aov, FUN.VALUE = 0.001)
  coefs <- lapply(term_labs, get_coefs, mod = mod, msum)
  coefs <- Map(
    f = give_coef_names,
    coefs = coefs,
    term_labs = term_labs,
    is_term_factors = is_term_factors
  )
  names(coefs) <- term_labs

  if (is.null(coef_inter)) {

    make_effect_row <- function(row, name) {
      rrow(
        name,
        rcell(row[1], format = "xx.xx"),                # Hazard Ratio
        rcell(row[c(2, 3)], format = "(xx.xx, xx.xx)"), # Confidence Interval
        rcell(row[4], format = "xx.xxxx")               # pval
      )
    }

    make_a_cov_chunk <- function(term_labs, pval) {

      if (nrow(coefs[[term_labs]]) > 1) {
        effect_row <- rtable(
          header = tbl_header,
          rrow(
            headline[term_labs], NULL, NULL,
            rcell(pval_cov[term_labs], format = "xx.xxxx")
          )
        )
      } else {
        effect_row <- rtable(
          header = tbl_header, rrow(headline[term_labs])
        )
      }

      if (is.numeric(data[[term_labs]])) {

        row_content <- rtable(
          header = tbl_header,
          make_effect_row(
            row = coefs[[term_labs]],
            name = rownames(coefs[[term_labs]])
          )
        )

        tbl <- rbind(row_content, rrow())

      } else {

        row_content <- split(coefs[[term_labs]], rownames(coefs[[term_labs]]))
        if (length(row_content) == 1) {
          row_content[[1]][4] <- pval_cov[term_labs]
        }
        row_content <- rtablel(
          header = tbl_header,
          Map(f = make_effect_row, row = row_content, name = names(row_content))
        )

        tbl <- rbind(effect_row, indent(row_content), rrow())

      }

      return(tbl)

    }

    tbl <- do.call(rbind, lapply(term_labs, make_a_cov_chunk))
    footer <-   switch(
      pval_method,
      wald = "* Wald CI/test",
      likelihood = "* Wald CI, likelihood ratio test"
    )

    tbl <- rbind(
      tbl,
      rrow(footer),
      rrow(paste("Ties:", capitalize(mod$method)))
    )

    if (!is.null(strat)) {
      tbl <- rbind(
        tbl,
        rrow(paste("Stratified by: ", strat$v))
      )
    }

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
