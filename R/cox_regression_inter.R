
#' Cox Regression Helper: Interactions
#'
#' @description `r lifecycle::badge("stable")`
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
#'
#' @name cox_regression_inter
#'
#' @examples
#' # Testing dataset [survival::bladder].
#' library(survival)
#' library(rtables)
#'
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
#'       levels = 1:4,
#'       labels = c("F", "F", "M", "M")
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
#'
#' @export
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
#'
#' @param at (`list`)\cr a list with items named after the covariate, every
#'   item is a vector of levels at which the interaction should be estimated.
#' @export
h_coxreg_inter_effect.numeric <- function(x, # nolint
                                          effect,
                                          covar,
                                          mod,
                                          label,
                                          control,
                                          at,
                                          ...) {
  betas <- stats::coef(mod)
  attrs <- attr(stats::terms(mod), "term.labels")
  term_indices <- grep(
    pattern = effect,
    x = attrs[!grepl("strata\\(", attrs)]
  )
  checkmate::assert_int(length(term_indices), lower = 2, upper = 2)
  betas <- betas[term_indices]
  betas_var <- diag(stats::vcov(mod))[term_indices]
  betas_cov <- stats::vcov(mod)[term_indices[1], term_indices[2]]
  xval <- if (is.null(at[[covar]])) {
    stats::median(x)
  } else {
    at[[covar]]
  }
  effect_index <- !grepl(covar, names(betas))
  coef_hat <- betas[effect_index] + xval * betas[!effect_index]
  coef_se <- sqrt(
    betas_var[effect_index] +
      xval ^ 2 * betas_var[!effect_index] + # styler: off
      2 * xval * betas_cov
  )
  q_norm <- stats::qnorm((1 + control$conf_level) / 2)
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
#' @export
h_coxreg_inter_effect.factor <- function(x, # nolint
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
#'
#' @export
#'
#' @examples
#' library(survival)
#'
#' mod <- coxph(Surv(time, status) ~ armcd * covar1, data = dta_bladder)
#' h_coxreg_extract_interaction(
#'   mod = mod, effect = "armcd", covar = "covar1", data = dta_bladder,
#'   control = control_coxreg()
#' )
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
    mod_aov <- muffled_car_anova(mod, test_statistic)
    sum_anova <- broom::tidy(mod_aov)
    pval <- sum_anova[sum_anova$term == effect, ][["p.value"]]

    # Test the interaction effect.
    pval_inter <- sum_anova[grep(":", sum_anova$term), ][["p.value"]]
    covar_test <- data.frame(
      effect = "Covariate:",
      term = covar,
      term_label = unname(labels_or_names(as.list(data[var], all.names = TRUE))),
      level = "",
      n = mod$n, hr = NA, lcl = NA, ucl = NA, pval = pval,
      pval_inter = pval_inter,
      stringsAsFactors = FALSE
    )
    # Estimate the interaction.
    y <- h_coxreg_inter_effect(
      data[[covar]],
      covar = covar,
      effect = effect,
      mod = mod,
      label = unname(labels_or_names(as.list(data[var], all.names = TRUE))),
      at = at,
      control = control,
      data = data
    )
    rbind(covar_test, y)
  }
}

#' @describeIn cox_regression_inter hazard ratio estimation in interactions.
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
#' @export
#'
#' @examples
#' # Testing dataset [survival::bladder].
#' library(survival)
#'
#' mod <- coxph(Surv(time, status) ~ armcd * covar1, data = dta_bladder)
#' result <- h_coxreg_inter_estimations(
#'   variable = "armcd", given = "covar1",
#'   lvl_var = levels(dta_bladder$armcd),
#'   lvl_given = levels(dta_bladder$covar1),
#'   mod = mod, conf_level = .95
#' )
#' result
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
    }
  )
  colnames(design_mat) <- interaction_names

  coef <- stats::coef(mod)
  vcov <- stats::vcov(mod)
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
    }
  )
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
