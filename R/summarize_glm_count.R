#' Summary for Poisson Negative Binomial.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Summarize results of a Poisson Negative Binomial Regression.
#' This can be used to analyze count and/or frequency data using a linear model.
#'
#' @name summarize_glm_count
#'
NULL

#' Helper Functions for Poisson Models.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper functions that can be used to return the results of various Poisson models.
#'
#' @seealso [summarize_glm_count]
#'
#' @name h_glm_count
#'
NULL

#' @describeIn h_glm_count Helper function to return results of a poisson model.
#' @inheritParams argument_convention
#' @param .df_row (`data frame`)\cr data set that includes all the variables that are called
#'   in `.var` and `variables`.
#' @param variables (named `list` of `strings`)\cr list of additional analysis variables, with
#'   expected elements:
#'   - `arm`: (`string`)\cr group variable, for which the covariate adjusted means of multiple
#'   groups will be summarized. Specifically, the first level of `arm` variable is taken as the
#'   reference group.
#'   - `covariates`: (`character`)\cr a vector that can contain single variable names (such as
#'   `"X1"`), and/or interaction terms indicated by `"X1 * X2"`.
#'   - `offset`: (`numeric`)\cr a numeric vector or scalar adding an offset.
#' @param `weights`(`character`)\cr a character vector specifying weights used
#'  in averaging predictions. Number of weights must equal the number of levels included in the covariates.
#'  Weights option passed to emmeans function (hyperlink) (link to emmeans documentation)
#'
#' @examples
#'
#' # Internal function - h_glm_poisson
#' \dontrun{
#' h_glm_poisson(
#'   .var = "AVAL",
#'   .df_row = anl,
#'   variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL)
#' )
#' }
#'
#' @keywords internal
h_glm_poisson <- function(.var,
                          .df_row,
                          variables,
                          weights) {
  arm <- variables$arm
  covariates <- variables$covariates
  offset <- .df_row[[variables$offset]]

  formula <- stats::as.formula(paste0(
    .var, " ~ ",
    " + ",
    paste(covariates, collapse = " + "),
    " + ",
    arm
  ))

  glm_fit <- stats::glm(
    formula = formula,
    offset = offset,
    data = .df_row,
    family = stats::poisson(link = "log")
  )

  emmeans_fit <- emmeans::emmeans(
    glm_fit,
    specs = arm,
    data = .df_row,
    type = "response",
    offset = 0,
    weights = weights
  )

  list(
    glm_fit = glm_fit,
    emmeans_fit = emmeans_fit
  )
}

#' @describeIn h_glm_count Helper function to return results of a quasipoisson model.
#' @inheritParams argument_convention
#' @inheritParams summarize_glm_count
#'
#' @examples
#'
#' # Internal function - h_glm_quasipoisson
#' \dontrun{
#' h_glm_quasipoisson(
#'   .var = "AVAL",
#'   .df_row = anl,
#'   variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("REGION1"))
#' )
#' }
#'
#' @keywords internal
h_glm_quasipoisson <- function(.var,
                               .df_row,
                               variables,
                               weights) {
  arm <- variables$arm
  covariates <- variables$covariates
  offset <- .df_row[[variables$offset]]

  formula <- stats::as.formula(paste0(
    .var, " ~ ",
    " + ",
    paste(covariates, collapse = " + "),
    " + ",
    arm
  ))

  glm_fit <- stats::glm(
    formula = formula,
    offset = offset,
    data = .df_row,
    family = stats::quasipoisson(link = "log")
  )

  emmeans_fit <- emmeans::emmeans(
    glm_fit,
    specs = arm,
    data = .df_row,
    type = "response",
    offset = 0,
    weights = weights
  )

  list(
    glm_fit = glm_fit,
    emmeans_fit = emmeans_fit
  )
}

#' @describeIn h_glm_count Helper function to return the results of the
#' selected model (poisson, quasipoisson, negative binomial).
#' @inheritParams argument_convention
#' @param .df_row (`data frame`)\cr data set that includes all the variables that are called
#'   in `.var` and `variables`.
#' @param variables (named `list` of `strings`)\cr list of additional analysis variables, with
#'   expected elements:
#'   - `arm`: (`string`)\cr group variable, for which the covariate adjusted means of multiple
#'   groups will be summarized. Specifically, the first level of `arm` variable is taken as the
#'   reference group.
#'   - `covariates`: (`character`)\cr a vector that can contain single variable names (such as
#'   `"X1"`), and/or interaction terms indicated by `"X1 * X2"`.
#'   - `offset`: (`numeric`)\cr a numeric vector or scalar adding an offset.
#' @param `weights`(`character`)\cr character vector specifying weights used in averaging predictions.
#' @param `distribution`(`character`)\cr a character value specifying the distribution
#' used in the regression (poisson, quasipoisson).
#'
#'
#' @examples
#'
#' # Internal function - h_glm_count
#' \dontrun{
#' h_glm_count(
#'   .var = "AVAL",
#'   .df_row = anl,
#'   variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = NULL),
#'   distribution = "poisson"
#' )
#' }
#'
#' @keywords internal
h_glm_count <- function(.var,
                        .df_row,
                        variables,
                        distribution,
                        weights) {
  if (distribution == "negbin") {
    stop("negative binomial distribution is not currently available.")
  }
  switch(distribution,
    poisson = h_glm_poisson(.var, .df_row, variables, weights),
    quasipoisson = h_glm_quasipoisson(.var, .df_row, variables, weights),
    negbin = list() # h_glm_negbin(.var, .df_row, variables, weights) # nolint
  )
}


#' @describeIn h_glm_count Helper function to return the estimated means.
#' @inheritParams argument_convention
#' @param .df_row (`data frame`)\cr data set that includes all the variables that are called
#'   in `.var` and `variables`.
# `list` of `strings`)\cr list of model fitting results.
#' @param conf_level (`numeric`) value used to derive the confidence interval for the rate.
#' @param obj (`glm.fit`) fitted model object used to derive the mean rate estimates in each treatment arm.
#' @param `arm`: (`string`)\cr group variable, for which the covariate adjusted means of multiple
#'   groups will be summarized. Specifically, the first level of `arm` variable is taken as the
#'   reference group.
#'
#' @examples
#'
#' # Internal function - h_ppmeans
#' \dontrun{
#' fits <- h_glm_count(
#'   .var = "AVAL",
#'   .df_row = anl,
#'   variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = c("REGION1")),
#'   distribution = "quasipoisson"
#' )
#'
#' h_ppmeans(
#'   obj = fits$glm_fit,
#'   .df_row = anl,
#'   arm = "ARM",
#'   conf_level = 0.95
#' )
#' }
#'
#' @keywords internal
h_ppmeans <- function(obj, .df_row, arm, conf_level) {
  alpha <- 1 - conf_level
  p <- 1 - alpha / 2

  arm_levels <- levels(.df_row[[arm]])

  out <- lapply(arm_levels, function(lev) {
    temp <- .df_row
    temp[[arm]] <- factor(lev, levels = arm_levels)

    mf <- stats::model.frame(obj$formula, data = temp)
    X <- stats::model.matrix(obj$formula, data = mf) # nolint

    rate <- stats::predict(obj, newdata = mf, type = "response")
    rate_hat <- mean(rate)

    zz <- colMeans(rate * X)
    se <- sqrt(as.numeric(t(zz) %*% stats::vcov(obj) %*% zz))
    rate_lwr <- rate_hat * exp(-stats::qnorm(p) * se / rate_hat)
    rate_upr <- rate_hat * exp(stats::qnorm(p) * se / rate_hat)

    c(rate_hat, rate_lwr, rate_upr)
  })

  names(out) <- arm_levels
  out <- do.call(rbind, out)
  if ("negbin" %in% class(obj)) {
    colnames(out) <- c("response", "asymp.LCL", "asymp.UCL")
  } else {
    colnames(out) <- c("rate", "asymp.LCL", "asymp.UCL")
  }
  out <- as.data.frame(out)
  out[[arm]] <- rownames(out)
  out
}

#' @describeIn summarize_glm_count Statistics function that produces a named list of results
#'   of the investigated poisson model.
#' @inheritParams argument_convention
#' @inheritParams h_glm_count
#'
#' @return A named list of 5 statistics:
#'   - `n`: count of complete sample size for the group.
#'   - `rate`: estimated event rate per follow-up time.
#'   - `rate_ci`: confidence level for estimated rate per follow-up time.
#'   - `rate_ratio`: Ratio of event rates in each treatment arm to the reference arm.
#'   - `rate_ratio_ci`: confidence level for the rate ratio.
#'   - `pval`: p-value.
#'
#'
#' @examples
#'
#' # Internal function - s_change_from_baseline
#' \dontrun{
#' s_glm_count(
#'   df = anl %>%
#'     filter(ARMCD == "ARM B"),
#'   .df_row = anl,
#'   .var = "AVAL",
#'   .in_ref_col = TRUE,
#'   variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = c("REGION1")),
#'   conf_level = 0.95,
#'   distribution = "quasipoisson",
#'   rate_mean_method = "ppmeans"
#' )
#' }
#'
#' @keywords internal
s_glm_count <- function(df,
                        .var,
                        .df_row,
                        variables,
                        .ref_group,
                        .in_ref_col,
                        distribution,
                        conf_level,
                        rate_mean_method,
                        weights,
                        scale = 1) {
  arm <- variables$arm

  y <- df[[.var]]
  smry_level <- as.character(unique(df[[arm]]))

  # ensure there is only 1 value
  checkmate::assert_scalar(smry_level)

  results <- h_glm_count(
    .var = .var,
    .df_row = .df_row,
    variables = variables,
    distribution = distribution,
    weights
  )

  if (rate_mean_method == "emmeans") {
    emmeans_smry <- summary(results$emmeans_fit, level = conf_level)
  } else if (rate_mean_method == "ppmeans") {
    emmeans_smry <- h_ppmeans(results$glm_fit, .df_row, arm, conf_level)
  }

  emmeans_smry_level <- emmeans_smry[emmeans_smry[[arm]] == smry_level, ]

  if (.in_ref_col) {
    list(
      n = length(y[!is.na(y)]),
      rate = formatters::with_label(
        ifelse(distribution == "negbin", emmeans_smry_level$response * scale, emmeans_smry_level$rate),
        "Adjusted Rate"
      ),
      rate_ci = formatters::with_label(
        c(emmeans_smry_level$asymp.LCL * scale, emmeans_smry_level$asymp.UCL * scale),
        f_conf_level(conf_level)
      ),
      rate_ratio = formatters::with_label(character(), "Adjusted Rate Ratio"),
      rate_ratio_ci = formatters::with_label(character(), f_conf_level(conf_level)),
      pval = formatters::with_label(character(), "p-value")
    )
  } else {
    emmeans_contrasts <- emmeans::contrast(
      results$emmeans_fit,
      method = "trt.vs.ctrl",
      ref = grep(
        as.character(unique(.ref_group[[arm]])),
        as.data.frame(results$emmeans_fit)[[arm]]
      )
    )

    contrasts_smry <- summary(
      emmeans_contrasts,
      infer = TRUE,
      adjust = "none"
    )

    smry_contrasts_level <- contrasts_smry[grepl(smry_level, contrasts_smry$contrast), ]

    list(
      n = length(y[!is.na(y)]),
      rate = formatters::with_label(
        ifelse(distribution == "negbin", emmeans_smry_level$response * scale, emmeans_smry_level$rate),
        "Adjusted Rate"
      ),
      rate_ci = formatters::with_label(
        c(emmeans_smry_level$asymp.LCL * scale, emmeans_smry_level$asymp.UCL * scale),
        f_conf_level(conf_level)
      ),
      rate_ratio = formatters::with_label(smry_contrasts_level$ratio, "Adjusted Rate Ratio"),
      rate_ratio_ci = formatters::with_label(
        c(smry_contrasts_level$asymp.LCL, smry_contrasts_level$asymp.UCL),
        f_conf_level(conf_level)
      ),
      pval = formatters::with_label(smry_contrasts_level$p.value, "p-value")
    )
  }
}

#' @describeIn summarize_glm_count Formatted Analysis function which can be further customized by calling
#'   [rtables::make_afun()] on it. It is used as `afun` in [rtables::analyze()].
#'
#' @examples
#'
#' # Internal function - s_change_from_baseline
#' \dontrun{
#' a_glm_count(
#'   df = anl %>%
#'     filter(ARMCD == "ARM A"),
#'   .var = "AVAL",
#'   .df_row = anl,
#'   variables = list(arm = "ARMCD", offset = "lgTMATRSK", covariates = c("REGION1")),
#'   .ref_group = "ARM B", .in_ref_col = TRUE,
#'   conf_level = 0.95,
#'   distribution = "poisson",
#'   rate_mean_method = "ppmeans"
#' )
#' }
#'
#' @keywords internal
a_glm_count <- make_afun(
  s_glm_count,
  .indent_mods = c(
    "n" = 0L,
    "rate" = 0L,
    "rate_ci" = 1L,
    "rate_ratio" = 0L,
    "rate_ratio_ci" = 1L,
    "pval" = 1L
  ),
  .formats = c(
    "n" = "xx",
    "rate" = "xx.xxxx",
    "rate_ci" = "(xx.xxxx, xx.xxxx)",
    "rate_ratio" = "xx.xxxx",
    "rate_ratio_ci" = "(xx.xxxx, xx.xxxx)",
    "pval" = "x.xxxx | (<0.0001)"
  ),
  .null_ref_cells = FALSE
)

#' @describeIn summarize_glm_count Layout creating function which can be be used for creating
#'   summary tables for analysis of count data using generalized linear models (poisson, quasipoisson).
#' @inheritParams argument_convention
#' @export
#' @examples
#' library(dplyr)
#' anl <- ex_adtte %>% filter(PARAMCD == "TNE")
#' anl$AVAL_f <- as.factor(anl$AVAL)
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM", ref_group = "B: Placebo") %>%
#'   add_colcounts() %>%
#'   summarize_vars(
#'     "AVAL_f",
#'     var_labels = "Number of exacerbations per patient",
#'     .stats = c("count_fraction"),
#'     .formats = c("count_fraction" = "xx (xx.xx%)"),
#'     .label = c("Number of exacerbations per patient")
#'   ) %>%
#'   summarize_glm_count(
#'     vars = "AVAL",
#'     variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL),
#'     conf_level = 0.95,
#'     distribution = "poisson",
#'     rate_mean_method = "emmeans",
#'     var_labels = "Unadjusted exacerbation rate (per year)",
#'     table_names = "unadj",
#'     .stats = c("rate"),
#'     .labels = c(rate = "Rate")
#'   ) %>%
#'   summarize_glm_count(
#'     vars = "AVAL",
#'     variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("REGION1")),
#'     conf_level = 0.95,
#'     distribution = "quasipoisson",
#'     rate_mean_method = "ppmeans",
#'     var_labels = "Adjusted (QP) exacerbation rate (per year)",
#'     table_names = "adj",
#'     .stats = c("rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
#'     .labels = c(
#'       rate = "Rate", rate_ci = "Rate CI", rate_ratio = "Rate Ratio",
#'       rate_ratio_ci = "Rate Ratio CI", pval = "p value"
#'     )
#'   )
#' build_table(lyt = lyt, df = anl)
summarize_glm_count <- function(lyt,
                                vars,
                                var_labels,
                                ...,
                                show_labels = "visible",
                                table_names = vars,
                                .stats = NULL,
                                .formats = NULL,
                                .labels = NULL,
                                .indent_mods = NULL) {
  afun <- make_afun(
    a_glm_count,
    .stats = .stats,
    .formats = .formats,
    .labels = .labels,
    .indent_mods = .indent_mods
  )

  analyze(
    lyt,
    vars,
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names,
    afun = afun,
    extra_args = list(...)
  )
}
