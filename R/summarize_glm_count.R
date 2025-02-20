# summarize_glm_count ----------------------------------------------------------
#' Summarize Poisson negative binomial regression
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Summarize results of a Poisson negative binomial regression.
#' This can be used to analyze count and/or frequency data using a linear model.
#' It is specifically useful for analyzing count data (using the Poisson or Negative
#' Binomial distribution) that is result of a generalized linear model of one (e.g. arm) or more
#' covariates.
#'
#' @inheritParams h_glm_count
#' @inheritParams argument_convention
#' @param rate_mean_method (`character(1)`)\cr method used to estimate the mean odds ratio. Defaults to `emmeans`.
#'   see details for more information.
#' @param scale (`numeric(1)`)\cr linear scaling factor for rate and confidence intervals. Defaults to `1`.
#' @param .stats (`character`)\cr statistics to select for the table.
#'
#'   Options are: ``r shQuote(get_stats("summarize_glm_count"), type = "sh")``
#'
#' @details
#' `summarize_glm_count()` uses `s_glm_count()` to calculate the statistics for the table. This
#' analysis function uses [h_glm_count()] to estimate the GLM with [stats::glm()] for Poisson and Quasi-Poisson
#' distributions or [MASS::glm.nb()] for Negative Binomial distribution. All methods assume a
#' logarithmic link function.
#'
#' At this point, rates and confidence intervals are estimated from the model using
#' either [emmeans::emmeans()] when `rate_mean_method = "emmeans"` or [h_ppmeans()]
#' when `rate_mean_method = "ppmeans"`.
#'
#' If a reference group is specified while building the table with `split_cols_by(ref_group)`,
#' no rate ratio or `p-value` are calculated. Otherwise, we use [emmeans::contrast()] to
#' calculate the rate ratio and `p-value` for the reference group. Values are always estimated
#' with `method = "trt.vs.ctrl"` and `ref` equal to the first `arm` value.
#'
#' @name summarize_glm_count
NULL

#' @describeIn summarize_glm_count Layout-creating function which can take statistics function arguments
#'   and additional format arguments. This function is a wrapper for [rtables::analyze()].
#'
#' @return
#' * `summarize_glm_count()` returns a layout object suitable for passing to further layouting functions,
#'   or to [rtables::build_table()]. Adding this function to an `rtable` layout will add formatted rows containing
#'   the statistics from `s_glm_count()` to the table layout.
#'
#' @examples
#' library(dplyr)
#'
#' anl <- tern_ex_adtte %>% filter(PARAMCD == "TNE")
#' anl$AVAL_f <- as.factor(anl$AVAL)
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM", ref_group = "B: Placebo") %>%
#'   add_colcounts() %>%
#'   analyze_vars(
#'     "AVAL_f",
#'     var_labels = "Number of exacerbations per patient",
#'     .stats = c("count_fraction"),
#'     .formats = c("count_fraction" = "xx (xx.xx%)"),
#'     .labels = c("Number of exacerbations per patient")
#'   ) %>%
#'   summarize_glm_count(
#'     vars = "AVAL",
#'     variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL),
#'     conf_level = 0.95,
#'     distribution = "poisson",
#'     rate_mean_method = "emmeans",
#'     var_labels = "Adjusted (P) exacerbation rate (per year)",
#'     table_names = "adjP",
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
#'     table_names = "adjQP",
#'     .stats = c("rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
#'     .labels = c(
#'       rate = "Rate", rate_ci = "Rate CI", rate_ratio = "Rate Ratio",
#'       rate_ratio_ci = "Rate Ratio CI", pval = "p value"
#'     )
#'   ) %>%
#'   summarize_glm_count(
#'     vars = "AVAL",
#'     variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = c("REGION1")),
#'     conf_level = 0.95,
#'     distribution = "negbin",
#'     rate_mean_method = "emmeans",
#'     var_labels = "Adjusted (NB) exacerbation rate (per year)",
#'     table_names = "adjNB",
#'     .stats = c("rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval"),
#'     .labels = c(
#'       rate = "Rate", rate_ci = "Rate CI", rate_ratio = "Rate Ratio",
#'       rate_ratio_ci = "Rate Ratio CI", pval = "p value"
#'     )
#'   )
#'
#' build_table(lyt = lyt, df = anl)
#'
#' @export
summarize_glm_count <- function(lyt,
                                vars,
                                variables,
                                distribution,
                                conf_level,
                                rate_mean_method = c("emmeans", "ppmeans")[1],
                                weights = stats::weights,
                                scale = 1,
                                var_labels,
                                na_str = default_na_str(),
                                nested = TRUE,
                                ...,
                                show_labels = "visible",
                                table_names = vars,
                                .stats = get_stats("summarize_glm_count"),
                                .stat_names = NULL,
                                .formats = NULL,
                                .labels = NULL,
                                .indent_mods = list(
                                  "n" = 0L,
                                  "rate" = 0L,
                                  "rate_ci" = 1L,
                                  "rate_ratio" = 0L,
                                  "rate_ratio_ci" = 1L,
                                  "pval" = 1L
                                )) {
  checkmate::assert_choice(rate_mean_method, c("emmeans", "ppmeans"))

  # Process standard extra arguments
  extra_args <- list(".stats" = .stats)
  if (!is.null(.stat_names)) extra_args[[".stat_names"]] <- .stat_names
  if (!is.null(.formats)) extra_args[[".formats"]] <- .formats
  if (!is.null(.labels)) extra_args[[".labels"]] <- .labels
  if (!is.null(.indent_mods)) extra_args[[".indent_mods"]] <- .indent_mods

  # Process additional arguments to the statistic function
  extra_args <- c(
    extra_args,
    variables = list(variables), distribution = list(distribution), conf_level = list(conf_level),
    rate_mean_method = list(rate_mean_method), weights = list(weights), scale = list(scale),
    ...
  )

  # Append additional info from layout to the analysis function
  extra_args[[".additional_fun_parameters"]] <- get_additional_afun_params(add_alt_df = FALSE)
  formals(a_glm_count) <- c(formals(a_glm_count), extra_args[[".additional_fun_parameters"]])

  analyze(
    lyt = lyt,
    vars = vars,
    afun = a_glm_count,
    na_str = na_str,
    nested = nested,
    extra_args = extra_args,
    var_labels = var_labels,
    show_labels = show_labels,
    table_names = table_names
  )
}

#' @describeIn summarize_glm_count Statistics function that produces a named list of results
#'   of the investigated Poisson model.
#'
#' @return
#' * `s_glm_count()` returns a named `list` of 5 statistics:
#'   * `n`: Count of complete sample size for the group.
#'   * `rate`: Estimated event rate per follow-up time.
#'   * `rate_ci`: Confidence level for estimated rate per follow-up time.
#'   * `rate_ratio`: Ratio of event rates in each treatment arm to the reference arm.
#'   * `rate_ratio_ci`: Confidence level for the rate ratio.
#'   * `pval`: p-value.
#'
#' @keywords internal
s_glm_count <- function(df,
                        .var,
                        .df_row,
                        .ref_group,
                        .in_ref_col,
                        variables,
                        distribution,
                        conf_level,
                        rate_mean_method,
                        weights,
                        scale = 1,
                        ...) {
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

  # This happens if there is a reference col. No Ratio is calculated?
  if (.in_ref_col) {
    list(
      n = length(y[!is.na(y)]),
      rate = formatters::with_label(
        ifelse(distribution == "negbin", emmeans_smry_level$response * scale, emmeans_smry_level$rate * scale),
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
        ifelse(distribution == "negbin",
          emmeans_smry_level$response * scale,
          emmeans_smry_level$rate * scale
        ),
        "Adjusted Rate"
      ),
      rate_ci = formatters::with_label(
        c(emmeans_smry_level$asymp.LCL * scale, emmeans_smry_level$asymp.UCL * scale),
        f_conf_level(conf_level)
      ),
      rate_ratio = formatters::with_label(
        smry_contrasts_level$ratio,
        "Adjusted Rate Ratio"
      ),
      rate_ratio_ci = formatters::with_label(
        c(smry_contrasts_level$asymp.LCL, smry_contrasts_level$asymp.UCL),
        f_conf_level(conf_level)
      ),
      pval = formatters::with_label(
        smry_contrasts_level$p.value,
        "p-value"
      )
    )
  }
}

#' @describeIn summarize_glm_count Formatted analysis function which is used as `afun` in `summarize_glm_count()`.
#'
#' @return
#' * `a_glm_count()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @keywords internal
a_glm_count <- function(df,
                        ...,
                        .stats = NULL,
                        .stat_names = NULL,
                        .formats = NULL,
                        .labels = NULL,
                        .indent_mods = NULL) {
  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)
  extra_afun_params <- retrieve_extra_afun_params(names(dots_extra_args$.additional_fun_parameters))
  dots_extra_args$.additional_fun_parameters <- NULL

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_glm_count,
    custom_stat_fnc_list = NULL,
    args_list = c(
      df = list(df),
      extra_afun_params,
      dots_extra_args
    )
  )

  # Fill in formatting defaults
  .stats <- get_stats("summarize_glm_count", stats_in = .stats)
  .formats <- get_formats_from_stats(.stats, .formats)
  .labels <- get_labels_from_stats(.stats, .labels)
  .indent_mods <- get_indents_from_stats(.stats, .indent_mods)

  x_stats <- x_stats[.stats]

  # Auto format handling
  .formats <- apply_auto_formatting(.formats, x_stats, extra_afun_params$.df_row, extra_afun_params$.var)

  # Get and check statistical names
  .stat_names <- get_stat_names(x_stats, .stat_names)

  in_rows(
    .list = x_stats,
    .formats = .formats,
    .names = .labels %>% .unlist_keep_nulls(),
    .stat_names = .stat_names,
    .labels = .labels %>% .unlist_keep_nulls(),
    .indent_mods = .indent_mods %>% .unlist_keep_nulls()
  )
}

# h_glm_count ------------------------------------------------------------------

#' Helper functions for Poisson models
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper functions that returns the results of [stats::glm()] when Poisson or Quasi-Poisson
#' distributions are needed (see `family` parameter), or [MASS::glm.nb()] for Negative Binomial
#' distributions. Link function for the GLM is `log`.
#'
#' @inheritParams argument_convention
#'
#' @seealso [summarize_glm_count]
#'
#' @name h_glm_count
NULL

#' @describeIn h_glm_count Helper function to return the results of the
#'   selected model (Poisson, Quasi-Poisson, negative binomial).
#'
#' @param .df_row (`data.frame`)\cr dataset that includes all the variables that are called
#'   in `.var` and `variables`.
#' @param variables (named `list` of `string`)\cr list of additional analysis variables, with
#'   expected elements:
#'   * `arm` (`string`)\cr group variable, for which the covariate adjusted means of multiple
#'     groups will be summarized. Specifically, the first level of `arm` variable is taken as the
#'     reference group.
#'   * `covariates` (`character`)\cr a vector that can contain single variable names (such as
#'     `"X1"`), and/or interaction terms indicated by `"X1 * X2"`.
#'   * `offset` (`numeric`)\cr a numeric vector or scalar adding an offset.
#' @param distribution (`character`)\cr a character value specifying the distribution
#'   used in the regression (Poisson, Quasi-Poisson, negative binomial).
#' @param weights (`character`)\cr a character vector specifying weights used
#'   in averaging predictions. Number of weights must equal the number of levels included in the covariates.
#'   Weights option passed to [emmeans::emmeans()].
#'
#' @return
#' * `h_glm_count()` returns the results of the selected model.
#'
#' @keywords internal
h_glm_count <- function(.var,
                        .df_row,
                        variables,
                        distribution,
                        weights) {
  checkmate::assert_subset(distribution, c("poisson", "quasipoisson", "negbin"), empty.ok = FALSE)
  switch(distribution,
    poisson = h_glm_poisson(.var, .df_row, variables, weights),
    quasipoisson = h_glm_quasipoisson(.var, .df_row, variables, weights),
    negbin = h_glm_negbin(.var, .df_row, variables, weights)
  )
}

#' @describeIn h_glm_count Helper function to return results of a Poisson model.
#'
#' @return
#' * `h_glm_poisson()` returns the results of a Poisson model.
#'
#' @keywords internal
h_glm_poisson <- function(.var,
                          .df_row,
                          variables,
                          weights) {
  arm <- variables$arm
  covariates <- variables$covariates

  formula <- stats::as.formula(paste0(
    .var, " ~ ",
    " + ",
    paste(covariates, collapse = " + "),
    " + ",
    arm
  ))

  if (is.null(variables$offset)) {
    glm_fit <- stats::glm(
      formula = formula,
      data = .df_row,
      family = stats::poisson(link = "log")
    )
  } else {
    offset <- .df_row[[variables$offset]]
    glm_fit <- stats::glm(
      formula = formula,
      offset = offset,
      data = .df_row,
      family = stats::poisson(link = "log")
    )
  }

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

#' @describeIn h_glm_count Helper function to return results of a Quasi-Poisson model.
#'
#' @return
#' * `h_glm_quasipoisson()` returns the results of a Quasi-Poisson model.
#'
#' @keywords internal
h_glm_quasipoisson <- function(.var,
                               .df_row,
                               variables,
                               weights) {
  arm <- variables$arm
  covariates <- variables$covariates

  formula <- stats::as.formula(paste0(
    .var, " ~ ",
    " + ",
    paste(covariates, collapse = " + "),
    " + ",
    arm
  ))

  if (is.null(variables$offset)) {
    glm_fit <- stats::glm(
      formula = formula,
      data = .df_row,
      family = stats::quasipoisson(link = "log")
    )
  } else {
    offset <- .df_row[[variables$offset]]
    glm_fit <- stats::glm(
      formula = formula,
      offset = offset,
      data = .df_row,
      family = stats::quasipoisson(link = "log")
    )
  }
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

#' @describeIn h_glm_count Helper function to return results of a negative binomial model.
#'
#' @return
#' * `h_glm_negbin()` returns the results of a negative binomial model.
#'
#' @keywords internal
h_glm_negbin <- function(.var,
                         .df_row,
                         variables,
                         weights) {
  arm <- variables$arm
  covariates <- variables$covariates
  formula <- stats::as.formula(paste0(
    .var, " ~ ",
    " + ",
    paste(covariates, collapse = " + "),
    " + ",
    arm
  ))

  if (is.null(variables$offset)) {
    formula <- stats::as.formula(paste0(
      .var, " ~ ",
      " + ",
      paste(covariates, collapse = " + "),
      " + ",
      arm
    ))
  } else {
    offset <- variables$offset
    formula_txt <- sprintf(
      "%s ~ %s + %s + offset(%s)",
      .var,
      arm, paste0(covariates, collapse = " + "), offset
    )
    formula <- stats::as.formula(
      formula_txt
    )
  }

  glm_fit <- MASS::glm.nb(
    formula = formula,
    data = .df_row,
    link = "log"
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

# h_ppmeans --------------------------------------------------------------------
#' Function to return the estimated means using predicted probabilities
#'
#' @description
#' For each arm level, the predicted mean rate is calculated using the fitted model object, with `newdata`
#' set to the result of `stats::model.frame`, a reconstructed data or the original data, depending on the
#' object formula (coming from the fit). The confidence interval is derived using the `conf_level` parameter.
#'
#' @param obj (`glm.fit`)\cr fitted model object used to derive the mean rate estimates in each treatment arm.
#' @param .df_row (`data.frame`)\cr dataset that includes all the variables that are called in `.var` and `variables`.
#' @param arm (`string`)\cr group variable, for which the covariate adjusted means of multiple groups will be
#'   summarized. Specifically, the first level of `arm` variable is taken as the reference group.
#' @param conf_level (`proportion`)\cr value used to derive the confidence interval for the rate.
#'
#' @return
#' * `h_ppmeans()` returns the estimated means.
#'
#' @seealso [summarize_glm_count()].
#'
#' @export
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
