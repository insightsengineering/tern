#' Multi-variable logistic regression table
#'
#' Logistic regression for binary outcome with categorical/continuous covariates in model statement.
#' For each covariate category (if categorical) or specified values (if continuous), present degrees of freedom,
#' regression parameter estimate and standard error (SE) relative to reference group or category.
#' Report odds ratios for each covariate category or specified values and corresponding Wald
#' confidence intervals as default but allow user to specify other confidence levels.
#' Report p-value for Wald chi-square test of the null hypothesis that covariate has no effect on
#' response in model containing all specified covariates.
#' Allow option to include one two-way interaction and present similar output for
#' each interaction degree of freedom.
#' Note: For the formula, the variable names need to be standard dataframe column name without
#' special characters.
#'
#' @section Model Specification:
#'
#' The `variables` list needs to include the following elements:\cr
#' - `arm`: usual treatment arm variable name.
#' - `response`: the response arm variable name. Usually this is a 0/1 variable.
#' - `covariates`: this is either `NULL` (no covariates) or
#'      a character vector of covariate variable names.
#' - `interaction`: this is either `NULL` (no interaction) or a string of a single
#'      covariate variable name already included in `covariates`. Then the interaction
#'      with the treatment arm is included in the model.
#'
#' @inheritParams argument_convention
#' @param fit_glm (`glm`)\cr logistic regression model fitted by [stats::glm()] with "binomial" family.
#'   Limited functionality is also available for conditional logistic regression models fitted by
#'   [survival::clogit()], currently this is used only by [extract_rsp_biomarkers()].
#' @param x (`string` or `character`)\cr a variable or interaction term in `fit_glm` (depending on the
#'   helper function).
#' @name logistic_regression
#'
NULL

#' @describeIn logistic_regression Fit a (conditional) logistic regression model.
#' @param data (`data frame`)\cr the data frame on which the model was fit.
#' @param response_definition (`string`)\cr the definition of what an event is in terms of `response`.
#'   This will be used when fitting the (conditional) logistic regression model on the left hand
#'   side of the formula.
#'
#' @details Note this function may hang or error for certain datasets when an old version of the
#'   survival package (< 3.2-13) is used.
#' @export
#'
#' @examples
#' library(scda)
#' library(dplyr)
#'
#' adrs <- synthetic_cdisc_data("latest")$adrs
#' adrs_f <- adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   filter(RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")) %>%
#'   mutate(
#'     Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
#'     RACE = factor(RACE),
#'     SEX = factor(SEX)
#'   )
#' var_labels(adrs_f) <- c(var_labels(adrs), Response = "Response")
#' mod1 <- fit_logistic(
#'   data = adrs_f,
#'   variables = list(
#'     response = "Response",
#'     arm = "ARMCD",
#'     covariates = c("AGE", "RACE")
#'   )
#' )
#' mod2 <- fit_logistic(
#'   data = adrs_f,
#'   variables = list(
#'     response = "Response",
#'     arm = "ARMCD",
#'     covariates = c("AGE", "RACE"),
#'     interaction = "AGE"
#'   )
#' )
fit_logistic <- function(data,
                         variables = list(
                           response = "Response",
                           arm = "ARMCD",
                           covariates = NULL,
                           interaction = NULL,
                           strata = NULL
                         ),
                         response_definition = "response") {
  assertthat::assert_that(
    is.list(variables),
    all(names(variables) %in% c("response", "arm", "covariates", "interaction", "strata")),
    is_df_with_variables(data, as.list(unlist(variables))),
    assertthat::is.string(response_definition),
    grepl("response", response_definition)
  )

  response_definition <- sub(
    pattern = "response",
    replacement = variables$response,
    x = response_definition,
    fixed = TRUE
  )
  form <- paste0(response_definition, " ~ ", variables$arm)
  if (!is.null(variables$covariates)) {
    form <- paste0(form, " + ", paste(variables$covariates, collapse = " + "))
  }
  if (!is.null(variables$interaction)) {
    assertthat::assert_that(
      assertthat::is.string(variables$interaction),
      variables$interaction %in% variables$covariates
    )
    form <- paste0(form, " + ", variables$arm, ":", variables$interaction)
  }
  if (!is.null(variables$strata)) {
    strata_arg <- if (length(variables$strata) > 1) {
      paste0("I(interaction(", paste0(variables$strata, collapse = ", "), "))")
    } else {
      variables$strata
    }
    form <- paste0(form, "+ strata(", strata_arg, ")")
  }
  formula <- stats::as.formula(form)
  if (is.null(variables$strata)) {
    stats::glm(
      formula = formula,
      data = data,
      family = stats::binomial("logit")
    )
  } else {
    clogit_with_tryCatch(
      formula = formula,
      data = data,
      x = TRUE
    )
  }
}

#' @describeIn logistic_regression Helper function to extract interaction variable names from a fitted
#'   model assuming only one interaction term.
#'
h_get_interaction_vars <- function(fit_glm) {
  assertthat::assert_that("glm" %in% class(fit_glm))
  terms_name <- attr(stats::terms(fit_glm), "term.labels")
  terms_order <- attr(stats::terms(fit_glm), "order")
  interaction_term <- terms_name[terms_order == 2]
  assertthat::assert_that(assertthat::is.string(interaction_term))
  strsplit(interaction_term, split = ":")[[1]]
}

#' @describeIn logistic_regression Helper function to get the right coefficient name from the
#'   interaction variable names and the given levels. The main value here is that the order
#'   of first and second variable is checked in the `interaction_vars` input.
#' @param interaction_vars (`character` of length 2)\cr interaction variable names.
#' @param first_var_with_level (`character` of length 2)\cr the first variable name with
#'   the interaction level.
#' @param second_var_with_level (`character` of length 2)\cr the second variable name with
#'   the interaction level.
#'
h_interaction_coef_name <- function(interaction_vars,
                                    first_var_with_level,
                                    second_var_with_level) {

  checkmate::assert_character(interaction_vars, len = 2)
  checkmate::assert_character(first_var_with_level, len = 2)
  checkmate::assert_character(second_var_with_level, len = 2)

  assertthat::assert_that(
    all(c(first_var_with_level[1], second_var_with_level[1]) %in% interaction_vars)
  )
  first_name <- paste(first_var_with_level, collapse = "")
  second_name <- paste(second_var_with_level, collapse = "")
  if (first_var_with_level[1] == interaction_vars[1]) {
    paste(first_name, second_name, sep = ":")
  } else if (second_var_with_level[1] == interaction_vars[1]) {
    paste(second_name, first_name, sep = ":")
  }
}

#' @describeIn logistic_regression Helper function to calculate the odds ratio estimates
#'   for the case when both the odds ratio and the interaction variable are categorical.
#' @param odds_ratio_var (`string`)\cr the odds ratio variable.
#' @param interaction_var (`string`)\cr the interaction variable.
#'
h_or_cat_interaction <- function(odds_ratio_var,
                                 interaction_var,
                                 fit_glm,
                                 conf_level = 0.95) {
  interaction_vars <- h_get_interaction_vars(fit_glm)
  assertthat::assert_that(
    assertthat::is.string(odds_ratio_var),
    assertthat::is.string(interaction_var),
    all(c(odds_ratio_var, interaction_var) %in% interaction_vars),
    identical(length(interaction_vars), 2L)
  )
  xs_level <- fit_glm$xlevels
  xs_coef <- stats::coef(fit_glm)
  xs_vcov <- stats::vcov(fit_glm)
  y <- list()
  for (var_level in xs_level[[odds_ratio_var]][-1]) {
    x <- list()
    for (ref_level in xs_level[[interaction_var]]) {
      coef_names <- paste0(odds_ratio_var, var_level)
      if (ref_level != xs_level[[interaction_var]][1]) {
        interaction_coef_name <- h_interaction_coef_name(
          interaction_vars,
          c(odds_ratio_var, var_level),
          c(interaction_var, ref_level)
        )
        coef_names <- c(
          coef_names,
          interaction_coef_name
        )
      }
      if (length(coef_names) > 1) {
        ones <- t(c(1, 1))
        est <- as.numeric(ones %*% xs_coef[coef_names])
        se <- sqrt(as.numeric(ones %*% xs_vcov[coef_names, coef_names] %*% t(ones)))
      } else {
        est <- xs_coef[coef_names]
        se <- sqrt(as.numeric(xs_vcov[coef_names, coef_names]))
      }
      or <- exp(est)
      ci <- exp(est + c(lcl = -1, ucl = 1) * stats::qnorm((1 + conf_level) / 2) * se)
      x[[ref_level]] <- list(or = or, ci = ci)
    }
    y[[var_level]] <- x
  }
  y
}

#' @describeIn logistic_regression Helper function to calculate the odds ratio estimates
#'   for the case when either the odds ratio or the interaction variable is continuous.
#' @param at (`NULL` or `numeric`)\cr optional values for the interaction variable. Otherwise
#'   the median is used.
#' @note We don't provide a function for the case when both variables are continuous because
#'   this does not arise in this table, as the treatment arm variable will always be involved
#'   and categorical.
#'
h_or_cont_interaction <- function(odds_ratio_var,
                                  interaction_var,
                                  fit_glm,
                                  at = NULL,
                                  conf_level = 0.95) {
  interaction_vars <- h_get_interaction_vars(fit_glm)
  assertthat::assert_that(
    assertthat::is.string(odds_ratio_var),
    assertthat::is.string(interaction_var),
    all(c(odds_ratio_var, interaction_var) %in% interaction_vars),
    identical(length(interaction_vars), 2L)
  )
  checkmate::assert_numeric(at, min.len = 1, null.ok = TRUE)
  xs_level <- fit_glm$xlevels
  xs_coef <- stats::coef(fit_glm)
  xs_vcov <- stats::vcov(fit_glm)
  xs_class <- attr(fit_glm$terms, "dataClasses")
  model_data <- fit_glm$model
  if (!is.null(at)) {
    assertthat::assert_that(
      xs_class[interaction_var] == "numeric"
    )
  }
  y <- list()
  if (xs_class[interaction_var] == "numeric") {
    if (is.null(at)) {
      at <- ceiling(stats::median(model_data[[interaction_var]]))
    }

    for (var_level in xs_level[[odds_ratio_var]][-1]) {
      x <- list()
      for (increment in at) {
        coef_names <- paste0(odds_ratio_var, var_level)
        if (increment != 0) {
          interaction_coef_name <- h_interaction_coef_name(
            interaction_vars,
            c(odds_ratio_var, var_level),
            c(interaction_var, "")
          )
          coef_names <- c(
            coef_names,
            interaction_coef_name
          )
        }
        if (length(coef_names) > 1) {
          xvec <- t(c(1, increment))
          est <- as.numeric(xvec %*% xs_coef[coef_names])
          se <- sqrt(as.numeric(xvec %*% xs_vcov[coef_names, coef_names] %*% t(xvec)))
        } else {
          est <- xs_coef[coef_names]
          se <- sqrt(as.numeric(xs_vcov[coef_names, coef_names]))
        }
        or <- exp(est)
        ci <- exp(est + c(lcl = -1, ucl = 1) * stats::qnorm((1 + conf_level) / 2) * se)
        x[[as.character(increment)]] <- list(or = or, ci = ci)
      }
      y[[var_level]] <- x
    }
  } else {
    assertthat::assert_that(
      xs_class[odds_ratio_var] == "numeric",
      xs_class[interaction_var] == "factor"
    )
    for (var_level in xs_level[[interaction_var]]) {
      coef_names <- odds_ratio_var
      if (var_level != xs_level[[interaction_var]][1]) {
        interaction_coef_name <- h_interaction_coef_name(
          interaction_vars,
          c(odds_ratio_var, ""),
          c(interaction_var, var_level)
        )
        coef_names <- c(
          coef_names,
          interaction_coef_name
        )
      }
      if (length(coef_names) > 1) {
        xvec <- t(c(1, 1))
        est <- as.numeric(xvec %*% xs_coef[coef_names])
        se <- sqrt(as.numeric(xvec %*% xs_vcov[coef_names, coef_names] %*% t(xvec)))
      } else {
        est <- xs_coef[coef_names]
        se <- sqrt(as.numeric(xs_vcov[coef_names, coef_names]))
      }
      or <- exp(est)
      ci <- exp(est + c(lcl = -1, ucl = 1) * stats::qnorm((1 + conf_level) / 2) * se)
      y[[var_level]] <- list(or = or, ci = ci)
    }
  }
  y
}

#' @describeIn logistic_regression Helper function to calculate the odds ratio estimates
#'   in case of an interaction. This is a wrapper for [h_or_cont_interaction()] and
#'   [h_or_cat_interaction()].
#'
h_or_interaction <- function(odds_ratio_var,
                             interaction_var,
                             fit_glm,
                             at = NULL,
                             conf_level = 0.95) {
  xs_class <- attr(fit_glm$terms, "dataClasses")
  if (any(xs_class[c(odds_ratio_var, interaction_var)] == "numeric")) {
    h_or_cont_interaction(
      odds_ratio_var,
      interaction_var,
      fit_glm,
      at = at,
      conf_level = conf_level
    )
  } else if (all(xs_class[c(odds_ratio_var, interaction_var)] == "factor")) {
    h_or_cat_interaction(
      odds_ratio_var,
      interaction_var,
      fit_glm,
      conf_level = conf_level
    )
  } else {
    stop("wrong interaction variable class, the interaction variable is not a numeric nor a factor")
  }
}

#' @describeIn logistic_regression Helper function to construct term labels from simple terms and the table
#'   of numbers of patients.
#' @param terms (`character`)\cr simple terms.
#' @param table (`table`)\cr table containing numbers for terms.
#'
h_simple_term_labels <- function(terms,
                                 table) {
  assertthat::assert_that(
    is_character_or_factor(terms),
    is.table(table)
  )
  terms <- as.character(terms)
  term_n <- table[terms]
  paste0(terms, ", n = ", term_n)
}

#' @describeIn logistic_regression Helper function to construct term labels from interaction terms and the table
#'   of numbers of patients.
#' @param terms1 (`character`)\cr terms for first dimension (rows).
#' @param terms2 (`character`)\cr terms for second dimension (rows).
#' @param any (`flag`)\cr whether any of `term1` and `term2` can be fulfilled to count the
#'   number of patients. In that case they can only be scalar (strings).
#'
h_interaction_term_labels <- function(terms1,
                                      terms2,
                                      table,
                                      any = FALSE) {
  assertthat::assert_that(
    is_character_or_factor(terms1),
    is_character_or_factor(terms2),
    is.table(table),
    assertthat::is.flag(any)
  )
  terms1 <- as.character(terms1)
  terms2 <- as.character(terms2)
  if (any) {
    assertthat::assert_that(assertthat::is.scalar(terms1), assertthat::is.scalar(terms2))
    paste0(
      terms1, " or ", terms2, ", n = ",
      # Note that we double count in the initial sum the cell [terms1, terms2], therefore subtract.
      sum(c(table[terms1, ], table[, terms2])) - table[terms1, terms2]
    )
  } else {
    term_n <- table[cbind(terms1, terms2)]
    paste0(terms1, " * ", terms2, ", n = ", term_n)
  }
}

#' @describeIn logistic_regression Helper function to tabulate the main effect
#'   results of a (conditional) logistic regression model.
#'
#' @export
#'
#' @examples
#' h_glm_simple_term_extract("AGE", mod1)
#' h_glm_simple_term_extract("ARMCD", mod1)
h_glm_simple_term_extract <- function(x, fit_glm) {
  assertthat::assert_that(
    inherits(fit_glm, c("glm", "clogit")),
    assertthat::is.string(x)
  )
  xs_class <- attr(fit_glm$terms, "dataClasses")
  xs_level <- fit_glm$xlevels
  xs_coef <- summary(fit_glm)$coefficients
  stats <- if (inherits(fit_glm, "glm")) {
    c("estimate" = "Estimate", "std_error" = "Std. Error", "pvalue" = "Pr(>|z|)")
  } else {
    c("estimate" = "coef", "std_error" = "se(coef)", "pvalue" = "Pr(>|z|)")
  }
  # Make sure x is not an interaction term.
  assertthat::assert_that(x %in% names(xs_class))
  x_sel <- if (xs_class[x] == "numeric") x else paste0(x, xs_level[[x]][-1])
  x_stats <- as.data.frame(xs_coef[x_sel, stats, drop = FALSE], stringsAsFactors = FALSE)
  colnames(x_stats) <- names(stats)
  x_stats$estimate <- as.list(x_stats$estimate)
  x_stats$std_error <- as.list(x_stats$std_error)
  x_stats$pvalue <- as.list(x_stats$pvalue)
  x_stats$df <- as.list(1)
  if (xs_class[x] == "numeric") {
    x_stats$term <- x
    x_stats$term_label <- if (inherits(fit_glm, "glm")) {
      var_labels(fit_glm$data[x], fill = TRUE)
    } else {
      # We just fill in here with the `term` itself as we don't have the data available.
      x
    }
    x_stats$is_variable_summary <- FALSE
    x_stats$is_term_summary <- TRUE
  } else {
    assertthat::assert_that(
      inherits(fit_glm, "glm"),
      msg = "for non-numeric variables only glm models are supported"
    )
    # The reason is that we don't have the original data set in the `clogit` object
    # and therefore cannot determine the `x_numbers` here.
    x_numbers <- table(fit_glm$data[[x]])
    x_stats$term <- xs_level[[x]][-1]
    x_stats$term_label <- h_simple_term_labels(x_stats$term, x_numbers)
    x_stats$is_variable_summary <- FALSE
    x_stats$is_term_summary <- TRUE
    main_effects <- car::Anova(fit_glm, type = 3, test.statistic = "Wald")
    x_main <- data.frame(
      pvalue = main_effects[x, "Pr(>Chisq)", drop = TRUE],
      term = xs_level[[x]][1],
      term_label = paste("Reference", h_simple_term_labels(xs_level[[x]][1], x_numbers)),
      df = main_effects[x, "Df", drop = TRUE],
      stringsAsFactors = FALSE
    )
    x_main$pvalue <- as.list(x_main$pvalue)
    x_main$df <- as.list(x_main$df)
    x_main$estimate <- list(numeric(0))
    x_main$std_error <- list(numeric(0))
    if (length(xs_level[[x]][-1]) == 1) {
      x_main$pvalue <- list(numeric(0))
      x_main$df <- list(numeric(0))
    }
    x_main$is_variable_summary <- TRUE
    x_main$is_term_summary <- FALSE
    x_stats <- rbind(x_main, x_stats)
  }
  x_stats$variable <- x
  x_stats$variable_label <- if (inherits(fit_glm, "glm")) {
    var_labels(fit_glm$data[x], fill = TRUE)
  } else {
    x
  }
  x_stats$interaction <- ""
  x_stats$interaction_label <- ""
  x_stats$reference <- ""
  x_stats$reference_label <- ""
  rownames(x_stats) <- NULL
  x_stats[c(
    "variable",
    "variable_label",
    "term",
    "term_label",
    "interaction",
    "interaction_label",
    "reference",
    "reference_label",
    "estimate",
    "std_error",
    "df",
    "pvalue",
    "is_variable_summary",
    "is_term_summary"
  )]
}

#' @describeIn logistic_regression Helper function to tabulate the interaction term
#'   results of a logistic regression model.
#' @export
#' @examples
#' h_glm_interaction_extract("ARMCD:AGE", mod2)
h_glm_interaction_extract <- function(x, fit_glm) {
  vars <- h_get_interaction_vars(fit_glm)
  xs_class <- attr(fit_glm$terms, "dataClasses")
  assertthat::assert_that(
    assertthat::is.string(x)
  )
  # Only take two-way interaction
  assertthat::assert_that(
    length(vars) == 2,
    # Only consider simple case: first variable in interaction is arm, a categorical variable
    xs_class[vars[1]] != "numeric"
  )
  xs_level <- fit_glm$xlevels
  xs_coef <- summary(fit_glm)$coefficients
  main_effects <- car::Anova(fit_glm, type = 3, test.statistic = "Wald")
  stats <- c("estimate" = "Estimate", "std_error" = "Std. Error", "pvalue" = "Pr(>|z|)")
  v1_comp <- xs_level[[vars[1]]][-1]
  if (xs_class[vars[2]] == "numeric") {
    x_stats <- as.data.frame(
      xs_coef[paste0(vars[1], v1_comp, ":", vars[2]), stats, drop = FALSE],
      stringsAsFactors = FALSE
    )
    colnames(x_stats) <- names(stats)
    x_stats$term <- v1_comp
    x_numbers <- table(fit_glm$data[[vars[1]]])
    x_stats$term_label <- h_simple_term_labels(v1_comp, x_numbers)
    v1_ref <- xs_level[[vars[1]]][1]
    term_main <- v1_ref
    ref_label <- h_simple_term_labels(v1_ref, x_numbers)
  } else if (xs_class[vars[2]] != "numeric") {
    v2_comp <- xs_level[[vars[2]]][-1]
    v1_v2_grid <- expand.grid(v1 = v1_comp, v2 = v2_comp)
    x_sel <- paste(
      paste0(vars[1], v1_v2_grid$v1),
      paste0(vars[2], v1_v2_grid$v2),
      sep = ":"
    )
    x_stats <- as.data.frame(xs_coef[x_sel, stats, drop = FALSE], stringsAsFactors = FALSE)
    colnames(x_stats) <- names(stats)
    x_stats$term <- paste(v1_v2_grid$v1, "*", v1_v2_grid$v2)
    x_numbers <- table(fit_glm$data[[vars[1]]], fit_glm$data[[vars[2]]])
    x_stats$term_label <- h_interaction_term_labels(v1_v2_grid$v1, v1_v2_grid$v2, x_numbers)
    v1_ref <- xs_level[[vars[1]]][1]
    v2_ref <- xs_level[[vars[2]]][1]
    term_main <- paste(vars[1], vars[2], sep = " * ")
    ref_label <- h_interaction_term_labels(v1_ref, v2_ref, x_numbers, any = TRUE)
  }
  x_stats$df <- as.list(1)
  x_stats$pvalue <- as.list(x_stats$pvalue)
  x_stats$is_variable_summary <- FALSE
  x_stats$is_term_summary <- TRUE
  x_main <- data.frame(
    pvalue = main_effects[x, "Pr(>Chisq)", drop = TRUE],
    term = term_main,
    term_label = paste("Reference", ref_label),
    df = main_effects[x, "Df", drop = TRUE],
    stringsAsFactors = FALSE
  )
  x_main$pvalue <- as.list(x_main$pvalue)
  x_main$df <- as.list(x_main$df)
  x_main$estimate <- list(numeric(0))
  x_main$std_error <- list(numeric(0))
  x_main$is_variable_summary <- TRUE
  x_main$is_term_summary <- FALSE

  x_stats <- rbind(x_main, x_stats)
  x_stats$variable <- x
  x_stats$variable_label <- paste(
    "Interaction of",
    var_labels(fit_glm$data[vars[1]], fill = TRUE),
    "*",
    var_labels(fit_glm$data[vars[2]], fill = TRUE)
  )
  x_stats$interaction <- ""
  x_stats$interaction_label <- ""
  x_stats$reference <- ""
  x_stats$reference_label <- ""
  rownames(x_stats) <- NULL
  x_stats[c(
    "variable",
    "variable_label",
    "term",
    "term_label",
    "interaction",
    "interaction_label",
    "reference",
    "reference_label",
    "estimate",
    "std_error",
    "df",
    "pvalue",
    "is_variable_summary",
    "is_term_summary"
  )]
}

#' @describeIn logistic_regression Helper function to tabulate the interaction
#'   results of a logistic regression model. This basically is a wrapper for
#'   [h_or_interaction()] and [h_glm_simple_term_extract()] which puts the results
#'   in the right data frame format.
#' @export
#' @examples
#' h_glm_inter_term_extract("AGE", "ARMCD", mod2)
h_glm_inter_term_extract <- function(odds_ratio_var,
                                     interaction_var,
                                     fit_glm,
                                     ...) {
  # First obtain the main effects.
  main_stats <- h_glm_simple_term_extract(odds_ratio_var, fit_glm)
  main_stats$is_reference_summary <- FALSE
  main_stats$odds_ratio <- NA
  main_stats$lcl <- NA
  main_stats$ucl <- NA

  # Then we get the odds ratio estimates and put into df form.
  or_numbers <- h_or_interaction(odds_ratio_var, interaction_var, fit_glm, ...)
  is_num_or_var <- attr(fit_glm$terms, "dataClasses")[odds_ratio_var] == "numeric"

  if (is_num_or_var) {
    # Numeric OR variable case.
    references <- names(or_numbers)
    n_ref <- length(references)

    extract_from_list <- function(l, name, pos = 1) {
      unname(unlist(
        lapply(or_numbers, function(x) {
          x[[name]][pos]
        })
      ))
    }
    or_stats <- data.frame(
      variable = odds_ratio_var,
      variable_label = unname(var_labels(fit_glm$data[odds_ratio_var], fill = TRUE)),
      term = odds_ratio_var,
      term_label = unname(var_labels(fit_glm$data[odds_ratio_var], fill = TRUE)),
      interaction = interaction_var,
      interaction_label = unname(var_labels(fit_glm$data[interaction_var], fill = TRUE)),
      reference = references,
      reference_label = references,
      estimate = NA,
      std_error = NA,
      odds_ratio = extract_from_list(or_numbers, "or"),
      lcl = extract_from_list(or_numbers, "ci", pos = "lcl"),
      ucl = extract_from_list(or_numbers, "ci", pos = "ucl"),
      df = NA,
      pvalue = NA,
      is_variable_summary = FALSE,
      is_term_summary = FALSE,
      is_reference_summary = TRUE
    )
  } else {
    # Categorical OR variable case.
    references <- names(or_numbers[[1]])
    n_ref <- length(references)

    extract_from_list <- function(l, name, pos = 1) {
      unname(unlist(
        lapply(or_numbers, function(x) {
          lapply(x, function(y) y[[name]][pos])
        })
      ))
    }
    or_stats <- data.frame(
      variable = odds_ratio_var,
      variable_label = unname(var_labels(fit_glm$data[odds_ratio_var], fill = TRUE)),
      term = rep(names(or_numbers), each = n_ref),
      term_label = h_simple_term_labels(rep(names(or_numbers), each = n_ref), table(fit_glm$data[[odds_ratio_var]])),
      interaction = interaction_var,
      interaction_label = unname(var_labels(fit_glm$data[interaction_var], fill = TRUE)),
      reference = unlist(lapply(or_numbers, names)),
      reference_label = unlist(lapply(or_numbers, names)),
      estimate = NA,
      std_error = NA,
      odds_ratio = extract_from_list(or_numbers, "or"),
      lcl = extract_from_list(or_numbers, "ci", pos = "lcl"),
      ucl = extract_from_list(or_numbers, "ci", pos = "ucl"),
      df = NA,
      pvalue = NA,
      is_variable_summary = FALSE,
      is_term_summary = FALSE,
      is_reference_summary = TRUE
    )
  }

  df <- rbind(
    main_stats[, names(or_stats)],
    or_stats
  )
  df[order(-df$is_variable_summary, df$term, -df$is_term_summary, df$reference), ]
}

#' @describeIn logistic_regression Helper function to tabulate the results including
#'   odds ratios and confidence intervals of simple terms.
#' @export
#' @examples
#' h_logistic_simple_terms("AGE", mod1)
h_logistic_simple_terms <- function(x, fit_glm, conf_level = 0.95) {
  assertthat::assert_that(inherits(fit_glm, c("glm", "clogit")))
  if (inherits(fit_glm, "glm")) {
    assertthat::assert_that(fit_glm$family$family == "binomial")
  }
  terms_name <- attr(stats::terms(fit_glm), "term.labels")
  xs_class <- attr(fit_glm$terms, "dataClasses")
  interaction <- terms_name[which(!terms_name %in% names(xs_class))]
  assertthat::assert_that(all(x %in% terms_name))
  if (length(interaction) != 0) {
    # Make sure any item in x is not part of interaction term
    assertthat::assert_that(all(!x %in% unlist(strsplit(interaction, ":"))))
  }
  x_stats <- lapply(x, h_glm_simple_term_extract, fit_glm)
  x_stats <- do.call(rbind, x_stats)
  q_norm <- stats::qnorm((1 + conf_level) / 2)
  x_stats$odds_ratio <- lapply(x_stats$estimate, exp)
  x_stats$lcl <- Map(function(or, se) exp(log(or) - q_norm * se), x_stats$odds_ratio, x_stats$std_error)
  x_stats$ucl <- Map(function(or, se) exp(log(or) + q_norm * se), x_stats$odds_ratio, x_stats$std_error)
  x_stats$ci <- Map(function(lcl, ucl) c(lcl, ucl), lcl = x_stats$lcl, ucl = x_stats$ucl)
  x_stats
}

#' @describeIn logistic_regression Helper function to tabulate the results including
#'   odds ratios and confidence intervals of interaction terms.
#' @export
#' @examples
#' h_logistic_inter_terms(c("RACE", "AGE", "ARMCD", "AGE:ARMCD"), mod2)
h_logistic_inter_terms <- function(x,
                                   fit_glm,
                                   conf_level = 0.95,
                                   at = NULL) {
  # Find out the interaction variables and interaction term.
  inter_vars <- h_get_interaction_vars(fit_glm)
  assertthat::assert_that(identical(length(inter_vars), 2L))
  inter_term_index <- intersect(grep(inter_vars[1], x), grep(inter_vars[2], x))
  inter_term <- x[inter_term_index]

  # For the non-interaction vars we need the standard stuff.
  normal_terms <- setdiff(x, union(inter_vars, inter_term))

  x_stats <- lapply(normal_terms, h_glm_simple_term_extract, fit_glm)
  x_stats <- do.call(rbind, x_stats)
  q_norm <- stats::qnorm((1 + conf_level) / 2)
  x_stats$odds_ratio <- lapply(x_stats$estimate, exp)
  x_stats$lcl <- Map(function(or, se) exp(log(or) - q_norm * se), x_stats$odds_ratio, x_stats$std_error)
  x_stats$ucl <- Map(function(or, se) exp(log(or) + q_norm * se), x_stats$odds_ratio, x_stats$std_error)
  normal_stats <- x_stats
  normal_stats$is_reference_summary <- FALSE

  # Now the interaction term itself.
  inter_term_stats <- h_glm_interaction_extract(inter_term, fit_glm)
  inter_term_stats$odds_ratio <- NA
  inter_term_stats$lcl <- NA
  inter_term_stats$ucl <- NA
  inter_term_stats$is_reference_summary <- FALSE

  is_intervar1_numeric <- attr(fit_glm$terms, "dataClasses")[inter_vars[1]] == "numeric"

  # Interaction stuff.
  inter_stats_one <- h_glm_inter_term_extract(
    inter_vars[1],
    inter_vars[2],
    fit_glm,
    conf_level = conf_level,
    at = `if`(is_intervar1_numeric, NULL, at)
  )
  inter_stats_two <- h_glm_inter_term_extract(
    inter_vars[2],
    inter_vars[1],
    fit_glm,
    conf_level = conf_level,
    at = `if`(is_intervar1_numeric, at, NULL)
  )

  # Now just combine everything in one data frame.
  col_names <- c(
    "variable",
    "variable_label",
    "term",
    "term_label",
    "interaction",
    "interaction_label",
    "reference",
    "reference_label",
    "estimate",
    "std_error",
    "df",
    "pvalue",
    "odds_ratio",
    "lcl",
    "ucl",
    "is_variable_summary",
    "is_term_summary",
    "is_reference_summary"
  )
  df <- rbind(
    inter_stats_one[, col_names],
    inter_stats_two[, col_names],
    inter_term_stats[, col_names]
  )
  if (length(normal_terms) > 0) {
    df <- rbind(
      normal_stats[, col_names],
      df
    )
  }
  df$ci <- combine_vectors(df$lcl, df$ucl)
  df
}

#' @describeIn logistic_regression Helper method (for [broom::tidy()]) to prepare a data frame from an
#'   `glm` object with `binomial` family.
#' @param fit_glm logistic regression model fitted by [stats::glm()] with "binomial" family.
#' @method tidy glm
#' @export
#' @examples
#' library(broom)
#' df <- tidy(mod1, conf_level = 0.99)
#' df2 <- tidy(mod2, conf_level = 0.99)
tidy.glm <- function(fit_glm, # nolint #nousage
                     conf_level = 0.95,
                     at = NULL) {
  assertthat::assert_that(
    "glm" %in% class(fit_glm),
    fit_glm$family$family == "binomial"
  )
  terms_name <- attr(stats::terms(fit_glm), "term.labels")
  xs_class <- attr(fit_glm$terms, "dataClasses")
  interaction <- terms_name[which(!terms_name %in% names(xs_class))]
  df <- if (length(interaction) == 0) {
    h_logistic_simple_terms(
      x = terms_name,
      fit_glm = fit_glm,
      conf_level = conf_level
    )
  } else {
    h_logistic_inter_terms(
      x = terms_name,
      fit_glm = fit_glm,
      conf_level = conf_level,
      at = at
    )
  }
  for (var in c("variable", "term", "interaction", "reference")) {
    df[[var]] <- factor(df[[var]], levels = unique(df[[var]]))
  }
  df
}

#' @describeIn logistic_regression Layout creating function for a multi-variable column layout summarizing
#'   logistic regression results.
#' @export
#'
logistic_regression_cols <- function(lyt,
                                     conf_level = 0.95) {
  vars <- c("df", "estimate", "std_error", "odds_ratio", "ci", "pvalue")
  var_labels <- c(
    df = "Degrees of Freedom",
    estimate = "Parameter Estimate",
    std_error = "Standard Error",
    odds_ratio = "Odds Ratio",
    ci = paste("Wald", f_conf_level(conf_level)),
    pvalue = "p-value"
  )
  split_cols_by_multivar(
    lyt = lyt,
    vars = vars,
    varlabels = var_labels
  )
}

#' @describeIn logistic_regression Constructor for content functions to be used to summarize
#'   logistic regression results.
#' @param flag_var (`string`)\cr variable name identifying which row should be used in this
#'   content function.
#' @export
#'
logistic_summary_by_flag <- function(flag_var) {
  assertthat::assert_that(assertthat::is.string(flag_var))
  function(lyt) {
    cfun_list <- list(
      df = cfun_by_flag("df", flag_var, format = "xx."),
      estimate = cfun_by_flag("estimate", flag_var, format = "xx.xxx"),
      std_error = cfun_by_flag("std_error", flag_var, format = "xx.xxx"),
      odds_ratio = cfun_by_flag("odds_ratio", flag_var, format = ">999.99"),
      ci = cfun_by_flag("ci", flag_var, format = format_extreme_values_ci(2L)),
      pvalue = cfun_by_flag("pvalue", flag_var, format = "x.xxxx | (<0.0001)")
    )
    summarize_row_groups(
      lyt = lyt,
      cfun = cfun_list
    )
  }
}

#' @describeIn logistic_regression Layout creating function which summarizes a logistic variable regression.
#' @export
#' @examples
#' result1 <- basic_table() %>%
#'   summarize_logistic(conf_level = 0.95) %>%
#'   build_table(df = df)
#' result1
#'
#' result2 <- basic_table() %>%
#'   summarize_logistic(conf_level = 0.95) %>%
#'   build_table(df = df2)
#' result2
summarize_logistic <- function(lyt,
                               conf_level) {
  sum_logistic_variable_test <- logistic_summary_by_flag("is_variable_summary")
  sum_logistic_term_estimates <- logistic_summary_by_flag("is_term_summary")
  sum_logistic_odds_ratios <- logistic_summary_by_flag("is_reference_summary")
  split_fun <- drop_and_remove_levels("")

  lyt <- logistic_regression_cols(lyt, conf_level = conf_level)
  lyt <- split_rows_by(lyt, var = "variable", labels_var = "variable_label", split_fun = split_fun)
  lyt <- sum_logistic_variable_test(lyt)
  lyt <- split_rows_by(lyt, var = "term", labels_var = "term_label", split_fun = split_fun)
  lyt <- sum_logistic_term_estimates(lyt)
  lyt <- split_rows_by(lyt, var = "interaction", labels_var = "interaction_label", split_fun = split_fun)
  lyt <- split_rows_by(lyt, var = "reference", labels_var = "reference_label", split_fun = split_fun)
  lyt <- sum_logistic_odds_ratios(lyt)
  lyt
}
