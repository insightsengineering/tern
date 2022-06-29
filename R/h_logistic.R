#' Helper Functions for Multi-Variable Logistic Regression
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper functions used in calculations for logistic regression.
#'
#' @inheritParams argument_convention
#' @param fit_glm (`glm`)\cr logistic regression model fitted by [stats::glm()] with "binomial" family.
#'   Limited functionality is also available for conditional logistic regression models fitted by
#'   [survival::clogit()], currently this is used only by [extract_rsp_biomarkers()].
#' @param x (`string` or `character`)\cr a variable or interaction term in `fit_glm` (depending on the
#'   helper function).
#'
#' @name h_logistic
NULL

#' @describeIn h_logistic Helper function to extract interaction variable names from a fitted
#'   model assuming only one interaction term.
#'
#' @export
h_get_interaction_vars <- function(fit_glm) {
  checkmate::assert_class(fit_glm, "glm")
  terms_name <- attr(stats::terms(fit_glm), "term.labels")
  terms_order <- attr(stats::terms(fit_glm), "order")
  interaction_term <- terms_name[terms_order == 2]
  checkmate::assert_string(interaction_term)
  strsplit(interaction_term, split = ":")[[1]]
}

#' @describeIn h_logistic Helper function to get the right coefficient name from the
#'   interaction variable names and the given levels. The main value here is that the order
#'   of first and second variable is checked in the `interaction_vars` input.
#'
#' @param interaction_vars (`character` of length 2)\cr interaction variable names.
#' @param first_var_with_level (`character` of length 2)\cr the first variable name with
#'   the interaction level.
#' @param second_var_with_level (`character` of length 2)\cr the second variable name with
#'   the interaction level.
#'
#' @export
h_interaction_coef_name <- function(interaction_vars,
                                    first_var_with_level,
                                    second_var_with_level) {
  checkmate::assert_character(interaction_vars, len = 2, any.missing = FALSE)
  checkmate::assert_character(first_var_with_level, len = 2, any.missing = FALSE)
  checkmate::assert_character(second_var_with_level, len = 2, any.missing = FALSE)
  checkmate::assert_subset(c(first_var_with_level[1], second_var_with_level[1]), interaction_vars)

  first_name <- paste(first_var_with_level, collapse = "")
  second_name <- paste(second_var_with_level, collapse = "")
  if (first_var_with_level[1] == interaction_vars[1]) {
    paste(first_name, second_name, sep = ":")
  } else if (second_var_with_level[1] == interaction_vars[1]) {
    paste(second_name, first_name, sep = ":")
  }
}

#' @describeIn h_logistic Helper function to calculate the odds ratio estimates
#'   for the case when both the odds ratio and the interaction variable are categorical.
#'
#' @param odds_ratio_var (`string`)\cr the odds ratio variable.
#' @param interaction_var (`string`)\cr the interaction variable.
#'
#' @export
h_or_cat_interaction <- function(odds_ratio_var,
                                 interaction_var,
                                 fit_glm,
                                 conf_level = 0.95) {
  interaction_vars <- h_get_interaction_vars(fit_glm)
  checkmate::assert_string(odds_ratio_var)
  checkmate::assert_string(interaction_var)
  checkmate::assert_subset(c(odds_ratio_var, interaction_var), interaction_vars)
  checkmate::assert_int(length(interaction_vars), lower = 2, upper = 2)

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

#' @describeIn h_logistic Helper function to calculate the odds ratio estimates
#'   for the case when either the odds ratio or the interaction variable is continuous.
#'
#' @param at (`NULL` or `numeric`)\cr optional values for the interaction variable. Otherwise
#'   the median is used.
#' @note We don't provide a function for the case when both variables are continuous because
#'   this does not arise in this table, as the treatment arm variable will always be involved
#'   and categorical.
#'
#' @export
h_or_cont_interaction <- function(odds_ratio_var,
                                  interaction_var,
                                  fit_glm,
                                  at = NULL,
                                  conf_level = 0.95) {
  interaction_vars <- h_get_interaction_vars(fit_glm)
  checkmate::assert_string(odds_ratio_var)
  checkmate::assert_string(interaction_var)
  checkmate::assert_subset(c(odds_ratio_var, interaction_var), interaction_vars)
  checkmate::assert_int(length(interaction_vars), lower = 2, upper = 2)
  checkmate::assert_numeric(at, min.len = 1, null.ok = TRUE, any.missing = FALSE)
  xs_level <- fit_glm$xlevels
  xs_coef <- stats::coef(fit_glm)
  xs_vcov <- stats::vcov(fit_glm)
  xs_class <- attr(fit_glm$terms, "dataClasses")
  model_data <- fit_glm$model
  if (!is.null(at)) {
    checkmate::assert_set_equal(xs_class[interaction_var], "numeric")
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
    checkmate::assert_set_equal(xs_class[odds_ratio_var], "numeric")
    checkmate::assert_set_equal(xs_class[interaction_var], "factor")
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

#' @describeIn h_logistic Helper function to calculate the odds ratio estimates
#'   in case of an interaction. This is a wrapper for [h_or_cont_interaction()] and
#'   [h_or_cat_interaction()].
#'
#' @export
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

#' @describeIn h_logistic Helper function to construct term labels from simple terms and the table
#'   of numbers of patients.
#'
#' @param terms (`character`)\cr simple terms.
#' @param table (`table`)\cr table containing numbers for terms.
#'
#' @export
h_simple_term_labels <- function(terms,
                                 table) {
  checkmate::assert_true(is.table(table))
  checkmate::assert_multi_class(terms, classes = c("factor", "character"))
  terms <- as.character(terms)
  term_n <- table[terms]
  paste0(terms, ", n = ", term_n)
}

#' @describeIn h_logistic Helper function to construct term labels from interaction terms and the table
#'   of numbers of patients.
#'
#' @param terms1 (`character`)\cr terms for first dimension (rows).
#' @param terms2 (`character`)\cr terms for second dimension (rows).
#' @param any (`flag`)\cr whether any of `term1` and `term2` can be fulfilled to count the
#'   number of patients. In that case they can only be scalar (strings).
#'
#' @export
h_interaction_term_labels <- function(terms1,
                                      terms2,
                                      table,
                                      any = FALSE) {
  checkmate::assert_true(is.table(table))
  checkmate::assert_flag(any)
  checkmate::assert_multi_class(terms1, classes = c("factor", "character"))
  checkmate::assert_multi_class(terms2, classes = c("factor", "character"))
  terms1 <- as.character(terms1)
  terms2 <- as.character(terms2)
  if (any) {
    checkmate::assert_scalar(terms1)
    checkmate::assert_scalar(terms2)
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

#' @describeIn h_logistic Helper function to tabulate the main effect
#'   results of a (conditional) logistic regression model.
#'
#' @examples
#' library(scda)
#' library(dplyr)
#' library(rtables)
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
#' formatters::var_labels(adrs_f) <- c(formatters::var_labels(adrs), Response = "Response")
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
#'
#' h_glm_simple_term_extract("AGE", mod1)
#' h_glm_simple_term_extract("ARMCD", mod1)
#'
#' @export
h_glm_simple_term_extract <- function(x, fit_glm) {
  checkmate::assert_multi_class(fit_glm, c("glm", "clogit"))
  checkmate::assert_string(x)

  xs_class <- attr(fit_glm$terms, "dataClasses")
  xs_level <- fit_glm$xlevels
  xs_coef <- summary(fit_glm)$coefficients
  stats <- if (inherits(fit_glm, "glm")) {
    c("estimate" = "Estimate", "std_error" = "Std. Error", "pvalue" = "Pr(>|z|)")
  } else {
    c("estimate" = "coef", "std_error" = "se(coef)", "pvalue" = "Pr(>|z|)")
  }
  # Make sure x is not an interaction term.
  checkmate::assert_subset(x, names(xs_class))
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
      formatters::var_labels(fit_glm$data[x], fill = TRUE)
    } else {
      # We just fill in here with the `term` itself as we don't have the data available.
      x
    }
    x_stats$is_variable_summary <- FALSE
    x_stats$is_term_summary <- TRUE
  } else {
    checkmate::assert_class(fit_glm, "glm")
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
    formatters::var_labels(fit_glm$data[x], fill = TRUE)
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

#' @describeIn h_logistic Helper function to tabulate the interaction term
#'   results of a logistic regression model.
#'
#' @examples
#' h_glm_interaction_extract("ARMCD:AGE", mod2)
#'
#' @export
h_glm_interaction_extract <- function(x, fit_glm) {
  vars <- h_get_interaction_vars(fit_glm)
  xs_class <- attr(fit_glm$terms, "dataClasses")

  checkmate::assert_string(x)

  # Only take two-way interaction
  checkmate::assert_int(length(vars), lower = 2, upper = 2)

  # Only consider simple case: first variable in interaction is arm, a categorical variable
  checkmate::assert_false(xs_class[vars[1]] == "numeric")

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
    formatters::var_labels(fit_glm$data[vars[1]], fill = TRUE),
    "*",
    formatters::var_labels(fit_glm$data[vars[2]], fill = TRUE)
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

#' @describeIn h_logistic Helper function to tabulate the interaction
#'   results of a logistic regression model. This basically is a wrapper for
#'   [h_or_interaction()] and [h_glm_simple_term_extract()] which puts the results
#'   in the right data frame format.
#'
#' @examples
#' h_glm_inter_term_extract("AGE", "ARMCD", mod2)
#'
#' @export
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
      variable_label = unname(formatters::var_labels(fit_glm$data[odds_ratio_var], fill = TRUE)),
      term = odds_ratio_var,
      term_label = unname(formatters::var_labels(fit_glm$data[odds_ratio_var], fill = TRUE)),
      interaction = interaction_var,
      interaction_label = unname(formatters::var_labels(fit_glm$data[interaction_var], fill = TRUE)),
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
      variable_label = unname(formatters::var_labels(fit_glm$data[odds_ratio_var], fill = TRUE)),
      term = rep(names(or_numbers), each = n_ref),
      term_label = h_simple_term_labels(rep(names(or_numbers), each = n_ref), table(fit_glm$data[[odds_ratio_var]])),
      interaction = interaction_var,
      interaction_label = unname(formatters::var_labels(fit_glm$data[interaction_var], fill = TRUE)),
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

#' @describeIn h_logistic Helper function to tabulate the results including
#'   odds ratios and confidence intervals of simple terms.
#'
#' @examples
#' h_logistic_simple_terms("AGE", mod1)
#'
#' @export
h_logistic_simple_terms <- function(x, fit_glm, conf_level = 0.95) {
  checkmate::assert_multi_class(fit_glm, c("glm", "clogit"))
  if (inherits(fit_glm, "glm")) {
    checkmate::assert_set_equal(fit_glm$family$family, "binomial")
  }
  terms_name <- attr(stats::terms(fit_glm), "term.labels")
  xs_class <- attr(fit_glm$terms, "dataClasses")
  interaction <- terms_name[which(!terms_name %in% names(xs_class))]
  checkmate::assert_subset(x, terms_name)
  if (length(interaction) != 0) {
    # Make sure any item in x is not part of interaction term
    checkmate::assert_false(any(x %in% unlist(strsplit(interaction, ":"))))
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

#' @describeIn h_logistic Helper function to tabulate the results including
#'   odds ratios and confidence intervals of interaction terms.
#'
#' @examples
#' h_logistic_inter_terms(c("RACE", "AGE", "ARMCD", "AGE:ARMCD"), mod2)
#'
#' @export
h_logistic_inter_terms <- function(x,
                                   fit_glm,
                                   conf_level = 0.95,
                                   at = NULL) {
  # Find out the interaction variables and interaction term.
  inter_vars <- h_get_interaction_vars(fit_glm)
  checkmate::assert_int(length(inter_vars), lower = 2, upper = 2)

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
