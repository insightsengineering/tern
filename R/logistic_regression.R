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
#' Note: For \code{glm} formula, the variable names need to be standard dataframe column name without
#' special characters. The big N is the total number of observations for complete cases.
#'
#' @md
#' @inheritParams argument_convention
#' @name logistic_regression
#'
NULL

#' @describeIn logistic_regression Fit a logistic regression model.
#' @param data (`data frame`)\cr the data frame on which the model was fit.
#' @export
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' adrs <- radrs(cached = TRUE)
#' adrs_f <- adrs %>%
#'   dplyr::filter(PARAMCD == "BESRSPI") %>%
#'   dplyr::filter(RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")) %>%
#'   dplyr::mutate(
#'     Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
#'     RACE = factor(RACE),
#'     SEX = factor(SEX)
#'   )
#' mod1 <- fit_logistic(
#'   data = adrs_f,
#'   variables = list(
#'     response = "Response", arm = "ARMCD", covariates = c("AGE", "RACE")
#'   )
#' )
#' mod2 <- fit_logistic(
#'   data = adrs_f,
#'   variables = list(
#'     response = "Response", arm = "ARMCD",
#'     covariates = c("AGE", "RACE"), interaction = "AGE"
#'   )
#' )
#'
fit_logistic <- function(data,
                         variables = list(
                           response = "Response",
                           arm = "ARMCD",
                           covariates = NULL,
                           interaction = NULL
                         )) {
  response <- variables$response
  arm <- variables$arm
  covariates <- variables$covariates
  interaction <- variables$interaction
  assert_that(
    is.list(variables),
    all(names(variables) %in% c("response", "arm", "covariates", "interaction")),
    is_df_with_variables(data, as.list(unlist(variables)))
  )
  forms <- paste0(response, " ~ ", arm)
  if (!is.null(covariates)) {
    forms <- paste0(forms, " + ", paste(covariates, collapse = " + "))
  }
  if (is.null(interaction)) {
    formula <- as.formula(forms)
  } else {
    assert_that(
      is.string(interaction),
      interaction %in% covariates
    )
    formula <- as.formula(paste0(forms, " + ", arm, ":", interaction))
  }
  glm(formula, family = "binomial", data = data)
}

#' Tabulation of logistic regression
#'
#' Tabulate the main effect results of a logistic regression model.
#' @md
#' @param x (`string`)\cr a variable in `fit_glm`.
#' @param fit_glm logistic regression model fitted by [stats::glm()].
#' @importFrom car Anova
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' adrs <- radrs(cached = TRUE)
#' adrs_f <- adrs %>%
#'   dplyr::filter(PARAMCD == "BESRSPI") %>%
#'   dplyr::filter(RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")) %>%
#'   dplyr::mutate(
#'     Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
#'     RACE = factor(RACE),
#'     SEX = factor(SEX)
#'   )
#' fit_glm <- fit_logistic(
#'   data = adrs_f,
#'   variables = list(response = "Response", arm = "ARMCD", covariates = c("AGE", "RACE"))
#' )
#' glm_simple_term_extract("AGE", fit_glm)
#' glm_simple_term_extract("ARMCD", fit_glm)
#'
glm_simple_term_extract <- function(x, fit_glm) {
  assert_that(
    "glm" %in% class(fit_glm),
    is.string(x)
  )
  xs_class <- attr(fit_glm$terms, "dataClasses")
  xs_level <- fit_glm$xlevels
  xs_coef <- summary(fit_glm)$coefficients
  stats <- c("estimate" = "Estimate", "std_error" = "Std. Error", "pvalue" = "Pr(>|z|)")
  # Make sure x is not interaction term
  assert_that(x %in% names(xs_class))
  x_sel <- if (xs_class[x] == "numeric") x else paste0(x, xs_level[[x]][-1])
  x_stats <- as.data.frame(xs_coef[x_sel, stats, drop = FALSE], stringsAsFactors = FALSE)
  colnames(x_stats) <- names(stats)
  x_stats$estimate <- as.list(x_stats$estimate)
  x_stats$std_error <- as.list(x_stats$std_error)
  x_stats$pvalue <- as.list(x_stats$pvalue)
  x_stats$df <- as.list(1)
  x_stats$term <- if (xs_class[x] == "numeric") x else xs_level[[x]][-1]
  if (xs_class[x] != "numeric") {
    main_effects <- car::Anova(fit_glm, type = 3, test.statistic = "Wald")
    x_main <- data.frame(
      pvalue = main_effects[x, "Pr(>Chisq)", drop = TRUE],
      term = paste(x, "Reference =", xs_level[[x]][1]),
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
    x_stats <- rbind(x_main, x_stats)
  }
  x_stats$variable <- x
  rownames(x_stats) <- NULL
  x_stats[c("variable", "term", "estimate", "std_error", "df", "pvalue")]
}

#' Tabulation of logistic regression
#'
#' Tabulate the result of interaction from a logistic regression model.
#' @md
#' @param x (`string`)\cr a interaction in `fit_glm`.
#' @param fit_glm logistic regression model fitted by [stats::glm()].
#' @importFrom car Anova
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' adrs <- radrs(cached = TRUE)
#' adrs_f <- adrs %>%
#'   dplyr::filter(PARAMCD == "BESRSPI") %>%
#'   dplyr::filter(RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")) %>%
#'   dplyr::mutate(Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0))
#' fit_glm <- fit_logistic(
#'   data = adrs_f,
#'   variables = list(
#'     response = "Response", arm = "ARMCD",
#'     covariates = c("AGE", "RACE"), interaction = "RACE"
#'   )
#' )
#' glm_interaction_extract("ARMCD:RACE", fit_glm)
#'
glm_interaction_extract <- function(x, fit_glm) {
  assert_that("glm" %in% class(fit_glm))
  terms_name <- attr(terms(fit_glm), "term.labels")
  xs_class <- attr(fit_glm$terms, "dataClasses")
  assert_that(
    is.string(x),
    # Only deal with one interaction term
    identical(x, terms_name[which(!terms_name %in% names(xs_class))])
  )
  vars <- unlist(strsplit(x, ":"))
  # Only take two-way interaction
  assert_that(
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
  } else if (xs_class[vars[2]] != "numeric") {
    v2_comp <- xs_level[[vars[2]]][-1]
    x_sel <- paste0(levels(interaction(paste0(vars[1], v1_comp), paste0(vars[2], v2_comp), sep = ":")))
    x_stats <- as.data.frame(xs_coef[x_sel, stats, drop = FALSE], stringsAsFactors = FALSE)
    colnames(x_stats) <- names(stats)
    x_stats$term <- paste0(levels(interaction(v1_comp, v2_comp, sep = " * ")))
  }
  x_stats$df <- as.list(1)
  x_stats$pvalue <- as.list(x_stats$pvalue)
  x_main <- data.frame(
    pvalue = main_effects[x, "Pr(>Chisq)", drop = TRUE],
    term = paste(vars[1], vars[2], sep = " * "),
    df = main_effects[x, "Df", drop = TRUE],
    stringsAsFactors = FALSE
  )
  x_main$pvalue <- as.list(x_main$pvalue)
  x_main$df <- as.list(x_main$df)
  x_main$estimate <- list(numeric(0))
  x_main$std_error <- list(numeric(0))

  x_stats <- rbind(x_main, x_stats)
  x_stats$variable <- x
  rownames(x_stats) <- NULL
  x_stats[c("variable", "term", "estimate", "std_error", "df", "pvalue")]
}

#' Helper of logistic regression
#'
#' Tabulate the results including odds ratios and confidence intervals of simple terms.
#' @md
#' @inheritParams argument_convention
#' @inheritParams glm_simple_term_extract
#' @param x (`string` or vector of `string`) \cr variable in `fit_glm`.
#' @param fit_glm logistic regression model fitted by [stats::glm()] with "binomial" family.
#' @export
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' adrs <- radrs(cached = TRUE)
#' adrs_f <- adrs %>%
#'   dplyr::filter(PARAMCD == "BESRSPI") %>%
#'   dplyr::filter(RACE %in% c("ASIAN", "WHITE", "BLACK OR AFRICAN AMERICAN")) %>%
#'   dplyr::mutate(Response = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0))
#' fit_glm <- fit_logistic(
#'   data = adrs_f,
#'   variables = list(
#'     response = "Response", arm = "ARMCD",
#'     covariates = c("AGE", "RACE"), interaction = "RACE"
#'   )
#' )
#' h_logistic_simple_terms("AGE", fit_glm)
#'
h_logistic_simple_terms <- function(x, fit_glm, conf_level = 0.95) {
  assert_that(
    "glm" %in% class(fit_glm),
    fit_glm$family$family == "binomial"
  )
  terms_name <- attr(terms(fit_glm), "term.labels")
  xs_class <- attr(fit_glm$terms, "dataClasses")
  interaction <- terms_name[which(!terms_name %in% names(xs_class))]
  assert_that(all(x %in% terms_name))
  if (length(interaction) != 0) {
    # Make sure any item in x is not part of interaction term
    assert_that(all(!x %in% unlist(strsplit(interaction, ":"))))
  }
  x_stats <- lapply(x, glm_simple_term_extract, fit_glm)
  x_stats <- do.call(rbind, x_stats)
  q_norm <- qnorm((1 + conf_level) / 2)
  x_stats$odds_ratio <- lapply(x_stats$estimate, exp)
  x_stats$lcl <- Map(function(or, se) exp(log(or) - q_norm * se), x_stats$odds_ratio, x_stats$std_error)
  x_stats$ucl <- Map(function(or, se) exp(log(or) + q_norm * se), x_stats$odds_ratio, x_stats$std_error)
  x_stats$ci <- Map(function(lcl, ucl) c(lcl, ucl), lcl = x_stats$lcl, ucl = x_stats$ucl)
  x_stats
}

#' @describeIn logistic_regression Helper method (for [broom::tidy()]) to prepare a data frame from an
#'   `glm` object with `binomial` family.
#' @param fit_glm logistic regression model fitted by [stats::glm()] with "binomial" family.
#' @method tidy glm
#' @export
#' @importFrom broom tidy
#' @examples
#' df <- broom::tidy(mod1, conf_level = 0.99)
#'
tidy.glm <- function(fit_glm, conf_level = 0.95) { #nolint
  assert_that(
    "glm" %in% class(fit_glm),
    fit_glm$family$family == "binomial"
  )
  terms_name <- attr(terms(fit_glm), "term.labels")
  xs_class <- attr(fit_glm$terms, "dataClasses")
  interaction <- terms_name[which(!terms_name %in% names(xs_class))]
  if (length(interaction) == 0) {
    h_logistic_simple_terms(x = terms_name, fit_glm = fit_glm, conf_level = conf_level)
  } else {
    # To add the part for model with interaction
  }
}

#' @describeIn logistic_regression transforms the tabulated results from `tidy`
#'  into a list.
#' @export
#'
#' @examples
#' s_logistic(broom::tidy(mod1), .var = "ci")
#'
s_logistic <- function(df, .var) {
  assert_that(
    is_df_with_variables(df, list(variable = "variable", term = "term", var = .var)),
    is_character_or_factor(df$term)
  )
  df$variable <- as.character(df$variable)
  df$variable <- factor(df$variable, levels = unique(df$variable))
  df$term <- as.character(df$term)
  y <- split(df, f = factor(df$term, unique(df$term)), drop = FALSE)
  y <- setNames(y, nm = rep(.var, length(y)))
  if (nrow(df) == 0) {
    setNames(list(character()), "estimate")
  } else {
    lapply(y, function(x) {
      setNames(as.list(x[[.var]]), x$term)
    })
  }
}

#' @describeIn logistic_regression layout creating function.
#' @param vars (`character`)\cr the name of statistics to be reported among
#'  `df` (degrees of freedom), `estimate` (regression coefficient),
#'  `std_error` (standard error of coefficient), `odds_ratio` (odds ratio) and
#'  `ci` (confidence interval of odds ratio), `pvalue` (p.value of the effect).
#' @export
#'
#' @examples
#' result <- basic_table() %>%
#'   split_rows_by("variable") %>%
#'   split_rows_by("term", split_fun = drop_split_levels) %>%
#'   summarize_logistic(conf_level = .99) %>%
#'   build_table(df = df)
#' result
#'
summarize_logistic <- function(lyt,
                               conf_level,
                               vars = c("df", "estimate",
                                        "std_error", "odds_ratio",
                                        "ci", "pvalue")) {
  afun_list <- Map(
    function(stat, format) {
      make_afun(s_logistic, .stats = stat, .formats = format, .ungroup_stats = stat)
    },
    stat = c("df", "estimate", "std_error", "odds_ratio", "ci", "pvalue"),
    format = c(
      df = "xx", estimate = "xx.xxx", std_error = "xx.xxx",
      odds_ratio = ">999.99", ci = "(xx.xx, xx.xx)", pvalue = "x.xxxx | (<0.0001)"
    )
  )
  lyt <- split_cols_by_multivar(
    lyt = lyt,
    vars = vars,
    varlabels = c(
      df = "Degrees of Freedom", estimate = "Parameter Estimate",
      std_error = "Standard Error", odds_ratio = "Odds Ratio",
      ci = paste0("Wald ", 100 * conf_level, "% CI"), pvalue = "p-value"
    )[vars]
  )
  analyze_colvars(lyt = lyt, afun = afun_list[vars])
}
