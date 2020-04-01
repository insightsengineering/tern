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
#' Note: For \code{glm} model, the variable names need to be standard dataframe column name without
#' special characters. The big N is the total number of observations for complete cases.
#'
#' @param glm_model a {\code{\link{glm}}} model object.
#' The model object can be all main effect model, and a model with one two-way interaction.
#' @param terms_label a named vector to control the displaying label of terms
#' from {\code{glm_model}}. If it's {\code{NULL}}, then variable name will be displayed in table.
#' @param increments a named list for specifying numeric values of continuous variables in {\code{glm_model}}
#' which interact with other variables. This is used to calculate the odds ratio when comparing
#' the other interaction variable effect. For example, for a model with ARM and AGE interaction,
#' {\code{increments = list(AGE = c(18, 65))}} will enable calculation of odds ratios of
#' comparison ARM vs. reference ARM at AGE = 18 and AGE = 65. If {\code{increments = NULL}}, then
#' default AGE value is ceiling of median.
#' @param conf_level confidence level for Wald odds ratio confidence interval.
#' @importFrom car Anova
#' @importFrom scales pvalue
#' @export
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADSL <- ADSL %>% dplyr::filter(SEX %in% c("F", "M"))
#' ADRS <- radrs(ADSL, seed = 2)
#' ADRS_f <- subset(ADRS, PARAMCD == "BESRSPI") %>%
#'   dplyr::mutate(Response = case_when(AVALC %in% c("PR", "CR") ~ 1,
#'                               TRUE ~ 0))
#' glm_model <- glm(
#'  formula = Response ~ ARM + AGE + SEX,
#'  data = ADRS_f,
#'  family = "binomial")
#' tbl <- t_logistic(glm_model = glm_model)
#' \dontrun{
#' Viewer(tbl)
#' }
#'
#' glm_model <- glm(
#'  formula = Response ~ ARM + AGE + BMRKR2 + ARM*BMRKR2,
#'  data = ADRS_f,
#'  family = "binomial")
#' tbl2 <- t_logistic(
#'    glm_model = glm_model,
#'    terms_label = c("ARM" = "Treatment",
#'                    "AGE" = "Age at baseline",
#'                    "BMRKR2" = "Biomarker",
#'                    "ARM:BMRKR2" = "Interaction of Treatment * Biomarker")
#' )
#' \dontrun{
#' Viewer(tbl2)
#' }
#'
#' glm_model <- glm(
#'  formula = Response ~ ARM + AGE + BMRKR1 + ARM*BMRKR1,
#'  data = ADRS_f,
#'  family = "binomial")
#' tbl3 <- t_logistic(
#'    glm_model = glm_model,
#'    terms_label = c("ARM" = "Treatment Effect",
#'                    "AGE" = "Age at baseline",
#'                    "BMRKR1" = "Continuous Biomarker",
#'                    "ARM:BMRKR1" = "Interaction of ARM* Biomarker"),
#'    increments = list("BMRKR1" = c(5, 10)),
#'    conf_level = 0.9
#' )
#' \dontrun{
#' Viewer(tbl3)
#' }
#' glm_model <- glm(
#'  formula = Response ~ ARM + AGE + BMRKR1 + AGE*BMRKR1,
#'  data = ADRS_f,
#'  family = "binomial")
#' tbl4 <- t_logistic(
#'    glm_model = glm_model,
#'    terms_label = c("ARM" = "Treatment Effect",
#'                    "AGE" = "Age at baseline",
#'                    "BMRKR1" = "Continuous Biomarker",
#'                    "AGE:BMRKR1" = "Interaction of Age* Biomarker"),
#'    increments = list("AGE" = c(20, 65), "BMRKR1" = c(5, 10)),
#'    conf_level = 0.9
#' )
#' \dontrun{
#' Viewer(tbl4)
#' }
t_logistic <- function(glm_model,
                       terms_label = NULL,
                       increments = NULL,
                       conf_level = 0.95) {
  stopifnot("glm" %in% class(glm_model))
  stopifnot(glm_model$family$family == "binomial")
  terms_name <- attr(terms(glm_model), "term.labels")
  # complete case data used in model
  model_data <- glm_model$model
  table_header <- rheader(rrow(
    row.name = "",
    "Degrees of \n Freedom",
    "Parameter \n Estimate",
    "Standard \n Error",
    "Odds \n Ratio",
    paste0("Wald \n ", conf_level * 100, "% CI"),
    "p-value"
  ))
  if (!is.null(terms_label)) {
    stopifnot(length(terms_label) == length(terms_name))
    stopifnot(identical(names(terms_label), terms_name))
  } else {
    terms_label <- terms_name
    names(terms_label) <- terms_name
  }

  table_single <- function(term, s_info, terms_label, header) {
    s_term <- s_info$results[[term]]
    term_type <- s_term$predictor$term_type
    summary_term <- s_term$summary
    if (term_type == "categorical") {
      ref <- s_term$predictor$term_ref_level
      comp <- s_term$predictor$term_comp_level
      main_rowname <- paste0(
        terms_label[term],
        " (Reference = ",
        ref,
        ", n = ",
        s_term$predictor$counts_by_level[ref],
        ")"
      )
      rbind(
        if (length(comp) == 1) {
          rrowl(row.name = main_rowname)
        } else {
          table_formats(s_term$main, header = header, row.name = main_rowname, row_format = "main")
        },
        indent(do.call("rbind", lapply(1:nrow(summary_term), function(i) {
          row_i <- summary_term[i, , drop = FALSE]
          row_nm <- rownames(row_i)
          table_formats(
            row_i,
            header = header,
            row.name = paste0(row_nm, ", n = ", s_term$predictor$counts_by_level[row_nm]),
            row_format = "full"
          )
        })))
      )
    } else {
      table_formats(s_term$summary, header = header, row.name = terms_label[term], row_format = "full")
    }
  }

  if (all(terms_name %in% colnames(model_data))) {
    s_info <- s_logistic_single(glm_model = glm_model, conf_level = conf_level)
    table_term <- lapply(names(s_info$results), function(term) {
      table_single(term = term,  s_info = s_info, terms_label = terms_label, header = table_header)
    })
  } else {
    s_info <- s_logistic_interaction(
      glm_model = glm_model,
      conf_level = conf_level,
      increments = increments
    )

    table_term <- sapply(names(s_info$results), function(term) {
      interact_with <- s_info$results[[term]]$predictor$interact_with
      is_interaction <- s_info$results[[term]]$predictor$term_type == "interaction"
      if (is.null(interact_with) && !is_interaction) {
        table_single(term = term, s_info = s_info, terms_label = terms_label, header = table_header)
      } else if (is_interaction) {
        if (nrow(s_info$results[[term]]$summary) == 1) {
          int_terms_type <- sapply(unlist(strsplit(term, ":")), function(i) {
            s_info$results[[i]]$predictor$term_type
          })
          if (all(int_terms_type == "continuous")) {
            table_formats(
              dfi = s_info$results[[term]]$summary,
              header = table_header,
              row.name = terms_label[[term]],
              row_format = "level"
            )
          } else {
            rbind(
              rrowl(row.name = terms_label[[term]]),
              indent(table_formats(
                dfi = s_info$results[[term]]$summary,
                header = table_header,
                row.name = rownames(s_info$results[[term]]$summary),
                row_format = "level"
              ))
            )
          }
        } else {
          rbind(
            table_formats(
              dfi = s_info$results[[term]]$main,
              header = table_header,
              row.name = terms_label[term],
              row_format = "main"
            ),
            indent(do.call("rbind", lapply(1:nrow(s_info$results[[term]]$summary), function(i) {
              dfi <- s_info$results[[term]]$summary[i, , drop = FALSE]
              table_formats(
                dfi = dfi,
                header = table_header,
                row.name = rownames(dfi),
                row_format = "level"
              )
            })))
          )
        }

      } else {
        term_type <- s_info$results[[term]]$predictor$term_type
        if (term_type == "categorical") {
          ref_level <- s_info$results[[term]]$predictor$term_ref_level
          comp_level <- s_info$results[[term]]$predictor$term_comp_level
          counts <- s_info$results[[term]]$predictor$counts_by_level
          term_row_name <- paste0(terms_label[term], "(Reference = ", ref_level, ", n = ", counts[ref_level], ")")
          table_by_level <- do.call("rbind", lapply(comp_level, function(lvl) {
            summary_comp <- s_info$results[[term]]$summary[[lvl]]
            summary_int <- summary_comp$summary_with_interaction
            rbind(
              indent(table_formats(
                dfi = summary_comp$summary_comp_level,
                header =  table_header,
                row.name = paste0(lvl, ", n = ", counts[lvl]),
                row_format = "level"
              )),
              indent(rtable(header = table_header, rrowl(row.name = terms_label[interact_with])), by = 2),
              indent(do.call("rbind", lapply(names(summary_int), function(i) {
                table_formats(dfi = summary_int[[i]], header = table_header, row.name = i, row_format = "or_ci")
              })), by = 3)
            )
          }))
          rbind(
            if (length(comp_level) == 1) {
              rrowl(row.name = term_row_name)
            } else {
              table_formats(
                dfi = s_info$results[[term]]$main,
                header = table_header,
                row.name = term_row_name,
                row_format = "main"
              )
            },
            table_by_level
          )
        } else {
          s_term <- s_info$results[[term]]$summary
          rbind(
            table_formats(
              s_term$summary_term,
              header = table_header,
              row.name = terms_label[term],
              row_format = "level"
            ),
            indent(rtable(header = table_header, rrowl(row.name = terms_label[interact_with]))),
            indent(do.call("rbind", lapply(names(s_term$summary_with_interaction), function(i) {
              table_formats(
                s_term$summary_with_interaction[[i]],
                header  = table_header,
                row.name = i,
                row_format = "or_ci"
              )
            })), by = 2)
          )
        }
      }
    })
  }
  rbind(
    rrowl(row.name = paste0("N = ", s_info$N)),
    rrow(),
    do.call("rbind", lapply(1:length(table_term), function(i) {
      if (i == length(table_term)) {
        table_term[[i]]
      } else {
        rbind(table_term[[i]], rrow())
      }
    }))
  )
}

#' Summary of logistic regression with no interaction term
#' @param glm_model a {\code{\link{glm}}} model object with all main effect model.
#' @param conf_level confidence level for Wald odds ratio confidence interval.
#' @export
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' library(purrr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADSL <- ADSL %>% mutate(
#'  SEX = as.character(SEX),
#'  SEX = case_when(!SEX %in% c("F", "M") ~ "U",
#'                TRUE ~ SEX))
#' ADRS <- radrs(ADSL, seed = 2)
#' ADRS_f <- subset(ADRS, PARAMCD == "BESRSPI") %>%
#'   mutate(Response = case_when(AVALC %in% c("PR", "CR") ~ 1,
#'                               TRUE ~ 0))
#' glm_model <- glm(
#'  formula = Response ~ ARM + AGE + SEX,
#'  data = ADRS_f,
#'  family = "binomial")
#' s_logistic_single(glm_model, conf_level = 0.90)
s_logistic_single <- function(glm_model,
                              conf_level = 0.95) {
  stopifnot("glm" %in% class(glm_model))
  stopifnot(glm_model$family$family == "binomial")
  terms_name <- attr(terms(glm_model), "term.labels")

  # data originally as input in glm_model
  model_data <- glm_model$model
  modsum <- summary(glm_model)
  if (!all(terms_name %in% colnames(model_data))) {
    stop("Terms not in data")
  }
  terms_class <- attr(terms(glm_model), "dataClasses")[-1]
  terms_levels <- glm_model$xlevels
  model_coef <- modsum$coefficients
  main_effect <- car::Anova(glm_model, type = 3, test.statistic = "Wald")
  extract_single <- extract_logistic_single(
    terms_name,
    terms_levels,
    terms_class,
    model_coef,
    model_data,
    conf_level
  )

  terms_results <- sapply(names(extract_single), function(x) {
    main <- main_effect[x, c("Df", "Pr(>Chisq)"), drop = FALSE]
    colnames(main) <- c("df", "p-value")
    c(list(main = main), extract_single[[x]])
  }, simplify = FALSE, USE.NAMES = TRUE)

  list(
    N = nrow(model_data),
    results = terms_results
  )
}

#' Summary of logistic regression with one two-way interaction term
#' @inheritParams t_logistic
#'
#' @export
#'
#' @importFrom stats vcov
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#' library(purrr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADSL <- ADSL %>% mutate(
#'  SEX = as.character(SEX),
#'  SEX = case_when(!SEX %in% c("F", "M") ~ "U",
#'                TRUE ~ SEX))
#' ADRS <- radrs(ADSL, seed = 2)
#' ADRS_f <- subset(ADRS, PARAMCD == "BESRSPI") %>%
#'   mutate(Response = case_when(AVALC %in% c("PR", "CR") ~ 1,
#'                               TRUE ~ 0))
#' glm_model <- glm(
#'  formula = Response ~ ARM + AGE + SEX + ARM*SEX,
#'  data = ADRS_f,
#'  family = "binomial")
#' s_logistic_interaction(glm_model, conf_level = 0.90)
#' glm_model2 <- glm(
#'  formula = Response ~ ARM + AGE + ARM*AGE,
#'  data = ADRS_f,
#'  family = "binomial")
#' s_logistic_interaction(glm_model2, conf_level = 0.90)
#' glm_model3 <- glm(
#'  formula = Response ~ ARM + AGE + BMRKR1 + BMRKR1*AGE,
#'  data = ADRS_f,
#'  family = "binomial")
#' s_logistic_interaction(glm_model3 , conf_level = 0.90)
s_logistic_interaction <- function(glm_model,
                                   conf_level = 0.95,
                                   increments = NULL) {
  stopifnot("glm" %in% class(glm_model))
  stopifnot(glm_model$family$family == "binomial")
  terms_name <- attr(terms(glm_model), "term.labels")

  # data used in model
  model_data <- glm_model$model
  terms_covariates <- terms_name[which(terms_name %in% colnames(model_data))]
  terms_interaction <-  terms_name[which(!terms_name %in% colnames(model_data))]
  if (length(terms_interaction) > 1) {
    stop("Not support multiple interaction terms")
  }
  # Only allow one two-variable interaction term
  # ":" must not be in variable name
  terms_interaction <- unlist(strsplit(terms_interaction, ":"))
  if (length(terms_interaction) != 2) {
    stop("Only support two-variable interaction term")
  }
  stopifnot(all(terms_interaction %in% terms_covariates))
  modsum <- summary(glm_model)
  terms_class <- attr(terms(glm_model), "dataClasses")[-1]
  terms_levels <- glm_model$xlevels
  model_coef <- modsum$coefficients
  terms_single <- terms_covariates[which(!terms_covariates %in% terms_interaction)]
  main_effect <- car::Anova(glm_model, type = 3, test.statistic = "Wald")
  # covariates only with main effect
  extract_single <- if (length(terms_single) >= 1) {
    extract_logistic_single(
      terms_single,
      terms_levels[terms_single],
      terms_class[terms_single],
      model_coef,
      model_data,
      conf_level
    )
  } else NULL

  # covarates with interaction
  vcov_coef <- vcov(glm_model)
  extract_interacton <- extract_logistic_interaction(
    terms_interaction,
    terms_class[terms_interaction],
    terms_levels[terms_interaction],
    model_coef,
    vcov_coef,
    model_data,
    increments = increments,
    conf_level = conf_level
  )

  extract_all <- c(extract_single, extract_interacton)
  terms_results <- sapply(terms_name, function(x) {
    main <- main_effect[x, c("Df", "Pr(>Chisq)"), drop = FALSE]
    colnames(main) <- c("df", "p-value")
    c(list(main = main),
      extract_all[[x]])
  }, simplify = FALSE, USE.NAMES = TRUE)
  list(
    N = nrow(model_data),
    results = terms_results
  )
}

#' Extract items from logistic regression coefficients for terms
#' that are not invovled in interaction term and calculate odds ratios and CIs
#' @noRd
#' @examples
#' \dontrun{
#' library(random.cdisc.data)
#' library(dplyr)
#' library(purrr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADSL <- ADSL %>% mutate(
#'  SEX = as.character(SEX),
#'  SEX = case_when(!SEX %in% c("F", "M") ~ "U",
#'                TRUE ~ SEX))
#' ADRS <- radrs(ADSL, seed = 2)
#' ADRS_f <- subset(ADRS, PARAMCD == "BESRSPI") %>%
#'   mutate(Response = case_when(AVALC %in% c("PR", "CR") ~ 1,
#'                               TRUE ~ 0))
#' glm_model <- glm(
#'  formula = Response ~ ARM + AGE + SEX,
#'  data = ADRS_f,
#'  family = "binomial")
#'
#' terms_name <- attr(terms(glm_model), "term.labels")
#' # data used in model (complete cases)
#' model_data <- glm_model$model
#' model_coef <-  summary(glm_model)$coefficients
#' terms_class <- attr(terms(glm_model),"dataClasses")[-1]
#' terms_levels <- glm_model$xlevels
#' extract_logistic_single(
#'   terms_name =  terms_name,
#'   terms_levels = terms_levels,
#'   terms_class = terms_class,
#'   model_coef = model_coef,
#'   model_data = model_data)
#' }
extract_logistic_single <- function(terms_name,
                                    terms_levels,
                                    terms_class,
                                    model_coef,
                                    model_data,
                                    conf_level = 0.95) {
  extract_items <- c("Estimate", "Std. Error", "Pr(>|z|)")
  terms_results <- sapply(terms_name, function(x) {
    if (terms_class[[x]] %in% c("factor", "character")) {
      ref_level <- terms_levels[[x]][1]
      comp_level <- terms_levels[[x]][-1]
      sel <- paste0(x, comp_level)
      x_coef <- model_coef[sel, extract_items, drop = FALSE]
      rownames(x_coef) <- terms_levels[[x]][-1]
      count_by_lvl <- table(model_data[[x]])
      x_type <- "categorical"
    } else if (terms_class[[x]] == "numeric") {
      x_coef <- model_coef[x, extract_items, drop = FALSE]
      x_type <- "continuous"
    }
    x_coef <- as.data.frame(x_coef)
    colnames(x_coef) <- c("coef",  "se", "p-value")
    rowname_x_coef <- rownames(x_coef)
    x_coef <- x_coef %>%
      mutate(
        or = exp(.data$coef),
        df = 1,
        lcl = exp(.data$coef - qnorm((1 + conf_level) / 2) * .data$se),
        ucl = exp(.data$coef + qnorm((1 + conf_level) / 2) * .data$se)
      )
    rownames(x_coef) <- rowname_x_coef
    predictor <- list(
      term_type = x_type,
      term_ref_level = if (x_type == "categorical") ref_level else NULL,
      counts_by_level = if (x_type == "categorical") count_by_lvl else NULL
    )

    list(
      predictor = predictor,
      summary = x_coef
    )
  }, simplify = FALSE, USE.NAMES = TRUE)
  terms_results
}

#' Extract items from logistic regression coefficients for terms
#' that are invovled in interaction term and calculate odds ratios and CIs
#' @noRd
#' @examples
#' \dontrun{
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADSL <- ADSL %>% mutate(
#'  SEX = as.character(SEX),
#'  SEX = case_when(!SEX %in% c("F", "M") ~ "U",
#'                TRUE ~ SEX))
#' ADRS <- radrs(ADSL, seed = 2)
#' ADRS_f <- subset(ADRS, PARAMCD == "BESRSPI") %>%
#'   mutate(Response = case_when(AVALC %in% c("PR", "CR") ~ 1,
#'                               TRUE ~ 0))
#' glm_model <- glm(
#'  formula = Response ~ ARM + AGE + SEX + ARM*SEX,
#'  data = ADRS_f,
#'  family = "binomial")
#'
#' terms_name <- attr(terms(glm_model), "term.labels")
#' # data used in model (complete cases)
#' model_data <- glm_model$model
#' model_coef <-  summary(glm_model)$coefficients
#' terms_class <- attr(terms(glm_model),"dataClasses")[-1]
#' terms_levels <- glm_model$xlevels
#' vcov_coef <- vcov(glm_model)
#' extract_logistic_interaction(
#'   terms_interaction =  c("ARM", "AGE"),
#'   terms_levels = terms_levels,
#'   terms_class = terms_class,
#'   model_coef = model_coef,
#'   model_data = model_data)
#' }
extract_logistic_interaction <- function(terms_interaction,
                                         terms_class,
                                         terms_levels,
                                         model_coef,
                                         vcov_coef,
                                         model_data,
                                         increments = NULL,
                                         conf_level = 0.95) {
  info_xy <- sapply(terms_interaction, function(term) {
    term_class <- terms_class[[term]]
    interact_with <- terms_interaction[which(terms_interaction != term)]
    if (term_class %in% c("character", "factor")) {
      lvls <- terms_levels[[term]]
      term_ref_level <- lvls[1]
      term_comp_level <- lvls[-1]
      list(
        interact_with = interact_with,
        term_type = "categorical",
        term_ref_level = term_ref_level,
        term_comp_level = term_comp_level
      )
    } else if (term_class == "numeric") {
      term_values <- if (!is.null(increments) & !is.null(increments[[term]])) {
        increments[[term]]
      } else {
        ceiling(median(model_data[[term]]))
      }
      list(
        interact_with = interact_with,
        term_type = "continuous",
        term_values = term_values
      )
    }
  },  simplify = FALSE, USE.NAMES = TRUE)
  results_interaction <- element_interaction(info_xy, model_coef, vcov_coef, model_data, conf_level)
  results_interaction
}

#' @importFrom scales pvalue
element_interaction <- function(info_xy,
                                model_coef,
                                vcov_coef,
                                model_data,
                                conf_level = 0.95) {
  terms_type <- sapply(info_xy, function(i) i$term_type)
  extract_items <- c("Estimate", "Std. Error", "Pr(>|z|)")
  if (all(terms_type == "categorical")) {
    lvls1 <- info_xy[[1]]$term_comp_level
    lvls2 <- info_xy[[2]]$term_comp_level
    int_lookup <- data.frame(
      rep(lvls1, length(lvls2)),
      rep(lvls2, each = length(lvls1)),
      stringsAsFactors = FALSE
    )
    int_lookup$INTERACTION <- paste( # nolint
      paste0(names(info_xy)[1], int_lookup[[1]]),
      paste0(names(info_xy)[2], int_lookup[[2]]),
      sep = ":"
    )
  } else if (all(terms_type == "continuous")) {
    int_lookup <- data.frame(
      names(info_xy)[1], names(info_xy)[2], paste(names(info_xy), collapse = ":"),
      stringsAsFactors = FALSE
    )
  } else {
    cat <- names(info_xy)[which(terms_type == "categorical")]
    con <- names(info_xy)[which(terms_type == "continuous")]
    cat_ref_level <- info_xy[[cat]]$term_ref_level
    cat_comp_level <- info_xy[[cat]]$term_comp_level
    int_lookup <- data.frame(cat_comp_level, con, stringsAsFactors = FALSE)
    int_lookup$INTERACTION <- if (which(names(info_xy) == cat) == 1) { # nolint
      paste(paste0(cat, cat_comp_level), con, sep = ":")
    } else {
      paste(con, paste0(cat, cat_comp_level), sep = ":")
    }
  }
  colnames(int_lookup) <- c(names(info_xy), "INTERACTION")
  or_ci_df <- function(sel_beta_term, sel_beta_int = NULL, constant_int = 0) {
    if (is.null(sel_beta_int)) {
      or <- exp(model_coef[sel_beta_term, "Estimate"])
      se_logor <- model_coef[sel_beta_term, "Std. Error"]
    } else {
      or <- exp(model_coef[sel_beta_term, "Estimate"] + constant_int * model_coef[sel_beta_int, "Estimate"])
      se_beta_term <- model_coef[sel_beta_term, "Std. Error"]
      se_beta_int <- model_coef[sel_beta_int, "Std. Error"]
      cov_betas <- vcov_coef[sel_beta_term, sel_beta_int]
      se_logor <- (se_beta_term^2 + constant_int^2 * se_beta_int^2 + 2 * constant_int * cov_betas) ^ (1 / 2)
    }
    data.frame(
      or = or,
      lcl = exp(log(or) - qnorm((1 + conf_level) / 2) * se_logor),
      ucl = exp(log(or) + qnorm((1 + conf_level) / 2) * se_logor),
      check.names = FALSE
    )
  }

  results_by_term <- sapply(names(info_xy), function(term) {
    interact_with <- info_xy[[term]]$interact_with
    if (all(terms_type == "categorical")) {
      counts_by_level <- table(model_data[[term]])
      term_comp_level <- info_xy[[term]]$term_comp_level
      int_ref_level <- info_xy[[interact_with]]$term_ref_level
      int_comp_level <- info_xy[[interact_with]]$term_comp_level
      summary_term <- sapply(term_comp_level, function(lvl) {
        level_coef <- as.data.frame(model_coef[paste0(term, lvl), extract_items, drop = FALSE])
        colnames(level_coef) <- c("coef", "se", "p-value")
        level_coef$df <- 1
        summary_with_interaction <- sapply(c(int_ref_level, int_comp_level), function(int_lvl) {
          if (int_lvl == int_ref_level) {
            sel_beta_int <- NULL
            constant_int <- 0
          } else {
            sel_int <- int_lookup[[term]] == lvl & int_lookup[[interact_with]] == int_lvl
            sel_beta_int <- int_lookup[sel_int, "INTERACTION"]
            constant_int <- 1
          }
          or_ci_df(sel_beta_term = paste0(term, lvl), sel_beta_int, constant_int = constant_int)
        }, simplify = FALSE, USE.NAMES = TRUE)
        list(
          summary_comp_level = level_coef,
          summary_with_interaction = summary_with_interaction
        )
      }, simplify = FALSE, USE.NAMES = TRUE)
      list(
        predictor = list(
          term_type = info_xy[[term]]$term_type,
          interact_with = interact_with,
          counts_by_level = counts_by_level,
          term_ref_level = info_xy[[term]]$term_ref_level,
          term_comp_level = term_comp_level),
        summary = summary_term
      )

    } else if (all(terms_type == "continuous")) {
      int_values <- info_xy[[interact_with]]$term_values
      term_coef <- as.data.frame(model_coef[term, extract_items, drop = FALSE])
      colnames(term_coef) <- c("coef", "se", "p-value")
      term_coef$df <- 1
      summary_with_interaction <- sapply(as.character(int_values), function(val) {
        or_ci_df(
          sel_beta_term = term,
          sel_beta_int = int_lookup$INTERACTION,
          constant_int = as.numeric(val)
        )
      }, simplify = FALSE, USE.NAMES = TRUE)

      list(
        predictor = list(
          term_type = info_xy[[term]]$term_type,
          interact_with = interact_with
        ),
        summary = list(
          summary_term = term_coef,
          summary_with_interaction = summary_with_interaction
        )
      )
    } else {
      if (term == cat) {
        counts_by_level <- table(model_data[[term]])
        int_values <- info_xy[[interact_with]]$term_values
        summary_comp <- sapply(cat_comp_level, function(lvl) {
          level_coef <- as.data.frame(model_coef[paste0(term, lvl), extract_items, drop = FALSE])
          colnames(level_coef) <- c("coef", "se", "p-value")
          level_coef$df <- 1
          summary_with_interaction <- sapply(as.character(int_values), function(val) {
            or_ci_df(
              sel_beta_term = paste0(term, lvl),
              sel_beta_int = int_lookup[int_lookup[[term]] == lvl, "INTERACTION"],
              constant_int = as.numeric(val)
            )
          }, simplify = FALSE, USE.NAMES = TRUE)
          list(
            summary_comp_level = level_coef,
            summary_with_interaction = summary_with_interaction
          )
        }, simplify = FALSE, USE.NAMES = TRUE)
        list(
          predictor = list(
            term_type = "categorical",
            interact_with = interact_with,
            counts_by_level = counts_by_level,
            term_ref_level = info_xy[[term]]$term_ref_level,
            term_comp_level = cat_comp_level),
          summary = summary_comp
        )
      } else {
        term_coef <- as.data.frame(model_coef[term, extract_items, drop = FALSE])
        colnames(term_coef) <- c("coef", "se", "p-value")
        term_coef$df <- 1
        summary_with_interaction <- sapply(c(cat_ref_level, cat_comp_level), function(cat_lvl) {
          or_ci_df(
            sel_beta_term = term,
            sel_beta_int = if (cat_lvl == cat_ref_level) {
              NULL
            } else {
              int_lookup[int_lookup[[cat]] == cat_lvl, "INTERACTION"]
            },
            constant_int = 1
          )
        }, simplify = FALSE, USE.NAMES = TRUE)
        list(
          predictor = list(term_type = info_xy[[term]]$term_type, interact_with = interact_with),
          summary = list(summary_term = term_coef,  summary_with_interaction = summary_with_interaction)
        )
      }
    }

  }, simplify = FALSE, USE.NAMES = TRUE)

  results_interaction <- as.data.frame(model_coef[int_lookup$INTERACTION, extract_items, drop = FALSE])
  colnames(results_interaction) <- c("coef", "se", "p-value")
  results_interaction$df <- 1
  if (all(terms_type == "categorical")) {
    n <- sapply(1:nrow(int_lookup), function(i) {
      model_data[
        model_data[[names(info_xy)[1]]] == int_lookup[i, names(info_xy)[1]] &
          model_data[[names(info_xy[2])]] == int_lookup[i, names(info_xy)[2]], , drop = FALSE] %>%
        nrow()
    })
    rownames(results_interaction) <- paste0(int_lookup[[1]], " * ", int_lookup[[2]], " , n = ", n)
  } else if (all(terms_type == "continuous")) {
    rownames(results_interaction) <- paste0(int_lookup[[1]], " * ", int_lookup[[2]])
  } else {
    n <- sapply(1:nrow(int_lookup), function(i) {
      model_data[model_data[[cat]] == int_lookup[i, cat], , drop = FALSE] %>%
        nrow()
    })
    rownames(results_interaction) <- paste0(int_lookup[[cat]], ", n = ", n)
  }

  results_all <- c(
    results_by_term,
    list(list(predictor = list(term_type = "interaction"), summary = results_interaction))
  )
  names(results_all) <- c(names(results_by_term), paste(names(info_xy), collapse = ":"))
  results_all
}

table_formats <- function(dfi,
                          header,
                          row.name,
                          row_format = c("full", "main", "level", "or_ci")) {
  row_format <- match.arg(row_format)
  format_or <- function(or, val = 999.99) {
    if (!is.na(or) & or > val) {
      sprintf_format(paste0(">", val))
    } else {
      "xx.xx"
    }
  }
  format_ci <- function(ucl, val = 999.99) {
    if (!is.na(ucl) & ucl > val) {
      sprintf_format(paste0("(%.2f, >",  val, ")"))
    } else {
      "(xx.xx, xx.xx)"
    }
  }
  format_pval <- function(pval, accuracy = 0.0001) {
    pvalue(pval, accuracy = accuracy, prefix = c("<", "", ">"))
  }

  row_content <- switch(
    row_format,
    full = rrow(
      row.name = row.name, rcell(dfi$df),
      rcell(dfi$coef,  format = "xx.xxx"),
      rcell(dfi$se, format = "xx.xxx"),
      rcell(dfi$or, format = format_or(dfi$or)),
      rcell(c(dfi$lcl, dfi$ucl), format = format_ci(dfi$ucl)),
      rcell(format_pval(dfi$`p-value`))
    ),
    main =  rrowl(
      row.name = row.name,
      c(
        list(rcell(dfi$df)),
        rep(list(NULL), 4),
        list(rcell(format_pval(dfi$`p-value`)))
      )
    ),
    level =  rrow(
      row.name = row.name,
      rcell(dfi$df),
      rcell(dfi$coef,  format = "xx.xxx"),
      rcell(dfi$se, format = "xx.xxx"),
      NULL,
      NULL,
      rcell(format_pval(dfi$`p-value`))
    ),
    or_ci = rrowl(
      row.name = row.name,
      c(rep(list(NULL), 3),
        list(rcell(dfi$or, format = format_or(dfi$or))),
        list(rcell(c(dfi$lcl, dfi$ucl), format = format_ci(dfi$ucl))),
        list(NULL)
      )
    )
  )
  rtable(header = header, row_content)
}
