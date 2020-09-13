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
#' @param formula a {\code{\link{glm}}} formula \cr
#'   The formula object can be all main effect model formula, and a formula with one two-way interaction.
#' @param data a \code{data.frame} with all the variables specified in
#'   \code{formula}.
#' @param increments (\code{named list})\cr
#'   Used to specify numeric values of continuous variables in {\code{glm}} model {\code{formula}}
#'   which interact with other variables. This is used to calculate the odds ratio when comparing
#'   the other interaction variable effect. For example, for a model with ARM and AGE interaction,
#'   {\code{increments = list(AGE = c(18, 65))}} will enable calculation of odds ratios of
#'   comparison ARM vs. reference ARM at AGE = 18 and AGE = 65. If {\code{increments = NULL}}, then
#'   default AGE value is ceiling of median.
#' @param conf_level (\code{numeric} value)\cr
#'   Confidence level for Wald odds ratio confidence interval.
#' @importFrom car Anova
#' @importFrom rlang "%||%"
#' @export
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADSL <- ADSL %>% dplyr::filter(SEX %in% c("F", "M"))
#' ADRS <- radrs(ADSL, seed = 2)
#'
#' ADRS_f <- subset(ADRS, PARAMCD == "BESRSPI") %>%
#'   dplyr::mutate(Response = ifelse(AVALC %in% c("PR", "CR"), 1, 0))
#' var_labels(ADRS_f) <- c(var_labels(ADRS), "Response" = "Tumor Response")
#'
#' tbl <- t_logistic(
#'   formula = Response ~ ARM + AGE + SEX,
#'   data = ADRS_f )
#'
#' tbl # or Viewer(tbl) for a html view
#'
#' tbl2 <- t_logistic(
#'   formula = Response ~ ARM + AGE + BMRKR2 + ARM*BMRKR2,
#'   data = ADRS_f
#' )
#'
#' tbl2
#'
#' tbl3 <- t_logistic(
#'   formula = Response ~ ARM + AGE + BMRKR1 + ARM*BMRKR1,
#'   data = ADRS_f,
#'   increments = list("BMRKR1" = c(5, 10)),
#'   conf_level = 0.9
#' )
#' tbl3
#'
#' tbl4 <- t_logistic(
#'   formula = Response ~ ARM + AGE + BMRKR1 + AGE*BMRKR1,
#'   data = ADRS_f,
#'   increments = list("AGE" = c(20, 65), "BMRKR1" = c(5, 10)),
#'   conf_level = 0.9
#' )
#' tbl4
#'
t_logistic <- function(formula,
                       data,
                       increments = NULL,
                       conf_level = 0.95) {
  stopifnot(class(formula) == "formula")
  stopifnot(is.data.frame(data))
  glm_model <- glm(
    formula = formula,
    family = "binomial",
    data = data
    )

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

  terms_label <- Map(function(term) {
    if (term %in% colnames(data)) {
      label(data[[term]]) %||% term
    } else {
      terms <- lapply(unlist(strsplit(term, ":")), function(i) {
        label(data[[i]]) %||% i
      })
      paste0("Interaction of ", paste(terms, collapse = " * "))
    }
  }, terms_name)

  table_single <- function(term, s_info, terms_label, header) {
    s_term <- s_info$results[[term]]
    term_type <- s_term$predictor$term_type
    summary_term <- s_term$summary
    if (term_type == "categorical") {
      ref <- s_term$predictor$term_ref_level
      comp <- s_term$predictor$term_comp_level
      main_rowname <- paste0(
        terms_label[[term]],
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
        indent(do.call("rbind", lapply(seq_len(nrow(summary_term)), function(i) {
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
      table_formats(s_term$summary, header = header, row.name = terms_label[[term]], row_format = "full")
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
              row.name = terms_label[[term]],
              row_format = "main"
            ),
            indent(do.call("rbind", lapply(seq_len(nrow(s_info$results[[term]]$summary)), function(i) {
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
          term_row_name <- paste0(terms_label[[term]], "(Reference = ", ref_level, ", n = ", counts[ref_level], ")")
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
              indent(rtable(header = table_header, rrowl(row.name = terms_label[[interact_with]])), by = 2),
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
              row.name = terms_label[[term]],
              row_format = "level"
            ),
            indent(rtable(header = table_header, rrowl(row.name = terms_label[[interact_with]]))),
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
    do.call("rbind", lapply(seq_len(length(table_term)), function(i) {
      if (i == length(table_term)) {
        table_term[[i]]
      } else {
        rbind(table_term[[i]], rrow())
      }
    }))
  )
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
  format_pval <- function(x, output) {
    if (x < 0.0001) {
      "<.0001"
    } else {
      paste(round(x, 4))
    }
  }

  row_content <- switch(
    row_format,
    full = rrow(
      row.name = row.name, rcell(dfi$df),
      rcell(dfi$coef,  format = "xx.xxx"),
      rcell(dfi$se, format = "xx.xxx"),
      rcell(dfi$or, format = format_or(dfi$or)),
      rcell(c(dfi$lcl, dfi$ucl), format = format_ci(dfi$ucl)),
      rcell(dfi$`p-value`, format = format_pval)
    ),
    main =  rrowl(
      row.name = row.name,
      c(
        list(rcell(dfi$df)),
        rep(list(NULL), 4),
        list(rcell(dfi$`p-value`, format = format_pval))
      )
    ),
    level =  rrow(
      row.name = row.name,
      rcell(dfi$df),
      rcell(dfi$coef,  format = "xx.xxx"),
      rcell(dfi$se, format = "xx.xxx"),
      NULL,
      NULL,
      rcell(dfi$`p-value`, format = format_pval)
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
