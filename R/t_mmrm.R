# Tabulation functions for MMRM models.

#' Tabulate the LS means of an MMRM model.
#'
#' This function summarizes adjusted \code{lsmeans} and standard error, as well as conducts
#' comparisons between groups' adjusted \code{lsmeans}, where the first level of the group
#' is the reference level.
#'
#' @param object the MMRM model result produced by \code{\link{s_mmrm}}.
#' @inheritParams argument_convention
#' @param show_relative should the "reduction" (\code{control - treatment}, default) or the "increase"
#'   (\code{treatment - control}) be shown for the relative change from baseline? It is also possible
#'   to suppress this row in the table ("none").
#'
#' @return \code{rtable} object or table tree, depending on the the `table_tree` argument
#'
#' @export
#' @import rtables
#'
#' @seealso \code{\link{s_mmrm}}
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' adsl <- radsl(cached = TRUE)
#' adqs <- radqs(cached = TRUE)
#' adqs_f <- adqs %>%
#'   dplyr::filter(PARAMCD=="FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
#'   droplevels() %>%
#'   dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
#'   dplyr::mutate(AVISITN = rank(AVISITN) %>% as.factor() %>% as.numeric() %>% as.factor())
#'
#' \dontrun{
#' # Chosen optimizer 'nloptwrap_bobyqa' led to problems during model fit
#' mmrm_results <- s_mmrm(
#'   vars = list(
#'     response = "AVAL",
#'     covariates = c("STRATA1", "BMRKR2"),
#'     id = "USUBJID",
#'     arm = "ARM",
#'     visit = "AVISIT"
#'   ),
#'   data = adqs_f,
#'   cor_struct = "unstructured",
#'   weights_emmeans = "proportional",
#'   optimizer = "nloptwrap_neldermead"  # Only to speed up this example.
#' )
#' t_mmrm_lsmeans(
#'   mmrm_results,
#'   col_N = table(adsl$ARM),
#'   show_relative = "increase",
#'   table_tree = FALSE
#' )
#' }
t_mmrm_lsmeans <- function(
  object,
  col_N, # nolint
  show_relative = c("reduction", "increase", "none"),
  table_tree = TRUE
) {
  stopifnot(is(object, "mmrm"))
  show_relative <- match.arg(show_relative)

  contrasts <- object$lsmeans$contrasts
  estimates <- object$lsmeans$estimates
  vars <- object$vars
  data <- object$fit@frame

  # Ensure that the contrast data frame has all levels (including reference level).
  contrasts[[vars$arm]] <- factor(contrasts[[vars$arm]], levels = levels(estimates[[vars$arm]]))

  s_contrasts_df <- split(
    contrasts,
    contrasts[vars$visit]
  )

  s_estimates_df <- split(
    estimates,
    estimates[vars$visit]
  )

  arm_lvl <- levels(data[[vars$arm]])

  tbl_head <- rheader(rrowl("", arm_lvl))

  mmrm_node_list <- Map(function(est_i, ctrs_i, visit) {

    top_part <- list(
      rrowl(
        "n",
        tapply(
          est_i$n, factor(est_i[[vars$arm]], levels = arm_lvl),
          FUN = identity
        ),
        format = "xx"
      ),
      rrowl(
        "Adjusted Mean (SE)",
        lapply(
          split(est_i, factor(est_i[[vars$arm]], levels = arm_lvl), drop = FALSE),
          function(vector_i) {
            if (is.null(vector_i)) {
              NULL
            } else {
              c(vector_i$estimate, vector_i$se)
            }
          }
        ),
        format = sprintf_format("%.3f (%.3f)")
      ),
      rrowl(
        paste0(object$conf_level * 100, "% CI"),
        lapply(
          split(est_i, factor(est_i[[vars$arm]], levels = arm_lvl), drop = FALSE),
          function(vector_i) {
            if (is.null(vector_i)) {
              NULL
            } else {
              c(vector_i$lower_cl, vector_i$upper_cl)
            }
          }),
        format = "(xx.xxx, xx.xxx)"
      ),
      rrow(),
      rrowl(
        paste0("Difference in Adjusted Means (SE) (vs. ", object$ref_level, ")"),
        lapply(
          split(ctrs_i, factor(ctrs_i[[vars$arm]], levels = arm_lvl), drop = FALSE),
          function(vector_i) {
            if (is.null(vector_i)) {
              NULL
            } else {
              c(vector_i$estimate, vector_i$se)
            }
          }),
        format = sprintf_format("%.3f (%.3f)")
      ),
      rrowl(
        paste0(object$conf_level * 100, "% CI"),
        lapply(
          split(ctrs_i, factor(ctrs_i[[vars$arm]], levels = arm_lvl), drop = FALSE),
          function(vector_i) {
            if (is.null(vector_i)) {
              NULL
            } else {
              c(vector_i$lower_cl, vector_i$upper_cl)
            }
          }),
        format = "(xx.xxx, xx.xxx)"
      )
    )

    middle_part <- if (show_relative == "none") {
      list(rrow())
    } else {
      list(
        rrowl(
          switch(
            show_relative,
            "reduction" = "Relative Reduction (%)",
            "increase" = "Relative Increase (%)"
          ),
          lapply(
            split(ctrs_i, ctrs_i[[vars$arm]], drop = FALSE),
            function(vector_i) {
              if (is.null(vector_i)) {
                NULL
              } else {
                switch(
                  show_relative,
                  "reduction" = c(vector_i$relative_reduc),
                  "increase" = - c(vector_i$relative_reduc)  # The negative of reduction is increase.
                )
              }
            }),
          format = "xx.x%"
        ),
        rrow()
      )
    }

    bottom_part <- list(
      rrowl(
        "p-value (MMRM)",
        lapply(
          split(ctrs_i, ctrs_i[[vars$arm]], drop = FALSE),
          function(vector_i) {
            if (is.null(vector_i)) {
              NULL
            } else {
              c(vector_i$p_value)
            }
          }),
        format = "x.xxxx | (<0.0001)"
      )
    )
    tbl <- rtablel(
      header = tbl_head,
      top_part,
      middle_part,
      bottom_part
    )
    tbl <- header_add_N(tbl, col_N)

    node(
      name = visit,
      content = tbl,
      children = NULL
    )

  }, est_i = s_estimates_df, ctrs_i = s_contrasts_df, visit = names(s_estimates_df))

  tree <- invisible_node(
    name = "root",
    children = mmrm_node_list,
    content = NULL
  )

  result <- if (table_tree) {
    tree
  } else {
    to_rtable(tree)
  }
  return(result)
}

#' Helper method to convert a \code{data.frame} into \code{rtable}.
#'
#' @param x the data frame
#' @param format the format
#' @return the \code{rtables} object
#'
#' @note This could eventually be moved to the \code{rtables} package. Currently we are not doing that
#'   because of the upcoming major change of that package.
#'
#' @export
as.rtable.data.frame <- function(x, format = "xx.xx") { # nousage # nolint
  do.call(
    rtable,
    c(
      list(
        header = names(x),
        format = format
      ),
      Map(
      function(row, row.name) {
        do.call(rrow,
                c(as.list(unname(row)),
                  row.name = row.name))
      },
      row = as.data.frame(t(x)), row.name = rownames(x)
      )
    )
  )
}

#' Helper function to \code{cbind} multiple \code{rtable} objects together.
#'
#' @param ... all \code{rtable} objects
#'
#' @return the resulting \code{rtable}
#'
#' @note This could eventually be moved to the \code{rtables} package. Currently we are not doing that
#'   because of the upcoming major change of that package.
#'
#' @importFrom rtables cbind_rtables
#' @export
cbind.rtable <- function(...) { # nousage # nolint
  all_args <- list(...)
  stopifnot(all(sapply(all_args, is, "rtable")))
  current <- all_args[[1]]
  while (length(all_args <- all_args[-1]) > 0) {
    current <- rtables::cbind_rtables(current, all_args[[1]])
  }
  return(current)
}

#' Tabulate the covariance matrix estimate of an MMRM fit.
#'
#' @inheritParams t_mmrm_lsmeans
#' @param format \code{rtables} format for the numbers (default is \code{"xx.xxxx"}).
#'
#' @return \code{rtable} object with the covariance matrix
#' @export
#'
#' @import rtables
#'
#' @examples
#' \dontrun{
#' # Model failed to converge with 1 negative eigenvalue
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' adsl <- radsl(cached = TRUE)
#' adqs <- radqs(cached = TRUE)
#' adqs_f <- adqs %>%
#'   dplyr::filter(PARAMCD=="FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
#'   droplevels() %>%
#'   dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
#'   dplyr::mutate(AVISITN = rank(AVISITN) %>% as.factor() %>% as.numeric() %>% as.factor())
#'
#' mmrm_results <- s_mmrm(
#'   vars = list(
#'     response = "AVAL",
#'     covariates = c("STRATA1", "BMRKR2"),
#'     id = "USUBJID",
#'     arm = "ARM",
#'     visit = "AVISIT"
#'   ),
#'   data = adqs_f,
#'   cor_struct = "random-quadratic",
#'   optimizer = "nloptwrap_bobyqa"
#' )
#' t_mmrm_cov(mmrm_results)
#' }
t_mmrm_cov <- function(object, format = "xx.xxxx") {
  stopifnot(is(object, "mmrm"))
  cov_estimate <- object$cov_estimate
  result <- as.rtable(as.data.frame(cov_estimate), format = format)
  return(result)
}

#' Tabulate the fixed effect estimates of an MMRM fit.
#'
#' @inheritParams t_mmrm_lsmeans
#' @param format \code{rtables} format for the numbers other than degrees of freedom and p-values
#'   (default is \code{"xx.xxxx"}).
#'
#' @return \code{rtable} object with the fixed effect estimates, standard errors,
#'   degrees of freedom, t-statistics, and p-values.
#' @export
#'
#' @import rtables
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' adsl <- radsl(cached = TRUE)
#' adqs <- radqs(cached = TRUE)
#' adqs_f <- adqs %>%
#'   dplyr::filter(PARAMCD=="FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
#'   droplevels() %>%
#'   dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
#'   dplyr::mutate(AVISITN = rank(AVISITN) %>% as.factor() %>% as.numeric() %>% as.factor())
#'
#' mmrm_results <- s_mmrm(
#'   vars = list(
#'     response = "AVAL",
#'     covariates = c("STRATA1", "BMRKR2"),
#'     id = "USUBJID",
#'     arm = "ARM",
#'     visit = "AVISIT"
#'   ),
#'   data = adqs_f,
#'   cor_struct = "random-quadratic",
#'   optimizer = "nloptwrap_bobyqa"
#' )
#' t_mmrm_fixed(mmrm_results)
t_mmrm_fixed <- function(object, format = "xx.xxxx") {
  stopifnot(is(object, "mmrm"))
  fixed_table <- as.data.frame(coef(summary(object$fit)))
  pvalue_column <- match("Pr(>|t|)", names(fixed_table))
  df_column <- match("df", names(fixed_table))
  pvalue_table <- as.rtable(fixed_table[, pvalue_column, drop = FALSE], format = "x.xxxx | (<0.0001)")
  df_table <- as.rtable(fixed_table[, df_column, drop = FALSE], format = "xx.")  # xx. rounds to 0 digits.
  remaining_table <- as.rtable(fixed_table[, - c(df_column, pvalue_column), drop = FALSE], format = format)
  result <- cbind(remaining_table, df_table, pvalue_table)
  return(result)
}

#' Tabulate the diagnostic statistics of an MMRM fit.
#'
#' @inheritParams t_mmrm_lsmeans
#' @param format \code{rtables} format for the numbers (default is \code{"xx.xxxx"}).
#'
#' @return \code{rtable} object with the model fit diagnostics
#' @export
#'
#' @import rtables
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' adsl <- radsl(cached = TRUE)
#' adqs <- radqs(cached = TRUE)
#' adqs_f <- adqs %>%
#'   dplyr::filter(PARAMCD=="FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
#'   droplevels() %>%
#'   dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
#'   dplyr::mutate(AVISITN = rank(AVISITN) %>% as.factor() %>% as.numeric() %>% as.factor())
#'
#' mmrm_results <- s_mmrm(
#'   vars = list(
#'     response = "AVAL",
#'     covariates = c("STRATA1", "BMRKR2"),
#'     id = "USUBJID",
#'     arm = "ARM",
#'     visit = "AVISIT"
#'   ),
#'   data = adqs_f,
#'   cor_struct = "random-quadratic",
#'   optimizer = "nloptwrap_bobyqa"
#' )
#' t_mmrm_diagnostic(mmrm_results)
t_mmrm_diagnostic <- function(object, format = "xx.xxxx") {
  stopifnot(is(object, "mmrm"))
  diagnostics <- object$diagnostics

  one_row <- function(row_i, row_name_i) {
    do.call(
      rrow,
      c(
        list(row_i),
        row.name = row_name_i,
        format = format
      )
    )
  }

  result <- do.call(
    rtable,
    c(
      list(header = c("Diagnostic statistic value")),
      Map(
        one_row,
        row_i = diagnostics,
        row_name_i = names(diagnostics)
      )
    )
  )
  return(result)
}

#' Mix model with repeated measurements (MMRM) model
#'
#' The MMRM table function summarizes MMRM test results by visit and groups. The
#' function produces adjusted \code{lsmeans} and standard error, as well as conducts
#' comparisons between groups' adjusted means, where the first level of the group
#' is the reference level.
#'
#' @details
#'
#' Create \code{rtable} of MMRM test results
#'
#' @inheritParams a_mmrm
#' @inheritParams argument_convention
#'
#' @return \code{rtable} object or table tree, depending on the the `table_tree` argument
#'
#' @export
#'
#' @seealso \code{\link{a_mmrm}}
#'
#' @examples
#' library(dplyr)
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADQS <- radqs(cached = TRUE)
#' ADQS_f <- ADQS %>%
#'   dplyr::filter(PARAMCD=="FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
#'   droplevels() %>%
#'   dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
#'   dplyr::mutate(AVISITN = rank(AVISITN) %>% as.factor() %>% as.numeric() %>% as.factor())
#'
#' t_mmrm(formula = AVAL ~ ARM + AVISIT + STRATA1 + BMRKR2 + ARM*AVISIT,
#'        data = ADQS_f,
#'        id_var = "USUBJID",
#'        arm_var = "ARM",
#'        visit_var = "AVISIT",
#'        col_N = table(ADSL$ARM),
#'        mode = "boot-satterthwaite",
#'        conf_level = 0.95,
#'        weights_emmeans = "proportional",
#'        corStruct = "corSymm",
#'        table_tree = FALSE
#' )

t_mmrm <- function(formula = AVAL ~ arm(ARM) + visit(AVISIT) + ARM * VISIT,
                   data,
                   id_var = "USUBJID",
                   arm_var = "ARM",
                   visit_var = "AVISIT",
                   col_N, # nolint
                   mode = c("df.error", "auto", "boot-satterthwaite"),
                   conf_level = 0.95, # nolint
                   weights_emmeans = "proportional",
                   corStruct = NULL, # nolint
                   table_tree = TRUE) {

  mmrm_result <- a_mmrm(
    formula = formula,
    data = data,
    id_var = id_var,
    arm_var = arm_var,
    visit_var = visit_var,
    mode = mode,
    conf_level = conf_level,
    weights_emmeans = weights_emmeans,
    corStruct = corStruct
  )

  s_contrast_df <- split(
    mmrm_result$`contrast`,
    mmrm_result$`contrast`[visit_var]
  )

  s_estimate_df <- split(
    mmrm_result$`estimate`,
    mmrm_result$`estimate`[visit_var]
  )

  arm_lvl <- levels(data[[arm_var]])

  tbl_head <- rheader(rrowl("", arm_lvl))

  format_pval <- function(x, output) {
    if (x < 0.0001) {
      "<.0001"
    } else {
      paste(round(x, 4))
    }
  }

  mmrm_node_list <- Map(function(df1_i, df2_i, visit) {

    tbl <- rtable(
      header = tbl_head,
      rrowl(
        "n",
        tapply(
          df1_i$`n`, factor(df1_i[[arm_var]], levels = arm_lvl),
          function(n_i) {
            c(n_i)
          }
        ),
        format = "xx"
      ),
      rrowl(
        "Adjusted Mean (SE)",
        lapply(
          split(df1_i, factor(df1_i[[arm_var]], levels = arm_lvl), drop = FALSE),
          function(vector_i) {
            if (is.null(vector_i)) {
              NULL
            } else {
              c(vector_i$`emmean`, vector_i$`SE`)
            }
          }
        ),
        format = sprintf_format("%.3f (%.3f)")
      ),
      rrowl(
        paste0(mmrm_result$`conf_level` * 100, "% CI"),
        lapply(
          split(df1_i, factor(df1_i[[arm_var]], levels = arm_lvl), drop = FALSE),
          function(vector_i) {
            if (is.null(vector_i)) {
              NULL
            } else {
              c(vector_i$`lower.CL`, vector_i$`upper.CL`)
            }
          }),
        format = "(xx.xxx, xx.xxx)"
      ),
      rrow(),
      rrowl(
        paste0("Difference in Adjusted Means (SE) (vs. ", mmrm_result$`ref_level`, ")"),
        lapply(
          split(df2_i, factor(df2_i[[arm_var]], levels = arm_lvl), drop = FALSE),
          function(vector_i) {
            if (is.null(vector_i)) {
              NULL
            } else {
              c(vector_i$`estimate`, vector_i$`SE`)
            }
          }),
        format = sprintf_format("%.3f (%.3f)")
      ),
      rrowl(
        paste0(mmrm_result$`conf_level` * 100, "% CI"),
        lapply(
          split(df2_i, factor(df2_i[[arm_var]], levels = arm_lvl), drop = FALSE),
          function(vector_i) {
            if (is.null(vector_i)) {
              NULL
            } else {
              c(vector_i$`lower.CL`, vector_i$`upper.CL`)
            }
          }),
        format = "(xx.xxx, xx.xxx)"
      ),
      rrowl(
        "Relative Reduction (%)",
        lapply(
          split(df2_i, df2_i[[arm_var]], drop = FALSE),
          function(vector_i) {
            if (is.null(vector_i)) {
              NULL
            } else {
              c(vector_i$`relative_reduc`)
            }
          }),
        format = "xx.x%"
      ),
      rrow(),
      rrowl(
        "p-value (MMRM)",
        lapply(
          split(df2_i, df2_i[[arm_var]], drop = FALSE),
          function(vector_i) {
            if (is.null(vector_i)) {
              NULL
            } else {
              c(vector_i$`p.value`)
            }
          }),
        format = format_pval
      )
    )

    tbl <- header_add_N(tbl, col_N)

    node(
      name = visit,
      content = tbl,
      children = NULL
    )

  }, s_estimate_df, s_contrast_df, names(s_estimate_df))

  tree <- invisible_node(
    name = "root",
    children = mmrm_node_list,
    content = NULL
  )

  if (table_tree) {
    tree
  } else {
    to_rtable(tree)
  }
}

#' MMRM model, test, estimate
#'
#'
#' @param formula a \code{gls} formula.
#' @param data a \code{data.frame} with all the variables specified in
#'   \code{formula}. Records with missing values in any independent variables
#'   will be excluded.
#' @param id_var a character describing the variable name used as subject IDs,
#'   \code{"USUBJID"} by default.
#' @param arm_var a character describing the variable name for \code{\link{arm}},
#'   \code{"ARM"} by default. The arm variable must be factor in \code{data}
#' @param visit_var a character describing the variable name for visit,
#'   \code{"AVISIT"} by default. This variable must be factor in data.
#' @param mode algorithm for degree of freedom: \code{auto}, \code{df.error} or
#'   \code{boot-satterthwaite}.
#' @param conf_level confidence level. Must be number greater than 0 and less
#'   than 1.
#' @param weights_emmeans argument from \code{\link[emmeans]{emmeans}}, "proportional" by default.
#' @param corStruct \code{NULL} by default or a string with the name of \code{\link[nlme]{corClasses}}.
#'
#' @return a dataframe with MMRM results
#'
#' @importFrom dplyr filter group_by_at left_join mutate n summarise rename ungroup
#' @importFrom nlme gls corSymm corAR1 corARMA corCAR1 corCompSymm
#'  corExp corGaus corLin corRatio corSpher varIdent
#' @importFrom emmeans emmeans contrast
#' @importFrom stats complete.cases na.exclude
#' @importFrom rlang :=
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADQS <- radqs(cached = TRUE)
#' ADQS_f <- ADQS %>%
#'   dplyr::filter(PARAMCD=="FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
#'   droplevels() %>%
#'   dplyr::mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
#'   dplyr::mutate(AVISITN = rank(AVISITN) %>% as.factor() %>% as.numeric() %>% as.factor())
#'
#' \dontrun{
#' # currently not executed because the algorithm to obtain the
#' # Satterthwaite estimator does not always converge
#' mmrm_results <- a_mmrm(
#'   data = ADQS_f,
#'   formula = AVAL ~ ARM + AVISIT + STRATA1 + BMRKR2 + ARM * AVISIT,
#'   id_var = "USUBJID",
#'   arm_var = "ARM",
#'   visit_var = "AVISIT",
#'   mode = "boot-satterthwaite",
#'   conf_level = 0.95,
#'   weights_emmeans = "proportional",
#'   corStruct = "corSymm"
#' )
#'
#' names(mmrm_results)
#'
#' mmrm_results["contrast"]
#' mmrm_results["estimate"]
#' }
a_mmrm <- function(data,
                   formula = AVAL ~ ARM + AVISIT + ARM * VISIT,
                   id_var = "USUBJID",
                   arm_var = "ARM",
                   visit_var = "AVISIT",
                   mode = c("df.error", "auto", "boot-satterthwaite"),
                   conf_level = 0.95, # nolint
                   weights_emmeans = "proportional",
                   corStruct = NULL # nolint
) {

  mode <- match.arg(mode)

  # extract relevant information from formula
  mt <- terms(formula, data = data)

  vars <- all.vars(attr(mt, "variables"))
  regressor_vars <- c(vars[-1L], id_var)

  stopifnot(
    is.data.frame(data),
    is.null(corStruct) || corStruct %in% c(
      "corAR1", "corARMA", "corCAR1", "corCompSymm", "corExp", "corGaus",
      "corLin", "corRatio", "corSpher", "corSymm"
    )
  )

  if (!id_var %in% names(data)) {
    stop(paste("Subject ID variable", id_var, "does not exist in input data"))
  }

  if (!arm_var %in% regressor_vars) {
    stop(paste("Arm variable", arm_var, "does not exist in formula"))
  }

  if (!visit_var %in% regressor_vars) {
    stop(paste("Visit variable", visit_var, "does not exist in formula"))
  }

  i_resp <- attr(mt, "response")
  if (i_resp == 0) {
    stop("need a response variable")
  }

  response_var <- vars[i_resp]

  if (!all(vars %in% names(data))) {
    stop("All variables in the formula  must appear in 'data' (no scoping allowed)")
  }

  environment(formula) <- new.env() # no scoping for formula elements needed

  # SAS excludes records with any missing independent varibles. In gls, any such missing value will cause error.
  if (!all(complete.cases(data[, regressor_vars]))) {
    warning(
      "Some records have missing independent variables, which will be excluded.",
      head(data[!complete.cases(data[, regressor_vars]), c(id_var, vars)]),
      call. = FALSE
    )
  }

  data_complete <- data %>%
    dplyr::filter(stats::complete.cases(data[, regressor_vars])) %>%
    droplevels()

  arm_symbol <- sym(arm_var)
  arm_values <- data_complete[[arm_var]]
  response_values <- data_complete[[response_var]]

  stopifnot(nlevels(arm_values) >= 2)
  reference_level <- levels(arm_values)[1]

  stopifnot(
    is.numeric(response_values),
    is.factor(arm_values),
    is.factor(data_complete[[visit_var]])
  )

  # Modeling step in SAS assumes non-missing response variable; however estimation step considers all data
  if (any(is.na(response_values))) {
    warning(
      "Some records have a missing endpoint, which will be excluded from MMRM modeling, but included in estimation.",
      head(data_complete[is.na(response_values), c(id_var, visit_var, response_var)]),
      call. = FALSE
    )
  }

  # remove all entries where response is NA, droplevels as well
  data_cc <- data_complete %>%
    dplyr::filter(!is.na(!!sym(response_var))) %>%
    droplevels()


  # check all arms will still be present after NA filtering
  stopifnot(nlevels(arm_values) == nlevels(data_cc[[arm_var]]))

  # each arm should have at least have 5 records
  if (!all(table(data_cc[[arm_var]]) > 5)) {
    stop(paste("Each group / arm should have at least 5 records with non-missing", response_var))
  }

  cor_formula <- as.formula(paste("~", "as.numeric(", visit_var, ") | ", id_var))
  correlation <- if (is.null(corStruct)) {
    NULL
  } else {
    do.call(corStruct, list(form = cor_formula))
  }

  fit_mmrm <- gls(
    model = formula,
    correlation = correlation,
    weights = varIdent(form = as.formula(paste(" ~ 1 |", visit_var))),
    method = "REML",
    data = data_cc, # model fit on complete case
    na.action = stats::na.exclude
  )

  # hacky for emmeans since emmeans evaluation and namespace is not good
  # https://github.com/rvlenth/emmeans/issues/13
  fit_mmrm$call$model <- substitute(formula)
  emm <- emmeans(
    fit_mmrm,
    mode = mode,
    specs = as.formula(paste("~ ", arm_var, "|", visit_var)),
    data = data_complete %>% select(-c(response_var)), # estimate on original data
    weights = weights_emmeans
  )

  # Relative Reduction (in change from baseline) is calculated using model based
  # LS means as 100*(LS mean change from baseline in Control Pooled group –
  # LS mean change from baseline in Treatment Group)/LS mean change from
  # baseline in Control Pooled group.

  ### adjusted estimate for each arm
  estimate <- confint(emm, level = conf_level) %>%
    as.data.frame()

  data_n <- data_complete %>%
    dplyr::group_by_at(.vars = c(visit_var, arm_var)) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::ungroup()

  estimate <- estimate %>%
    dplyr::left_join(data_n, by = c(visit_var, arm_var))

  # get emmean for reference group to join into full dataframe so that relative reduction in
  # emmean (mean of response variable) can be computed with respect to reference level (e.g. ARM A)
  means_at_ref <- estimate %>%
    dplyr::filter(!!arm_symbol == reference_level) %>%
    dplyr::select(c(visit_var, "emmean")) %>%
    dplyr::rename(ref = .data$emmean)

  relative_reduc <- estimate %>%
    dplyr::filter(!!arm_symbol != reference_level) %>%
    dplyr::left_join(means_at_ref, by = c(visit_var)) %>%
    dplyr::mutate(relative_reduc = (.data$ref - .data$emmean) / .data$ref) %>%
    dplyr::select(c(visit_var, arm_var, "relative_reduc"))

  sum_fit_diff <- summary(
    contrast(emm, method = "trt.vs.ctrl", parens = NULL),
    level = conf_level,
    infer = c(TRUE, TRUE),
    adjust = "none"
  )

  # get the comparison group name from "contrast" column, e.g. "ARMB - ARMA" returns "ARMB", i.e. remove " - ARMA"
  contrast <- sum_fit_diff %>%
    dplyr::mutate(
      col_by = factor(gsub(paste0("\\s-\\s", reference_level), "", contrast), levels = levels(data[[arm_var]]))
    ) %>%
    dplyr::select(-contrast) %>%
    dplyr::rename(!!arm_symbol := .data$col_by) %>%
    dplyr::left_join(relative_reduc, by = c(visit_var, arm_var))

  message("MMRM methodology in R is different from SAS. Please use as exploratory purpose.")

  list(
    contrast = contrast,
    estimate = estimate,
    ref_level = reference_level,
    conf_level = conf_level
  )
}
