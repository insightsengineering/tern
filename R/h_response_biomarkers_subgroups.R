#' Helper functions for tabulating biomarker effects on binary response by subgroup
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Helper functions which are documented here separately to not confuse the user
#' when reading about the user-facing functions.
#'
#' @inheritParams response_biomarkers_subgroups
#' @inheritParams extract_rsp_biomarkers
#' @inheritParams argument_convention
#'
#' @examples
#' library(dplyr)
#' library(forcats)
#'
#' adrs <- tern_ex_adrs
#' adrs_labels <- formatters::var_labels(adrs)
#'
#' adrs_f <- adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   mutate(rsp = AVALC == "CR")
#' formatters::var_labels(adrs_f) <- c(adrs_labels, "Response")
#'
#' @name h_response_biomarkers_subgroups
NULL

#' @describeIn h_response_biomarkers_subgroups helps with converting the "response" function variable list
#'   to the "logistic regression" variable list. The reason is that currently there is an
#'   inconsistency between the variable names accepted by `extract_rsp_subgroups()` and `fit_logistic()`.
#'
#' @param biomarker (`string`)\cr the name of the biomarker variable.
#'
#' @return
#' * `h_rsp_to_logistic_variables()` returns a named `list` of elements `response`, `arm`, `covariates`, and `strata`.
#'
#' @examples
#' # This is how the variable list is converted internally.
#' h_rsp_to_logistic_variables(
#'   variables = list(
#'     rsp = "RSP",
#'     covariates = c("A", "B"),
#'     strata = "D"
#'   ),
#'   biomarker = "AGE"
#' )
#'
#' @export
h_rsp_to_logistic_variables <- function(variables, biomarker) {
  if ("strat" %in% names(variables)) {
    warning(
      "Warning: the `strat` element name of the `variables` list argument to `h_rsp_to_logistic_variables() ",
      "was deprecated in tern 0.9.4.\n  ",
      "Please use the name `strata` instead of `strat` in the `variables` argument."
    )
    variables[["strata"]] <- variables[["strat"]]
  }
  checkmate::assert_list(variables)
  checkmate::assert_string(variables$rsp)
  checkmate::assert_string(biomarker)
  list(
    response = variables$rsp,
    arm = biomarker,
    covariates = variables$covariates,
    strata = variables$strata
  )
}

#' @describeIn h_response_biomarkers_subgroups prepares estimates for number of responses, patients and
#'   overall response rate, as well as odds ratio estimates, confidence intervals and p-values, for multiple
#'   biomarkers in a given single data set.
#'   `variables` corresponds to names of variables found in `data`, passed as a named list and requires elements
#'   `rsp` and `biomarkers` (vector of continuous biomarker variables) and optionally `covariates`
#'   and `strata`.
#'
#' @return
#' * `h_logistic_mult_cont_df()` returns a `data.frame` containing estimates and statistics for the selected biomarkers.
#'
#' @examples
#' # For a single population, estimate separately the effects
#' # of two biomarkers.
#' df <- h_logistic_mult_cont_df(
#'   variables = list(
#'     rsp = "rsp",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "SEX"
#'   ),
#'   data = adrs_f
#' )
#' df
#'
#' # If the data set is empty, still the corresponding rows with missings are returned.
#' h_coxreg_mult_cont_df(
#'   variables = list(
#'     rsp = "rsp",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "SEX",
#'     strata = "STRATA1"
#'   ),
#'   data = adrs_f[NULL, ]
#' )
#'
#' @export
h_logistic_mult_cont_df <- function(variables,
                                    data,
                                    control = control_logistic()) {
  if ("strat" %in% names(variables)) {
    warning(
      "Warning: the `strat` element name of the `variables` list argument to `h_logistic_mult_cont_df() ",
      "was deprecated in tern 0.9.4.\n  ",
      "Please use the name `strata` instead of `strat` in the `variables` argument."
    )
    variables[["strata"]] <- variables[["strat"]]
  }
  assert_df_with_variables(data, variables)

  checkmate::assert_character(variables$biomarkers, min.len = 1, any.missing = FALSE)
  checkmate::assert_list(control, names = "named")

  conf_level <- control[["conf_level"]]
  pval_label <- "p-value (Wald)"

  # If there is any data, run model, otherwise return empty results.
  if (nrow(data) > 0) {
    bm_cols <- match(variables$biomarkers, names(data))
    l_result <- lapply(variables$biomarkers, function(bm) {
      model_fit <- fit_logistic(
        variables = h_rsp_to_logistic_variables(variables, bm),
        data = data,
        response_definition = control$response_definition
      )
      result <- h_logistic_simple_terms(
        x = bm,
        fit_glm = model_fit,
        conf_level = control$conf_level
      )
      resp_vector <- if (inherits(model_fit, "glm")) {
        model_fit$model[[variables$rsp]]
      } else {
        as.logical(as.matrix(model_fit$y)[, "status"])
      }
      data.frame(
        # Dummy column needed downstream to create a nested header.
        biomarker = bm,
        biomarker_label = formatters::var_labels(data[bm], fill = TRUE),
        n_tot = length(resp_vector),
        n_rsp = sum(resp_vector),
        prop = mean(resp_vector),
        or = as.numeric(result[1L, "odds_ratio"]),
        lcl = as.numeric(result[1L, "lcl"]),
        ucl = as.numeric(result[1L, "ucl"]),
        conf_level = conf_level,
        pval = as.numeric(result[1L, "pvalue"]),
        pval_label = pval_label,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, args = c(l_result, make.row.names = FALSE))
  } else {
    data.frame(
      biomarker = variables$biomarkers,
      biomarker_label = formatters::var_labels(data[variables$biomarkers], fill = TRUE),
      n_tot = 0L,
      n_rsp = 0L,
      prop = NA,
      or = NA,
      lcl = NA,
      ucl = NA,
      conf_level = conf_level,
      pval = NA,
      pval_label = pval_label,
      row.names = seq_along(variables$biomarkers),
      stringsAsFactors = FALSE
    )
  }
}
