#' Helper Functions for Tabulating Biomarker Effects on Binary Response by Subgroup
#'
#' Helper functions which are documented here separately to not confuse the user
#' when reading about the user-facing functions.
#'
#' @inheritParams response_biomarkers_subgroups
#' @inheritParams argument_convention
#' @name h_response_biomarkers_subgroups
#' @order 1
#' @examples
#' # Testing dataset.
#' library(scda)
#' library(dplyr)
#' library(forcats)
#' library(rtables)
#'
#' adrs <- synthetic_cdisc_data("latest")$adrs
#' adrs_labels <- formatters::var_labels(adrs)
#'
#' adrs_f <- adrs %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   mutate(rsp = AVALC == "CR")
#' formatters::var_labels(adrs_f) <- c(adrs_labels, "Response")
#'
#' @keywords internal
NULL

#' @describeIn h_response_biomarkers_subgroups helps with converting the "response" function variable list
#'   to the "logistic regression" variable list. The reason is that currently there is an
#'   inconsistency between the variable names accepted by `extract_rsp_subgroups()` and `fit_logistic()`.
#' @param biomarker (`string`)\cr the name of the biomarker variable.
#'
#' @examples
#' # This is how the variable list is converted internally.
#' tern:::h_rsp_to_logistic_variables(
#'   variables = list(
#'     rsp = "RSP",
#'     covariates = c("A", "B"),
#'     strat = "D"
#'   ),
#'   biomarker = "AGE"
#' )
#'
#' @keywords internal
h_rsp_to_logistic_variables <- function(variables, biomarker) {
  assertthat::assert_that(
    is.list(variables),
    assertthat::is.string(variables$rsp),
    assertthat::is.string(biomarker)
  )
  list(
    response = variables$rsp,
    arm = biomarker,
    covariates = variables$covariates,
    strata = variables$strat
  )
}

#' @describeIn h_response_biomarkers_subgroups prepares estimates for number of responses, patients and
#'   overall response rate, as well as odds ratio estimates, confidence intervals and p-values, for multiple
#'   biomarkers in a given single data set.
#'   `variables` corresponds to names of variables found in `data`, passed as a named list and requires elements
#'   `rsp` and `biomarkers` (vector of continuous biomarker variables) and optionally `covariates`
#'   and `strat`.
#'
#' @examples
#' # For a single population, estimate separately the effects
#' # of two biomarkers.
#' df <- tern:::h_logistic_mult_cont_df(
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
#' tern:::h_coxreg_mult_cont_df(
#'   variables = list(
#'     rsp = "rsp",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "SEX",
#'     strat = "STRATA1"
#'   ),
#'   data = adrs_f[NULL, ]
#' )
#'
#' @keywords internal
h_logistic_mult_cont_df <- function(variables,
                                    data,
                                    control = control_logistic()) {
  assertthat::assert_that(
    is.list(variables),
    is_df_with_variables(data, as.list(unlist(variables)))
  )

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

#' @describeIn h_response_biomarkers_subgroups prepares a single sub-table given a `df_sub` containing
#'   the results for a single biomarker.
#' @param df (`data.frame`)\cr results for a single biomarker, as part of what is
#'   returned by [extract_rsp_biomarkers()] (it needs a couple of columns which are
#'   added by that high-level function relative to what is returned by [h_logistic_mult_cont_df()],
#'   see the example).
#'
#' @examples
#' # Starting from above `df`, zoom in on one biomarker and add required columns.
#' df1 <- df[1, ]
#' df1$subgroup <- "All patients"
#' df1$row_type <- "content"
#' df1$var <- "ALL"
#' df1$var_label <- "All patients"
#' tern:::h_tab_rsp_one_biomarker(
#'   df1,
#'   vars = c("n_tot", "n_rsp", "prop", "or", "ci", "pval")
#' )
#'
#' @keywords internal
h_tab_rsp_one_biomarker <- function(df,
                                    vars) {
  afuns <- a_response_subgroups()[vars]
  colvars <- d_rsp_subgroups_colvars(
    vars,
    conf_level = df$conf_level[1],
    method = df$pval_label[1]
  )
  h_tab_one_biomarker(
    df = df,
    afuns = afuns,
    colvars = colvars
  )
}
