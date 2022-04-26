#' Helper Functions for Tabulating Biomarker Effects on Survival by Subgroup
#'
#' Helper functions which are documented here separately to not confuse the user
#' when reading about the user-facing functions.
#'
#' @inheritParams survival_biomarkers_subgroups
#' @inheritParams argument_convention
#' @inheritParams fit_coxreg_multivar
#' @name h_survival_biomarkers_subgroups
#' @order 1
#' @examples
#' # Testing dataset.
#' library(scda)
#' library(dplyr)
#' library(forcats)
#' library(rtables)
#'
#' adtte <- synthetic_cdisc_data("latest")$adtte
#'
#' # Save variable labels before data processing steps.
#' adtte_labels <- formatters::var_labels(adtte, fill = FALSE)
#'
#' adtte_f <- adtte %>%
#'   filter(PARAMCD == "OS") %>%
#'   mutate(
#'     AVALU = as.character(AVALU),
#'     is_event = CNSR == 0
#'   )
#' labels <- c("AVALU" = adtte_labels[["AVALU"]], "is_event" = "Event Flag")
#' formatters::var_labels(adtte_f)[names(labels)] <- labels
NULL

#' @describeIn h_survival_biomarkers_subgroups helps with converting the "survival" function variable list
#'   to the "Cox regression" variable list. The reason is that currently there is an inconsistency between the variable
#'   names accepted by `extract_survival_subgroups()` and `fit_coxreg_multivar()`.
#' @param biomarker (`string`)\cr the name of the biomarker variable.
#' @export
#' @examples
#' # This is how the variable list is converted internally.
#' h_surv_to_coxreg_variables(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "EVNT",
#'     covariates = c("A", "B"),
#'     strata = "D"
#'   ),
#'   biomarker = "AGE"
#' )
h_surv_to_coxreg_variables <- function(variables, biomarker) {
  assertthat::assert_that(
    is.list(variables),
    assertthat::is.string(variables$tte),
    assertthat::is.string(variables$is_event),
    assertthat::is.string(biomarker)
  )
  list(
    time = variables$tte,
    event = variables$is_event,
    arm = biomarker,
    covariates = variables$covariates,
    strata = variables$strata
  )
}

#' @describeIn h_survival_biomarkers_subgroups prepares estimates for number of events, patients and median survival
#'   times, as well as hazard ratio estimates, confidence intervals and p-values, for multiple biomarkers
#'   in a given single data set.
#'   `variables` corresponds to names of variables found in `data`, passed as a named list and requires elements
#'   `tte`, `is_event`, `biomarkers` (vector of continuous biomarker variables) and optionally `subgroups` and `strat`.
#' @export
#' @examples
#'
#' # For a single population, estimate separately the effects
#' # of two biomarkers.
#' df <- h_coxreg_mult_cont_df(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "SEX",
#'     strata = c("STRATA1", "STRATA2")
#'   ),
#'   data = adtte_f
#' )
#' df
#'
#' # If the data set is empty, still the corresponding rows with missings are returned.
#' h_coxreg_mult_cont_df(
#'   variables = list(
#'     tte = "AVAL",
#'     is_event = "is_event",
#'     biomarkers = c("BMRKR1", "AGE"),
#'     covariates = "REGION1",
#'     strata = c("STRATA1", "STRATA2")
#'   ),
#'   data = adtte_f[NULL, ]
#' )
h_coxreg_mult_cont_df <- function(variables,
                                  data,
                                  control = control_coxreg()) {
  assertthat::assert_that(
    is.list(variables),
    is_df_with_variables(data, as.list(unlist(variables)))
  )
  checkmate::assert_list(control, names = "named")
  checkmate::assert_character(variables$biomarkers, min.len = 1, any.missing = FALSE)
  conf_level <- control[["conf_level"]]
  pval_label <- paste0(
    "p-value (", gsub("(^[a-z])", "\\U\\1", trimws(control[["pval_method"]]), perl = TRUE), ")"
  )
  # If there is any data, run model, otherwise return empty results.
  if (nrow(data) > 0) {
    bm_cols <- match(variables$biomarkers, names(data))
    l_result <- lapply(variables$biomarkers, function(bm) {
      coxreg_list <- fit_coxreg_multivar(
        variables = h_surv_to_coxreg_variables(variables, bm),
        data = data,
        control = control
      )
      result <- do.call(
        h_coxreg_multivar_extract,
        c(list(var = bm), coxreg_list[c("mod", "data", "control")])
      )
      data_fit <- as.data.frame(as.matrix(coxreg_list$mod$y))
      data_fit$status <- as.logical(data_fit$status)
      median <- s_surv_time(
        df = data_fit,
        .var = "time",
        is_event = "status"
      )$median
      data.frame(
        # Dummy column needed downstream to create a nested header.
        biomarker = bm,
        biomarker_label = formatters::var_labels(data[bm], fill = TRUE),
        n_tot = coxreg_list$mod$n,
        n_tot_events = coxreg_list$mod$nevent,
        median = as.numeric(median),
        result[1L, c("hr", "lcl", "ucl")],
        conf_level = conf_level,
        pval = result[1L, "pval"],
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
      n_tot_events = 0L,
      median = NA,
      hr = NA,
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

#' @describeIn h_survival_biomarkers_subgroups prepares a single sub-table given a `df_sub` containing
#'   the results for a single biomarker.
#' @param df (`data.frame`)\cr results for a single biomarker, as part of what is
#'   returned by [extract_survival_biomarkers()] (it needs a couple of columns which are
#'   added by that high-level function relative to what is returned by [h_coxreg_mult_cont_df()],
#'   see the example).
#' @export
#' @examples
#'
#' # Starting from above `df`, zoom in on one biomarker and add required columns.
#' df1 <- df[1, ]
#' df1$subgroup <- "All patients"
#' df1$row_type <- "content"
#' df1$var <- "ALL"
#' df1$var_label <- "All patients"
#' h_tab_surv_one_biomarker(
#'   df1,
#'   vars = c("n_tot", "n_tot_events", "median", "hr", "ci", "pval"),
#'   time_unit = "days"
#' )
h_tab_surv_one_biomarker <- function(df,
                                     vars,
                                     time_unit) {
  afuns <- a_survival_subgroups()[vars]
  colvars <- d_survival_subgroups_colvars(
    vars,
    conf_level = df$conf_level[1],
    method = df$pval_label[1],
    time_unit = time_unit
  )
  h_tab_one_biomarker(
    df = df,
    afuns = afuns,
    colvars = colvars
  )
}
