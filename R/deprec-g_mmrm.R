#' MMRM Plots
#'
#' This summarizes available plotting functions for MMRM.
#'
#' @seealso [g_mmrm_diagnostic], [g_mmrm_lsmeans]
#' @name g_mmrm
NULL

#' Deprecated by [tern.mmrm::g_mmrm_diagnostic]: Diagnostic Plots for MMRM
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' This function produces diagnostic plots.
#'
#' @param object (`mmrm`)\cr model result produced by \code{\link{fit_mmrm}}.
#' @param type (`string`)\cr specifying the type of diagnostic plot to be produced:
#'   \describe{
#'     \item{fit-residual}{A fitted vs residuals plot, grouped by visits. This allows to see if there is remaining
#'       structure in the residuals that might be captured by adding additional covariates to the model.}
#'     \item{q-q-residual}{A Q-Q plot for the residuals (i.e. sorted standardized residuals vs. normal quantiles),
#'       grouped by visits. Observations with an absolute standardized residual above \code{z_threshold} will be
#'       labeled.}
#'   }
#' @param z_threshold (`numeric`)\cr optional number indicating the normal quantile threshold for the Q-Q plot.
#'
#' @return a \code{ggplot2} plot
#'
#' @details Here we use marginal fitted values and residuals. That is, only the fixed effects are used
#'   to estimate fitted values, and the difference of those fitted values vs. the observed data are
#'   the residuals. This is in line with the MMRM philosophy where random effects are only used to
#'   model the marginal residual distribution.
#'
#' @export
#'
#' @seealso \code{\link{g_mmrm_lsmeans}} for plotting the LS means and contrasts.
#'
#' @examples
#' library(dplyr)
#' library(scda)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adqs <- synthetic_cdisc_data("latest")$adqs
#' adqs_f <- adqs %>%
#'   filter(PARAMCD == "FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
#'   droplevels() %>%
#'   mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
#'   mutate(AVISITN = rank(AVISITN) %>% as.factor() %>% as.numeric() %>% as.factor())
#' \dontrun{
#' mmrm_results <- fit_mmrm(
#'   vars = list(
#'     response = "AVAL",
#'     covariates = c("STRATA1", "BMRKR2"),
#'     id = "USUBJID",
#'     arm = "ARM",
#'     visit = "AVISIT"
#'   ),
#'   data = adqs_f,
#'   cor_struct = "random-quadratic",
#'   weights_emmeans = "equal"
#' )
#' g_mmrm_diagnostic(mmrm_results)
#' g_mmrm_diagnostic(mmrm_results, type = "q-q-residual")
#' }
g_mmrm_diagnostic <- function(object,
                              type = c("fit-residual", "q-q-residual"),
                              z_threshold = NULL) {
  lifecycle::deprecate_stop(
    when = "0.7.7",
    what = "tern::g_mmrm_diagnostic()",
    with = "tern.mmrm::g_mmrm_diagnostic()"
  )
}

#' Deprecated by [tern.mmrm::g_mmrm_diagnostic]: Plot LS means for MMRM
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' This function summarizes adjusted \code{lsmeans} and standard error, as well as conducts
#' comparisons between groups' adjusted \code{lsmeans}, where the first level of the group
#' is the reference level.
#'
#' @param object (`mmrm`)\cr model result produced by \code{\link{fit_mmrm}}.
#' @param select (`character`)\cr to select one or both of "estimates" and "contrasts" plots.
#' Note "contrasts" option is not applicable to model summaries excluding an arm variable.
#' @param titles (`character`)\cr with elements \code{estimates} and \code{contrasts} containing the plot titles.
#' @param ylab (`string`)\cr with the y axis label.
#' @param width (`numeric`)\cr width of the error bars.
#' @param show_pval (`flag`)\cr should the p-values for the differences of
#' LS means vs. control be displayed (not default)?
#'
#' @return a \code{ggplot2} plot
#'
#' @details If variable labels are available in the original data set, then these are used. Otherwise
#'   the variable names themselves are used for annotating the plot.
#' @details Contrast plot is not going to be returned if treatment is not
#' considered in the `mmrm` object considered in `object` argument,
#' no matter if `select` argument contains `contrasts` value.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(scda)
#' library(rtables)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adqs <- synthetic_cdisc_data("latest")$adqs
#' adqs_f <- adqs %>%
#'   filter(PARAMCD == "FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
#'   droplevels() %>%
#'   mutate(ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
#'   mutate(AVISITN = rank(AVISITN) %>% as.factor() %>% as.numeric() %>% as.factor())
#' formatters::var_labels(adqs_f) <- formatters::var_labels(adqs)
#' \dontrun{
#' mmrm_results <- fit_mmrm(
#'   vars = list(
#'     response = "AVAL",
#'     covariates = c("STRATA1", "BMRKR2"),
#'     id = "USUBJID",
#'     arm = "ARM",
#'     visit = "AVISIT"
#'   ),
#'   data = adqs_f,
#'   cor_struct = "random-quadratic",
#'   weights_emmeans = "equal"
#' )
#' g_mmrm_lsmeans(mmrm_results)
#' g_mmrm_lsmeans(mmrm_results, select = "estimates")
#' g_mmrm_lsmeans(
#'   mmrm_results,
#'   select = "contrasts",
#'   titles = c(contrasts = "Contrasts of FKSI-FWB means"),
#'   show_pval = TRUE,
#'   width = 0.8
#' )
#'
#' adqs_f2 <- adqs_f %>%
#'   filter(ARMCD == "ARM A")
#'
#' mmrm_results_no_arm <- fit_mmrm(
#'   vars = list(
#'     response = "AVAL",
#'     covariates = c("STRATA1", "BMRKR2"),
#'     id = "USUBJID",
#'     visit = "AVISIT"
#'   ),
#'   data = adqs_f2,
#'   cor_struct = "random-quadratic",
#'   weights_emmeans = "equal"
#' )
#'
#' g_mmrm_lsmeans(mmrm_results_no_arm, select = "estimates")
#' g_mmrm_lsmeans(
#'   mmrm_results_no_arm,
#'   select = c("estimates", "contrasts"),
#'   titles = c(
#'     estimates = "Adjusted mean of FKSI-FWB",
#'     contrasts = "it will not be created"
#'   ),
#'   show_pval = TRUE,
#'   width = 0.8
#' )
#'
#' g_mmrm_lsmeans(
#'   mmrm_results_no_arm,
#'   select = c("estimates"),
#'   titles = c(estimates = "Adjusted mean of FKSI-FWB"),
#'   show_pval = TRUE,
#'   width = 0.8
#' )
#' }
g_mmrm_lsmeans <- function(object,
                           select = c("estimates", "contrasts"),
                           titles = c(
                             estimates = paste("Adjusted mean of", object$labels$response, "by treatment at visits"),
                             contrasts = paste0(
                               "Differences of ", object$labels$response, " adjusted means vs. control ('", object$ref_level, "')"
                             )
                           ),
                           ylab = paste0("Estimates with ", round(object$conf_level * 100), "% CIs"),
                           width = 0.6,
                           show_pval = TRUE) {
  lifecycle::deprecate_stop(
    when = "0.7.7",
    what = "tern::g_mmrm_lsmeans()",
    with = "tern.mmrm::g_mmrm_lsmeans()"
  )
}
