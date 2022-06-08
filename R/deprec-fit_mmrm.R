
#' Deprecated by `tern.mmrm::fit_mmrm`: MMRM Analysis
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Does the MMRM analysis. Multiple other functions can be called on the result to produce
#' tables and graphs.
#'
#' @param vars (named `list` of `string` or `character`)\cr specifying the variables in the MMRM.
#'   The following elements need to be included as character vectors and match corresponding columns
#'   in \code{data}:
#'   \describe{
#'   \item{response}{the response variable}
#'   \item{covariates}{the additional covariate terms (might also include interactions)}
#'   \item{id}{the subject ID variable}
#'   \item{arm}{the treatment group variable (factor)}
#'   \item{visit}{the visit variable (factor)}
#'   }
#'   Note that the main effects and interaction of `arm` and `visit` are by default included in the model.
#' @param data a \code{data.frame} with all the variables specified in
#'   \code{vars}. Records with missing values in any independent variables
#'   will be excluded.
#' @inheritParams argument_convention
#' @param cor_struct a string specifying the correlation structure, defaults to
#'   \code{"unstructured"}. See the details.
#' @param weights_emmeans argument from [emmeans::emmeans()], "proportional" by default.
#' @param optimizer a string specifying the optimization algorithm which should be used. By default, "automatic"
#'   will (if necessary) try all possible optimization algorithms and choose the best result. If another algorithm
#'   is chosen and does not give a valid result, an error will occur.
#' @param parallel flag that controls whether "automatic" optimizer search can use available free cores on the
#'   machine (not default).
#'
#' @details Only `Satterthwaite` adjusted degrees of freedom (d.f.) are supported, because they
#'   match the results obtained in SAS (confirmed for unstructured and compound symmetry correlation structures).
#'
#'   For the correlation structure (\code{cor_struct}), the user can choose among the following options, sorted
#'   in descending number of variance parameters:
#'   \describe{
#'   \item{unstructured}{Unstructured covariance matrix. This is the most flexible choice and default.
#'      If there are \code{T} visits, then \code{T * (T+1) / 2} variance parameters are used.
#'      Note: the current actual implementation uses one more variance parameter, which does not have any
#'      effect of the results. Therefore we report here the actually relevant number of parameters.}
#'   \item{random-quadratic}{Random quadratic spline for the random effects of the time variable.
#'      7 variance parameters are used.}
#'   \item{random-slope}{Random slope for the random effects of the time variable. 4 variance parameters are used.}
#'   \item{compound-symmetry}{Constant correlation between visits. 2 variance parameters are used.}
#'   }
#'
#'   For the \code{optimizer}, the user can choose among the following alternatives to the recommended "automatic":
#'   \describe{
#'   \item{nloptwrap_neldermead}{\code{NLopt} version of the `Nelder-Mead` algorithm (via package \code{nloptr})}
#'   \item{nloptwrap_bobyqa}{\code{NLopt} version of the `BOBYQA` algorithm (via package \code{nloptr})}
#'   \item{bobyqa}{`BOBYQA` algorithm (via package \code{minqa})}
#'   \item{nlminbwrap}{`nlminb` algorithm (wrapper for \code{\link[stats]{nlminb})}}
#'   \item{neldermead}{`lme4` version of the `Nelder-Mead` algorithm with box constraints (via package \code{lme4})}
#'   \item{nmkbw}{`Nelder-Mead` algorithm (via package \code{dfoptim})}
#'   \item{optimx_lbfgsb}{L-BFGS-B algorithm (via package \code{optimx})}
#'   }
#'
#' @return An \code{mmrm} object which is a list with MMRM results:
#' \describe{
#'   \item{fit}{The \code{lmerModLmerTest} object which was fitted to the data. Note that the attribute \code{optimizer}
#'     contains the finally used optimization algorithm, which can be useful for refitting the model later on.}
#'   \item{cov_estimate}{The matrix with the covariance matrix estimate.}
#'   \item{diagnostics}{A list with model diagnostic statistics (REML criterion, AIC, corrected AIC, BIC).}
#'   \item{lsmeans}{This is a list with data frames \code{estimates} and \code{contrasts}.}
#'   \item{vars}{The variable list.}
#'   \item{labels}{Corresponding list with variable labels extracted from \code{data}.}
#'   \item{ref_level}{The reference level for the arm variable, which is always the first level.}
#'   \item{conf_level}{The confidence level which was used to construct the confidence intervals.}
#' }
#'
#' @note
#' The ordering of the input data sets can lead to slightly different numerical results or
#' different convergence behavior. This is a known observation with the used package
#' \code{lme4}. However, once convergence is achieved, the results are reliable up to
#' numerical precision.
#'
#' @export
#'
#' @examples
#' library(scda)
#' library(dplyr)
#' library(rtables)
#'
#' adsl <- synthetic_cdisc_data("latest")$adsl
#' adqs <- synthetic_cdisc_data("latest")$adqs
#' adqs_f <- adqs %>%
#'   filter(PARAMCD == "FKSI-FWB" & !AVISIT %in% c("BASELINE")) %>%
#'   droplevels() %>%
#'   mutate(ARMCD = factor(ARMCD, levels = c("ARM B", "ARM A", "ARM C"))) %>%
#'   mutate(
#'     AVISITN = rank(AVISITN) %>%
#'       as.factor() %>%
#'       as.numeric() %>%
#'       as.factor()
#'   )
#' formatters::var_labels(adqs_f) <- formatters::var_labels(adqs)
#' \dontrun{
#' # sometimes results in failure to converge with 1 negative eigenvalue
#' # in the event that it fails to converge, change the optimizer to "automatic"
#' mmrm_results <- fit_mmrm(
#'   vars = list(
#'     response = "AVAL",
#'     covariates = c("STRATA1", "BMRKR2"),
#'     id = "USUBJID",
#'     arm = "ARMCD",
#'     visit = "AVISIT"
#'   ),
#'   data = adqs_f,
#'   cor_struct = "unstructured",
#'   weights_emmeans = "equal",
#'   optimizer = "nloptwrap_neldermead" # Only to speed up this example.
#' )
#' }
#'
fit_mmrm <- function(vars = list(
                       response = "AVAL",
                       covariates = c(),
                       id = "USUBJID",
                       arm = "ARM",
                       visit = "AVISIT"
                     ),
                     data,
                     conf_level = 0.95,
                     cor_struct = "unstructured",
                     weights_emmeans = "proportional",
                     optimizer = "automatic",
                     parallel = FALSE) {
  lifecycle::deprecate_stop(
    when = "0.7.7",
    what = "tern::fit_mmrm()",
    with = "tern.mmrm::fit_mmrm()"
  )
}
