
#' Proportional Hazards Regression Model Fit Summary Table
#'
#' An \code{\link[rtables]{rtable}} format of \code{\link[tern]{s_coxph_pairwise}}
#' results for further annotation on top of Kaplan-Meier grob
#'
#' @inheritParams s_coxph_pairwise
#'
#' @import survival
#' @import rtables
#'
#' @export
#'
#' @template author_wangh107
#'
#' @examples
#' library(random.cdisc.data)
#' ADSL <- cadsl
#' ADTTE <- cadtte
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")
#'
#' tbl <- t_coxph(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARM) + strata(SEX),
#'   data = ADTTE_f
#' )
#' tbl
#'
#' tbl <- t_coxph(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + strata(SEX),
#'   data = ADTTE_f,
#'   conf.int = 0.8, pval_method = "wald", ties = "exact"
#' )
#' tbl
#'
#'
t_coxph <- function(formula,
                    data,
                    conf.int = 0.95, # nolint
                    pval_method = c("log-rank", "wald",  "likelihood"),
                    ...) {

  pval_method <- match.arg(pval_method)

  # this runs both stratified and unstratified if there is a strata special
  coxph_values <- s_coxph_pairwise(formula, data, conf.int, pval_method, ...)

  sel <- ifelse(has_special_strata(formula), "stratified", "unstratified")
  pval_method_str <- capitalize(pval_method)


  tbl <- rtablel(
    header = c("HR", paste0(conf.int * 100, "% CI of HR"), paste0(pval_method_str, " p-value")),
    lapply(coxph_values[-1], function(xi) { # don't want row for reference arm

      vals <- xi[[sel]]
      rrow(
        row.name = xi$compare["comparison"],
        rcell(vals$hr, format = "xx.xxxx"),
        rcell(vals$hr_ci, format = "(xx.xxxx, xx.xxxx)"),
        rcell(vals$pvalue, format = "xx.xxxx")
      )
    })
  )

  attr(tbl, "footnotes") <- paste0("pairwise comparison to \"", names(coxph_values)[1], "\"")
  tbl
}


#' Run Pairwise (ARM) CoxPH model for unstratified and stratified analysis
#'
#'
#' @param formula a survival formula, the arm variable needs to be wrapped in \code{\link{arm}}. The
#'   \code{\link[survival]{strata}} special will only be used for the stratified analysis. If there is not
#'   \code{\link[survival]{strata}} specification then the stratified analysis is omitted.
#' @param data a \code{data.frame} with all the variable that are used in \code{formula}
#' @param conf.int level for computation of the confidence intervals. If set to {FALSE} no confidence intervals are
#'   printed
#' @param pval_method the method used to calculate the p-value, should be one of "wald", "log-rank", "likelihood",
#'   default is "log-rank".
#' @param ... Other arguments will be passed to \code{\link[survival]{coxph}}
#'
#' @return a list of dataframes with unstratified and/or stratified analysis results
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")
#'
#' cox_res <-  s_coxph_pairwise(
#'   formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARM) + strata(RACE),
#'   data = ADTTE_f,
#'   pval_method = "wald",  ties = "exact"
#' )
#' names(cox_res)
#' cox_res[[2]]$stratified
#'
#' cox_res2 <- s_coxph_pairwise(
#'   formula =  Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARM),
#'   data = ADTTE_f,
#'   conf.int = 0.8, pval_method = "likelihood",  ties = "breslow"
#' )
#' cox_res2[[2]]$stratified
#' cox_res2[[2]]$unstratified
#'
s_coxph_pairwise <- function(formula,
                    data,
                    conf.int = 0.95, # nolint
                    pval_method = c("log-rank", "wald",  "likelihood"),
                    ...) {

  # extracted data
  cl <- match.call()
  stopifnot(is.data.frame(data))

  tm <- t_tte_items(formula, cl, data, parent.frame())

  arm <- tm$arm
  if (nlevels(arm) < 2) {
    stop("at least to arms expected to calculate the pairwise comparisons")
  }

  arm_var <- attr(arm, "varname")
  form_unstr <- tm$formula_nostrata
  form_str <- tm$formula_strata

  reference_lvl <- levels(arm)[1]
  comparison_lvls <- levels(arm)[-1]

  coxph_results <- lapply(comparison_lvls, function(lvl) {

    df_lvl <- data[arm %in% c(reference_lvl, lvl), , drop = FALSE]
    df_lvl[[arm_var]] <- droplevels(df_lvl[[arm_var]])


    fit_unstratified <- coxph(formula = form_unstr, data = df_lvl, ...)
    unstratified <- coxph_extract(fit_unstratified,
                                  conf.int = conf.int,
                                  pval_method = pval_method)

    stratified <- if (is.null(form_str)) {
      NULL
    } else {
      fit_stratified <- coxph(formula = form_str, data = df_lvl, ...)
      coxph_extract(fit_stratified,
                    conf.int = conf.int,
                    pval_method = pval_method)
    }

    list(
      compare = c(reference = reference_lvl, comparison = lvl),
      unstratified = unstratified,
      stratified = stratified,
      conf.int = conf.int,
      pval_method = pval_method
    )
  })

  coxph_results <- setNames(c(list(NULL), coxph_results), levels(arm))

  coxph_results
}


#' Extract Info from coxph object
#'
#' @param fit an object of {\link{\code{coxph}}}
#' @inheritParams s_coxph_pairwise
#' @noRd
#'
#'
#' @examples
#'
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")
#'
#' fit <- coxph(Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARM) + strata(RACE), data = ADTTE_f)
#'
#' tern:::coxph_extract(fit)
coxph_extract <- function(fit,
                          conf.int = 0.95, # nolint
                          pval_method = c("log-rank", "wald",  "likelihood")) {

  stopifnot(is(fit, "coxph"))
  pval_method <- match.arg(pval_method)

  msum <- summary(fit, conf.int = conf.int)

  pval <- switch(pval_method,
    "wald" = msum$waldtest["pvalue"],
    "log-rank" = msum$sctest["pvalue"], # Score (logrank) test,
    "likelihood" = msum$logtest["pvalue"]
  )

  if (all(is.na(fit$coefficients))) {
    hr <- NA_real_
    hr_ci <- c(NA_real_, NA_real_)
  } else {
    hr <- msum$conf.int[1, 1]
    hr_ci <- msum$conf.int[1, 3:4]
  }

  list(
    pvalue = as.vector(pval),
    hr =  hr,
    hr_ci = hr_ci,
    conf.int = conf.int,
    pval_method = pval_method
  )

}
