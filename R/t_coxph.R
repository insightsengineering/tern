# The survival package is not strict because of this we need the
# donttest.
#' Proportional Hazards Regression Model Fit Summary Table
#'
#' An \code{\link[rtables]{rtable}} format of \code{\link[tern]{s_coxph}}
#' results for further annotation on top of Kaplan-Meier grob
#'
#' @inheritParams s_coxph
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
#' \donttest{
#' formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARM) + strata(SEX)
#' tbl <- t_coxph(formula, ADTTE_f)
#' tbl
#'
#' formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARMCD) + strata(SEX)
#' tbl <- t_coxph(formula, ADTTE_f, conf.int = 0.8, pval_method = "wald", ties = "exact")
#' tbl
#' }
t_coxph <- function(formula,
                    data,
                    conf.int = 0.95,
                    pval_method = c("log-rank", "wald",  "likelihood"),
                    ...){

  coxph_values <- s_coxph(formula, data, conf.int, pval_method, ...)
  arm_var <- coxph_values$arm_var
  info <- if (!is.null(coxph_values$stratified)){
    coxph_values$stratified
  } else {
    coxph_values$unstratified
  }
  sinfo <- split(info, seq_len(nrow(info)))
  pval_method <- match.arg(pval_method)
  pval_method_str <- paste0(toupper(substring(pval_method, 1, 1)), substring(pval_method, 2))

  rtablel(
    header = c("HR", paste0(conf.int*100, "% CI of HR"), paste0(pval_method_str, " p-value")),
    lapply(sinfo, function(xi) {
      rrow(
        row.name = xi[, arm_var],
        rcell(xi$hr, format = "xx.xxxx"),
        rcell(unlist(xi$hr_ci), format = "(xx.xxxx, xx.xxxx)"),
        rcell(xi$pvalue, format = "xx.xxxx")
      )
    })
  )

}


#' CoxPH model results for unstratified and stratified analysis
#'
#' @param formula a survival formula, the arm variable needs to be wrapped in
#'   \code{\link{arm}}. The \code{\link[survival]{strata}} special will only be
#'   used for the stratified analysis. If there is not
#'   \code{\link[survival]{strata}} specification then the stratified analysis
#'   is omitted.
#' @param data a \code{data.frame} with all the variable that are used in
#'   \code{formula}
#' @param conf.int level for computation of the confidence intervals. If set to {FALSE} no confidence intervals are printed
#' @param pval_method the method used to calculate the p-value,
#' should be one of "wald", "log-rank", "likelihood", default is "log-rank".
#' @param ... Other arguments will be passed to \code{\link[survival]{coxph}}
#' @return a list of dataframes with unstratified and/or stratified analysis results
#' @export
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADTTE <- cadtte
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")
#' \donttest{
#' formula = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARM) + strata(RACE)
#' cox_res <-  s_coxph(formula, data = ADTTE_f, pval_method = "wald",  ties = "exact")
#' names(cox_res)
#' cox_res$stratified
#'
#' formula2 = Surv(time = AVAL, event = 1 - CNSR) ~ arm(ARM)
#' cox_res2 <- s_coxph(formula2, data = ADTTE_f, 0.8, pval_method = "likelihood",  ties = "breslow")
#' }
#'
s_coxph <- function(formula,
                    data,
                    conf.int = 0.95,
                    pval_method = c("log-rank", "wald",  "likelihood"), ...){
  cl <- match.call()
  stopifnot(is.data.frame(data))
  # extracted data
  tm <- t_tte_items(formula, cl, data, parent.frame())
  arm <- tm$arm
  varname <- attr(arm, "varname")
  form_unstr <- tm$formula_nostrata
  form_str <- tm$formula_strata
  reference_lvl <- levels(arm)[1]
  comparison_lvls <- levels(arm)[-1]

  coxph_results <- lapply(comparison_lvls, function(lvl){
    df_lvl <- data[arm %in% c(reference_lvl, lvl), , drop = FALSE]
    df_lvl[[varname]] <- droplevels(df_lvl[[varname]])
    unstratified <- coxph_extract(formula = form_unstr,
                                  data = df_lvl,
                                  conf.int = conf.int,
                                  pval_method = pval_method,
                                  ...)
    unstratified[[varname]] <- lvl
    if (is.null(form_str)){
      stratified <- NULL
    } else {
      stratified <- coxph_extract(formula = form_str,
                                  data = df_lvl,
                                  conf.int = conf.int,
                                  pval_method = pval_method,
                                  ...)
      stratified[[varname]] <- lvl
    }
    list(unstratified = unstratified, stratified = stratified)
  })

  unstratified <- lapply(coxph_results, function(res) res[["unstratified"]]) %>% do.call("rbind", .)
  stratified <- lapply(coxph_results, function(res) res[["stratified"]]) %>% do.call("rbind", .)

  list(unstratified = unstratified,
       stratified = stratified,
       ref_level = reference_lvl,
       arm_var = varname)
}


#' @inheritParams s_coxph
#' @noRd
coxph_extract <- function(formula,
                          data,
                          conf.int = 0.95,
                          pval_method = c("log-rank", "wald",  "likelihood"), ...){
  pval_method <- match.arg(pval_method)
  fit_coxph <- tryCatch(
    coxph(formula, data = data, ...),
    error = function(e) NULL
  )
  msum <- summary(fit_coxph, conf.int)
  pval <- if (pval_method == "wald"){
    if (is.null(fit_coxph)) NA else msum$waldtest["pvalue"]
  } else if (pval_method == "log-rank"){
    # Score (logrank) test
    if (is.null(fit_coxph)) NA else msum$sctest["pvalue"]
  } else if (pval_method == "likelihood"){
    if (is.null(fit_coxph)) NA else msum$logtest["pvalue"]
  }

  hr <- if (is.null(fit_coxph) || all(is.na(fit_coxph$coefficients))){
    NA
  } else {
    msum$conf.int[1, 1]
  }
  hr_ci <- if (is.null(fit_coxph) || all(is.na(fit_coxph$coefficients))){
    c(NA, NA)
  } else {
    msum$conf.int[1, 3:4]
  }

  data.frame(pvalue = pval,
             hr =  hr,
             hr_ci = I(list(hr_ci)),
             row.names = NULL)

}
