#' Proportional Hazards Regression Model Fit Summary Table
#' 
#' An \code{\link[rtables]{rtable}} format of \code{\link[survival]{coxph}}
#' object for further annotation on top of Kaplan-Meier grob
#' 
#' @param fit_coxph a class \code{\link{coxph}} object.
#' @param info_coxph label information for Cox PH model.
#' 
#' @import survival
#' @import rtables
#' 
#' @export
#' 
#' @template author_wangh107
#' 
#' @examples 
#' OS <- data.frame(
#'   AVAL = abs(rnorm(200)), 
#'   CNSR = sample(c(0, 1), 200, TRUE), 
#'   ARM = sample(LETTERS[1:3], 200, TRUE),
#'   SEX = sample(c("M","F"), 200, TRUE),
#'   RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'   ECOG = sample(c(0, 1), 200, TRUE)
#' )
#'                  
#' fit_coxph <- coxph(Surv(AVAL, 1-CNSR) ~ ARM + strata(RACE), data = OS, ties = "exact")
#' tbl <- t_coxph(fit_coxph)
#' tbl
#' 
t_coxph <- function(fit_coxph, info_coxph = "Cox Porportional Hazard Model"){
  
  if (!is(fit_coxph, "coxph")) stop("fit_coxph needs to be of class coxph")
  
  sfit <- summary(fit_coxph)
  
  hr <- sfit$coefficients[, "exp(coef)", drop = FALSE]  
  ci <- sfit$conf.int[, c("lower .95", "upper .95"), drop = FALSE]  
  pvalues <- sfit$coefficients[, "Pr(>|z|)", drop = FALSE]
  
  info <- cbind(hr, ci, pvalues)
  sinfo <- split(as.data.frame(info), 1:nrow(info))
  
  rtablel(
    header = c("HR", "95% CI of HR", "Wald p-value"),
    lapply(sinfo, function(xi) {
      rrow(
        row.name = rownames(xi),
        rcell(xi$'exp(coef)', format = "xx.xxxx"),
        rcell(c(xi$`lower .95`, xi$`upper .95`), format = "(xx.xxxx, xx.xxxx)"),
        rcell(xi$'Pr(>|z|)', format = "xx.xxxx")
      )
    })
  )
  
}
