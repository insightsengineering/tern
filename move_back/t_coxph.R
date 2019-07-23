# The survival package is not strict because of this we need the
# donttest.
#' Proportional Hazards Regression Model Fit Summary Table
#'
#' An \code{\link[rtables]{rtable}} format of \code{\link[survival]{coxph}}
#' object for further annotation on top of Kaplan-Meier grob
#'
#' @param fit_coxph a class \code{\link{survival}{coxph}} object.
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
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(seed = 1)
#' ADSL$RACE <- factor(
#'   vapply(as.character(ADSL$RACE),
#'          function(x) {
#'            if (nchar(x)>9) paste0(substr(x, 1,9), "...") else x
#'          },
#'          character(1)
#'   )
#' )
#'
#' ADTTE <- radtte(ADSL, seed = 2)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")
#' \donttest{
#' fit_coxph <- coxph(formula = Surv(time = AVAL, time2 = 1-CNSR) ~ ARM + strata(RACE),
#'   data = ADTTE_f, ties = "exact"
#' )
#
#' tbl <- t_coxph(fit_coxph)
#' tbl
#' }
t_coxph <- function(fit_coxph, info_coxph = "Cox Porportional Hazard Model") {
  stopifnot(is(fit_coxph, "coxph"))

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
        rcell(xi$`exp(coef)`, format = "xx.xxxx"),
        rcell(c(xi$`lower .95`, xi$`upper .95`), format = "(xx.xxxx, xx.xxxx)"),
        rcell(xi$`Pr(>|z|)`, format = "xx.xxxx")
      )
    })
  )

}
