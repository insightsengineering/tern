# The survival package is not strict because of this we need the
# donttest.
#' Proportional Hazards Regression Model Fit Summary Table
#'
#' An \code{\link[rtables]{rtable}} format of \code{\link[survival]{coxph}}
#' object for further annotation on top of Kaplan-Meier grob
#'
#' @param fit_coxph a class \code{\link{survival}{coxph}} object.
#' @param info_coxph label information for Cox PH model.
#' @param pval_method the method used to calculate the p-value,
#' should be one of "wald", "logrank", "likelihood"
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
#' ADSL <- cadsl
#' ADSL$RACE <- factor(
#'   vapply(as.character(ADSL$RACE),
#'          function(x) {
#'            if (nchar(x)>9) paste0(substr(x, 1,9), "...") else x
#'          },
#'          character(1)
#'   )
#' )
#'
#' ADTTE <- cadtte
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")
#' \donttest{
#' fit_coxph <- coxph(formula = Surv(time = AVAL, time2 = 1-CNSR) ~ ARM + strata(RACE),
#'   data = ADTTE_f, ties = "exact"
#' )
#' tbl <- t_coxph(fit_coxph)
#' tbl
#' }
t_coxph <- function(fit_coxph, info_coxph = "Cox Porportional Hazard Model", pval_method = "wald"){

  stopifnot(is(fit_coxph, "coxph"))
  stopifnot(pval_method %in% c("wald", "logrank", "likelihood"))

  sfit <- summary(fit_coxph)

  hr <- sfit$coefficients[, "exp(coef)", drop = FALSE]
  ci <- sfit$conf.int[, c("lower .95", "upper .95"), drop = FALSE]

  pvalues <- if (pval_method == "wald") {
    sfit$coefficients[, "Pr(>|z|)", drop = FALSE]
  } else if (pval_method == "logrank") {
    sfit$logtest["pvalue"]
  } else {
    # likelihood test
    sfit$sctest["pvalue"]
  }

  info <- data.frame(hr, ci, `Pr(>|z|)` = pvalues, check.names = FALSE)
  sinfo <- split(info, seq_len(nrow(info)))

  pval_method_str <- paste0(toupper(substring(pval_method, 1, 1)), substring(pval_method, 2))

  rtablel(
    header = c("HR", "95% CI of HR", paste0(pval_method_str, " p-value")),
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
