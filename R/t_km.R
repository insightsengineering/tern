#' Kaplan Meier Fit Summary Table
#'
#' An \code{\link[rtables]{rtable}} format of KM model data for further
#' annotation on top of Kaplan-Meier grob
#'
#' @param fit_km a class \code{\link{survfit}} object.
#'
#' @template author_wangh107
#'
#' @import rtables
#'
#' @export
#'
#' @seealso \code{\link{g_km}}
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(seed = 1)
#' ADSL$RACE <- factor(sapply(as.character(ADSL$RACE), function(x) {
#'   if (nchar(x)>9) paste0(substr(x, 1,9), "...") else x
#' }))
#'
#' ADTTE <- radtte(ADSL, seed = 2)
#' ADTTE_f <- subset(ADTTE, PARAMCD == "OS")
#'
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = ADTTE_f, conf.type = "plain")
#' tbl <- t_km(fit_km)
#' tbl
#'
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ 1, data = ADTTE_f, conf.type = "plain")
#' tbl <- t_km(fit_km)
#' tbl
t_km <- function(fit_km) {

  if (!is(fit_km, "survfit"))
    stop("fit_km needs to be of class survfit")

  sumtable <- summary(fit_km)$table

  if (is.null(dim(sumtable))) {
    kminfo <- sumtable[c("records", "median", "0.95LCL", "0.95UCL")]
    names(kminfo) <- c("records", "median", "LCL", "UCL")
    kminfo <- data.frame(as.list(kminfo))
    rownames(kminfo) <- "All"
  } else {
    kminfo <- summary(fit_km)$table[, c("records", "median", "0.95LCL", "0.95UCL"), drop = FALSE]
    colnames(kminfo) <-  c("records", "median", "LCL", "UCL")
  }

  skminfo <- split(as.data.frame(kminfo), 1:nrow(kminfo))

  rtablel(
    header = c("N", "median", "95% CI for median"),
    lapply(skminfo, function(xi) {
      rrow(
        row.name = rownames(xi),
        rcell(xi$records, format = "xx"),
        rcell(xi$median, format = "xx.xx"),
        rcell(c(xi$`LCL`, xi$`UCL`), format = "(xx.xx, xx.xx)")
      )
    })
  )
}
