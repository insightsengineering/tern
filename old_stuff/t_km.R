#' Kaplan-Meier Fit Summary Table
#'
#' An \code{\link[rtables]{rtable}} format of KM model data for further
#' annotation on top of Kaplan-Meier grob
#'
#' @param formula (\code{formula})\cr
#'   Specifies \code{\link[survival:Surv]{survival model}}
#' @param data (\code{data.frame})\cr with all the variable that are used in
#'   \code{formula}
#' @param conf_level (\code{numeric} value)\cr
#'   level for computation of the confidence intervals.
#' @param ... (optional)\cr
#'   additional parameters passed to \code{\link{survfit}}
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
#'
#' formula <- Surv(AVAL, 1-CNSR) ~ ARM
#' tbl <- t_km(formula, data = ADTTE_f, conf.type = "plain")
#' tbl
#'
#' tbl <- t_km(formula, data = ADTTE_f, conf_level = 0.8)
#' tbl
t_km <- function(formula,
                 data,
                 conf_level = 0.95,  # nolint
                 ...) {

  fit_km <- survfit(formula, data, conf.int = conf_level, ...)
  sumtable <- summary(fit_km)$table

  if (is.null(dim(sumtable))) {
    kminfo <- sumtable[c("records", "median", paste0(conf_level, "LCL"), paste0(conf_level, "UCL"))]
    names(kminfo) <- c("records", "median", "LCL", "UCL")
    kminfo <- data.frame(as.list(kminfo))
    rownames(kminfo) <- "All"
  } else {
    kminfo <- sumtable[, c("records", "median", paste0(conf_level, "LCL"), paste0(conf_level, "UCL")), drop = FALSE]
    colnames(kminfo) <-  c("records", "median", "LCL", "UCL")
  }

  skminfo <- split(as.data.frame(kminfo), seq_len(nrow(kminfo)))

  rtablel(
    header = c("N", "median", paste0(conf_level * 100, "% CI for median")),
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
