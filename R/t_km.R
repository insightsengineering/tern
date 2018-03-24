#' Prepare K-M model annotation data with rtable format
#' 
#' An \code{\link[rtables]{rtable}} format of KM model data for further
#' annotation on top of Kaplan-Meier grob
#' 
#' @param fit_km a class \code{\link{survfit}} object.
#' 
#' 
#' @template author_wangh107
#' 
#' @import rtables
#' 
#' @export
#' 
#' 
#' @examples 
#' 
#' OS <- data.frame(AVAL = abs(rnorm(200)), 
#'                  CNSR = sample(c(0, 1), 200, TRUE), 
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = OS, conf.type = "plain")
#' tbl <- t_km(fit_km)
#' tbl
#' 
t_km <- function(fit_km) {
  if (!is(fit_km, "survfit")) stop("fit_km needs to be of class survfit")
  kminfo <- summary(fit_km)$table[ , c("records", "median", "0.95LCL", "0.95UCL")]
  skminfo <- split(as.data.frame(kminfo), 1:nrow(kminfo))
  tbl  <- do.call(
    rtable,
    c(
      list(header = c("N", "median", "95% CI for median")),
      lapply(skminfo, function(xi) {
        rrow(
          row.name = rownames(xi),
          rcell(xi$records, format = "xx"),
          rcell(xi$median, format = "xx.xx"),
          rcell(c(xi$`0.95LCL`, xi$`0.95UCL`), format = "(xx.xx, xx.xx)")
        )
      })
    )
  )
  tbl 
}