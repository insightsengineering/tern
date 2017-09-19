

#' Create Demographics Table from ADSL data
#' 
#' Similiar as in STREAM
#' 
#' @param ADSL ADSL dataset with the following variables: USUBJID, STUDYID, AGE,
#'   and SEX
#' 
#' @details
#' Give a detailed description of what this function does.
#' 
#' @export
#' 
#' @author Xiao Yu Mo (mox5), \email{xiao_yu.mo@roche.com}
#' 
#' @examples 
#' n <- 100
#' ASL <- data.frame(
#'    USUBJID = paste0("id-", 1:n),
#'    STUDYID = "study 1",
#'    AGE = 40 + rnorm(n, 0, 20),
#'    SEX = sample(c("M", "F", "UNDEFINED", NA), n, replace = TRUE),
#'    stringAsFactors = FALSE
#' )
#' 
#' demographic_table(ASL)
#' 
#' \dontrun{
#' library(atezo.data)
#' ADSL <- asl(com.roche.atezo.cdpt7722.wo29637)
#' 
#' demographic_table(ADSL)
#' }
#' 
#' 
demographic_table <- function(ADSL) {
  
  ADSL %needs% c("USUBJID", "STUDYID", "AGE", "SEX")
  
  table(ADSL$SEX, ADSL$STUDYID)
}
