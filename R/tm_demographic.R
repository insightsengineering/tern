

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
#' library(dplyr)
#' 
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' tbl_expected <- get_demographic_table(com.roche.cdt30019.go29436.re)
#' 
#' ADSL <- ASL %>% filter(ITTFL = 'Y') 
#' tbl <- demographic_table(ADSL)
#' 
#' compare_rtables(tbl, tbl_expected)
#'
#' # if all is good then
#' tbl <- tbl_expected
#' compare_rtables(tbl, tbl_expected)
#' 
#' }
#' 
#' 
demographic_table <- function(ADSL) {
  
  ADSL %needs% c("USUBJID", "STUDYID", "AGE", "SEX")
  
  table(ADSL$SEX, ADSL$STUDYID)
  
  ## get numbers
  
  ## create rtable object
  tbl <- rtable(
    col.names = c("A", "B", "C", "D"),
    format = "xx",
    rrow("row 1", 1,2,3,4),
    rrow("row 2", c(1.4223423, 2.444444), c(2,3), c(3,4), c(1,4), format = "(xx.xx, xx.xx)")
  )
  
  # Viewer(tbl)
  
  ## return rtable object
  tbl
}
