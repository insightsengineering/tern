

#' Create the Response Table
#' 
#' @param time_to_event time to event variable
#' @param event is a boolean with \code{TRUE} if event and \code{FALSE} if
#'   censored
#' @param arm arm data
#' 
#' @export
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' library(atezo.data)
#' 
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' 
#' library(dplyr)
#' 
#' ATE_F <- ATE %>% filter(PARAMCD == "OS")
#' 
#' response_table(
#'    time_to_event = ATE_F$AVAL,
#'    event = ATE_F$CNSR == 0,
#'    arm = ATE_F$ARM
#' )
#' 
#' get_response_table(com.roche.cdt30019.go29436.re)
#' 
#' } 
#' 
response_table <- function(time_to_event, event, arm) {
  
  # do argument checking, e.g length of vectors consistend
  
  
  # do the math
  
  #  survfit(Surv(time_to_event, event) ~ arm)  
  
  # build your data structure
  
  list(
    "Responders" = list(c(1,1), c(2,2)),
    "Non-Responders" = list(c(2,2), c(3,3))
  )
}



#' with ASL and ATE adam data
#' 
#' @param ASL is the asl dataset
#' @param ATE is the ate dataset
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' response_table_adam(ASL, ATE)
#' 
#' }
response_table_adam <- function(ASL, ATE) {
  
  response_table(ATE$AVAL, !ATE$CNSR, ATE$ARM)
  
}
