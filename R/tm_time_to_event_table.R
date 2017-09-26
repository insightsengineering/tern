

#' Time to Event Table
#' 
#' This is the description of the function
#' 
#' @param time_to_event time to event data
#' @param is_event boolean with \code{TRUE} if event and \code{FALSE} if censored
#' 
#' 
#' @return a named list with one element per row where each element contains the
#'   data for the row
#' 
#' @export
#' 
#' 
#' @examples 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr)
#' ATE <- ate(com.roche.cdpt7722.wo29637.rl)
#' ATE_f <- ATE %>% filter(PARAM == "OS")
#' X <- time_to_event_table(
#'     time_to_event = ATE_f$AVAL,
#'     is_event = (ATE$CNSR == "N")
#' )
#'  
#' }
#' 
#' time_to_event_table(NULL)
time_to_event_table <- function(time_to_event, is_event) {
  
  table(iris$Species)
  
}