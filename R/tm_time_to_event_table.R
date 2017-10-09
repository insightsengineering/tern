

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
#' 
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' 
#' 
#' ATE_f <- ATE %>% filter(PARAM == "OS")
#' 
#' tbl <- time_to_event_table(
#'     time_to_event = ATE_f$AVAL,
#'     is_event = (ATE_f$CNSR == "N"),
#'     arm = ATE_f$ARM,
#'     arm.ref = "DUMMY A",
#'     arm.rest = c("DUMMY C", "DUMMY B")
#' )
#' 
#' tbl_stream <- get_time_to_event_table(com.roche.cdt30019.go29436.re)
#' 
#' compare_table(tbl, tbl_stream)
#' 
#' time_to_event_table(NULL)
#' 
#' }
time_to_event_table <- function(time_to_event, event, arm, arm.ref,
                                arm.rest = setdiff(arm, arm.ref)) {
  
  ## first do math
  
  ## check your arguments
  
  ARM <- recode(arm, arm.ref, arm.rest)
  ## do arm codings (reference, etc)

  surv_km_results <- survival::survfit(formula = Surv(time_to_event, event) ~ ARM, 
                                       conf.type="plain")
  
  
  surv_km_results_summary <- summary(surv_km_results)
  surv_km_results_tidy <- broom::tidy(surv_km_results)
  
  ## then to data struture
  rtable(
    colnames = c(""),
    rrow()
  )
}

#' @export
time_to_event_table_ADAM <- function(ASL, ATE, PARAMCD, ...) {
  
   ATE_f <- ATE %>% filter(PARAMCD == PARAMCD)
   
   tbl <- time_to_event_table(
       time_to_event = ATE_f$AVAL,
       is_event = (ATE_f$CNSR == "N"),
       arm = ATE_f$ARM,
       ...
   )
   
   tbl
  
}


drop_label <- function(x){
  for (j in (1:ncol(x))){
    attr(x[[j]],"label") <- NULL
  }
  return(x)
}