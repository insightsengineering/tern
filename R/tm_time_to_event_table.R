

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
#' @importFrom survival Surv survfit 
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
#' tbl_stream <- get_time_to_event_table(com.roche.cdt30019.go29436.re)
#' Viewer(tbl_stream)
#' 
#' ATE_f <- ATE %>% filter(PARAMCD == "OS")
#' 
#' tbl <- time_to_event_table(
#'     time_to_event = ATE_f$AVAL,
#'     event = (ATE_f$CNSR == 0)*1,
#'     arm = ATE_f$ARM1,
#'     arm.ref = "DUMMY C"
#' )
#' 
#' tbl
#' 
#' compare_rtables(tbl, tbl_stream, tol = 0.2)
#' 
#' 
#' }
time_to_event_table <- function(time_to_event, event,
                                arm, arm.ref,
                                vars_strat
                                ) {
  
  # argument checking
  n <- length(time_to_event)
  if (length(event) != n) stop("event has wrong length")
  if (length(arm) != n) stop("arm has wrong length")
  
  if (length(arm.ref)!=1) stop("referemce arm to be expected length 1")

  if (!(arm.ref %in% arm)) stop("arm.ref not in arm")
  
  ARM <- factor(arm, levels = c(arm.ref, setdiff(arm, arm.ref)))

  surv_km_fit <- survfit(
    formula = Surv(time_to_event, event) ~ ARM, 
    conf.type="plain"
  )
  
  surv_km_results_summary <- summary(surv_km_fit)
  surv_km_results_tidy <- broom::tidy(surv_km_fit)
  
  surv_km_results <- surv_km_results_summary$table
  
  
  #----------------------#
  # Top 3rd of TTE Table #
  #----------------------#
  ref_BIG_N <- surv_km_results[1,1]
  comp_BIG_N <- surv_km_results[2,1]
  
  ref_n_events <- surv_km_results[1,4]
  comp_n_events <- surv_km_results[2,4]
  
  ref_n_wo_events <- ref_BIG_N - ref_n_events
  comp_n_wo_events <- comp_BIG_N - comp_n_events
  
  ref_p_events <- ref_n_events / ref_BIG_N
  comp_p_events <- comp_n_events / comp_BIG_N
  
  ref_p_wo_events <- ref_n_wo_events / ref_BIG_N
  comp_p_wo_events <- comp_n_wo_events / comp_BIG_N
  
  ref_med_tte <- surv_km_results[1,7]
  comp_med_tte <- surv_km_results[2,7]
  
  ref_med_tte_lcl <- surv_km_results[1,8]
  ref_med_tte_ucl <- surv_km_results[1,9]
  
  comp_med_tte_lcl <- surv_km_results[2,8]
  comp_med_tte_ucl <- surv_km_results[2,9]
  
  
  surv_km_quantiles <- quantile(surv_km_fit)$quantile
  
  ref_25th <- surv_km_quantiles[1,1]
  ref_75th <- surv_km_quantiles[1,3]
  
  comp_25th <- surv_km_quantiles[2,1]
  comp_75th <- surv_km_quantiles[2,3]
  
  surv_km_ranges <- broom::tidy(surv_km_fit)
  
  ref_km_min <- min(surv_km_ranges[which(stringr::str_sub(surv_km_ranges$strata,5,
                                                          nchar(surv_km_ranges$strata)) == arm.ref),]$time,na.rm=TRUE)
  
  ref_km_max <- max(surv_km_ranges[which(stringr::str_sub(surv_km_ranges$strata,5,
                                                          nchar(surv_km_ranges$strata)) == arm.ref),]$time,na.rm=TRUE)
  
#  comp_km_min <- min(surv_km_ranges[which(stringr::str_sub(surv_km_ranges$strata,5,
#                                                           nchar(surv_km_ranges$strata)) == comp_arm),]$time,na.rm=TRUE)
  
#  comp_km_max <- max(surv_km_ranges[which(stringr::str_sub(surv_km_ranges$strata,5,
#                                                           nchar(surv_km_ranges$strata)) == comp_arm),]$time,na.rm=TRUE)
  
#  tte_BIG_N <- data.frame(ref_BIG_N,comp_BIG_N)
#  tte_np_events <- data.frame(ref_n_events,ref_p_events,comp_n_events,comp_p_events)
#  tte_np_wo_events <- data.frame(ref_n_wo_events,ref_p_wo_events,comp_n_wo_events,comp_p_wo_events)
#  tte_median <- data.frame(ref_med_tte,ref_med_tte_lcl,ref_med_tte_ucl,
#                           comp_med_tte,comp_med_tte_lcl,comp_med_tte_ucl)
#  tte_quantiles <- data.frame(ref_25th,ref_75th,comp_25th,comp_75th)
#  tte_range <- data.frame(ref_km_min,ref_km_max,comp_km_min,comp_km_max)
  
  #-------------------------#
  # Middle 3rd of TTE Table #
  #-------------------------#
#  cox_ph_hr_lcl_ucl <- summary(survival::coxph(Surv(time,status) ~ arm, data = tte_data))$conf.int
#  cox_ph_hr <- cox_ph_hr_lcl_ucl[,1]
#  cox_ph_hr_lcl <- cox_ph_hr_lcl_ucl[,3]
#  cox_ph_hr_ucl <- cox_ph_hr_lcl_ucl[,4]
  
#  cox_ph_coefficients  <- summary(survival::coxph(Surv(time,status) ~ arm, data = tte_data))$coefficients
#  cox_ph_hr_pvalue <- cox_ph_coefficients[,5]
  
#  unstrat_cox <- data.frame(cox_ph_hr_pvalue,cox_ph_hr,cox_ph_hr_lcl,cox_ph_hr_ucl)
  
#  strat_cox_ph_hr_lcl_ucl <- summary(survival::coxph(Surv(time,status) ~ arm + strata(cox_strat1,cox_strat2,cox_strat3), data = tte_data))$conf.int
#  strat_cox_ph_hr <- strat_cox_ph_hr_lcl_ucl[,1]
#  strat_cox_ph_hr_lcl <- strat_cox_ph_hr_lcl_ucl[,3]
#  strat_cox_ph_hr_ucl <- strat_cox_ph_hr_lcl_ucl[,4]
  
#  strat_cox_ph_coefficients <- summary(survival::coxph(Surv(time,status) ~ arm + strata(cox_strat1,cox_strat2,cox_strat3), data = tte_data))$coefficients
#  start_cox_pvalue <- strat_cox_ph_coefficients[,5]
  
#  strat_cox <- data.frame(strat_cox_ph_hr_pvalue,strat_cox_ph_hr,strat_cox_ph_hr_lcl,strat_cox_ph_hr_ucl)
  
#  tte_table <- list(tte_BIG_N,tte_np_events,tte_np_wo_events,tte_median,tte_quantiles,tte_range,unstrat_cox,strat_cox)
  
#  return(tte_table)
  
  ## then to data struture
  tbl <- rtable(
    col.names = c("DUMMY C\n(N=255)", "DUMMY B\n(N=254)", "DUMMY A\n(N=254)"),
    format = "xx",
    rrow("Responders", c(1, 0.1), c(2, .3), c(33, .43245), format = "xx (xx.xx%)"),
    rrow("Non-Responders", c(3, 0.2), c(43,0.32), c(33, .3442), format = "xx (xx.xx%)"),
    rrow(),
    rrow("95% CI for Response Rates (Clopper−Pearson)", c(0,1), c(0, 2), c(4,5), format = "(xx.xx, xx.xx)")
  )
  
  # compare_rtables(tbl, tbl_stream, tol = 0.2)
  
  tbl
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