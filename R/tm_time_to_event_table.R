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
#' library(teal.oncology)
#' library(atezo.data)
#' library(dplyr)
#' library(survival)
#' library(broom)
#' library(stringr)
#' 
#' ASL <- atezo.data::asl(com.roche.cdt30019.go29436.re)
#' ATE <- atezo.data::ate(com.roche.cdt30019.go29436.re)
#' 
#' tte_tbl_stream <- atezo.data::get_time_to_event_table(com.roche.cdt30019.go29436.re)
#' teal.oncology::Viewer(tte_tbl_stream)
#'
#' ASL_f <- dplyr::filter(ASL,ITTFL == "Y")
#'         
#' ATE_f <- dplyr::filter(ATE,ITTFL == "Y" & PARAMCD == "OS")
#' 
#' }

time_to_event <- ATE_f$AVAL
event <- ifelse(is.na(ATE_f$CNSR),NA,
         ifelse(ATE_f$CNSR==1,0,1))
arm = ATE_f$ARM1
big_n_arm <- ASL_f$ARM1
arm.ref = "DUMMY C"

  
  ARM <- factor(arm, levels = c(arm.ref, setdiff(arm, arm.ref)))
  
  levels(big_n_arm) <- as.vector(levels(ARM))
  
  surv_km_fit <- survival::survfit(
    formula = Surv(time_to_event, event) ~ ARM, 
    conf.type="plain"
  )
  
  surv_km_summary <- summary(surv_km_fit)
  surv_km_table <- surv_km_summary$table
  surv_km_broom <- broom::tidy(surv_km_fit)
  ref_surv_km_ranges <- surv_km_broom[which(stringr::str_sub(surv_km_broom$strata,5,
                                      nchar(surv_km_broom$strata)) == as.vector(levels(ARM))[1]),]$time

  comp1_surv_km_ranges <- surv_km_broom[which(stringr::str_sub(surv_km_broom$strata,5,
                                        nchar(surv_km_broom$strata)) == as.vector(levels(ARM))[2]),]$time

  comp2_surv_km_ranges <- surv_km_broom[which(stringr::str_sub(surv_km_broom$strata,5,
                                        nchar(surv_km_broom$strata)) == as.vector(levels(ARM))[3]),]$time
  
  surv_km_quantile <- quantile(surv_km_fit)$quantile

  # BIG N #
  ref_BIG_N <- length(big_n_arm[big_n_arm == as.vector(levels(big_n_arm))[1]])
  comp1_BIG_N <- length(big_n_arm[big_n_arm == as.vector(levels(big_n_arm))[2]])
  comp2_BIG_N <- length(big_n_arm[big_n_arm == as.vector(levels(big_n_arm))[3]])

  ref_big_n <- surv_km_table[1,1]
  comp1_big_n <- surv_km_table[2,1]
  comp2_big_n <- surv_km_table[3,1]
  
  if (as.numeric(ref_BIG_N) != ref_big_n) stop("Reference Population Differences Between ASL and ATE!")
  if (as.numeric(comp1_BIG_N) != comp1_big_n) stop("Comparator 1 Population Differences Between ASL and ATE!")
  if (as.numeric(comp2_BIG_N) != comp2_big_n) stop("Comparator 2 Population Differences Between ASL and ATE!")
  
  tte_big_n <- data.frame(ref_big_n,comp1_big_n,comp2_big_n)
  
  # Patients With Events #
  ref_n_events <- surv_km_table[1,4]
  comp1_n_events <- surv_km_table[2,4]
  comp2_n_events <- surv_km_table[3,4]
  
  ref_p_events <- ref_n_events / ref_big_n
  comp1_p_events <- comp1_n_events / comp1_big_n
  comp2_p_events <- comp2_n_events / comp2_big_n

  tte_np_events <- data.frame(ref_n_events,ref_p_events,comp1_n_events,comp1_p_events,
                              comp2_n_events,comp2_p_events)
  
  # Patients Without Events #  
  ref_n_wo_events <- ref_big_n - ref_n_events
  comp1_n_wo_events <- comp1_big_n - comp1_n_events
  comp2_n_wo_events <- comp2_big_n - comp2_n_events
  
  ref_p_wo_events <- ref_n_wo_events / ref_big_n
  comp1_p_wo_events <- comp1_n_wo_events / comp1_big_n
  comp2_p_wo_events <- comp2_n_wo_events / comp2_big_n

  tte_np_wo_events <- data.frame(ref_n_wo_events,ref_p_wo_events,comp1_n_wo_events,comp1_p_wo_events,
                                 comp2_n_wo_events,comp2_p_wo_events)
  
  # Median Time to Event #
  ref_med_tte <- surv_km_table[1,7]
  comp1_med_tte <- surv_km_table[2,7]
  comp2_med_tte <- surv_km_table[3,7]
  
  ref_med_tte_lcl <- surv_km_table[1,8]
  ref_med_tte_ucl <- surv_km_table[1,9]
  
  comp1_med_tte_lcl <- surv_km_table[2,8]
  comp1_med_tte_ucl <- surv_km_table[2,9]

  comp2_med_tte_lcl <- surv_km_table[3,8]
  comp2_med_tte_ucl <- surv_km_table[3,9]
 
  tte_median <- data.frame(ref_med_tte,ref_med_tte_lcl,ref_med_tte_ucl,
                           comp1_med_tte,comp1_med_tte_lcl,comp1_med_tte_ucl,
                           comp2_med_tte,comp2_med_tte_lcl,comp2_med_tte_ucl)

  # 25% and 75% Time to Event Percentiles #
  ref_25th <- surv_km_quantile[1,1]
  ref_75th <- surv_km_quantile[1,3]
  
  comp1_25th <- surv_km_quantile[2,1]
  comp1_75th <- surv_km_quantile[2,3]
  
  comp2_25th <- surv_km_quantile[3,1]
  comp2_75th <- surv_km_quantile[3,3]

  tte_quantiles <- data.frame(ref_25th,ref_75th,comp1_25th,comp1_75th,comp2_25th,comp2_75th)
  
  # Minimum and Maximum Time to Events #
  ref_km_min <- min(ref_surv_km_ranges)
  ref_km_max <- max(ref_surv_km_ranges)
  
  comp1_km_min <- min(comp1_surv_km_ranges)
  comp1_km_max <- max(comp1_surv_km_ranges)
  
  comp2_km_min <- min(comp2_surv_km_ranges)
  comp2_km_max <- max(comp2_surv_km_ranges
                      )  
  tte_range <- data.frame(ref_km_min,ref_km_max,comp1_km_min,comp1_km_max,comp2_km_min,comp2_km_max)
  
  
  tte_table <- list(tte_big_n,tte_np_events,tte_np_wo_events,tte_median,tte_quantiles,tte_range)
  
  #return(tte_table)
  
  
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
#tbl <- rtable(
#    col.names = c("DUMMY C\n(N=255)", "DUMMY B\n(N=254)", "DUMMY A\n(N=254)"),
#    format = "xx",
#    rrow("Responders", c(1, 0.1), c(2, .3), c(33, .43245), format = "xx (xx.xx%)"),
#    rrow("Non-Responders", c(3, 0.2), c(43,0.32), c(33, .3442), format = "xx (xx.xx%)"),
#    rrow(),
#    rrow("95% CI for Response Rates (Clopper−Pearson)", c(0,1), c(0, 2), c(4,5), format = "(xx.xx, xx.xx)")
#  )
  
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


#'          dplyr::mutate(time_to_event   = AVAL,
#'                        status = 
#'                                 
#'                                 #time_to_event_table <- function(time_to_event, event,
#                                arm, arm.ref,
#                                vars_strat
#                                ) {
ref_BIG_N <- table(big_n_arm)[[1]]
comp1_BIG_N <- table(big_n_arm)[[2]]
comp2_BIG_N <- table(big_n_arm)[[3]]
# argument checking
#  n <- length(time_to_event)
#  if (length(event) != n) stop("event has wrong length")
#  if (length(arm) != n) stop("arm has wrong length")
#   if (!(arm.ref %in% arm)) stop("arm.ref not in arm")

#  if (length(arm.ref)!=1) stop("referemce arm to be expected length 1")