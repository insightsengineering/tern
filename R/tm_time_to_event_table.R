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
#' library(readr)
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

#ASL_csv <- dplyr::select(ASL_f,USUBJID,ARM1)
#ATE_csv <- dplyr::select(ATE_f,USUBJID,ARM1,AVAL,CNSR)
#readr::write_csv(ASL_csv,"ASL_f.csv")
#readr::write_csv(ATE_csv,"ATE_f.csv")

#time_to_event <- ATE_f$AVAL
#event <- ifelse(is.na(ATE_f$CNSR),NA,
#                ifelse(ATE_f$CNSR==1,0,1))
#arm <- ATE_f$ARM1
#big_n_arm <- ASL_f$ARM1
#arm.ref <- "DUMMY C"
#strata1 <- as.factor(ATE_f$SEX)
#strata2 <- as.factor(ATE_f$MLIVER)
#strata3 <- as.factor(ATE_f$TCICLVL2)
#time_point <- as.numeric(6)

time_to_event_table <- function(time_to_event,event,arm,big_n_arm,arm.ref,
                                strata1,strata2,strata3,time_point) {

  # Argument Checking #
  n <- length(time_to_event)
  if (length(event) != n) stop("event has incorrect length!")
  if (length(arm) != n) stop("arm has incorrect length!")
  if (!(arm.ref %in% arm)) stop("arm.ref is not in arm!")
  if (length(arm.ref)!=1) stop("reference arm should have length 1!")

  ARM <- factor(arm, levels = c(arm.ref, setdiff(arm, arm.ref)))
  
  levels(big_n_arm) <- as.vector(levels(ARM))
  
  surv_km_fit <- survival::survfit(
    formula = Surv(time_to_event, event) ~ ARM, 
    conf.type="plain"
  )
  
  surv_cox_ph <- survival::coxph(
    formula = Surv(time_to_event, event) ~ ARM
  )
  
  strat_surv_cox_ph <- survival::coxph(
    formula = Surv(time_to_event, event) ~ ARM + strata(strata1,strata2,strata3)
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

  # Time Point Analysis #  
  ref_time_points <- surv_km_broom[which(stringr::str_sub(surv_km_broom$strata,5,
                                   nchar(surv_km_broom$strata)) == as.vector(levels(ARM))[1]),]
  ref_time_point <- max(subset(ref_time_points,time <= time_point)$time)
  ref_time_point_row <- subset(ref_time_points,time == ref_time_point)
  ref_patients_remaining_at_risk <- ref_time_point_row$n.risk
  ref_patients_event_free_rate <- ref_time_point_row$estimate
  ref_patients_event_free_rate_lcl <- ref_time_point_row$conf.low
  ref_patients_event_free_rate_ucl <- ref_time_point_row$conf.high
  
  comp1_time_points <- surv_km_broom[which(stringr::str_sub(surv_km_broom$strata,5,
                                     nchar(surv_km_broom$strata)) == as.vector(levels(ARM))[2]),]
  comp1_time_point <- max(subset(comp1_time_points,time <= time_point)$time)
  comp1_time_point_row <- subset(comp1_time_points,time == comp1_time_point)
  comp1_patients_remaining_at_risk <- comp1_time_point_row$n.risk
  comp1_patients_event_free_rate <- comp1_time_point_row$estimate
  comp1_patients_event_free_rate_lcl <- comp1_time_point_row$conf.low
  comp1_patients_event_free_rate_ucl <- comp1_time_point_row$conf.high
  
  comp2_time_points <- surv_km_broom[which(stringr::str_sub(surv_km_broom$strata,5,
                                     nchar(surv_km_broom$strata)) == as.vector(levels(ARM))[3]),]
  comp2_time_point <- max(subset(comp2_time_points,time <= time_point)$time)
  comp2_time_point_row <- subset(comp2_time_points,time == comp2_time_point)
  comp2_patients_remaining_at_risk <- comp2_time_point_row$n.risk
  comp2_patients_event_free_rate <- comp2_time_point_row$estimate
  comp2_patients_event_free_rate_lcl <- comp2_time_point_row$conf.low
  comp2_patients_event_free_rate_ucl <- comp2_time_point_row$conf.high
  
  time_point_analysis <- data.frame(time_point,ref_patients_remaining_at_risk,ref_patients_event_free_rate,
                                    ref_patients_event_free_rate_lcl,ref_patients_event_free_rate_ucl,
                                    comp1_patients_remaining_at_risk,comp1_patients_event_free_rate,
                                    comp1_patients_event_free_rate_lcl,comp1_patients_event_free_rate_ucl,
                                    comp2_patients_remaining_at_risk,comp2_patients_event_free_rate,
                                    comp2_patients_event_free_rate_lcl,comp2_patients_event_free_rate_ucl)
  
  # BIG N #
  ref_BIG_N <- length(big_n_arm[big_n_arm == as.vector(levels(big_n_arm))[1]])
  comp1_BIG_N <- length(big_n_arm[big_n_arm == as.vector(levels(big_n_arm))[2]])
  comp2_BIG_N <- length(big_n_arm[big_n_arm == as.vector(levels(big_n_arm))[3]])

  ref_big_n <- surv_km_table[1,1]
  comp1_big_n <- surv_km_table[2,1]
  comp2_big_n <- surv_km_table[3,1]
  
  # ASL vs. ATE Checking #
  if (as.numeric(ref_BIG_N) != ref_big_n) stop("Reference Population Differences Between ASL and ATE!")
  if (as.numeric(comp1_BIG_N) != comp1_big_n) stop("Comparator 1 Population Differences Between ASL and ATE!")
  if (as.numeric(comp2_BIG_N) != comp2_big_n) stop("Comparator 2 Population Differences Between ASL and ATE!")
  
  tte_big_n <- data.frame(ref_big_n,comp1_big_n,comp2_big_n)
  
  # Patients With Events (N & %) #
  ref_n_events <- surv_km_table[1,4]
  comp1_n_events <- surv_km_table[2,4]
  comp2_n_events <- surv_km_table[3,4]
  
  ref_p_events <- ref_n_events / ref_big_n
  comp1_p_events <- comp1_n_events / comp1_big_n
  comp2_p_events <- comp2_n_events / comp2_big_n

  tte_np_events <- data.frame(ref_n_events,ref_p_events,comp1_n_events,comp1_p_events,
                              comp2_n_events,comp2_p_events)
  
  # Patients Without Events (N & %) #  
  ref_n_wo_events <- ref_big_n - ref_n_events
  comp1_n_wo_events <- comp1_big_n - comp1_n_events
  comp2_n_wo_events <- comp2_big_n - comp2_n_events
  
  ref_p_wo_events <- ref_n_wo_events / ref_big_n
  comp1_p_wo_events <- comp1_n_wo_events / comp1_big_n
  comp2_p_wo_events <- comp2_n_wo_events / comp2_big_n

  tte_np_wo_events <- data.frame(ref_n_wo_events,ref_p_wo_events,comp1_n_wo_events,comp1_p_wo_events,
                                 comp2_n_wo_events,comp2_p_wo_events)
  
  # Median Time to Event + LCL & UCL #
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
  
  # Unstratified COX Proportional Hazard Ratio:  p-value, HR, LCL, and LCL # 
  cox_ph_hr_lcl_ucl <- summary(surv_cox_ph)$conf.int
  comp1_cox_ph_hr <- cox_ph_hr_lcl_ucl[1,1]
  comp1_cox_ph_hr_lcl <- cox_ph_hr_lcl_ucl[1,3]
  comp1_cox_ph_hr_ucl <- cox_ph_hr_lcl_ucl[1,4]

  comp2_cox_ph_hr <- cox_ph_hr_lcl_ucl[2,1]
  comp2_cox_ph_hr_lcl <- cox_ph_hr_lcl_ucl[2,3]
  comp2_cox_ph_hr_ucl <- cox_ph_hr_lcl_ucl[2,4]
  
  cox_ph_coefficients <- summary(surv_cox_ph)$coefficients
  comp1_cox_ph_hr_pvalue <- cox_ph_coefficients[1,5]
  comp2_cox_ph_hr_pvalue <- cox_ph_coefficients[2,5]
  
  unstrat_cox <- data.frame(comp1_cox_ph_hr_pvalue,comp1_cox_ph_hr,comp1_cox_ph_hr_lcl,comp1_cox_ph_hr_ucl,
                            comp2_cox_ph_hr_pvalue,comp2_cox_ph_hr,comp2_cox_ph_hr_lcl,comp2_cox_ph_hr_ucl)
  
  # Stratified COX Proportional Hazard Ratio:  p-value, HR, LCL, and LCL # 
  strat_cox_ph_hr_lcl_ucl <- summary(strat_surv_cox_ph)$conf.int
  comp1_strat_cox_ph_hr <- strat_cox_ph_hr_lcl_ucl[1,1]
  comp1_strat_cox_ph_hr_lcl <- strat_cox_ph_hr_lcl_ucl[1,3]
  comp1_strat_cox_ph_hr_ucl <- strat_cox_ph_hr_lcl_ucl[1,4]
  
  comp2_strat_cox_ph_hr <- strat_cox_ph_hr_lcl_ucl[2,1]
  comp2_strat_cox_ph_hr_lcl <- strat_cox_ph_hr_lcl_ucl[2,3]
  comp2_strat_cox_ph_hr_ucl <- strat_cox_ph_hr_lcl_ucl[2,4]
  
  strat_cox_ph_coefficients <- summary(strat_surv_cox_ph)$coefficients
  comp1_strat_cox_ph_hr_pvalue <- strat_cox_ph_coefficients[1,5]
  comp2_strat_cox_ph_hr_pvalue <- strat_cox_ph_coefficients[2,5]
  
  strat_cox <- data.frame(comp1_strat_cox_ph_hr_pvalue,comp1_strat_cox_ph_hr,comp1_strat_cox_ph_hr_lcl,comp1_strat_cox_ph_hr_ucl,
                          comp2_strat_cox_ph_hr_pvalue,comp2_strat_cox_ph_hr,comp2_strat_cox_ph_hr_lcl,comp2_strat_cox_ph_hr_ucl)
  
  tte_table <- list(tte_big_n,tte_np_events,tte_np_wo_events,tte_median,tte_quantiles,tte_range,
                    unstrat_cox,strat_cox,time_point_analysis)
  
  return(tte_table)

}  

tte_tbl_R_data <- time_to_event_table(time_to_event = ATE_f$AVAL,
                                      event = ifelse(is.na(ATE_f$CNSR),NA,
                                              ifelse(ATE_f$CNSR==1,0,1)),
                                              arm = ATE_f$ARM1,
                                              big_n_arm = ASL_f$ARM1,
                                              arm.ref = "DUMMY C",
                                              strata1 = as.factor(ATE_f$SEX),
                                              strata2 = as.factor(ATE_f$MLIVER),
                                              strata3 = as.factor(ATE_f$TCICLVL2),
                                      time_point = as.numeric(6))

# Time-to-Event rtable Generation #

tte_tbl_R <- teal.oncology::rtable(
    col.names = c(paste("DUMMY C\n(N=",tte_tbl_R_data[[1]]$ref_big_n,")",sep=""), 
                  paste("DUMMY B\n(N=",tte_tbl_R_data[[1]]$comp1_big_n,")",sep=""),
                  paste("DUMMY A\n(N=",tte_tbl_R_data[[1]]$comp2_big_n,")",sep="")),
    format = "xx",
    rrow("Patients with event (%)", 
         c(tte_tbl_R_data[[2]]$ref_n_events,   tte_tbl_R_data[[2]]$ref_p_events), 
         c(tte_tbl_R_data[[2]]$comp1_n_events, tte_tbl_R_data[[2]]$comp1_p_events), 
         c(tte_tbl_R_data[[2]]$comp2_n_events, tte_tbl_R_data[[2]]$comp2_p_events), format = "xx (xx.x%)"),
    rrow("Patients without event (%)", 
         c(tte_tbl_R_data[[3]]$ref_n_wo_events,   tte_tbl_R_data[[3]]$ref_p_wo_events), 
         c(tte_tbl_R_data[[3]]$comp1_n_wo_events, tte_tbl_R_data[[3]]$comp1_p_wo_events), 
         c(tte_tbl_R_data[[3]]$comp2_n_wo_events, tte_tbl_R_data[[3]]$comp2_p_wo_events), format = "xx (xx.x%)"),
    rrow(),
    rrow("Time to Event (months)"),
    rrow("Median",
         c(tte_tbl_R_data[[4]]$ref_med_tte),
         c(tte_tbl_R_data[[4]]$comp1_med_tte),
         c(tte_tbl_R_data[[4]]$comp2_med_tte), format = "xx.x"),
    rrow("95% CI",
         c(tte_tbl_R_data[[4]]$ref_med_tte_lcl,   tte_tbl_R_data[[4]]$ref_med_tte_ucl),
         c(tte_tbl_R_data[[4]]$comp1_med_tte_lcl, tte_tbl_R_data[[4]]$comp1_med_tte_ucl),
         c(tte_tbl_R_data[[4]]$comp2_med_tte_lcl, tte_tbl_R_data[[4]]$comp2_med_tte_ucl), format = "(xx.x, xx.x)"),
    rrow("25% and 75%−ile",
         c(tte_tbl_R_data[[5]]$ref_25th,   9999),
         c(tte_tbl_R_data[[5]]$comp1_25th, tte_tbl_R_data[[5]]$comp1_75th),
         c(tte_tbl_R_data[[5]]$comp2_25th, 9999), format = "(xx.x, xx.x)"),
    rrow("Range",
         c(tte_tbl_R_data[[6]]$ref_km_min,   tte_tbl_R_data[[6]]$ref_km_max),
         c(tte_tbl_R_data[[6]]$comp1_km_min, tte_tbl_R_data[[6]]$comp1_km_max),
         c(tte_tbl_R_data[[6]]$comp2_km_min, tte_tbl_R_data[[6]]$comp2_km_max), format = "(xx.x, xx.x)")
)
teal.oncology::Viewer(tte_tbl_R)
  
# teal.oncology::compare_rtables(tte_tbl_R, tte_tbl_stream, tol = 0.2)
# tte_tbl_R





