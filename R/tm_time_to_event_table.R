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
#comp1.arm <- "DUMMY B"
#comp2.arm <- "DUMMY A"
#strata1 <- as.factor(ATE_f$SEX)
#strata2 <- as.factor(ATE_f$MLIVER)
#strata3 <- as.factor(ATE_f$TCICLVL2)
#time_point <- as.numeric(6)

time_to_event_table <- function(time_to_event,event,arm,big_n_arm,arm.ref,comp1.arm,comp2.arm,
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
  
  survdiff_data <- data.frame(time_to_event = as.numeric(time_to_event),
                              event = as.numeric(event),
                              ARM = ARM,
                              strata1 = strata1,
                              strata2 = strata2,
                              strata3 = strata3,
                              stringsAsFactors = FALSE)
  
  comp1_vs_ref <- subset(survdiff_data, ARM %in% c(arm.ref,comp1.arm))
  
  surv_km_test1 <- survival::survdiff(
    formula = Surv(time_to_event, event) ~ ARM, rho=0, data=comp1_vs_ref
  )
  
  strat_surv_km_test1 <- survival::survdiff(
    formula = Surv(time_to_event, event) ~ ARM + strata(strata1,strata2,strata3), rho=0, data=comp1_vs_ref
  )
  
  comp2_vs_ref <- subset(survdiff_data, ARM %in% c(arm.ref,comp2.arm))
  
  surv_km_test2 <- survival::survdiff(
    formula = Surv(time_to_event, event) ~ ARM, rho=0, data=comp2_vs_ref
  )
  
  strat_surv_km_test2 <- survival::survdiff(
    formula = Surv(time_to_event, event) ~ ARM + strata(strata1,strata2,strata3), rho=0, data=comp2_vs_ref
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
  surv_km_time_point <- summary(surv_km_fit,times=c(time_point))

  ref_patients_remaining_at_risk <- surv_km_time_point$n.risk[[1]]
  ref_patients_event_free_rate <- surv_km_time_point$surv[[1]]
  ref_patients_event_free_rate_lcl <- surv_km_time_point$lower[[1]]
  ref_patients_event_free_rate_ucl <- surv_km_time_point$upper[[1]]
  
  comp1_patients_remaining_at_risk <- surv_km_time_point$n.risk[[2]]
  comp1_patients_event_free_rate <- surv_km_time_point$surv[[2]]
  comp1_patients_event_free_rate_lcl <- surv_km_time_point$lower[[2]]
  comp1_patients_event_free_rate_ucl <-surv_km_time_point$upper[[2]]
  
  comp2_patients_remaining_at_risk <- surv_km_time_point$n.risk[[3]]
  comp2_patients_event_free_rate <- surv_km_time_point$surv[[3]]
  comp2_patients_event_free_rate_lcl <- surv_km_time_point$lower[[3]]
  comp2_patients_event_free_rate_ucl <- surv_km_time_point$upper[[3]]
  
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
  
  # Unstratified COX Proportional Hazard Ratio:  log-rank p-value, HR, LCL, and LCL # 
  pdiff1 <- pchisq(surv_km_test1$chisq, length(surv_km_test1$n)-1, lower.tail = FALSE)
  cox_ph_hr_lcl_ucl <- summary(surv_cox_ph)$conf.int
  comp1_cox_ph_hr <- cox_ph_hr_lcl_ucl[1,1]
  comp1_cox_ph_hr_lcl <- cox_ph_hr_lcl_ucl[1,3]
  comp1_cox_ph_hr_ucl <- cox_ph_hr_lcl_ucl[1,4]
  
  pdiff2 <- pchisq(surv_km_test2$chisq, length(surv_km_test2$n)-1, lower.tail = FALSE)
  comp2_cox_ph_hr <- cox_ph_hr_lcl_ucl[2,1]
  comp2_cox_ph_hr_lcl <- cox_ph_hr_lcl_ucl[2,3]
  comp2_cox_ph_hr_ucl <- cox_ph_hr_lcl_ucl[2,4]
  
  unstrat_cox <- data.frame(pdiff1,comp1_cox_ph_hr,comp1_cox_ph_hr_lcl,comp1_cox_ph_hr_ucl,
                            pdiff2,comp2_cox_ph_hr,comp2_cox_ph_hr_lcl,comp2_cox_ph_hr_ucl)
  
  # Stratified COX Proportional Hazard Ratio:  log-rank p-value, HR, LCL, and LCL # 
  p_strat_diff1 <- pchisq(strat_surv_km_test1$chisq, length(strat_surv_km_test1$n)-1, lower.tail = FALSE)
  strat_cox_ph_hr_lcl_ucl <- summary(strat_surv_cox_ph)$conf.int
  comp1_strat_cox_ph_hr <- strat_cox_ph_hr_lcl_ucl[1,1]
  comp1_strat_cox_ph_hr_lcl <- strat_cox_ph_hr_lcl_ucl[1,3]
  comp1_strat_cox_ph_hr_ucl <- strat_cox_ph_hr_lcl_ucl[1,4]

  p_strat_diff2 <- pchisq(strat_surv_km_test2$chisq, length(strat_surv_km_test2$n)-1, lower.tail = FALSE)
  comp2_strat_cox_ph_hr <- strat_cox_ph_hr_lcl_ucl[2,1]
  comp2_strat_cox_ph_hr_lcl <- strat_cox_ph_hr_lcl_ucl[2,3]
  comp2_strat_cox_ph_hr_ucl <- strat_cox_ph_hr_lcl_ucl[2,4]

  strat_cox <- data.frame(p_strat_diff1,comp1_strat_cox_ph_hr,comp1_strat_cox_ph_hr_lcl,comp1_strat_cox_ph_hr_ucl,
                          p_strat_diff2,comp2_strat_cox_ph_hr,comp2_strat_cox_ph_hr_lcl,comp2_strat_cox_ph_hr_ucl)
  
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
                                              comp1.arm = "DUMMY B",
                                              comp2.arm = "DUMMY A",
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
         c(tte_tbl_R_data[[6]]$comp2_km_min, tte_tbl_R_data[[6]]$comp2_km_max), format = "(xx.x, xx.x)"),
    rrow(),
    rrow("Unstratified Analysis"),
    rrow("p-value (log-rank)",
         c(9999), c(tte_tbl_R_data[[7]]$pdiff1), c(tte_tbl_R_data[[7]]$pdiff2), format = "xx.xxx"),
    rrow(),
    rrow("Hazard Ratio",
         c(9999), c(tte_tbl_R_data[[7]]$comp1_cox_ph_hr), c(tte_tbl_R_data[[7]]$comp2_cox_ph_hr), format = "xx.xx"),
    rrow("95% CI",
         c(9999, 9999), c(tte_tbl_R_data[[7]]$comp1_cox_ph_hr_lcl, tte_tbl_R_data[[7]]$comp1_cox_ph_hr_ucl),
                        c(tte_tbl_R_data[[7]]$comp2_cox_ph_hr_lcl, tte_tbl_R_data[[7]]$comp2_cox_ph_hr_ucl),
         format = "(xx.xx, xx.xx)"),
    rrow("Stratified Analysis"),
    rrow("p-value (log-rank)",
         c(9999), c(tte_tbl_R_data[[8]]$p_strat_diff1), c(tte_tbl_R_data[[8]]$p_strat_diff2), format = "xx.xxx"),
    rrow(),
    rrow("Hazard Ratio",
         c(9999), c(tte_tbl_R_data[[8]]$comp1_strat_cox_ph_hr), c(tte_tbl_R_data[[8]]$comp2_strat_cox_ph_hr), format = "xx.xx"),
    rrow("95% CI",
         c(9999, 9999), c(tte_tbl_R_data[[8]]$comp1_strat_cox_ph_hr_lcl, tte_tbl_R_data[[8]]$comp1_strat_cox_ph_hr_ucl),
         c(tte_tbl_R_data[[8]]$comp2_strat_cox_ph_hr_lcl, tte_tbl_R_data[[8]]$comp2_strat_cox_ph_hr_ucl),
         format = "(xx.xx, xx.xx)"),
    rrow(),
    rrow("Time Point Analysis"),
    rrow(c(tte_tbl_R_data[[9]]$time_point)," months"),
    rrow("Patients remaining at risk",
         c(tte_tbl_R_data[[9]]$ref_patients_remaining_at_risk, tte_tbl_R_data[[9]]$comp1_patients_remaining_at_risk,
           tte_tbl_R_data[[9]]$comp2_patients_remaining_at_risk, format = "xx"))
)
teal.oncology::Viewer(tte_tbl_R)
  
# teal.oncology::compare_rtables(tte_tbl_R, tte_tbl_stream, tol = 0.2)
# tte_tbl_R





