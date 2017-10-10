#' alternative tablist
#'
#' @param time_to_event time to event data
#' @param event is boolean, \code{TRUE} if event, \code{FALSE} if time_to_event
#'   is censored
#' @param group_by data frame with one column per grouping
#' @param arm vector with arm information
#' @param arm.ref a character vector defining which arms in arm should be taken 
#'   as the reference
#' @param arm.comp a character vector defining which arms in arm should be taken 
#'   as the comparison
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(atezo.data)
#' library(dplyr)
#' library(survival)
#' library(broom)
#' library(stringr)
#' source("./R/utils.R")
#' 
#' drop_label <- function(x){
#'      for (j in (1:ncol(x))){
#'           attr(x[[j]],"label") <- NULL
#'      }
#'      return(x)
#' }
#' 
#' ASL_ <- atezo.data::asl(com.roche.cdt30019.go29436.re)
#' ATE_ <- atezo.data::ate(com.roche.cdt30019.go29436.re)
#' 
#' ASL <- drop_label(ASL_)
#' ATE <- drop_label(ATE_)
#' 
#' ASL1 <- dplyr::select(ASL,USUBJID,ARM1,SEX,MLIVER,TCICLVL2,ITTFL) %>%
#'         dplyr::filter(ITTFL=="Y") %>%
#'         dplyr::select(-ITTFL)
#'         
#' ATE1 <- dplyr::select(ATE,USUBJID,PARAMCD,PARAM,EVNTDESC,CNSR,AVAL,AVALU) %>%
#'         dplyr::filter(PARAMCD == "OS") %>%
#'         dplyr::mutate(time   = AVAL,
#'                       status = ifelse(is.na(CNSR),NA,
#'                                ifelse(CNSR==1,0,1)))
#' 
#' ATE2 <- dplyr::inner_join(ASL1,ATE1,by=c("USUBJID"))
#' 
#' }
tte_tbl <- function(tte_data,ref_arm,comp_arm) {
  
  tte_data=ATE2
  ref_arm="DUMMY C"
  comp_arm="DUMMY B"
  tte_data$cox_strat1 <- as.factor(ATE2$SEX)
  tte_data$cox_strat2 <- as.factor(ATE2$MLIVER)
  tte_data$cox_strat3 <- as.factor(ATE2$TCICLVL2)
  tte_data$arm <- ATE2$ARM1
  
  tte_data <- tte_data[which(tte_data$arm %in% c(ref_arm,comp_arm)),]
  tte_data$arm <- factor(tte_data$arm, levels = c(ref_arm, comp_arm))

  surv_km_results <- summary(survival::survfit(formula = Surv(time, status) ~ arm, 
                                               data=tte_data, conf.type="plain"))$table
  
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

  surv_km_quantiles <- quantile(survival::survfit(formula = Surv(time, status) ~ arm, 
                                                  data=tte_data))$quantile

  ref_25th <- surv_km_quantiles[1,1]
  ref_75th <- surv_km_quantiles[1,3]

  comp_25th <- surv_km_quantiles[2,1]
  comp_75th <- surv_km_quantiles[2,3]

  surv_km_ranges <- broom::tidy(survival::survfit(formula = Surv(time, status) ~ arm, 
                                                  data=tte_data, conf.type="plain"))

  ref_km_min <- min(surv_km_ranges[which(stringr::str_sub(surv_km_ranges$strata,5,
                                                          nchar(surv_km_ranges$strata)) == ref_arm),]$time,na.rm=TRUE)

  ref_km_max <- max(surv_km_ranges[which(stringr::str_sub(surv_km_ranges$strata,5,
                                                          nchar(surv_km_ranges$strata)) == ref_arm),]$time,na.rm=TRUE)

  comp_km_min <- min(surv_km_ranges[which(stringr::str_sub(surv_km_ranges$strata,5,
                                                           nchar(surv_km_ranges$strata)) == comp_arm),]$time,na.rm=TRUE)

  comp_km_max <- max(surv_km_ranges[which(stringr::str_sub(surv_km_ranges$strata,5,
                                                           nchar(surv_km_ranges$strata)) == comp_arm),]$time,na.rm=TRUE)

  tte_BIG_N <- data.frame(ref_BIG_N,comp_BIG_N)
  tte_np_events <- data.frame(ref_n_events,ref_p_events,comp_n_events,comp_p_events)
  tte_np_wo_events <- data.frame(ref_n_wo_events,ref_p_wo_events,comp_n_wo_events,comp_p_wo_events)
  tte_median <- data.frame(ref_med_tte,ref_med_tte_lcl,ref_med_tte_ucl,
                           comp_med_tte,comp_med_tte_lcl,comp_med_tte_ucl)
  tte_quantiles <- data.frame(ref_25th,ref_75th,comp_25th,comp_75th)
  tte_range <- data.frame(ref_km_min,ref_km_max,comp_km_min,comp_km_max)
  
  #-------------------------#
  # Middle 3rd of TTE Table #
  #-------------------------#
  cox_ph_hr_lcl_ucl <- summary(survival::coxph(Surv(time,status) ~ arm, data = tte_data))$conf.int
  cox_ph_hr <- cox_ph_hr_lcl_ucl[,1]
  cox_ph_hr_lcl <- cox_ph_hr_lcl_ucl[,3]
  cox_ph_hr_ucl <- cox_ph_hr_lcl_ucl[,4]
  
  cox_ph_coefficients  <- summary(survival::coxph(Surv(time,status) ~ arm, data = tte_data))$coefficients
  cox_ph_hr_pvalue <- cox_ph_coefficients[,5]
  
  unstrat_cox <- data.frame(cox_ph_hr_pvalue,cox_ph_hr,cox_ph_hr_lcl,cox_ph_hr_ucl)
  
  strat_cox_ph_hr_lcl_ucl <- summary(survival::coxph(Surv(time,status) ~ arm + strata(cox_strat1,cox_strat2,cox_strat3), data = tte_data))$conf.int
  strat_cox_ph_hr <- strat_cox_ph_hr_lcl_ucl[,1]
  strat_cox_ph_hr_lcl <- strat_cox_ph_hr_lcl_ucl[,3]
  strat_cox_ph_hr_ucl <- strat_cox_ph_hr_lcl_ucl[,4]
  
  strat_cox_ph_coefficients <- summary(survival::coxph(Surv(time,status) ~ arm + strata(cox_strat1,cox_strat2,cox_strat3), data = tte_data))$coefficients
  start_cox_pvalue <- strat_cox_ph_coefficients[,5]
  
  strat_cox <- data.frame(strat_cox_ph_hr_pvalue,strat_cox_ph_hr,strat_cox_ph_hr_lcl,strat_cox_ph_hr_ucl)
  
  tte_table <- list(tte_BIG_N,tte_np_events,tte_np_wo_events,tte_median,tte_quantiles,tte_range,unstrat_cox,strat_cox)

  return(tte_table)
  
}

tte_1st_3rd_B_vs_C <- tte_tbl(tte_data=ATE2,ref_arm="DUMMY C",comp_arm="DUMMY B")
tte_1st_3rd_B_vs_C

tte_1st_3rd_A_vs_C <- tte_tbl(tte_data=ATE2,ref_arm="DUMMY C",comp_arm="DUMMY A")
tte_1st_3rd_A_vs_C

