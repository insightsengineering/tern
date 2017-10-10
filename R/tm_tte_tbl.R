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
#' ASL1 <- dplyr::select(ASL,USUBJID,ARM,ITTFL) %>%
#'         dplyr::filter(ITTFL=="Y") %>%
#'         dplyr::select(USUBJID,ARM)
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
  
  tte_data <- ATE2[which(ATE2$ARM %in% c(ref_arm,comp_arm)),]
  tte_data$ARM <- factor(tte_data$ARM, levels = c(ref_arm, comp_arm))

  surv_km_results <- summary(survival::survfit(formula = Surv(time, status) ~ ARM, 
                                               data=tte_data, conf.type="plain"))$table
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

  surv_km_quantiles <- quantile(survival::survfit(formula = Surv(time, status) ~ ARM, 
                                                  data=tte_data))$quantile

  ref_25th <- surv_km_quantiles[1,1]
  ref_75th <- surv_km_quantiles[1,3]

  comp_25th <- surv_km_quantiles[2,1]
  comp_75th <- surv_km_quantiles[2,3]

  surv_km_ranges <- broom::tidy(survival::survfit(formula = Surv(time, status) ~ ARM, 
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
  
  tte_top_3rd <- list(tte_BIG_N,tte_np_events,tte_np_wo_events)
  return(tte_top_3rd)
  
}

tte_tbl(tte_data=ATE2,
        ref_arm="DUMMY C",
        comp_arm="DUMMY B")

#cox_ph_results  <- summary(survival::coxph(Surv(time,status) ~ ARM, data = tte_data))
