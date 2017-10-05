
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
#' library(atezo.data)
#' library(dplyr)
#' '%needs%' <- teal.oncology:::'%needs%'
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' ATE_f <- ATE %>% filter(PARAMCD == "OS")
#' 
#' ASL$BAGED <- ifelse(ASL$BAGE <= median(ASL$BAGE), "<=median", ">median")
#' 
#' group_by <- merge(
#'  ATE_f[c("USUBJID", "STUDYID")],
#'  ASL[c("USUBJID", "STUDYID", "BAGED", "SEX", "BECOG")],
#'  all.x = TRUE, all.y = FALSE
#' )
#' 
#' head(group_by)
#' 
#' surv_subgroup(
#'    time_to_event = ATE_f$AVAL,
#'    event = ATE_f$CNSR == 0,
#'    arm = ATE_f$ARM, 
#'    group_by = group_by[, -c(1,2), drop=FALSE],
#'    arm.ref = "DUMMY A",
#'    arm.comp = "DUMMY B"
#' )
#' 
#' 
#'   
surv_subgroup <- function(time_to_event, event, arm, arm.ref, arm.comp, group_by, covariates = NULL) {
  
  # argument checking
  n <- length(time_to_event)
  if (length(event) != n) stop("event has wrong length")
  if (length(arm) != n) stop("arm has wrong length")
  if (!is.data.frame(group_by)) stop("group_by is expected to be a data.frame")
  if (nrow(group_by) != n) stop("group_by has wrong number of rows")
  
  
  arm_for_model <- arm_for_model(arm, arm.ref, arm.comp)
  
  cox_data <- subset(
    data.frame(
      time_to_event,
      event,
      arm = arm_for_model
    ), arm %in% c(arm.ref, arm.comp)
  )
  
  # split data into a tree for data
  # where each leaf is a data.frame with 
  # the data to compute the survival analysis with
  data_list <- c(
    list(ALL = list(ALL = cox_data)),
    lapply(group_by, function(var) {
      lapply(setNames(unique(var[var != ""]), unique(var[var != ""])), function(value) {
          cox_data[var == value , , drop= FALSE] 
      })
    })
  )

  # apply the survival analysis
  results_survival <- lapply(data_list, function(varname) {
    lapply(varname, function(data_for_value) {
      survival_results(data_for_value)
    })
  })
  
  # reduce results into a table
  results_survival2 <- unlist(results_survival, recursive = FALSE)
  X <- Reduce(rbind, results_survival2)
  row.names(X) <-names(results_survival2)
  
  X
}



#' explain what you do
arm_for_model <- function(arm, arm.ref, arm.comp) {
  
  if (!all(arm.ref %in% arm)) stop("not all arms in arm.ref are in arm")
  if (!all(arm.comp %in% arm)) stop("not all arms in arm.comp are in arm")
  
  name_arm_ref <- paste(arm.ref, collapse = "/")
  name_arm_comp <- paste(arm.comp, collapse = "/")
  
  arm2 <- vapply(arm, function(x) {
    if (x %in% arm.ref) {
      name_arm_ref
    } else if (x %in% arm.comp) {
      name_arm_comp
    } else {
      "not possible"
    }
  }, character(1))
  
  factor(arm2, levels = c(name_arm_ref, name_arm_comp))
}

#' survival_results(data_for_value)
survival_results <- function(data){
  
  #KM Estimate
  km_sum <- summary(survfit(Surv(time_to_event,event) ~ arm, data = data))$table
  km_ref_n <- km_sum[1, 1]
  km_comp_n <- km_sum[2,1]
  km_ref_event <- km_sum[1, 4]
  km_comp_event <- km_sum[2, 4]
  km_ref_median <- km_sum[1, 7]
  km_comp_median <- km_sum[2, 7]
  
  #Cox Model
  cox_sum  <- summary(coxph(Surv(time_to_event,event) ~ arm, data = data))
  cox_hr   <- cox_sum$conf.int[1]
  cox_lcl  <- cox_sum$conf.int[3]
  cox_ucl  <- cox_sum$conf.int[4]
  cox_pval <- cox_sum$coefficients[5]
  
  km_ref_lcl <- km_sum[1, 8]
  km_comp_lcl <- km_sum[2, 8]
  km_ref_ucl <- km_sum[1, 9]
  km_comp_ucl <- km_sum[2, 9]
  km_table <- data.frame(km_ref_n, km_comp_n, 
                         km_ref_event, km_comp_event, 
                         km_ref_median, km_comp_median, 
                         cox_hr, cox_lcl, cox_ucl, cox_pval)
}

#' Forest Plot Numbers for Survival data with ADAM data structure
#' 
#' @export
#' 
#' @inheritParams surv_subgroup
#' @param ASL asl data frame
#' @param ATE data frame
#' 
#' @importFrom dplyr %>% filter
#' 
#' @examples 
#' \dontrun{
#' 
#' rm(list = ls())
#' library(atezo.data)
#' library(dplyr)
#' 
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' ASL$BAGED <- ifelse(ASL$BAGE <= median(ASL$BAGE), "<=median", ">median")
#' 
#' surv_subgroup_ADAM(
#'   ASL, ATE,
#'   groupvar = c("SEX", "BECOG", "COUNTRY"),
#'   arm.ref = "DUMMY A", arm.comp = "DUMMY B"
#' )
#'   
#' }
surv_subgroup_ADAM <- function(ASL, ATE,
                               outcome = "Overall Survival",
                               groupvar,
                               arm.ref,
                               arm.comp,
                               arm.var = "ARM",
                               time_to_event.var = "AVAL",
                               event.var = "CNSR", negate.event.var = TRUE) {
  
  ATE %needs% c("USUBJID", "STUDYID", "PARAM", time_to_event.var, event.var)
  ASL %needs% c("USUBJID", "STUDYID", groupvar, arm.var)
  
  event <- ATE[[event.var]]
  if (!(is.numeric(event) || is.logical(event))) stop("event var needs to be numeric or boolean")  
  
  ATE_f <- ATE %>% filter(PARAM == outcome)
  
  if (nrow(ATE_f) <= 0) stop("ATE data left after filtering")
   
  group_by <- merge(
    ATE_f[c("USUBJID", "STUDYID")],
    ASL[c("USUBJID", "STUDYID", groupvar)],
    all.x = TRUE, all.y = FALSE
  )
  
  surv_subgroup(
    time_to_event = ATE_f[[time_to_event.var]],
    event = if(negate.event.var) !ATE_f[[event.var]] else ATE_f[[event.var]],
    arm = ATE_f[[arm.var]], 
    group_by = group_by[, -c(1,2), drop=FALSE],
    arm.ref = arm.ref,
    arm.comp = arm.comp
  )
}
