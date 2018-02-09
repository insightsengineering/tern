
#' Time-to-event Forest Plot Table
#'
#' @param time_to_event time to event data
#' @param event is boolean, \code{TRUE} if event, \code{FALSE} if time_to_event
#'   is censored
#' @param group_data data frame with one column per grouping
#' @param arm vector with arm information
#' @param covariates set to NULL; currently not available for multivariate survival analysis
#' 
#' @details
#' Cox PH model is used for hazard ratio calculation
#'  
#' @importFrom survival survfit Surv coxph
#' 
#' @export
#' 
#' @author Yuyao Song (songy24), \email{yuyao.song@roche.com}
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' ASL <- radam("ASL")
#' ATE <- radam("ATE", ADSL = ASL)
#' 
#' ATE_f <- ATE %>% filter(PARAMCD == "OS") 
#' 
#' ASL_f <- right_join(ASL %>% select(USUBJID, STUDYID, SEX, RACE, ARM),
#'                         ATE_f %>% select(USUBJID, STUDYID))
#' 
#' tbl <- forest_rsp(
#'   response = ATE_f$AVAL,
#'   event = ATE_f$CNSR == 0,
#'   arm = ASL_f$ARM, 
#'   group_data = ASL_f %>% select("SEX", "RACE")
#' )
#' 
#' tbl
#' 
#' 
#' 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr)
#' library(forcats)
#' library(survival)
#' library(teal.oncology)
#' 
#' '%needs%' <- teal.oncology:::'%needs%'
#' 
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' ASL$temp <- c(rep("A", 200), rep("B", 250), rep(NA,1202-450))
#' 
#' tbl_stream <- get_forest_survival_table(com.roche.cdt30019.go29436.re)
#' Viewer(tbl_stream)
#' 
#' 
#' ATE_f <- ATE %>% filter(PARAMCD == "OS") %>% 
#'              filter(ITTWTFL == "Y") %>% 
#'              filter(ARM %in% c("DUMMY A", "DUMMY C")) %>%
#'              select(c("USUBJID", "STUDYID", "AVAL", "CNSR", "ARMCD"))
#' ASL_f <- ASL %>% filter(ITTWTFL == "Y") %>% 
#'              filter(ARM %in% c("DUMMY A", "DUMMY C")) %>% 
#'              select(c("USUBJID", "STUDYID", "SEX", "MLIVER", "TCICLVL2", "AGE4CAT", "RACE", "temp"))
#' 
#'
#' group_data <- left_join(ASL_f, ATE_f %>% select(c("USUBJID", "STUDYID")))
#' group_data$MLIVER <- factor(group_data$MLIVER, levels = c("Y", "N"), labels = c("Yes", "No"))
#' group_data$TCICLVL2 <- factor(group_data$TCICLVL2, levels = c("TC3 or IC2/3", "TC0/1/2 and IC0/1"), labels = c("TC3 or IC2/3", "TC0/1/2 and IC0/1"))#' 
#' group_data$SEX <- factor(group_data$SEX, levels = c("F", "M"), labels = c("FEMALE", "MALE"))
#' group_data$AGE4CAT <- factor(group_data$AGE4CAT, levels = c("<65", "65 to 74", "75 to 84", ">=85"), labels = c("<65", "65 to 74", "75 to 84", ">=85"))
#' group_data$RACE <- factor(group_data$RACE, levels = c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "UNKNOWN"), labels = c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "UNKNOWN"))
#' names(group_data) <- labels_over_names(group_data)
#' head(group_data)
#' 
#' arm <- fct_relevel(ATE_f$ARMCD, "C")
#' 
#' tbl <- forest_tte(
#'    time_to_event = ATE_f$AVAL,
#'    event = ATE_f$CNSR == 0,
#'    group_data = group_data[, -c(1,2), drop=FALSE],
#'    arm = arm
#' )
#' Viewer(tbl)
#' 
#' Viewer(tbl, tbl_stream)
#' 
#' compare_rtables(tbl, tbl_stream, comp.attr = FALSE)
#' 
#' forest_tte_plot(tbl)
#' 
#' }
#' 
#' # forest_tte(Surv(AVAL ~ I(CNSR != 'N') ~ ARM + SEX, data = ATE))
t_forest_tte <- function(time_to_event, event, 
                       arm, group_data, covariates = NULL) {
  
  # argument checking
  n <- length(time_to_event)
  if (length(event) != n) stop("event has wrong length")
  if (length(arm) != n) stop("arm has wrong length")
  if (!is.data.frame(group_data)) stop("group_data is expected to be a data.frame")
  if (nrow(group_data) != n) stop("group_data has wrong number of rows")
  if (any(grepl(".", group_data, fixed = TRUE))) stop("no . are allowed in the group_data variable names")
  
  cox_data <- data.frame(
    time_to_event,
    event,
    arm = arm)
  
  # split data into a tree for data
  # where each leaf is a data.frame with 
  # the data to compute the survival analysis with
  data_list <- c(
    list(ALL = list(ALL = cox_data)),
    lapply(group_data, function(var) {
      sub_data <- cbind(cox_data, var)
      sub_data <- subset(sub_data, var != "")
      #sub_data <- sub_data %>% filter(var != "")
      if ("" %in% levels(sub_data$var)) sub_data$var <- factor(sub_data$var, levels = levels(sub_data$var)[-which(levels(sub_data$var) == "")])
   #   sub_data$var <- as.factor(as.character(sub_data$var))
      sub_data$var <- as.factor(sub_data$var)
      lapply(split(sub_data, sub_data$var), function(x){
        x[,-4]
      })
    })
  )

  # varname=data_list$RACE
  # data_for_value = varname[4]
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
  
 ## for debugging
 # rtab <- results_survival2[[1]]
 # for (i in 2:length(results_survival2)) {rtab <- rbind(rtab,results_survival2[[i]])}

  additonal_args <- list(
    col.names = c("Total n",
                  "n", "events", "Median Events\n(Months)",
                  "n", "events", "Median Events\n(Months)",
                  "Hazard Ratio", "95% Wald\nCI"),
    format = "xx"
  )
  
  # rname <- rownames(X)[3]
  # x <- split(X, 1:nrow(X))[[1]]
  last_header <- "ALL"
  rrow_collection <- Filter(
    function(x)!is.null(x),
    unlist(
      Map(function(x, rname) {
        
        i <- regexpr(".", rname, fixed = TRUE)
        header_row_name <- c(substr(rname, 1, i-1), substring(rname, i+1))
        
        is_new_category <- header_row_name[1] != last_header
        last_header <<- header_row_name[1]
        
        list(
          if (is_new_category) rrow() else NULL,
          if (is_new_category) rrow(last_header) else NULL,
          rrow(
            row.name = header_row_name[2],
            x$km_ref_n + x$km_comp_n, # total n
            x$km_ref_n,
            x$km_ref_event,
            rcell(x$km_ref_median, format = "xx.x"),
            x$km_comp_n,
            x$km_comp_event,
            rcell(x$km_comp_median, format = "xx.x"),
            rcell(x$cox_hr, format = "xx.xx"),
            rcell(c(x$cox_lcl, x$cox_ucl), format = "(xx.xx, xx.xx)"),
            indent = if (header_row_name[1] == "ALL") 0 else 1
          )
        )
      }, split(X, 1:nrow(X)), rownames(X)),
      recursive = FALSE)
  )
  
  tbl <- do.call(rtable, c(additonal_args, rrow_collection))
  
  # Viewer(tbl)
  class(tbl) <- c("forest_survival", "forest_table", class(tbl))
  
  tbl
}

# survival_results(data_for_value[[1]])
# data = data_for_value[[1]]
survival_results <- function(data){
  
  # KM Estimate
  # Three scenarios:
  # 1. two arms
  # 2. ref arm has no records
  # 3. comp arm has no records
  arm_freq <- table(data$arm) 
  if (arm_freq[names(arm_freq) == levels(data$arm)[2]] == 0){
    km_sum <- as.data.frame(t(summary(survfit(Surv(time_to_event,event) ~ arm, data = data))$table))
    km_ref_n <- as.numeric(km_sum[1])
    km_comp_n <- 0
    km_ref_event <- as.numeric(km_sum[4])
    km_comp_event <- 0
    km_ref_median <- as.numeric(km_sum[7])
    km_comp_median <- NA
  } else if (arm_freq[names(arm_freq) == levels(data$arm)[1]] == 0){
    km_sum <- as.data.frame(t(summary(survfit(Surv(time_to_event,event) ~ arm, data = data))$table))
    km_ref_n <- 0
    km_comp_n <- as.numeric(km_sum[1])
    km_ref_event <- 0
    km_comp_event <- as.numeric(km_sum[4])
    km_ref_median <- NA
    km_comp_median <- as.numeric(km_sum[7])
  } else if (arm_freq[names(arm_freq) == levels(data$arm)[2]] * arm_freq[names(arm_freq) == levels(data$arm)[1]] > 0){
    km_sum <- summary(survfit(Surv(time_to_event,event) ~ arm, data = data))$table
    km_ref_n <- km_sum[1, 1]
    km_comp_n <- km_sum[2,1]
    km_ref_event <- km_sum[1, 4]
    km_comp_event <- km_sum[2, 4]
    km_ref_median <- km_sum[1, 7]
    km_comp_median <- km_sum[2, 7]
  } else stop("Invalid Arm Counts")
  
  # Cox Model
  # Three Scenarios: 
  # 1. both arms have events; 
  # 2. at least one of the two arms have no events
  # 3. data has only one arm.
  if (nrow(km_sum) == 2 & km_ref_event * km_comp_event > 0){
    cox_sum  <- summary(coxph(Surv(time_to_event,event) ~ arm, data = data))
    cox_hr   <- cox_sum$conf.int[1]
    cox_lcl  <- cox_sum$conf.int[3]
    cox_ucl  <- cox_sum$conf.int[4]
    cox_pval <- cox_sum$conf.int[5]
  } else {
    cox_hr   <- NA
    cox_lcl  <- NA
    cox_ucl  <- NA
    cox_pval <- NA
  }
  
  surv_table <- data.frame(km_ref_n, km_comp_n, 
                           km_ref_event, km_comp_event, 
                           km_ref_median, km_comp_median, 
                           cox_hr, cox_lcl, cox_ucl)
}