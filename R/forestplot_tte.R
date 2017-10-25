
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
#' 
#' @importFrom survival survfit Surv coxph
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr)
#' library(survival)
#' library(forcats)
#' library(forestplot) 
#' 
#' '%needs%' <- teal.oncology:::'%needs%'
#' 
#' surv_tbl_stream <- get_forest_survival_table(com.roche.cdt30019.go29436.re)
#' Viewer(surv_tbl_stream )
#' 
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' ATE_f <- ATE %>% filter(PARAMCD == "OS") %>% 
#'              filter(ITTWTFL == "Y") %>% 
#'              filter(ARM %in% c("DUMMY A", "DUMMY C")) %>%
#'              select(c("USUBJID", "STUDYID", "AVAL", "CNSR", "ARM"))
#' ASL_f <- ASL %>% filter(ITTWTFL == "Y") %>% 
#'              filter(ARM %in% c("DUMMY A", "DUMMY C")) %>% 
#'              select(c("USUBJID", "STUDYID", "SEX", "MLIVER", "TCICLVL2", "AGE4CAT", "RACE", "BECOG"))
#' 
#'
#' group_by <- left_join(ASL_f, ATE_f %>% select(c("USUBJID", "STUDYID")))
#' old_grp_names <- names(group_by) 
#' new_grp_names <- c(Reduce(cbind,lapply(group_by, function(x){
#'      if (is.null(attr(x,"label"))) NA else attr(x,"label")
#'       })
#'      ))
#' names(group_by) <- ifelse (is.na (new_grp_names), old_grp_names, new_grp_names)
#' 
#' head(group_by)
#' 
#' # Dummy C First (comparison in survfit and glm)
#' arm <- fct_relevel(ATE_f$ARM, "DUMMY C")
#' 
#' # Collapse Factors
#' # fct_collapse(arm, "ARM A/B" = c("ARM A", "ARM B"))
#' # rename a level
#' # arm <- fct_recode(ATE_f$ARM, "Treat ARM" = "DUMMY A")
#' 
#' tbl <- forest_tte(
#'    time_to_event = ATE_f$AVAL,
#'    event = ATE_f$CNSR == 0,
#'    group_by = group_by[, -c(1,2), drop=FALSE],
#'    arm = arm
#' )
#' tbl <- forest_tte_table(tbl)
#' tbl
#' Viewer(tbl)
#' fplot <- forest_tte_plot(tbl)
#' compare_rtables(tbl, surv_tbl_stream, comp.attr = FALSE)
#' 
#' plot(tbl)
#' 
#' 
#' }
#' 
#' # forest_tte(Surv(AVAL ~ I(CNSR != 'N') ~ ARM + SEX, data = ATE))
forest_tte <- function(time_to_event, event, 
                          arm, group_by, covariates = NULL) {
  
  # argument checking
  n <- length(time_to_event)
  if (length(event) != n) stop("event has wrong length")
  if (length(arm) != n) stop("arm has wrong length")
  if (!is.data.frame(group_by)) stop("group_by is expected to be a data.frame")
  if (nrow(group_by) != n) stop("group_by has wrong number of rows")
  if (any(grepl(".", group_by, fixed = TRUE))) stop("no . are allowed in the group_by variable names")
  
  cox_data <- data.frame(
      time_to_event,
      event,
      arm = arm)
  
  # split data into a tree for data
  # where each leaf is a data.frame with 
  # the data to compute the survival analysis with
  data_list <- c(
    list(ALL = list(ALL = cox_data)),
    lapply(group_by, function(var) {
      sub_data <- cbind(cox_data, var)
      sub_data <- subset(sub_data, var != "")
      sub_data$var <- as.factor(as.character(sub_data$var))
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
  X
}

forest_tte_table <- function(X){
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



#' plot
#' 
#' @importFrom grid forestplot
#' @export
forest_tte_plot <- function(x) {
  
  cochrane_from_rmeta <- 
    structure(list(
      mean  = c(NA, NA, X$cox_hr), 
      lower = c(NA, NA, X$cox_lcl),
      upper = c(NA, NA, X$cox_ucl)),
      .Names = c("hr", "lower", "upper"), 
      row.names = c(NA, -nrow(X)-2), 
      class = "data.frame")
  
  i <- regexpr(".", row.names(X), fixed = TRUE)
  rname <- substr(row.names(X), 1, i-1)
  catname <- substr(row.names(X), i+1, nchar(row.names(X)))
  
  tabletext<-cbind(
    
    c("", "", catname),
    c("Total", "(n)", c(X$km_comp_n + X$km_ref_n)),
    # c("n", "(Reference)", X$km_ref_n),
    # c("Events", "(Reference)", X$km_ref_event),
    # c("Median (Month)", "(Reference)", round(X$km_ref_median,2)),
    # c("n", "(Treatment)", X$km_comp_n),
    # c("Events", "(Treatment)", X$km_comp_event),
    # c("Median (Month)", "(Treatment)", round(X$km_comp_median,2)),
    c("Hazard", "Ratio", round(X$cox_hr, 2)),
    c("95%", "C.I", paste(round(X$cox_lcl, 2), ", ",round(X$cox_ucl, 2))))
  
  forestplot(tabletext, 
             cochrane_from_rmeta,new_page = TRUE,
             is.summary=c(TRUE,TRUE,rep(FALSE,nrow(X))),
             clip=c(0.1,2.5),
             graphwidth = unit(8, "cm") ,
             col=fpColors(box="royalblue",line="darkblue", summary="royalblue"))

  
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
    km_ref_n <- km_sum[1]
    km_comp_n <- 0
    km_ref_event <- km_sum[4]
    km_comp_event <- 0
    km_ref_median <- ifelse (is.na(km_sum[7]), -999, km_sum[7])
    km_comp_median <- -999
  } else if (arm_freq[names(arm_freq) == levels(data$arm)[1]] == 0){
    km_sum <- as.data.frame(t(summary(survfit(Surv(time_to_event,event) ~ arm, data = data))$table))
    km_ref_n <- 0
    km_comp_n <- km_sum[1]
    km_ref_event <- 0
    km_comp_event <- km_sum[4]
    km_ref_median <- -999
    km_comp_median <- ifelse (is.na(km_sum[7]), -999, km_sum[7])
  } else if (arm_freq[names(arm_freq) == levels(data$arm)[2]] * arm_freq[names(arm_freq) == levels(data$arm)[1]] > 0){
    km_sum <- summary(survfit(Surv(time_to_event,event) ~ arm, data = data))$table
    km_ref_n <- km_sum[1, 1]
    km_comp_n <- km_sum[2,1]
    km_ref_event <- km_sum[1, 4]
    km_comp_event <- km_sum[2, 4]
    km_ref_median <- ifelse (is.na(km_sum[1, 7]), -999, km_sum[1, 7])
    km_comp_median <- ifelse (is.na(km_sum[2, 7]), -999, km_sum[2, 7])
  } else stop("Invalid Arm Counts")
    
  # Cox Model
  # Three Scenarios: 
  # 1. both arms have events; 
  # 2. at least one of the two arms have no events
  # 3. data has only one arm.
  if (nrow(km_sum) == 2 & km_ref_event * km_comp_event > 0){
    cox_sum  <- summary(coxph(Surv(time_to_event,event) ~ arm, data = data))
    cox_hr   <- ifelse (is.na(cox_sum$conf.int[1]), -999, cox_sum$conf.int[1])
    cox_lcl  <- ifelse (is.na(cox_sum$conf.int[1]), -999, cox_sum$conf.int[3])
    cox_ucl  <- ifelse (is.na(cox_sum$conf.int[1]), -999, cox_sum$conf.int[4])
    cox_pval <- ifelse (is.na(cox_sum$conf.int[1]), -999, cox_sum$conf.int[5])
  } else {
    cox_hr   <- -999
    cox_lcl  <- -999
    cox_ucl  <- -999
    cox_pval <- -999
  }

  surv_table <- data.frame(km_ref_n, km_comp_n, 
                         km_ref_event, km_comp_event, 
                         km_ref_median, km_comp_median, 
                         cox_hr, cox_lcl, cox_ucl)
}

#' Forest Plot Numbers for Survival data with ADAM data structure
#' 
#' @export
#' 
#' @inheritParams forest_tte
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
#' forest_tte_ADAM(
#'   ASL, ATE,
#'   groupvar = c("SEX", "BECOG", "COUNTRY"),
#'   arm.ref = "DUMMY A", arm.comp = "DUMMY B"
#' )
#'   
#' }
forest_tte_ADAM <- function(ASL, ATE,
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
  
  forest_tte(
    time_to_event = ATE_f[[time_to_event.var]],
    event = if(negate.event.var) !ATE_f[[event.var]] else ATE_f[[event.var]],
    arm = ATE_f[[arm.var]], 
    group_by = group_by[, -c(1,2), drop=FALSE],
    arm.ref = arm.ref,
    arm.comp = arm.comp
  )
}

