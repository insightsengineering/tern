
#' Time-to-event Forest Plot Table
#'
#' @param tte time to event data
#' @param is_event is boolean, \code{TRUE} if event, \code{FALSE} if \code{tte}
#'   is censored
#' @param group_data data frame with one column per grouping
#' @param col_by factor with reference and comparison group information, the
#'   first \code{level} indicates the reference group
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
#' 
#' ASL <- radam("ASL")
#' ATE <- radam("ATE", ADSL = ASL)
#' 
#' ATE_f <- ATE %>% filter(PARAMCD == "OS") 
#' 
#' ANL <- merge(ASL %>% select(USUBJID, STUDYID, SEX, RACE, ARM), ATE_f)
#' 
#' tbl <- t_forest_tte(
#'   tte = ANL$AVAL,
#'   is_event = ANL$CNSR == 0,
#'   col_by = factor(ANL$ARM), 
#'   group_data = as.data.frame(lapply(ANL[, c("SEX", "RACE")], as.factor))
#' )
#' 
#' Viewer(tbl)
#' 
t_forest_tte <- function(tte, is_event, col_by, group_data = NULL, strata_data = NULL, total = 'ALL', time_unit = "month", na.omit.group = TRUE) {
  
  if (!is.null(strata_data)) stop("strata_data argument is currently not implemented")
  
  check_same_N(tte = tte, is_event = is_event, group_data = group_data)
  check_col_by(col_by)
  if (length(levels(col_by)) != 2) stop("col_by can only have two levels")
  
  ## note that there is no missing
  check_data_frame(group_data) # change name of check_strata_data
  if (!is.null(group_data)) {
    is_fct <- vapply(group_data, is.factor, logical(1)) 
    if (!all(is_fct)) stop("not all variables in group_data are factors: ", paste(names(is_fct)[!is_fct], collapse = ", "))
  }
  
  
  # Derive Output
  cox_data <- data.frame(time_to_event = tte, event = is_event, arm = col_by)
  
  table_header <- rheader(
   rrow(row.name = "",
        rcell(NULL),
        rcell(levels(col_by)[1], colspan = 3),
        rcell(levels(col_by)[2], colspan = 3),
        rcell(NULL),
        rcell(NULL)
   ),
   rrow(row.name = "Baseline Risk Factors",
        "Total n",
        "n", "Events", paste0("Median (", time_unit, ")"),
        "n", "Events", paste0("Median (", time_unit, ")"),
        "Hazard Ratio",
        "95% Wald CI"
   )
  )
  
  tbl_total <- if(is.null(total)) {
    NULL
  } else {
    rtable(header = table_header, rrowl(row.name = total, 
      format_survival_analysis(
        survival_results(cox_data)
      )
    )) 
  }
  
  
  tbl_group_data <- if (is.null(group_data)) {
    NULL
  } else {
    
    # split data into a tree for data
    # where each leaf is a data.frame with 
    # the data to compute the survival analysis with
    data_tree <- lapply(group_data, function(var) {
      dt <- if (na.omit.group) subset(cox_data, !is.na(var)) else cox_data
      split(dt, var, drop = FALSE)
    })
  
    list_of_tables <- Map(function(dfs, varname) {
      tbls_var <- Map(function(dfi, level) {
        rtable(header = table_header,
               rrowl(
                 row.name = level,
                 indent = 1,
                 format_survival_analysis(
                   survival_results(dfi)
                 )
               )) 
      }, dfs, names(dfs))
      rbind(
        rtable(header = table_header, rrow(row.name = varname)),
        Reduce(rbind, tbls_var)
      )
    }, data_tree, names(data_tree))
    
    stack_rtables_l(list_of_tables)
  }
  
  stack_rtables(
    tbl_total,
    tbl_group_data
  )
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

  list(
    total_n = nrow(data),
    ref_n  = km_ref_n,
    ref_events = km_ref_event,
    ref_median = km_ref_median,
    comp_n = km_comp_n,
    comp_events = km_comp_event,
    comp_median = km_comp_median,
    cox_hr = cox_hr,
    cox_lcl = cox_lcl,
    cox_ucl = cox_ucl
  )
}

format_survival_analysis <- function(x) {
  list(
    rcell(x[["total_n"]], "xx"),
    rcell(x[["ref_n"]], "xx"),
    rcell(x[["ref_events"]], "xx"),
    rcell(x[["ref_median"]], "xx"),
    rcell(x[["comp_n"]], "xx"), 
    rcell(x[["comp_events"]], "xx"),
    rcell(x[["comp_median"]], "xx"),
    rcell(x[["cox_hr"]], format = "xx.xx"),
    rcell(c(x[['cox_lcl']], x[["cox_ucl"]]), format = "(xx.xx, xx.xx)")
  )
}
