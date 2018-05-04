
#' Time-to-event Table as used for Forest Plot
#'
#'The time-to-event forest plot table summarizes time-to-event data by groups. 
#'The function returns event counts and median survival time for each analysis 
#'arm, as well as a hazard ratio and the corresponding 95\% confidence interval 
#'from a Cox proportional hazard model.
#'
#' @param tte a vector of time to event data
#' @param is_event is boolean, \code{TRUE} if event, \code{FALSE} if \code{tte}
#'   is censored
#' @param group_data data frame with one variable per grouping
#' @param strata_data data frame with stratification variables
#' @param col_by factor with reference and comparison group information, the
#'   first \code{level} indicates the reference group
#' @param total character with the row name of the analysis run on all data. If
#'   \code{NULL} analysis is omitted.
#' @param time_unit The unit of median survival time. Default is \code{months}.
#' @param ties the method used for tie handling in \code{\link[survival]{coxph}}.
#' @param na.omit.group is boolean. Default is \code{TRUE}, do not display NA's as a category. 
#' @param dense_header Display the table headers in mulitple rows. Default is \code{FALSE}. 

#' @details
#' Cox propotionl hazard model is used for hazard ratio calculation
#'
#'
#' The returned table contains one row per analysis within a subgroup of data
#' (indicated by the row name). The analysis is summarized with the following 9
#' columns:
#' 
#' \describe{
#'   \item{1}{\emph{Total n} the total number of subjects included in analysis population}
#'   \item{2-4}{Survival statistics for reference arm, \emph{n} and \emph{Events} are the total number of patients and the number of events in reference arm, respectively.
#'   \code{Median (month)} is the survival time estimated by Kaplan-Meier method. Time unit can be modified per study needs.}
#'   \item{5-7}{same analysis as for reference arm now for comparison arm}
#'   \item{8}{\emph{Hazard Ratio} ranges from 0 to infinity. The hazard ratio is an estimate of the ratio of the hazard rate in the comparison group versus that in the reference group.
#'    Univariate Cox proportional hazard model is applied to obtain the estimated hazard ratio. 
#'   Hazard ratio > 1 implies better treatment effect in reference arm, and hazard ratio < 1 when comparion arm is better. }
#'   \item{9}{\emph{95\% Wald CI} The 95% confidence interval indicates the level of uncertainty 
#'   around the measure of effect (Hazard Ratio). Because only a small sample of the overall 
#'   population is included in the analysis, by having an upper and lower confidence limit 
#'   we can infer that the true treatment effect lies in between. If the 95% confidence interval 
#'   includes 1, then we say that the difference between two arms is not significant at a significance level of 0.05.}
#' }
#'
#' 
#' @export
#' 
#' @template author_song24
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL")
#' ATE <- radam("ATE", ADSL = ASL)
#' 
#' ATE_f <- subset(ATE, PARAMCD == "OS") 
#' 
#' ANL <- merge(ASL, ATE_f)
#' 
#' tbl <- t_forest_tte(
#'   tte = ANL$AVAL,
#'   is_event = ANL$CNSR == 0,
#'   col_by = factor(ANL$ARM), 
#'   group_data = as.data.frame(lapply(ANL[, c("SEX", "RACE")], as.factor)),
#'   ties = "exact",
#'   dense_header = TRUE
#' )
#' 
#' tbl
#' Viewer(tbl)
#' 
t_forest_tte <- function(tte, 
                         is_event, 
                         col_by, 
                         group_data = NULL,
                         strata_data = NULL, 
                         total = 'ALL', 
                         time_unit = "month",
                         ties = "exact",
                         na.omit.group = TRUE,
                         dense_header = FALSE) {
  
  if (!is.null(strata_data)) stop("strata_data argument is currently not implemented")
  
  check_same_N(tte = tte, is_event = is_event, group_data = group_data)
  check_col_by(col_by)
  if (length(levels(col_by)) != 2) stop("col_by can only have two levels")
  
  if (!is.null(group_data)) {
    check_data_frame(group_data, allow_missing = TRUE)
    group_data <- all_as_factor(group_data)
  }
  
  # Derive Output
  table_header <- if (dense_header) {
    rheader(
      rrow(row.name = "",
           NULL,
           rcell(levels(col_by)[1], colspan = 3),
           rcell(levels(col_by)[2], colspan = 3),
           NULL,
           NULL
      ),
      rrow(row.name = "Baseline Risk",
           "Total",
           "n", "Events", "Median",
           "n", "Events", "Median",
           "Hazard",
           "95%"
      ),
      rrow(row.name = "Factors",
           "n",
           NULL, NULL, paste0("(", time_unit, ")"),
           NULL, NULL, paste0("(", time_unit, ")"),
           "Ratio",
           "Wald CI"
      )
    )
  } else {
    rheader(
      rrow(row.name = "",
           NULL,
           rcell(levels(col_by)[1], colspan = 3),
           rcell(levels(col_by)[2], colspan = 3),
           NULL,
           NULL
      ),
      rrow(row.name = "Baseline Risk Factors",
           "Total n",
           "n", "Events", paste0("Median (", time_unit, ")"),
           "n", "Events", paste0("Median (", time_unit, ")"),
           "Hazard Ratio",
           "95% Wald CI"
      )
    )
  }  
  
  cox_data <- data.frame(time_to_event = tte, event = is_event, arm = col_by)
  
  tbl_total <- if(is.null(total)) {
    NULL
  } else {
    rtable(header = table_header, rrowl(row.name = total, 
                                        format_survival_analysis(
                                          survival_results(cox_data, ties)
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
      if (!na.omit.group) var <- na_as_level(var)
      split(cox_data, var, drop = FALSE)
    })
    
    names(data_tree) <- var_labels(group_data, fill = TRUE)
    
    list_of_tables <- Map(function(dfs, varname) {
      tbls_var <- Map(function(dfi, level) {
        rtable(header = table_header,
               rrowl(
                 row.name = level,
                 indent = 1,
                 format_survival_analysis(
                   survival_results(dfi, ties)
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
survival_results <- function(data, ties){
  
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
    cox_sum  <- suppressWarnings(summary(coxph(Surv(time_to_event,event) ~ arm, data = data, ties = ties)))
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
  format.hr <- ifelse(!is.na(x[["cox_hr"]]) & x[["cox_hr"]] > 999.9, ">999.9",  "xx.xx")
<<<<<<< HEAD
  format.ci <- ifelse(!is.na(x[["cox_ucl"]]) & x[["cox_ucl"]] > 999.9,  expression(sprintf_format("(%.2f, >999.9)")),  expression("(xx.xx, xx.xx)"))
=======
  format.ci <- ifelse(!is.na(x[["cox_ucl"]]) & x[["cox_ucl"]] > 999.9,  "(%.2f, >999.9)",  "(%.2f, %.2f)")
>>>>>>> 4d6739bdc2246cde9aa146ad1c837654abf4f94e
  list(
    rcell(x[["ref_n"]] + x[["comp_n"]], "xx"),
    rcell(x[["ref_n"]], "xx"),
    rcell(x[["ref_events"]], "xx"),
    rcell(x[["ref_median"]], "xx.xx"),
    rcell(x[["comp_n"]], "xx"), 
    rcell(x[["comp_events"]], "xx"),
    rcell(x[["comp_median"]], "xx.xx"),
    rcell(x[["cox_hr"]], format.hr),
<<<<<<< HEAD
    rcell(c(x[['cox_lcl']], x[["cox_ucl"]]), format = eval(format.ci))
=======
    rcell(c(x[['cox_lcl']], x[["cox_ucl"]]), format = sprintf_format(format.ci))
>>>>>>> 4d6739bdc2246cde9aa146ad1c837654abf4f94e
  )
}
