
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
#' 
#' '%needs%' <- teal.oncology:::'%needs%'
#' 
#' surv_tbl_stream <- get_forest_survival_table(com.roche.cdt30019.go29436.re)
#' Viewer(surv_tbl_stream )
#' 
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
#' tbl <- surv_subgroup(
#'    time_to_event = ATE_f$AVAL,
#'    event = ATE_f$CNSR == 0,
#'    arm = ATE_f$ARM, 
#'    group_by = group_by[, -c(1,2), drop=FALSE],
#'    arm.ref = "DUMMY A"
#' )
#' tbl
#' Viewer(tbl)
#' 
#' compare_rtables(tbl, surv_tbl_stream)
#' 
#' plot(tbl)
#' 
#' 
#' }
#' 
#' # surv_subgroup(Surv(AVAL ~ I(CNSR != 'N') ~ ARM + SEX, data = ATE))
surv_subgroup <- function(time_to_event, event, 
                          arm, arm.ref, arm.comp = setdiff(arm, arm.ref),
                          group_by, covariates = NULL) {
  
  # argument checking
  n <- length(time_to_event)
  if (length(event) != n) stop("event has wrong length")
  if (length(arm) != n) stop("arm has wrong length")
  if (!is.data.frame(group_by)) stop("group_by is expected to be a data.frame")
  if (nrow(group_by) != n) stop("group_by has wrong number of rows")
  if (any(grepl(".", group_by, fixed = TRUE))) stop("no . are allowed in the group_by variable names")
  
  arm_for_model <- combine_arm(arm, arm.ref, arm.comp)
  
  cox_data <- subset(
    data.frame(
      time_to_event,
      event,
      arm = arm_for_model
    ), !is.na(arm_for_model)
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

  additonal_args <- list(
    col.names = c("Total n",
                  "n", "events", "Median Events (Months)",
                  "n", "events", "Median Events (Months)",
                  "Hazard Ratio", "95% Wald", "p value"),
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
            rcell(x$km_ref_median, format = "xx.xx"),
            x$km_comp_n,
            x$km_comp_event,
            rcell(x$km_comp_median, format = "xx.xx"),
            rcell(x$cox_hr, format = "xx.xx"),
            rcell(c(x$cox_lcl, x$cox_ucl), format = "(xx.xx, xx.xx)"),
            rcell(x$cox_pval, format = "xx.xx"),
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
#' @import grid
#' @export
plot.forest_survival <- function(x, ...) {
  
  grid.newpage()
  
  grid.text("Plot of a forst survival table")
  
  
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




#' Forest Survival Plot teal module
#' 
#' @export
#' 
#' @examples  
#' 
#' \donotrun{
#' library(atezo.data)
#' library(dplyr)
#' library(survival)
#' 
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL, ATE = ATE),
#'   modules = root_modules(
#'     tm_variable_browser(),
#'     tm_data_table(),
#'     tm_forest_survival(
#'        label = "Forest Survival",
#'        endpoint = "OS",
#'        endpoint_choices = unique(ATE$PARAMCD)
#'    )
#'   )
#' )   
#' shinyApp(x$ui, x$server) 
#' 
#'   
#' } 
tm_forest_survival <- function(label,
                               endpoint = "OS",
                               endpoint_choices = "OS",
                               plot_height = c(600, 200, 2000),
                               pre_output = NULL, post_output = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_forest_survival,
    ui = ui_forest_survival,
    ui_args = args,
    filters = "ATE"
  )
}

ui_forest_survival <- function(id, label,
                               endpoint = "OS",
                               endpoint_choices = "OS",
                               plot_height,
                               pre_output,
                               post_output) {
  ns <- NS(id)
  
  standard_layout(
    output = uiOutput(ns("forest_plot")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ATE")),
      optionalSelectInput(ns("endpoint"), "Time to Event", endpoint_choices, endpoint, multiple = FALSE),
      optionalSelectInput(ns("x_sel"), "x selector", LETTERS[1:4], "A", multiple = FALSE),
      optionalSelectInput(ns("y_sel"), "y selector", "a", "a", multiple = FALSE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
} 

srv_forest_survival <- function(input, output, session, datasets) {

  
  
  output$forest_plot <- renderUI({
    
    ATE_filtered <- datasets$get_data("ATE", reactive = TRUE, filtered = TRUE)
    
    teal:::as.global(ATE_filtered)
    
    validate(need(!is.null(ATE_filtered) && is.data.frame(ATE_filtered), "no data left"))
    validate(need(nrow(ATE_filtered) > 0 , "no observations left"))
    
    endpoint <- input$endpoint
    
    validate(need(endpoint %in% ATE_filtered$PARAMCD, "time to event PARAMCD does not exist"))
  
      
    ## you need to add the encodings
    ATE_f <- ATE_filtered %>% filter(PARAMCD == endpoint)
    
    validate(need(nrow(ATE_f) > 0, "no data left"))
    
    
    ASL$BAGED <- ifelse(ASL$BAGE <= median(ASL$BAGE), "<=median", ">median")
    
    group_by <- merge(
      ATE_f[c("USUBJID", "STUDYID")],
      ASL[c("USUBJID", "STUDYID", "BAGED", "SEX", "BECOG")],
      all.x = TRUE, all.y = FALSE
    )
    
    tbl <- surv_subgroup(
      time_to_event = ATE_f$AVAL,
      event = ATE_f$CNSR == 0,
      arm = ATE_f$ARM, 
      group_by = group_by[, -c(1,2), drop=FALSE],
      arm.ref = "DUMMY A"
    )
    
    as_html(tbl)
  })
}
