
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
#' arms <- unique(ASL$ARM)
#' x <- teal::init(
#'   data = list(ASL = ASL, ATE = ATE),
#'   modules = root_modules(
#'     tm_variable_browser(),
#'     tm_data_table(),
#'     tm_forest_survival(
#'        label = "Forest Survival",
#'        endpoint = "OS",
#'        endpoint_choices = unique(ATE$PARAMCD),
#'        baseline_risk = c("BAGED", "SEX", "BECOG"),
#'        arm.ref = arms[1],
#'        arm.ref_choices = arms,
#'        arm.comp = arms[-1],
#'        arm.comp_choices = arms
#'    )
#'   )
#' )   
#' shinyApp(x$ui, x$server) 
#' 
#'   
#' } 
tm_forest_survival <- function(label,
                               arm.ref,
                               arm.ref_choices = arm.ref,
                               arm.comp,
                               arm.comp_choices = arm.comp,
                               baseline_risk,
                               baseline_risk_choices = baseline_risk,
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
                               arm.ref,
                               arm.ref_choices = arm.ref,
                               arm.comp,
                               arm.comp_choices = arm.comp,
                               baseline_risk,
                               baseline_risk_choices = baseline_risk,
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
      optionalSelectInput(ns("ref_arm"), "Reference Group", arm.ref_choices, arm.ref, multiple = TRUE),
      optionalSelectInput(ns("treat_arm"), "Treatment Group", arm.comp_choices, arm.comp, multiple = TRUE),
      optionalSelectInput(ns("baseline_risk"), "Baseline Risk Factors", baseline_risk_choices, baseline_risk, multiple = TRUE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
} 

srv_forest_survival <- function(input, output, session, datasets) {
  
  
  
  output$forest_plot <- renderUI({
    
    ATE_filtered <- datasets$get_data("ATE", reactive = TRUE, filtered = TRUE)
    ASL_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    
    validate(need(!is.null(ATE_filtered) && is.data.frame(ATE_filtered), "no data left"))
    validate(need(nrow(ATE_filtered) > 0 , "no observations left"))
    
    
    endpoint <- input$endpoint
    baseline_risk <- input$baseline_risk
    ref_arm <- input$ref_arm
    treat_arm <- input$treat_arm
    
    teal:::as.global(baseline_risk)
    
    validate(need(!is.null(treat_arm) && !is.null(ref_arm),
                  "need at least one treatment and one reference arm"))
    validate(need(length(intersect(ref_arm, treat_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    
    validate(need(endpoint %in% ATE_filtered$PARAMCD, "time to event PARAMCD does not exist"))
    
    
    ## you need to add the encodings
    ATE_f <- ATE_filtered %>% filter(PARAMCD == endpoint, ARM %in% c(ref_arm, treat_arm))
    
    validate(need(nrow(ATE_f) > 0, "no data left"))
    
    ASL_filtered$BAGED <- ifelse(ASL_filtered$BAGE <= median(ASL_filtered$BAGE), "<=median", ">median")
    
    validate(need(all(baseline_risk %in% names(ASL_filtered)),
                  "some baseline risk variables are not valid"))
    
    
    
    group_by <- merge(
      ATE_f[c("USUBJID", "STUDYID")],
      ASL_filtered[c("USUBJID", "STUDYID", baseline_risk)],
      all.x = TRUE, all.y = FALSE
    )
    
    tbl <- surv_subgroup(
      time_to_event = ATE_f$AVAL,
      event = ATE_f$CNSR == 0,
      arm = ATE_f$ARM, 
      group_by = group_by[, -c(1,2), drop=FALSE],
      arm.ref = ref_arm,
      arm.comp = treat_arm
    )
    
    as_html(tbl)
  })
}
