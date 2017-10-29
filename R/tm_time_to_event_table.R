

#' Time To Event Table teal module
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
#'     tm_time_to_event_table(
#'        label = "Time To Event Table",
#'        endpoint = "OS",
#'        endpoint_choices = unique(ATE$PARAMCD),
#'        strata_var = c("SEX", "MLIVER", "TCICLVL2"),
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
tm_time_to_event_table <- function(label,
                               arm.ref,
                               arm.ref_choices = arm.ref,
                               arm.comp,
                               arm.comp_choices = arm.comp,
                               strata_var,
                               strata_var_choices = strata_var,
                               endpoint = "OS",
                               endpoint_choices = "OS",
                               pre_output = NULL, post_output = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_time_to_event_table,
    ui = ui_time_to_event_table,
    ui_args = args,
    filters = "ATE"
  )
}

ui_time_to_event_table <- function(id, label,
                               arm.ref,
                               arm.ref_choices = arm.ref,
                               arm.comp,
                               arm.comp_choices = arm.comp,
                               strata_var,
                               strata_var_choices = strata_var,
                               endpoint = "OS",
                               endpoint_choices = "OS",
                               pre_output,
                               post_output) {
  ns <- NS(id)
  
  standard_layout(
    output = uiOutput(ns("tte_table")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ATE")),
      optionalSelectInput(ns("endpoint"), "Time to Event", endpoint_choices, endpoint, multiple = FALSE),
      optionalSelectInput(ns("ref_arm"), "Reference Group", arm.ref_choices, arm.ref, multiple = TRUE),
      optionalSelectInput(ns("treat_arm"), "Treatment Group", arm.comp_choices, arm.comp, multiple = TRUE),
      optionalSelectInput(ns("strata_var"), "Stratify by", strata_var_choices, strata_var, multiple = TRUE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
} 

srv_time_to_event_table <- function(input, output, session, datasets) {
  
  
  
  output$tte_table <- renderUI({

    # resolve all reactive expressions    
    ATE_filtered <- datasets$get_data("ATE", reactive = TRUE, filtered = TRUE)

    endpoint <- input$endpoint
    strata_var <- input$strata_var
    ref_arm <- input$ref_arm
    treat_arm <- input$treat_arm
    
    # then validate your input values
    validate(need(!is.null(ATE_filtered) && is.data.frame(ATE_filtered), "no data left"))
    validate(need(nrow(ATE_filtered) > 10 , "need more than 10 observations to calculate the table"))
    
    validate(need(!is.null(strata_var), "need strata variables"))
             
    validate(need(!is.null(treat_arm) && !is.null(ref_arm),
                  "need at least one treatment and one reference arm"))
    validate(need(length(intersect(ref_arm, treat_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    
    validate(need(endpoint %in% ATE_filtered$PARAMCD, "time to event PARAMCD does not exist"))
    
    
    ## Now comes the static analysis code
    ## this is what you see 1:1 on the "show R code" code
    
    print(" -- pass here in tte table ---")
    teal:::as.global(ATE_filtered)
    teal:::as.global(endpoint)
    teal:::as.global(strata_var)
    teal:::as.global(ref_arm)
    teal:::as.global(treat_arm)
    
    ## you need to add the encodings
    
    ATE_f <- ATE_filtered %>%
      filter(PARAMCD == endpoint, ARM %in% c(ref_arm, treat_arm))
    
    validate(need(nrow(ATE_f) > 15, "need at least 15 data points"))
    validate(need(all(strata_var %in% names(ATE_f)),
                  "some baseline risk variables are not valid"))
    
    tbl <- time_to_event_table(
      time_to_event = ATE_f$AVAL,
      event = ATE_f$CNSR == 0,
      arm = ATE_f$ARM,
      is_earliest_contr_event_death = ATE_f$EVNTDESC == "Death",
      arm.ref = ref_arm,
      strata_data = ATE_f[strata_var]
    )
    
    as_html(tbl)
  })
}
