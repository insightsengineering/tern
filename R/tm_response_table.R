
#' Forest Response Plot teal module
#' 
#' @export
#' 
#' @examples  
#' 
#' \donotrun{
#' library(atezo.data)
#' library(dplyr)
#' 
#' ARS <- ars(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' options(teal_logging = FALSE)
#' 
#' arms <- unique(ASL$ARM)
#' x <- teal::init(
#'   data = list(ASL = ASL, ARS = ARS),
#'   modules = root_modules(
#'     tm_variable_browser(),
#'     tm_data_table(),
#'     tm_response_table(
#'        label = "Response Table",
#'        paramcd = "OVRSPI",
#'        paramcd_choices = unique(ARS$PARAMCD),
#'        response = "CR",
#'        response_choices = unique(ARS$AVALC),
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
tm_response_table <- function(label,
                               paramcd,
                               paramcd_choices = paramcd,
                               arm_var = "ARM",
                               arm_var_choices = arm_var,
                               arm.ref,
                               arm.ref_choices = arm.ref,
                               arm.comp,
                               arm.comp_choices = arm.comp,
                               response,
                               response_choices = response,
                               plot_height = c(600, 200, 2000),
                               pre_output = NULL, post_output = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_response_table,
    ui = ui_response_table,
    ui_args = args,
    filters = "ARS"
  )
}

ui_response_table <- function(id, label,
                               paramcd,
                               paramcd_choices = paramcd,
                               arm_var = "ARM",
                               arm_var_choices = arm_var,
                               arm.ref,
                               arm.ref_choices = arm.ref,
                               arm.comp,
                               arm.comp_choices = arm.comp,
                               response = "OS",
                               response_choices = "OS",
                               plot_height,
                               pre_output,
                               post_output) {
  ns <- NS(id)
  
  
  
  standard_layout(
    output = uiOutput(ns("response_table")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ARS")),
      optionalSelectInput(ns("paramcd"), div("PARAMCD", tags$br(), helpText("some help text here")), paramcd_choices, paramcd, multiple = FALSE),
      optionalSelectInput(ns("responses"), "Responses", response_choices, response, multiple = TRUE),
      optionalSelectInput(ns("ref_arm"), "Reference Group", arm.ref_choices, arm.ref, multiple = TRUE),
      optionalSelectInput(ns("treat_arm"), "Treatment Group", arm.comp_choices, arm.comp, multiple = TRUE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
} 

srv_response_table <- function(input, output, session, datasets) {
  
  output$response_table <- renderUI({
    
    
    # Deal With Reactivity/Inputs
    ARS_filtered <- datasets$get_data("ARS", reactive = TRUE, filtered = TRUE)
    
    paramcd <- input$paramcd
    responses <- input$responses
    ref_arm <- input$ref_arm
    treat_arm <- input$treat_arm
    
    
    # Validate your input
    validate(need(!is.null(ARS_filtered) && is.data.frame(ARS_filtered), "no data left"))
    validate(need(nrow(ARS_filtered) > 0 , "no observations left"))
    
    validate(need(!is.null(treat_arm) && !is.null(ref_arm),
                  "need at least one treatment and one reference arm"))
    validate(need(length(intersect(ref_arm, treat_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    
    validate(need(!is.null(responses) && all(responses %in% ARS_filtered$AVALC),
                  "responses AVALC does not exist"))
    
    validate(need(!is.null(paramcd) && paramcd %in% ARS_filtered$PARAMCD,
                  "PARAMCD does not exist"))
    
    print("------- HELLO WORLD ----------")
    teal:::as.global(ARS_filtered)
    teal:::as.global(responses)
    teal:::as.global(ref_arm)
    teal:::as.global(treat_arm)
    teal:::as.global(paramcd)
    
    ## Do your computations
    
    
    ## you need to add the encodings
    ARS_f <- ARS_filtered %>% filter(PARAMCD == paramcd, ARM %in% c(ref_arm, treat_arm))
    validate(need(nrow(ARS_f) > 0, "no data left"))
    
    
    
    ARM <- factor(ARS_f[['ARM']])
    
    #' library(forcats)
    #' 
    # # Make dummy B as a reference 
    # fct_relevel(ARM, "DUMMY B")
    # 
    # # Collapse Factors
    # fct_collapse(ARM, "DUMMY A/B" = c("DUMMY A", "DUMMY B"))
    # 
    # # rename a level
    # fct_recode(ARM, "Treat ARM" = "DUMMY A")
    # 
    # # only want dummy a vs dummy b
    # ARS_f2 <- ARS_f[ARM %in% c("DUMMY A", "DUMMY B"), ]
    # ARS_f2$ARM  

    
    tbl <- response_table(
      response = ARS_f$AVALC,
      value.resp = responses,
      value.nresp = setdiff(ARS_f$AVALC, responses),
      arm = ARM,
      arm.ref = ref_arm,
      arm.comp = treat_arm,
      arm.comp.combine = TRUE
    )
    
    as_html(tbl)
  })
}
