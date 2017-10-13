#' teal module for KM plot
#' 
#' @import shiny
#' @import teal
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' ### copy the code and save it as file app.R
#' library(atezo.data)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ## Initialize Teal
#' x <- teal::init(
#'   data = list(ASL = ASL, ATE = ATE),
#'   modules = root_modules(
#'     tm_data_table(),
#'     tm_variable_browser(),
#'     tm_kmplot_ADAM(
#'        label = "KM PLOT",
#'        reference_arm = ASL$ARM[1],
#'        reference_arm_choices = unique(ASL$ARM),
#'        tratement_var_choices = "ARM",
#'        endpoint_choices = c("OS", "PFS")
#'     )  
#'   )
#' )
#' ## Initiate Shiny App
#' shinyApp(ui = x$ui, server = x$server)
#' }
 

tm_kmplot_ADAM <- function(label,
                           reference_arm,
                           reference_arm_choices = reference_arm,
                           treatment_var = "ARM",
                           tratement_var_choices = treatment_var,
                           endpoint = "OS",
                           endpoint_choices = endpoint,
                           strat_vars = NULL,
                           strat_var_choices = strat_vars,
                           facet_var = NULL,
                           facet_var_choices = facet_var,
                           plot_height=700
                           ){
  
  args <- as.list(environment())
  
  module(
    label = label,
    filters = "ATE",
    server = srv_kmplot_ADAM,
    ui = ui_kmplot_ADAM,
    ui_args = args
  )
}

ui_kmplot_ADAM <- function(
  id, 
  label,
  reference_arm,
  reference_arm_choices = reference_arm,
  treatment_var = "ARM",
  tratement_var_choices = treatment_var,
  endpoint = "OS",
  endpoint_choices = endpoint,
  strat_vars = NULL,
  strat_var_choices = strat_var,
  facet_var = NULL,
  facet_var_choices = facet_var,
  plot_height=700) {
  
  ns <- NS(id)
  
  standard_layout(    output = uiOutput(ns("outplot")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis Data: ", tags$code("ATE")),
      optionalSelectInput(ns("armvar"), "Treatment Variable", choices = tratement_var_choices,
                  selected = treatment_var, multiple = FALSE),
      optionalSelectInput(ns("tteout"), "Time to Event (Endpoint)", choices = endpoint_choices, 
                          selected = endpoint, multiple = FALSE),
      optionalSelectInput(ns("strat"), "Stratify by", choices = strat_vars, 
                          selected = strat_var_choices, multiple = FALSE),
      optionalSelectInput(ns("facetby"), "Facet Plots by:", choices = facet_var_choices, 
                          selected = facet_var, multiple = FALSE),
      optionalSelectInput(ns("refarm"), "Reference Arm", choices = reference_arm_choices, 
                  selected = reference_arm, multiple = TRUE),
      tags$label("Plot Settings", class = "text-primary"),
      sliderInput(ns("plotht"), "Plot Height", min=400, max=3000, step = 10, value = plot_height)
    )
    
  )
}


srv_kmplot_ADAM <- function(input, output, session, datasets){
  ATE_Filtered <- reactive({
    ATE_F <- datasets$get_data("ATE", filtered = TRUE, reactive = TRUE)
    validate(need(ATE_F, "Need ATE data"))
    ATE_F
  })
 
  
#  observe({
#    ANL <- ATE_Filtered()
#    chs <- as.list(colnames(ANL))
#    updateSelectInput(session,  "armvar" , choices = chs, selected = "ARM")
#    updateSelectInput(session, "tteout", choices = unique(ANL[["PARAMCD"]]), selected = "OS")
#
#    updateSelectInput(session, "facetby", choices = chs, selected = NULL)
#    
#  })
  
#  ns <- session$ns
#  observe({
#    if (length(input$armvar) != 0){
#      ANL <- ATE_Filtered()
#      updateSelectInput(session, "refarm", choices = unique(ANL[[input$armvar]]), 
#                        selected = ANL[[input$armvar]] %>% unique() %>% sort() %>% "["(1))
#    }
#    
#  })
  

  output$outplot <- renderUI({
    
    plot_height <- input$plotht
    
    validate(need(plot_height, "need valid plot height"))
    
    plotOutput(session$ns("kmplot"), width = "100%", height=plot_height)
  })
  
  
  output$kmplot <- renderPlot({
    ATE_Filtered <- ATE_Filtered()
    ANL <- ATE_Filtered %>% filter(PARAMCD == input$tteout)
    
    armvar <- input$armvar
    facetby <- input$facetby
    refarm <- input$refarm
    
    teal:::as.global(refarm)
    teal:::as.global(ANL)
    teal:::as.global(facetby)
    teal:::as.global(armvar)

    
    validate(need(nrow(ANL) > 10, "Need more than 10 observations"))
    validate(need(armvar %in% names(ANL), "armvar is not in ANL"))
    validate(need(facetby == "" || facetby %in% names(ANL), "facet by not correct"))
    validate(need(refarm %in% ANL[[armvar]], "reference arm does not exist in left over ARM values"))

    kmPlot(
      time_to_event = ANL[["AVAL"]],
      event = ANL[["CNSR"]] == 0,
      arm = ANL[[armvar]],
      facet_by = if (length(facetby)!=0) ANL[[facetby]] else NULL,
      arm.ref = refarm  
    )
    
  })
}