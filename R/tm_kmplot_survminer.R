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
#' library(atezo.data)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ## Initialize Teal
#' x <- teal::init(
#'   data = list(ASL = ASL, ATE = ATE),
#'   modules = root_modules(
#'     tm_kmplot_survminer(
#'        label = "KM PLOT",
#'        tratement_var_choices = c("ARM", "ARMCD"),
#'        endpoint_choices = c("OS", "PFSINV"),
#'        facet_var = "TOBHX",
#'        facet_var_choices = c("SEX", "RACE", "TOBHX"),
#'        strat_var = "HIST",
#'        strat_var_choices = c("SEX", "MLIVER", "TC2IC2", "HIST")
#'     )  
#'   )
#' )
#' ## Initiate Shiny App
#' shinyApp(ui = x$ui, server = x$server)
#' }
#' 
tm_kmplot_survminer <- function(label,
                           treatment_var = "ARM",
                           tratement_var_choices = treatment_var,
                           endpoint = "OS",
                           endpoint_choices = endpoint,
                           facet_var = NULL,
                           facet_var_choices = facet_var,
                           strat_var = NULL,
                           strat_var_choices = strat_var,
                           plot_height=700
                           ){
  
  args <- as.list(environment())
  
  module(
    label = label,
    filters = "ATE",
    server = srv_kmplot_survminer,
    ui = ui_kmplot_survminer,
    ui_args = args
  )
}

ui_kmplot_survminer <- function(
  id, 
  label,
  treatment_var = "ARM",
  tratement_var_choices = treatment_var,
  endpoint = "OS",
  endpoint_choices = endpoint,
  strat_var = NULL,
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
      optionalSelectInput(ns("strat"), "Stratify by", choices = strat_var_choices, 
                          selected = strat_var, multiple = TRUE),
      optionalSelectInput(ns("facetby"), "Facet Plots by:", choices = facet_var_choices, 
                          selected = facet_var, multiple = TRUE),
      selectInput(ns("refarm"), "Reference Arm", choices = NULL, 
                  selected = NULL, multiple = TRUE),
      tags$label("Plot Settings", class = "text-primary"),
      sliderInput(ns("plotht"), "Plot Height", min=400, max=3000, step = 10, value = plot_height)
    )
    
  )
}


srv_kmplot_survminer <- function(input, output, session, datasets){
  ATE_Filtered <- reactive({
    ATE_F <- datasets$get_data("ATE", filtered = TRUE, reactive = TRUE)
    validate(need(ATE_F, "Need ATE data"))
    ATE_F
  })
 
  
 observe({
   ANL <- ATE_Filtered()
   chs <- as.list(colnames(ANL))
   updateSelectInput(session,  "refarm" , choices = unique(ANL[[input$armvar]]), 
                     selected = ANL[[input$armvar]] %>% unique() %>% sort() %>% "["(1))

 })
  

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
    strat <- input$strat
    # teal:::as.global(refarm)
    # teal:::as.global(ANL)
    # teal:::as.global(facetby)
    # teal:::as.global(armvar)
    # teal:::as.global(strat)
    
    validate(need(nrow(ANL) > 10, "Need more than 10 observations"))
    validate(need(armvar %in% names(ANL), "armvar is not in ANL"))
    validate(need(is.null(facetby)  || facetby %in% names(ANL), "facet by not correct"))
    validate(need(refarm %in% ANL[[armvar]], "reference arm does not exist in left over ARM values"))

    kmplot_survminer(
      time_to_event = ANL[["AVAL"]],
      event = ANL[["CNSR"]] == 0,
      arm = ANL[[armvar]],
      facet_by = if (length(facetby) != 0) ANL[[facetby]] else NULL,
      stratum_df = if (length(strat) != 0) ANL[strat] else NULL,
      arm.ref = refarm  
    )
  })
}