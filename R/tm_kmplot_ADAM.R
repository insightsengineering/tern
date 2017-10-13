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
#'  modules = root_modules(
#'     tm_data_table(),
#'     tm_variable_browser(),
#'     tm_kmplot_ADAM( label = "KM PLOT" ) 
#'   )
#' )
#' ## Initiate Shiny App
#' shinyApp(ui = x$ui, server = x$server)
#' }
 

tm_kmplot_ADAM <- function(label){
  module(
    label = label,
    filters = "ATE",
    server = srv_kmplot_ADAM,
    ui = ui_kmplot_ADAM
  )
}

ui_kmplot_ADAM <- function(id, plot_height=700){
  ns <- NS(id)
  standard_layout(
    output = uiOutput(ns("outplot")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis Data: ", tags$code("ATE")),
      selectInput(ns("armvar"), "Treatment Variable", choices = NULL,
                  selected = NULL, multiple = TRUE),
      selectInput(ns("tteout"), "Time to Event (Endpoint)", choices = NULL, 
                  selected = NULL, multiple = FALSE),
      selectInput(ns("facetby"), "Facet Plots by:", choices = NULL, 
                  selected = NULL, multiple = TRUE),
      
      selectInput(ns("refarm"), "Reference Arm", choices = NULL, 
                  selected = NULL, multiple = TRUE),
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
 
  
  observe({
    ANL <- ATE_Filtered()
    chs <- as.list(colnames(ANL))
    updateSelectInput(session,  "armvar" , choices = chs, selected = "ARM")
    updateSelectInput(session, "tteout", choices = unique(ANL[["PARAMCD"]]), selected = "OS")

    updateSelectInput(session, "facetby", choices = chs, selected = NULL)
    
  })
  
  ns <- session$ns
  observe({
    if (length(input$armvar) != 0){
      ANL <- ATE_Filtered()
      updateSelectInput(session, "refarm", choices = unique(ANL[[input$armvar]]), 
                        selected = ANL[[input$armvar]] %>% unique() %>% sort() %>% "["(1))
    }
    
  })
  

  output$outplot <- renderUI({
    
    plot_height <- input$plotht
    
    validate(need(plot_height, "need valid plot height"))
    
    plotOutput(ns("kmplot"), width = "100%", height=plot_height)
  })
  
  
  output$kmplot <- renderPlot({
    ANL <- ATE_Filtered()
    ANL <- ANL %>% filter(PARAMCD == input$tteout)
    if (length(input$facetby)==0){
      outkmplot <- kmPlot(
        time_to_event = ANL[["AVAL"]],
        event = ANL[["CNSR"]] == 0,
        arm = ANL[[input$armvar]],
        arm.ref = input$refarm
      )
    } else{
      outkmplot <- kmPlot(
        time_to_event = ANL[["AVAL"]],
        event = ANL[["CNSR"]] == 0,
        arm = ANL[[input$armvar]],
        facet_by = ANL[[input$facetby]],
        arm.ref =  input$refarm  
      )
    }
    outkmplot
  })
}