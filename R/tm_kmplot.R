#' teal module for new KM plot
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
#'     tm_data_table(),
#'     tm_variable_browser(),
#'     tm_kmplot_ADAM2(
#'        label = "KM PLOT",
#'        tratement_var_choices = c("ARM", "ARMCD"),
#'        endpoint_choices = c("OS", "PFSINV"),
#'        facet_var = "SEX",
#'        facet_var_choices = c("SEX", "TOBHX"),
#'        strat_var = "HIST",
#'        strat_var_choices = c("SEX", "MLIVER", "TC2IC2", "HIST")
#'     )  
#'   )
#' )
#' ## Initiate Shiny App
#' shinyApp(ui = x$ui, server = x$server)
#' }


tm_kmplot_ADAM2 <- function(label,
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
    server = srv_kmplot_ADAM2,
    ui = ui_kmplot_ADAM2,
    ui_args = args
  )
}

ui_kmplot_ADAM2 <- function(
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
                        optionalSelectInput(ns("var_arm"), "Treatment Variable", choices = tratement_var_choices,
                                            selected = treatment_var, multiple = FALSE),
                        optionalSelectInput(ns("tteout"), "Time to Event (Endpoint)", choices = endpoint_choices, 
                                            selected = endpoint, multiple = FALSE),
                        optionalSelectInput(ns("strat"), "Stratify by", choices = strat_var_choices, 
                                            selected = strat_var, multiple = TRUE),
                        optionalSelectInput(ns("facetby"), "Facet Plots by:", choices = facet_var_choices, 
                                            selected = facet_var, multiple = TRUE),
                        selectInput(ns("ref_arm"), "Reference Arm", choices = NULL, 
                                    selected = NULL, multiple = TRUE),
                        helpText("Reference groups automatically combined into a single group if more than one value selected."),
                        selectInput(ns("comp_arm"), "Comparison Group", choices = NULL, selected = NULL, multiple = TRUE),
                        checkboxInput(ns("combine_arm"), "Combine all comparison groups?", value = FALSE),
                        tags$label("Plot Settings", class = "text-primary"),
                        sliderInput(ns("plotht"), "Plot Height", min=400, max=3000, step = 10, value = plot_height)
                      )
                      
  )
}


srv_kmplot_ADAM2 <- function(input, output, session, datasets){
  ATE_Filtered <- reactive({
    ATE_F <- datasets$get_data("ATE", filtered = TRUE, reactive = TRUE)
    validate(need(ATE_F, "Need ATE data"))
    ATE_F
  })
  
  
  observe({
    ANL <- ATE_Filtered()
    chs <- as.list(colnames(ANL))
    updateSelectInput(session,  "ref_arm" , choices = unique(ANL[[input$var_arm]]), 
                      selected = ANL[[input$var_arm]] %>% unique() %>% sort() %>% "["(1))
    updateSelectInput(session, "comp_arm", choices = unique(ANL[[input$var_arm]]),
                      selected = ANL[[input$var_arm]] %>% unique() %>% sort() %>% "["(-1))
    
  })
  
  
  output$outplot <- renderUI({
    
    plot_height <- input$plotht
    
    validate(need(plot_height, "need valid plot height"))
    
    plotOutput(session$ns("kmplot"), width = "100%", height=plot_height)
  })
  
  
  output$kmplot <- renderPlot({
    ATE_Filtered <- ATE_Filtered()
    
    tteout <- input$tteout
    ANL <- ATE_Filtered %>% filter(PARAMCD == tteout) 
    
    
    var_arm <- input$var_arm
    facetby <- input$facetby
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    strat <- input$strat
    combine_arm <- input$combine_arm
    
    # teal:::as.global(var_arm)
    # teal:::as.global(ANL)
    # teal:::as.global(facetby)
    # teal:::as.global(ref_arm)
    # teal:::as.global(strat)
    # teal:::as.global(comp_arm)
    validate(need(nrow(ANL) > 10, "Need more than 10 observations"))
    validate(need(var_arm %in% names(ANL), "var_arm is not in ANL"))
    validate(need(is.null(facetby)  || facetby %in% names(ANL), "facet by not correct"))
    validate(need(ref_arm %in% ANL[[var_arm]], "reference arm does not exist in left over ARM values"))
   
    
    
    if (length(ref_arm)>1) {
      new_ref_arm <- paste(ref_arm, collapse = "/")
      ANL[[var_arm]] <- do.call(fct_collapse, setNames(list(ANL[[var_arm]], ref_arm), c("f", new_ref_arm)))
      ref_arm <- new_ref_arm
    }
    
    if (combine_arm) {
      ANL[[var_arm]] <- do.call(fct_collapse, setNames(list(ANL[[var_arm]], comp_arm), c("f", paste(comp_arm, collapse = "/"))))
    }
    
    ANL[[var_arm]] <- fct_relevel(ANL[[var_arm]], ref_arm)
    
    formula_km <- as.formula(
      paste0("Surv(AVAL, 1-CNSR) ~", var_arm)
    )
    
    if (length(strat) != 0){
      formula_coxph <- as.formula(
        paste0("Surv(AVAL, 1-CNSR) ~", var_arm ,  "+ strata(", paste(strat, collapse = ","), ")")
      )
      info_coxph <- paste0("Cox Proportional Model: Stratified by ", paste(strat, collapse = ","))
    } else{
      formula_coxph <- formula_km
      info_coxph <- "Cox Proportional Model: Unstratified Analysis"
    }

     
    if (length(facetby) == 0){
      kmplot(formula_km, data = ANL, add_km = TRUE, 
             add_coxph = TRUE, formula_coxph, 
             info_coxph,
             add = FALSE, 
             title = "Kaplan - Meier Plot")
    } else{
      nplots <- length(unique(ANL[[facetby]]))
      dfs <- split(ANL, ANL[[facetby]])
      grid.newpage()
      pushViewport(plotViewport(margin = c(3, 10, 2, 2)))
      pushViewport(viewport(layout = grid.layout(ncol = 1, nrow = 2*nplots-1,
        heights = unit(head(rep(c(1, 7), nplots), -1), head(rep(c("null", "lines"), nplots), -1))
         )))
      
      Map(function(i) {
        pushViewport(viewport(layout.pos.row = i*2 - 1))
        kmplot(formula_km, data = dfs[[i]], add_km = TRUE,
               add_coxph = TRUE, formula_coxph,
               info_coxph,
               add = TRUE,
               title = paste0("Kaplan - Meier Plot for ", unique(dfs[[i]][[facetby]])))
        popViewport()
      }, 1:length(dfs))
      
       
    }
 
  })
}

