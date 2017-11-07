#' teal module for Kaplan-Meier Plot from grid 
#' 
#' This is teal module produces a grid style KM plot for data with ADaM structure
#' 
#' @param label unique name for tabpanel
#' @param treatment_var parameter for seperating curves
#' @param treatment_var_choices options for \code{treatment_var}
#' @param endpoint selected endpoint from ADaM variable \code{PARAMCD}
#' @param endpoint_choices options for \code{endpoint}
#' @param facet_var parameter for facet plotting
#' @param facet_var_choices options for \code{facet_var}
#' @param strata_var parameter for stratification analysis in Cox PH model
#' @param strata_var_choices options for \code{strata_var}
#' @param plot_height plot height specification
#' 
#' @import shiny
#' @import teal
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' ASL <- data.frame(USUBJID = paste0("GO99999-", sprintf("%03d",seq(1:200))) ,
#'                   STUDYID = rep("GO99999", 200),
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  ARMCD = sample(c("C1", "C2", "C3"), 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE)
#'                  )
#'                  
#' ATE <- data.frame(USUBJID = rep(paste0("GO99999-", sprintf("%03d",seq(1:200))), 2) ,
#'                   STUDYID = rep("GO99999", 400),
#'                   PARAMCD = rep(c("OS", "PFS"), each = 200),
#'                   AVAL = abs(rnorm(400)), 
#'                  CNSR = sample(c(0, 1), 400, TRUE)
#'                  )
#' 
#' ATE <- merge(ATE, ASL, by = c("USUBJID", "STUDYID"))
#'                  
#' ## Initialize Teal
#' x <- teal::init(
#'   data = list(ASL = ASL, ATE = ATE),
#'   modules = root_modules(
#'     tm_kmplot(
#'        label = "KM PLOT",
#'        treatment_var_choices = c("ARM", "ARMCD"),
#'        endpoint_choices = c("OS", "PFS"),
#'        facet_var = "SEX",
#'        facet_var_choices = c("SEX", "ECOG"),
#'        strata_var = "RACE",
#'        strata_var_choices = c("SEX", "RACE", "ECOG")
#'     )  
#'   )
#' )
#' ## Initiate Shiny App
#' shinyApp(ui = x$ui, server = x$server)
#' }
#' 
tm_kmplot <- function(label,
                           treatment_var = "ARM",
                           treatment_var_choices = treatment_var,
                           endpoint = "OS",
                           endpoint_choices = endpoint,
                           facet_var = NULL,
                           facet_var_choices = facet_var,
                           strata_var = NULL,
                           strata_var_choices = strata_var,
                           plot_height = c(1200, 400, 5000)
){
  
  args <- as.list(environment())
  
  module(
    label = label,
    filters = "ATE",
    server = srv_kmplot,
    ui = ui_kmplot,
    ui_args = args
  )
}

ui_kmplot <- function(
  id, 
  label,
  treatment_var = "ARM",
  treatment_var_choices = treatment_var,
  endpoint = "OS",
  endpoint_choices = endpoint,
  strata_var = NULL,
  strata_var_choices = strata_var,
  facet_var = NULL,
  facet_var_choices = facet_var,
  plot_height = c(700, 400, 3000)) {
  
  ns <- NS(id)
  
  standard_layout(    output = uiOutput(ns("plot_ui")),
                      encoding = div(
                        tags$label("Encodings", class = "text-primary"),
                        helpText("Analysis Data: ", tags$code("ATE")),
                        optionalSelectInput(ns("var_arm"), "Treatment Variable", choices = treatment_var_choices,
                                            selected = treatment_var, multiple = FALSE),
                        optionalSelectInput(ns("tteout"), "Time to Event (Endpoint)", choices = endpoint_choices, 
                                            selected = endpoint, multiple = FALSE),
                        optionalSelectInput(ns("strat"), "Stratify by", choices = strata_var_choices, 
                                            selected = strata_var, multiple = TRUE),
                        optionalSelectInput(ns("facetby"), "Facet Plots by:", choices = facet_var_choices, 
                                            selected = facet_var, multiple = TRUE),
                        selectInput(ns("ref_arm"), "Reference Arm", choices = NULL, 
                                    selected = NULL, multiple = TRUE),
                        helpText("Reference groups automatically combined into a single group if more than one value selected."),
                        selectInput(ns("comp_arm"), "Comparison Group", choices = NULL, selected = NULL, multiple = TRUE),
                        checkboxInput(ns("combine_arm"), "Combine all comparison groups?", value = FALSE),
                        tags$label("Plot Settings", class = "text-primary"),
                        optionalSliderInputValMinMax(ns("plot_height"), "plot height", plot_height, ticks = FALSE)
                      )
                      
  )
}


srv_kmplot <- function(input, output, session, datasets) {
  
  
  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("kmplot"), height=plot_height)
  })
  
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
  
  
  output$kmplot <- renderPlot({
    ATE_Filtered <- ATE_Filtered()
    
    tteout <- input$tteout
    var_arm <- input$var_arm
    facetby <- input$facetby
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    strat <- input$strat
    combine_arm <- input$combine_arm
    
    # teal:::as.global(ATE_Filtered)
    # teal:::as.global(var_arm)
    # teal:::as.global(ANL)
    # teal:::as.global(facetby)
    # teal:::as.global(ref_arm)
    # teal:::as.global(strat)
    # teal:::as.global(comp_arm)
    # teal:::as.global(combine_arm)

    validate(need(!is.null(comp_arm), "select at least one comparison arm"))
    validate(need(!is.null(ref_arm), "select at least one reference arm"))
    validate(need(length(intersect(ref_arm, comp_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    validate(need(var_arm %in% names(ATE_Filtered), "var_arm is not in ATE"))
    validate(need(is.null(facetby)  || facetby %in% names(ATE_Filtered), "facet by not correct"))
    validate(need(ref_arm %in% ATE_Filtered[[var_arm]], "reference arm does not exist in left over ARM values"))

    
    
    ANL1 <- ATE_Filtered %>% filter(PARAMCD == tteout) 
    ANL <- ANL1[ANL1[[var_arm]] %in% c(comp_arm, ref_arm) , ]
    
    validate(need(nrow(ANL) > 10, "Need more than 10 observations"))
   
    
    
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
    } else {
      
      facet_df <- ANL[facetby]
      
      n_unique <- sum(!duplicated(facet_df))

      lab <- Map(function(var, x) paste0(var, "= '", x,"'"), facetby, facet_df) %>%
        unname() %>%
        Reduce(function(x, y) paste(x, y, sep = ", "), .) %>%
        unlist() %>%
        factor()
      
      if (length(unique(lab)) != n_unique) stop("algorithm error")  
  
      dfs <- split(ANL, lab)
      
      nplots <- n_unique
      grid.newpage()
      pushViewport(plotViewport(margin = c(3, 10, 2, 2)))
      pushViewport(viewport(layout = grid.layout(ncol = 1, nrow = 2*nplots-1,
        heights = unit(head(rep(c(1, 7), nplots), -1), head(rep(c("null", "lines"), nplots), -1))
         )))
      
      Map(function(dfi, i, label) {
        pushViewport(viewport(layout.pos.row = i*2 - 1))
        kmplot(
          formula_km, data = dfi, add_km = TRUE,
          add_coxph = TRUE, formula_coxph,
          info_coxph,
          add = TRUE,
          title = paste0("Kaplan - Meier Plot for: ", label)
        )
        popViewport()
      }, dfs, 1:length(dfs), levels(lab))
       
    }
 
  })
}

