
#' Forest Response Plot teal module
#' 
#' @export
#' 
#' @examples  
#' 
#' \donotrun{
#' library(atezo.data)
#' library(dplyr)
#' library(survival)
#' library(forcats)
#' 
#' ARS <- ars(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#'  
#' ASL$BAGED <- ifelse(ASL$BAGE <= median(ASL$BAGE), "<=median", ">median")
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL, ARS = ARS),
#'   modules = root_modules(
#'     tm_variable_browser(),
#'     tm_data_table(),
#'     tm_forest_response(
#'        label = "Forest Response",
#'        paramcd = "OVRSPI",
#'        paramcd_choices = c("BESRSPI","LSTASDI","MBESRSPI","MLSTASDI","OVRSPI"),
#'        plot_height = c(600, 200, 2000),
#'        subgroup_var = c("BAGED", "SEX", "BECOG"),
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD", "ACTARM")
#'    )
#'   )
#' )   
#' shinyApp(x$ui, x$server) 
#' 
#'   
#' } 


tm_forest_response <- function(label,
                               paramcd = "OVRSPI",
                               paramcd_choices = paramcd,
                               arm_var = "ARM",
                               arm_var_choices = arm_var,
                               subgroup_var,
                               subgroup_var_choices = subgroup_var,
                               plot_height = c(700, 200, 2000),
                               cex = 1.5,
                               pre_output = helpText("graph needs to be of a certain width to be displayed"),
                               post_output = NULL){
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_forest_response,
    ui = ui_forest_response,
    ui_args = args,
    server_args = list(cex = cex),
    filters = "ARS"
  )
}

ui_forest_response <- function(id, label,
                               paramcd = "OVRSPI",
                               paramcd_choices = paramcd,
                               arm_var = "ARM",
                               arm_var_choices = arm_var,
                               subgroup_var,
                               subgroup_var_choices = subgroup_var,
                               plot_height,
                               cex = 1.5,
                               pre_output,
                               post_output) {
  ns <- NS(id)
  
  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ARS")),
      optionalSelectInput(ns("paramcd"), div("PARAMCD", tags$br(), helpText("Select one type of response to analyze.")), 
                          paramcd_choices, paramcd, multiple = FALSE),
      selectInput(ns("responders"), "Responders", 
                  choices = NULL, selected = NULL, multiple = TRUE),
      optionalSelectInput(ns("arm_var"), div("Arm Variable", tags$br(), helpText("Select one variable to use for comparison")), 
                                                                                        arm_var_choices, arm_var, multiple = FALSE),
      selectInput(ns("ref_arm"), "Reference Arm", 
                  choices = NULL, selected = NULL, multiple = TRUE),
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      selectInput(ns("comp_arm"), "Comparison Arm", choices = NULL, selected = NULL, multiple = TRUE),
      helpText("Multiple arms automatically combined into a single arm if more than one value selected."),
      optionalSelectInput(ns("subgroup_var"), "Subgroup Variables", subgroup_var_choices, subgroup_var, multiple = TRUE),
      tags$label("Plot Settings", class="text-primary", style="margin-top: 15px;"),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", plot_height, ticks = FALSE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
} 

srv_forest_response <- function(input, output, session, datasets, cex = 1.5) {
  
  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("forest_plot"), height=plot_height)
  })
  
  
  # Deal With Reactivity/Inputs
  ARS_filtered <- reactive({
    ARS_f <- datasets$get_data("ARS", reactive = TRUE, filtered = TRUE)
    ARS_f
  })
  
  # Update UI choices depending on selection of previous options
  observe({
    input$paramcd
    ANL <- datasets$get_data("ARS", filtered = FALSE, reactive = FALSE)
    updateSelectInput(session, "responders", 
                      choices = unique(ANL$AVALC[ANL$PARAMCD == input$paramcd]),
                      selected = c("CR", "PR"))
    updateSelectInput(session, "ref_arm", choices = unique(ANL[[input$arm_var]]),
                      selected = ANL[[input$arm_var]] %>% unique %>% sort %>% "["(1))
    updateSelectInput(session, "comp_arm", choices = unique(ANL[[input$arm_var]]),
                      selected = ANL[[input$arm_var]] %>% unique %>% sort %>% "["(-1))
    
  })
  
  
  output$forest_plot <- renderPlot({
    
    ARS_filtered <- ARS_filtered()
    ASL_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    
    validate(need(!is.null(ARS_filtered) && is.data.frame(ARS_filtered), "no data left"))
    validate(need(nrow(ARS_filtered) > 0 , "no observations left"))
    
    
    paramcd <- input$paramcd
    responders <- input$responders
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    subgroup_var <- input$subgroup_var
    
    validate(need(!is.null(comp_arm) && !is.null(ref_arm),
                  "need at least one treatment and one reference arm"))
    validate(need(length(intersect(ref_arm, comp_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    
    validate(need(!is.null(responders) && all(responders %in% ARS_filtered$AVALC),
                  "response AVALC does not exist"))
    
    validate(need(!is.null(paramcd) && paramcd %in% ARS_filtered$PARAMCD,
                  "PARAMCD does not exist"))
    
    
    ## Get final datasets
    ARS_filtered$arm_var <- ARS_filtered[[arm_var]]
    ARS_f <- ARS_filtered %>% filter(PARAMCD == paramcd & arm_var %in% c(ref_arm, comp_arm)) 
    
    validate(need(nrow(ARS_f) > 0, "no data left"))
    
    validate(need(all(subgroup_var %in% names(ASL_filtered)), "some baseline risk variables are not valid"))
    
    ASL_f <- ASL_filtered %>% select("STUDYID", "USUBJID", subgroup_var) 
    
    validate(need(all(subgroup_var %in% names(ASL_f)), "some subgroup variables are not valid"))
    
    ARS_anl <- inner_join(ASL_f %>% select("STUDYID", "USUBJID") , ARS_f, by = c("STUDYID","USUBJID"))
    
    validate(need(all(c(ref_arm, comp_arm) %in% ARS_anl$arm_var), "data needs to include at least one patient from the reference and comparison arm"))  
    
    #Filter ASL to get the grouping variables
    group_data <- inner_join(ASL_f, ARS_f %>% select(c("USUBJID", "STUDYID")), by = c("STUDYID","USUBJID"))
    names(group_data) <- labels_over_names(group_data)
    head(group_data)
    ## add
    ## the arm combine & filtering and converting to a factor here...paste0(ref_arm, collapse = "/")
    ## using forcats
    arm <- fct_collapse(ARS_anl$arm_var, ref_arm = ref_arm, comp_arm = comp_arm)
    arm <- ifelse (arm == "ref_arm", paste0(ref_arm, collapse = "/"), paste0(comp_arm, collapse = "/")) 
    arm <- fct_relevel(arm, paste0(ref_arm, collapse = "/"))
    
    tbl <- forest_rsp(
      response = ARS_anl$AVAL,
      event = ARS_anl$AVALC %in% responders,
      arm = arm, 
      group_data = group_data[, -c(1,2), drop=FALSE]
    )
    
    forest_rsp_plot(tbl, levels(arm)[1], levels(arm)[2], cex = cex)
  })
}
