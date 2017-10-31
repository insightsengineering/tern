
#' Forest Survival Plot teal module
#' 
#' @export
#' 
#' @examples  
#' 
#' \donotrun{
#' library(atezo.data)
#' library(teal.oncology)
#' library(dplyr)
#' library(forcats)
#' 
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' ASL$BAGED <- ifelse(ASL$BAGE <= median(ASL$BAGE), "<=median", ">median")
#' 
#' options(teal_logging = FALSE)
#' 
#' arms <- unique(ASL$ARM)
#' x <- teal::init(
#'   data = list(ASL = ASL, ATE = ATE),
#'   modules = root_modules(
#'     tm_forest_survival(
#'        label = "Forest Survival",
#'        paramcd = "OS",
#'        paramcd_choices = unique(ATE$PARAMCD),
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
tm_forest_survival <- function(label,
                               arm_var = "ARM",
                               arm_var_choices = arm_var,
                               subgroup_var,
                               subgroup_var_choices = subgroup_var,
                               paramcd = "OS",
                               paramcd_choices = paramcd,
                               plot_height = c(600, 200, 2000),
                               pre_output = helpText("graph needs to be of a certain width to be displayed"),
                               post_output = NULL) {
  

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
                               arm_var = "ARM",
                               arm_var_choices = arm_var,
                               subgroup_var,
                               subgroup_var_choices = subgroup_var,
                               paramcd = "OS",
                               paramcd_choices = paramcd,
                               plot_height,
                               pre_output,
                               post_output) {
  ns <- NS(id)
  
  
  ## use helpText to explain your user interface
  
  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ATE")),
      optionalSelectInput(ns("paramcd"), div("PARAMCD", tags$br(), helpText("Select an endpoint to analyze.")), 
                          paramcd_choices, paramcd, multiple = FALSE),
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

srv_forest_survival <- function(input, output, session, datasets) {
  
  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("forest_plot"), height=plot_height)
  })
  
  
  # Deal With Reactivity/Inputs
  ATE_filtered <- reactive({
    ATE_f <- datasets$get_data("ATE", reactive = TRUE, filtered = TRUE)
    ATE_f
  })
  
  # Update UI choices depending on selection of previous options
  observe({
    input$paramcd
    ANL <- datasets$get_data("ATE", filtered = FALSE, reactive = FALSE)
    updateSelectInput(session, "ref_arm", choices = unique(ANL[[input$arm_var]]),
                      selected = ANL[[input$arm_var]] %>% unique %>% sort %>% "["(1))
    updateSelectInput(session, "comp_arm", choices = unique(ANL[[input$arm_var]]),
                      selected = ANL[[input$arm_var]] %>% unique %>% sort %>% "["(-1))
    
  })
  
  
  output$forest_plot <- renderPlot({
    
    ATE_filtered <- ATE_filtered()
    ASL_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
  
    paramcd <- input$paramcd
    subgroup_var <- input$subgroup_var
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    
    
    teal:::as.global(ATE_filtered)
    teal:::as.global(ASL_filtered)
    teal:::as.global(paramcd)
    teal:::as.global(subgroup_var)
    teal:::as.global(ref_arm)
    teal:::as.global(comp_arm)
    
    ## 2: Validate if your inputs can produce the requested output
    
    validate(need(!is.null(ATE_filtered) && is.data.frame(ATE_filtered), "no data left"))
    validate(need(nrow(ATE_filtered) > 10 , "need more than 10 patients to do the plot"))
    
    validate(need(!is.null(comp_arm) && !is.null(ref_arm),
                  "need at least one treatment and one reference arm"))
    validate(need(length(intersect(ref_arm, comp_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    
    validate(need(paramcd %in% ATE_filtered$PARAMCD, "time to event PARAMCD does not exist"))
    
    
    print(" ---------- passing by -------------")
    
    ## 3: Do your static code (this should also run in rmarkdown)
    
    ## Get final datasets
    ATE_filtered$arm_var <- ATE_filtered[[arm_var]]
    ATE_f <- ATE_filtered %>% filter(PARAMCD == paramcd & arm_var %in% c(ref_arm, comp_arm))
    
    validate(need(nrow(ATE_f) > 0, "no data left"))
    
    validate(need(all(subgroup_var %in% names(ASL_filtered)), "some baseline risk variables are not valid"))
    
    ASL_f <- ASL_filtered %>% select("STUDYID", "USUBJID", subgroup_var) 
    
    validate(need(all(subgroup_var %in% names(ASL_f)), "some subgroup variables are not valid"))
    ATE_anl <- inner_join(ASL_f %>% select("STUDYID", "USUBJID") , ATE_f, by = c("STUDYID","USUBJID"))
    
    validate(need(all(c(ref_arm, comp_arm) %in% ATE_anl$arm_var), "data needs to include at least one patient from the reference and comparison arm"))  
    
    #Filter ASL to get the grouping variables
    group_data <- inner_join(ASL_f, ATE_f %>% select(c("USUBJID", "STUDYID")), by = c("STUDYID","USUBJID"))
    names(group_data) <- labels_over_names(group_data)
    head(group_data)
    
    ## add
    ## the arm combine & filtering and converting to a factor here...paste0(ref_arm, collapse = "/")
    ## using forcats
    arm <- fct_collapse(ATE_f$arm_var, ref_arm = ref_arm, comp_arm = comp_arm)
    arm <- ifelse (arm == "ref_arm", paste0(ref_arm, collapse = "/"), paste0(comp_arm, collapse = "/")) 
    arm <- fct_relevel(arm, paste0(ref_arm, collapse = "/"))
    
    tbl <- forest_tte(
      time_to_event = ATE_f$AVAL,
      event = ATE_f$CNSR == 0,
      arm = arm, 
      group_data = group_data[, -c(1,2), drop=FALSE]
    )
    
    #as_html(tbl)
    
    forest_tte_plot(tbl, levels(arm)[1], levels(arm)[2], cex = 1.5)
  })
}
