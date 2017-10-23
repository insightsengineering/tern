
#' Response Table teal module
#' 
#' @export
#' 
#' @examples  
#' 
#' \donotrun{
#' library(atezo.data)
#' library(dplyr)
#' library(forcats)
#' 
#' ARS <- ars(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' options(teal_logging = FALSE)
#' 
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL, ARS = ARS),
#'   modules = root_modules(
#'     tm_variable_browser(),
#'     tm_data_table(),
#'     tm_response_table(
#'        label = "Response Table",
#'        paramcd = "OVRSPI",
#'        paramcd_choices = unique(ARS$PARAMCD),
#'        arm.var = "ARM",
#'        arm.var_choices = c("ARM", "ARMCD")
#'    )
#'   )
#' )   
#' shinyApp(x$ui, x$server) 
#' 
#'   
#' } 
tm_response_table <- function(label,
                               paramcd = "OVRSPI",
                               paramcd_choices = paramcd,
                               arm.var = "ARM",
                               arm.var_choices = arm.var,
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
                               paramcd = "OVRSPI",
                               paramcd_choices = paramcd,
                               arm.var = "ARM",
                               arm.var_choices = arm.var,
                               pre_output,
                               post_output) {
  ns <- NS(id)
  
  
  standard_layout(
    output = uiOutput(ns("response_table")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ARS")),
      optionalSelectInput(ns("paramcd"), div("PARAMCD", tags$br(), helpText("Select one type of response to analyze.")), 
                          paramcd_choices, paramcd, multiple = FALSE),
      selectInput(ns("responders"), "Responders", 
                  choices = NULL, selected = NULL, multiple = TRUE),
      checkboxInput(ns("incl_missing"), "Include missing as non-responders?", value = TRUE),
      
      optionalSelectInput(ns("var_arm"), div("Grouping Variable", tags$br(), helpText("Select one variable to use for grouping")), 
                          arm.var_choices, arm.var, multiple = FALSE),
      selectInput(ns("ref_arm"), "Reference Group", 
                          choices = NULL, selected = NULL, multiple = TRUE),
      helpText("Reference groups automatically combined into a single group if more than one value selected."),
      selectInput(ns("comp_arm"), "Comparison Group", choices = NULL, selected = NULL, multiple = TRUE),
      checkboxInput(ns("combine_arm"), "Combine all comparison groups?", value = FALSE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
}

srv_response_table <- function(input, output, session, datasets) {

  # Deal With Reactivity/Inputs
  ARS_filtered <- reactive({
    ARS_f <- datasets$get_data("ARS", reactive = TRUE, filtered = TRUE)
    ARS_f
  })
  
  # Update UI choices depending on selection of previous options
  observe({
    ANL <- ARS_filtered()
    updateSelectInput(session, "responders", 
                      choices = unique(ANL$AVALC[ANL$PARAMCD == input$paramcd]),
                      selected = c("CR", "PR"))
    updateSelectInput(session, "responders", 
                      choices = unique(ANL$AVALC[ANL$PARAMCD == input$paramcd]),
                      selected = c("CR", "PR"))
    updateSelectInput(session, "ref_arm", choices = unique(ANL[[input$var_arm]]),
                      selected = ANL[[input$var_arm]] %>% unique %>% sort %>% "["(1))
    updateSelectInput(session, "comp_arm", choices = unique(ANL[[input$var_arm]]),
                      selected = ANL[[input$var_arm]] %>% unique %>% sort %>% "["(-1))
    
  })

  
  output$response_table <- renderUI({
    
    ARS_filtered <- ARS_filtered()
  
    paramcd <- input$paramcd
    responders <- input$responders
    incl_missing <- input$incl_missing
    var_arm <- input$var_arm
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    combine_arm <- input$combine_arm
    
    
    # Validate your input
    validate(need(!is.null(ARS_filtered) && is.data.frame(ARS_filtered), "no data left"))
    validate(need(nrow(ARS_filtered) > 0 , "no observations left"))
    
    validate(need(!is.null(paramcd) && paramcd %in% ARS_filtered$PARAMCD,
                  "PARAMCD does not exist"))
    
    validate(need(!is.null(comp_arm) && !is.null(ref_arm),
                  "need at least one treatment and one reference arm"))
    validate(need(length(intersect(ref_arm, comp_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    
    validate(need(!is.null(responders) && all(responders %in% ARS_filtered$AVALC),
                  "responders AVALC does not exist"))
    
    # Assign inputs to global
    teal:::as.global(ARS_filtered)
    teal:::as.global(paramcd)
    teal:::as.global(responders)
    teal:::as.global(incl_missing)
    teal:::as.global(var_arm)
    teal:::as.global(ref_arm)
    teal:::as.global(comp_arm)
    teal:::as.global(combine_arm)
    
    
    # Get final analysis dataset
    ANL <- ARS_filtered %>% filter(PARAMCD == paramcd, get(var_arm) %in% c(ref_arm, comp_arm))
    validate(need(nrow(ANL) > 0, "no data left"))
    
    #--- Manipulation of response and arm variables ---#
    # Recode/filter responses if want to include missing as non-responders
    if (incl_missing == TRUE) {
      ANL$AVALC[ANL$AVALC==""] <- "NE"
    } else {
      ANL <- ANL %>% filter(AVALC != "")
    }
    
    # Recode grouping according to ref_arm, comp_arm and combine_arm settings
    arm1 <- factor(ANL[[var_arm]])
  
    if (length(ref_arm) > 1) {
      refname <- paste0(ref_arm, collapse = "/")
      arm2 <- fct_collapse(arm1, refs = ref_arm)
      levels(arm2)[which(levels(arm2)=="refs")] <- refname
    } else {
      arm2 <- fct_relevel(arm1, ref_arm)
    }
    
    if (length(comp_arm) > 1 && combine_arm == TRUE) {
      compname <- paste0(comp_arm, collapse = "/")
      ARM <- fct_collapse(arm2, comps = comp_arm)
      levels(ARM)[which(levels(ARM)=="comps")] <- compname
    } else {
      ARM <- arm2
    }

    tbl <- response_table(
      response = ANL$AVALC,
      value.resp = responders,
      value.nresp = setdiff(ANL$AVALC, responders),
      arm = ARM
    )
    
    as_html(tbl)
  })
}
