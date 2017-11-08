#' @title Response Table Teal Module
#' 
#' @description
#' This module produces a response summary table that matches the STREAM
#' template rspt01
#' 
#' @inheritParams teal::standard_layout
#' @param label full name label of module
#' @param paramcd filter the rows in ARS given the paramcd value
#' @param paramcd_choices choices of possible poaramcd
#' @param arm.var selected variable to use as arms
#' @param arm.var_choices choices of arm variables
#' @param strata.var categorical variable name(s) for stratified model
#' @param strata.var_choices choices of stratification factors 
#' 
#' @details 
#' Additional standard UI inputs include \code{responders}, \code{incl_missing} (default TRUE), 
#' \code{ref_arm}, \code{comp_arm} and \code{combine_arm} (default FALSE)
#' 
#' Default values of the inputs \code{var_arm}, \code{ref_arm} and \code{comp_arm} 
#' are set to NULL, and updated accordingly based on seletion of \code{paramcd} and \code{arm.var}
#' 
#' Package \code{forcats} used to re-format arm data into leveled factors. 
#' 
#' Reference arms automatically combined if multiple arms selected as reference group. 
#' 
#' @return an \code{\link[teal]{module}} object
#' 
#' @export
#' 
#' @examples  
#' 
#' \donotrun{
#' library(atezo.data)
#' library(dplyr)
#' library(teal.oncology)
#' library(forcats)
#' library(epiR)
#' 
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
#'        paramcd_choices = c("BESRSPI","LSTASDI","MBESRSPI","MLSTASDI","OVRSPI"),
#'        #paramcd_choices = unique(ARS$PARAMCD),
#'        arm.var = "ARM",
#'        arm.var_choices = c("ARM", "ARMCD"),
#'        #arm.var_choices = names(ARS),
#'        strata.var = NULL,
#'        strata.var_choices = c("SEX", "TC1IC1", "TC2IC2", "TC3IC3", "TCIC4GRP",
#'                               "AGE4CAT", "AGEGRP", "AGE65", "TOBHX")
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
                              strata.var = NULL,
                              strata.var_choices = strata.var,
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


#' UI part for response table teal module
#' 
#' @inheritParams tm_response_table
#' @param id namespace id
#' 
#' @details 
#' Additional standard UI inputs include \code{responders}, \code{incl_missing} (default TRUE), 
#' \code{ref_arm}, \code{comp_arm} and \code{combin_arm} (default FALSE)
#' 
#' Default values of the inputs \code{var_arm}, \code{ref_arm} and \code{comp_arm} are set to NULL, 
#' and updated accordingly based on seletion of \code{paramcd} and \code{arm.var}
#' 
#' @noRd
#' 
ui_response_table <- function(id, label,
                              paramcd = "OVRSPI",
                              paramcd_choices = paramcd,
                              arm.var = "ARM",
                              arm.var_choices = arm.var,
                              strata.var = NULL,
                              strata.var_choices = strata.var,
                              pre_output,
                              post_output) {
  ns <- NS(id)
  
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("response_table"))),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ARS")),
      #Response related parameters
      optionalSelectInput(ns("paramcd"), div("PARAMCD", tags$br(), helpText("Select one type of response to analyze.")), 
                          paramcd_choices, paramcd, multiple = FALSE),
      selectInput(ns("responders"), "Responders", 
                  choices = NULL, selected = NULL, multiple = TRUE),
      checkboxInput(ns("incl_missing"), "Include missing as non-responders?", value = TRUE),
      #Arm related parameters
      optionalSelectInput(ns("var_arm"), div("Grouping Variable", tags$br(), helpText("Select one variable to use for grouping")), 
                          arm.var_choices, arm.var, multiple = FALSE),
      selectInput(ns("ref_arm"), "Reference Group", 
                          choices = NULL, selected = NULL, multiple = TRUE),
      helpText("Reference groups automatically combined into a single group if more than one value selected."),
      selectInput(ns("comp_arm"), "Comparison Group", choices = NULL, selected = NULL, multiple = TRUE),
      checkboxInput(ns("combine_arm"), "Combine all comparison groups?", value = FALSE),
      #Stratification related parameters
      selectInput(ns("var_strata"), div("Stratification Factors", tags$br(), helpText("Categorical variable(s) only")), 
                  choices = strata.var_choices, selected = strata.var, multiple = TRUE)
      
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' Server part for response table teal module
#' 
#' @inheritParams tm_response_table
#' @param id namespace id
#' 
#' @details 
#' Selection for standard UI inputs \code{responders}, \code{ref_arm} and \code{comp_arm} 
#' are updated upon selection of \code{paramcd} and \code{arm.var}.
#' 
#' Package \code{forcats} used to re-format arm data into leveled factors. 
#' Reference arms automatically combined if multiple arms selected as reference group. 
#' 
#' @importFrom forcats fct_relevel fct_collapse
#' 
#' @noRd
#' 
srv_response_table <- function(input, output, session, datasets) {

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
    var_strata <- input$var_strata
    
    
    # Validate your input
    validate(need(!is.null(ARS_filtered) && is.data.frame(ARS_filtered), "no data left"))
    validate(need(nrow(ARS_filtered) > 0 , "no observations left"))
    
    validate(need(!is.null(paramcd) && paramcd %in% ARS_filtered$PARAMCD,
                  "PARAMCD does not exist"))
    
    validate(need(!is.null(responders) && all(responders %in% ARS_filtered$AVALC),
                  "responders AVALC does not exist"))
   
    validate(need(!is.null(comp_arm) && !is.null(ref_arm),
                  "need at least one treatment and one reference arm"))
    validate(need(length(intersect(ref_arm, comp_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    
    validate(need(all(c(ref_arm, comp_arm) %in% ARS_filtered[[var_arm]]), "arm variable not found in ARS"))
    
    validate(need(all(var_strata %in% names(ARS_filtered)), "stratification factor not found in ARS"))
    
    # Assign inputs to global
    # teal:::as.global(ARS_filtered)
    # teal:::as.global(paramcd)
    # teal:::as.global(responders)
    # teal:::as.global(incl_missing)
    # teal:::as.global(var_arm)
    # teal:::as.global(ref_arm)
    # teal:::as.global(comp_arm)
    # teal:::as.global(combine_arm)
    # teal:::as.global(var_strata)
    
    
    # Get final analysis dataset
    ANL1 <- ARS_filtered %>% filter(PARAMCD == paramcd)

    ANL <- ANL1[ANL1[[var_arm]] %in% c(ref_arm, comp_arm), ]
  
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
      armtmp <- fct_collapse(arm1, refs = ref_arm)
      arm2 <- fct_relevel(armtmp, "refs", comp_arm)
      levels(arm2)[which(levels(arm2)=="refs")] <- refname
    } else {
      arm2 <- fct_relevel(arm1, ref_arm, comp_arm)
    }
    
    if (length(comp_arm) > 1 && combine_arm == TRUE) {
      compname <- paste0(comp_arm, collapse = "/")
      ARM <- fct_collapse(arm2, comps = comp_arm)
      levels(ARM)[which(levels(ARM)=="comps")] <- compname
    } else {
      ARM <- arm2
    }

    tbl <- try(response_table(
      response = ANL$AVALC,
      value.resp = responders,
      value.nresp = setdiff(ANL$AVALC, responders),
      arm = ARM,
      strata_data = if (!is.null(var_strata)) ANL[var_strata] else NULL
    ))
    
    if (is(tbl, "try-error")) validate(need(FALSE, "could not calculate response table"))
    
    as_html(tbl)
  })
}
