#' @title Change from Baseline Table Teal Module
#'   
#' @description This module produces a summary table for test results and change
#' from baseline by visit, that matches STREAM template LBT01
#' 
#' @inheritParams teal::standard_layout
#' @param label full name label of module
#' @param paramcd filter the rows in AQS given the paramcd value
#' @param paramcd_choices choices of possible poaramcd
#' @param arm_var selected variable to use as arms
#' @param arm_var_choices choices of arm variables
#'   
#' @details 
#' Package \code{forcats} used to re-format arm data into leveled factors.
#'   
#' @author Chendi Liao (liaoc10), \email{chendi.liao@roche.com}
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
#' library(tidyr)
#' library(teal.oncology)
#' library(forcats)
#' 
#' ASL <- asl(com.roche.cdpt7722.wo29637.rl)
#' AQS <- aqs(com.roche.cdpt7722.wo29637.rl)
#' 
#' options(teal_logging = FALSE)
#' 
#' parammap <- unique(AQS[c("PARAM", "PARAMCD")]) 
#' paramcd_choices_labelled <- setNames(parammap$PARAMCD, paste(parammap$PARAMCD, parammap$PARAM, sep = " - "))
#' 
#' arm_var_choices_list <- c("ARM", "ARMCD", "AGE65", "SEX", "HSTSTYP", "PMETA", 
#'                           "IC","ICC", "TCC", "ICCAT1", "ICCAT2", "ICCAT3", "BIO2ICL")
#' arm_var_choices_labelled <- setNames(arm_var_choices_list, 
#'                             paste(arm_var_choices_list, labels_over_names(ASL[arm_var_choices_list]), sep=" - "))
#' 
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL, AQS = AQS),
#'   modules = root_modules(
#'     #tm_variable_browser(),
#'     #tm_data_table(),
#'     tm_chgfbl_table(
#'        label = "Change from Baseline Table",
#'        paramcd = "FATIGI",
#'        paramcd_choices = paramcd_choices_labelled,
#'        arm_var = "ARM",
#'        arm_var_choices = arm_var_choices_labelled,
#'    ))
#' )   
#' shinyApp(x$ui, x$server) 
#' 
#'   
#' } 
tm_chgfbl_table <- function(label,
                            paramcd,
                            paramcd_choices = paramcd,
                            arm_var = "ARM",
                            arm_var_choices = arm_var,
                            pre_output = NULL, post_output = NULL
                            ){
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_chgfbl_table,
    ui = ui_chgfbl_table,
    ui_args = args,
    filters = "AQS"
  )
}



#' UI part of change from baseline table teal module
#' 
#' @inheritParams tm_chgfbl_table
#' @param id namespace id
#'   
#' @details 
#' 
#' @noRd
#'
ui_chgfbl_table <- function(id, label,
                            paramcd,
                            paramcd_choices = paramcd,
                            arm_var = "ARM",
                            arm_var_choices = arm_var,
                            pre_output = NULL, post_output = NULL) {
  
  
  ns <- NS(id)
 
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("chgfbl_table"))),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ASL"), tags$code("AQS")),
      
      optionalSelectInput(ns("paramcd"), div("PARAMCD", tags$br(), helpText("Select one parameter to analyze.")), 
                          paramcd_choices, paramcd, multiple = FALSE),

      optionalSelectInput(ns("arm_var"), div("Grouping Variable", tags$br(), helpText("Select one variable to use for grouping")), 
                          arm_var_choices, arm_var, multiple = FALSE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
}


#' Server part of change from baseline table teal module
#' 
#' @inheritParams tm_chgfbl_table
#' @param id namespace id
#' 
#' @details
#' 
#' @importFrom forcats fct_relevel fct_collapse
#' 
#' @noRd
#' 
srv_chgfbl_table <- function(input, output, session, datasets) {
  
  output$chgfbl_table <- renderUI({
    
    ASL_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    AQS_filtered <- datasets$get_data("AQS", reactive = TRUE, filtered = TRUE)
    
    paramcd <- input$paramcd
    arm_var <- input$arm_var

    # Validating inputs
    validate(need(!is.null(AQS_filtered) && is.data.frame(AQS_filtered), "no data left"))
    validate(need(nrow(AQS_filtered) > 0 , "no observations left"))
    
    validate(need(!is.null(paramcd) && paramcd %in% AQS_filtered$PARAMCD,
                  "PARAMCD does not exist"))
    
    validate(need(ASL_filtered[[arm_var]], "no valid arm selected"))
    
    
    ## Get final datasets
    ASL_f <- ASL_filtered[c("STUDYID", "USUBJID", arm_var)]
    AQS_f <- AQS_filtered %>% filter(PARAMCD == paramcd) 
    
    ANL <- left_join(ASL_f, AQS_f, by = c("STUDYID", "USUBJID"))
    
    validate(need(nrow(ANL) > 0, "no data left"))
    
    
    ## Create change from baseline data to be used for table function
    df <- chgfbl_data(data = ANL,
                      arm_var = arm_var)
    
    
    
    ## Produce table
    tbl <- try(chgfbl_table(df))
    
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not create chgfbl table:\n\n", tbl)))
    
    as_html(tbl)
  })
}
