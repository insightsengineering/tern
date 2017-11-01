

#' Demographic Table Teal Module
#' 
#' @export
#' 
#' @examples 
#' \donotrun{
#' library(atezo.data)
#' library(dplyr)
#' library(survival)
#' 
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   modules = root_modules(
#'     tm_demographic_table(
#'        label = "Demographic Table",
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD"),
#'        summarize_vars =  toupper(c("sex", "mliver", "tciclvl2")),
#'        summarize_vars_choices = toupper(c("sex", "mliver", "tciclvl2", "bage", "age4cat",
#'             "ethnic", "race", "bwt", "tobhx", "hist", "EGFRMUT",
#'             "alkmut", "krasmut", "becog"))
#'    )
#'   )
#' )   
#' shinyApp(x$ui, x$server) 
#' 
#'   
#' } 
#' 
#' 
tm_demographic_table <- function(label,
                                 arm_var,
                                 arm_var_choices = arm_var,
                                 summarize_vars,
                                 summarize_vars_choices = summarize_vars,
                                 pre_output = NULL, post_output = NULL) {

  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_demographic_table,
    ui = ui_demographic_table,
    ui_args = args,
    filters = "ASL"
  )
  
}

ui_demographic_table <- function(id,
                                 label,
                                 arm_var,
                                 arm_var_choices = arm_var,
                                 summarize_vars,
                                 summarize_vars_choices = summarize_vars,
                                 pre_output = NULL, post_output = NULL) {
  
  ns <- NS(id)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("demographic_table"))),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ASL")),
      optionalSelectInput(ns("arm_var"), "Arm Variable", arm_var_choices, arm_var, multiple = FALSE),
      optionalSelectInput(ns("summarize_vars"), "Summarize Variables", summarize_vars_choices, summarize_vars, multiple = TRUE)
    ),
    pre_output = pre_output,
    post_output = post_output
  )
  
}

srv_demographic_table <- function(input, output, session, datasets) {
  
  output$demographic_table <- renderUI({
    
    ASL_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    
    arm_var <- input$arm_var
    summarize_vars <- input$summarize_vars
    
    # teal:::as.global(ASL_filtered)
    # teal:::as.global(arm_var)
    # teal:::as.global(summarize_vars)
  
    
    validate(need(!is.null(summarize_vars), "please select 'summarize variables'"))
    validate(need(all(summarize_vars %in% names(ASL_filtered)), "not all variables available"))
    validate(need(nrow(ASL_filtered) > 10, "Need more than 10 patients to make the table"))
    validate(need(ASL_filtered[[arm_var]], "Arm variable does not exist"))
    
    tbl <- try(demographic_table(
      data = ASL_filtered,
      arm_var = "ARM",
      all.patients = TRUE,
      group_by_vars = summarize_vars
    ))
    
    if (is(tbl, "try-error")) validate(need(FALSE, "could not calculate demographic table"))
    
    as_html(tbl)
  })
  
}

