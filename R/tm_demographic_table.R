

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
#'     tm_variable_browser(),
#'     tm_data_table(),
#'     tm_demographic_table(
#'        label = "Demographic Table",
#'        group_by_vars = toupper(c("sex", "mliver", "tciclvl2", "bage", "age4cat",
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
                                 group_by_vars,
                                 group_by_vars_choices = group_by_vars,
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
                                 group_by_vars,
                                 group_by_vars_choices = group_by_vars,
                                 pre_output = NULL, post_output = NULL) {
  
  ns <- NS(id)
  
  standard_layout(
    output = uiOutput(ns("demographic_table")),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ARS")),
      optionalSelectInput(ns("group_by_vars"), "For variables", group_by_vars_choices, group_by_vars, multiple = TRUE)
    ),
    pre_output = pre_output,
    post_output = post_output
  )
  
}

srv_demographic_table <- function(input, output, session, datasets) {
  
  output$demographic_table <- renderUI({
    
    ASL_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    
    group_by_vars <- input$group_by_vars
    
    validate(need(!is.null(group_by_vars), "please select 'for variables'"))
    validate(need(all(group_by_vars %in% names(ASL_filtered)), "not all variables"))
    validate(need(nrow(ASL_filtered) > 10, "Need more than 10 patients to make the table"))
    
    tbl <- demographic_table(
      data = ASL_filtered,
      arm_var = "ARM",
      all.patients = TRUE,
      group_by_vars = group_by_vars
    )
    
    as_html(tbl)
  })
  
}

