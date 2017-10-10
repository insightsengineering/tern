#' Teal Module to display demographic information
#' 
#' Describe what this teal module does
#' 
#' 
#' @param label label of tab
#' 
#' @export
#' 
#' @author Xiao Yu Mo (mox5), \email{xiao_yu.mo@roche.com}
#' 
#' @examples 
#' 
#' \dontrun{
#' library(atezo.data)
#' 
#' ASL <- asl(com.roche.atezo.cdpt7722.wo29637)
#' 
#' x <- teal::init(
#'    data = list(ASL = ASL),
#'    modules = root_modules(
#'        tm_demographic()
#'    )
#' )
#' shinyApp(x$ui, x$server)
#' 
#' }
tm_demographic <- function(label = "Demographic Table") {
  module(
    label = label,
    server = srv_demographic,
    ui = ui_demographic,
    filters = "ASL"
  )
}


ui_demographic <- function(id) {
  ns <- NS(id)
  standard_layout(
    output = tableOutput(ns("table"))
  )
}

srv_demographic <- function(input, output, session, datasets) {
  
  output$table <- renderTable({
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    
    tbl <- demographic_table(ASL_FILTERED)
    
    validate(need(is(tbl, "table"), "need table object"))
    
    as.data.frame.matrix(tbl, row.names = rownames(tbl))
    
  }, rownames=TRUE, bordered=TRUE, html.table.attributes = 'style="background-color:white;"')
}