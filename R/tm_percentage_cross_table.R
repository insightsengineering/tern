


#' Cross table based on rtables
#' 
#' @export
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' ASL <- data.frame(
#'   USUBJID = paste("p", 1:100, sep = "-"),
#'   STUDYID = "A",
#'   x = sample(c("Y", "N"), 100, TRUE),
#'   y = sample(c("A", "B", "C"), 100, TRUE),
#'   stringsAsFactors = FALSE
#' )
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   modules = root_modules(
#'     tm_percentage_cross_table("Cross Table", x_var = "x", y_var = "y")
#'   )
#' )
#' 
#' shinyApp(x$ui, x$server)
#'
#' 
#' 
#' }
tm_percentage_cross_table <- function(
  label = "Cross Table",
  x_var,
  x_var_choices = x_var,
  y_var,
  y_var_choices = y_var
) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_percentage_cross_table,
    ui = ui_percentage_cross_table,
    ui_args = args,
    filters = "ASL"
  )
  
}


ui_percentage_cross_table <- function(id,
                                      label = "Cross Table",
                                      x_var,
                                      x_var_choices,
                                      y_var,
                                      y_var_choices) {
  
  ns <- NS(id)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("table"))),
    encoding = div(
      helpText("Dataset:", tags$code("ASL")),
      optionalSelectInput(ns("x_var"), "x var", x_var_choices, x_var),
      optionalSelectInput(ns("y_var"), "y var", y_var_choices, y_var)
    )
  )
}

srv_percentage_cross_table <- function(input, output, session, datasets) {
  

  output$table <- renderUI({
    
    ASL_filtered <- datasets$get_data("ASL", filtered = TRUE, reactive = TRUE)
    x_var <- input$x_var
    y_var <- input$y_var
    
    validate(need(ASL_filtered, "data missing"))
    validate(need(nrow(ASL_filtered) > 10, "need at least 10 patients"))
    
    x <- ASL_filtered[[x_var]]
    y <- ASL_filtered[[y_var]]
    
    validate(need(x, "selected x_var does not exist"))
    validate(need(y, "selected y_var does not exist"))
    
    
    teal:::as.global(x)
    teal:::as.global(y)
    
    X <- addmargins(table(x,y))
    P <- X / X[nrow(X), ncol(X)]
    
    tbl <- try({
      rows <- lapply(1:nrow(X), function(i) {
        
        x_i <- X[i, ]
        p_i <- P[i, ]
        
        r <- Map(function(xii, pii) c(xii, pii), x_i, p_i)
        
        do.call(rrow, c(list(rownames(X)[i]), r))
        
      })
      
      do.call(rtable, c(
        list(
          col.names = colnames(X),
          format = "xx (xx.xx%)"    
        ),
        rows
      ))
    })


    if (is(tbl, "try-error")) validate(need(FALSE, paste0("creating the table failed:\n\n", tbl)))
    
    as_html(tbl)
    
  })
  
}

