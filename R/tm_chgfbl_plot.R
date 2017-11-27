#' @title Change from Baseline Plot Teal Module
#'   
#' @description This module produces a plot of the mean (of value at visit or
#'   change from baseline) by arm, that's similar to STREAM template MNG01
#'   
#' @inheritParams teal::standard_layout
#' @param label full name label of module
#' @param paramcd filter the rows in AQS given the paramcd value
#' @param paramcd_choices choices of possible paramcd
#' @param arm_var selected variable to use as arms
#' @param arm_var_choices choices of arm variables
#' @param arm_label label to display on plot for the selected arm variable
#' @param ytype selected type of value to plot for y-axis
#' @param ytype_choices choices of possible ytype: AVAL=value at visit, CHG = change from baseline, PCHG = % change from baseline
#' @param errbar selected type of error bar for the plot
#' @param errbar_choices choices of types of error bar: SE = standarnd error, SD = standard deviation, 95CI = 95% confidence interval of the mean
#' @param ref_line_txt horizontal reference lines to display on plot, entered as text separated by comma 
#' 
#'   
#' @details Package \code{forcats} used to re-format arm data into leveled
#' factors.
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
#' library(ggplot2)
#' library(grid)
#' library(gridExtra)
#' 
#' ASL <- asl(com.roche.cdpt7722.wo29637.rl)
#' AQS <- aqs(com.roche.cdpt7722.wo29637.rl)
#' 
#' options(teal_logging = FALSE)
#' 
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL, AQS = AQS),
#'   modules = root_modules(
#'     #tm_variable_browser(),
#'     #tm_data_table(),
#'     tm_chgfbl_plot(
#'        label = "Change from Baseline Plot",
#'        paramcd = "FATIGI",
#'        paramcd_choices = unique(AQS$PARAMCD),
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD", "AGE65"),
#'        #arm_label = "Treatment Arm",
#'        ytype = "CHG",
#'        ytype_choices = c("CHG", "AVAL"),
#'        errbar = "SE",
#'        errbar_choices = c("SE", "95CI")
#'        #ref_line_txt = "2, -2"
#'    ))
#' )   
#' shinyApp(x$ui, x$server)
#' 
#'    
#' } 
tm_chgfbl_plot <- function(label,
                           paramcd,
                           paramcd_choices = paramcd,
                           arm_var = "ARM",
                           arm_var_choices = arm_var,
                           arm_label = NULL,
                           ytype = "CHG",
                           ytype_choices = ytype,
                           errbar = "SE",
                           errbar_choices = errbar,
                           ref_line_txt = NULL,
                           #plot_height = c(600, 200, 2000),
                           pre_output = NULL, post_output = NULL
                           ){
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_chgfbl_plot,
    ui = ui_chgfbl_plot,
    ui_args = args,
    filters = "AQS"
  )
}



#' UI part of change from baseline table teal module
#' 
#' @inheritParams tm_chgfbl_plot
#' @param id namespace id
#'   
#' @details 
#' 
#' @noRd
#'
ui_chgfbl_plot <- function(id, 
                           label,
                           paramcd,
                           paramcd_choices = paramcd,
                           arm_var = "ARM",
                           arm_var_choices = arm_var,
                           arm_label = NULL,
                           ytype = "CHG",
                           ytype_choices = ytype,
                           errbar = "SE",
                           errbar_choices = errbar,
                           ref_line_txt = NULL,
                           #plot_height,
                           pre_output = NULL, post_output = NULL
                           ) {
  
  
  ns <- NS(id)
  
  standard_layout(
    output = uiOutput(ns("chgfbl_plot")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ASL"), tags$code("AQS")),
      
      optionalSelectInput(ns("paramcd"), div("PARAMCD", tags$br(), helpText("Select one parameter to analyze.")), 
                          paramcd_choices, paramcd, multiple = FALSE),
      optionalSelectInput(ns("arm_var"), div("Grouping Variable", tags$br(), helpText("Select one variable to use for grouping")), 
                          arm_var_choices, arm_var, multiple = FALSE),
      #textInput(ns("arm_label"), div("Grouping Variable Label", tags$br(), helpText("Enter the label for grouping variable to display on plot"))),
      
      optionalSelectInput(ns("ytype"), div("Y-axis value type", tags$br(), helpText("Select one type of value to plot on y-axis")), 
                          ytype_choices, ytype, multiple = FALSE),
      optionalSelectInput(ns("errbar"), div("Error bar type", tags$br(), helpText("Select the type of error bar to display")), 
                          errbar_choices, errbar, multiple = FALSE)
      #textInput(ns("ref_line_txt"), div("Reference Line(s)", tags$br(), helpText("Enter numeric value(s) of the horizontal reference lines, separated by comma")))
      #optionalSliderInputValMinMax(ns("plot_height"), "plot height", plot_height, ticks = FALSE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
}


#' Server part of change from baseline table teal module
#' 
#' @inheritParams tm_chgfbl_plot
#' @param id namespace id
#' 
#' @details
#' 
#' @importFrom forcats fct_relevel fct_collapse
#' 
#' @noRd
#' 
srv_chgfbl_plot <- function(input, output, session, datasets) {
  
  # ## dynamic plot height
  # output$plot_ui <- renderUI({
  #   plot_height <- input$plot_height
  #   validate(need(plot_height, "need valid plot height"))
  #   plotOutput(session$ns("chgfblplot"), height=plot_height)
  # })
  
  output$chgfbl_plot <- renderUI({
    
    ASL_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    AQS_filtered <- datasets$get_data("AQS", reactive = TRUE, filtered = TRUE)
    
    paramcd      <- input$paramcd
    arm_var      <- input$arm_var
    arm_label    <- input$arm_label
    ytype        <- input$ytype
    errbar       <- input$errbar
    ref_line_txt <- input$ref_line_txt
    
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
    
    # If refernece lines are requested
    # if (!is.null(ref_line_txt)) {
    #   ref_line <- as.numeric(unlist(strsplit(ref_line_txt, ",")))
    #   validate(need(all(!is.na(ref_line))), "Not all values entered for reference line(s) were numeric")
    # }
    
    
    ## Create change from baseline data to be used for plot function
    df <- chgfbl_data(data = ANL,
                      arm_var = arm_var)
    
    ## Produce plot
    chgfbl_plot(data = df,
                        ytype = ytype,
                        errbar = errbar
                        #arm_label = if (is.null(arm_label)) "TESTING",
                        #ref_line = if (is.null(ref_line)) NULL
                        )
    
    #if (is(plot, "try-error")) validate(need(FALSE, paste0("could not create change from baseline plot:\n\n", plot)))

    
  })
}
