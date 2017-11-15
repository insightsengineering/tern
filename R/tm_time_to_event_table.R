

#' Time To Event Table teal module
#' 
#' @export
#' 
#' 
#' @importFrom forcats fct_collapse fct_relevel
#' 
#' @examples  
#' 
#' \donotrun{
#' library(atezo.data)
#' library(dplyr)
#' library(survival)
#' 
#' 
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' x <- teal::init(
#'   data = list(ASL = ASL, ATE = ATE),
#'   modules = root_modules(
#'     tm_time_to_event_table(
#'        label = "Time To Event Table",
#'        time_points = 6,
#'        time_points_choices = c(6, 8),
#'        time_points_unit = "months",
#'        arm_var = "ARM",
#'        arm_var_choices = c("ARM", "ARMCD"),
#'        ref_arm = "DUMMY C",
#'        paramcd = "OS",
#'        paramcd_choices = unique(ATE$PARAMCD),
#'        strata_var = "SEX",
#'        strata_var_choices = c("SEX", "MLIVER", "TCICLVL2")
#'    )
#'   )
#' )   
#' shinyApp(x$ui, x$server) 
#' 
#'   
#' } 
tm_time_to_event_table <- function(label,
                                   time_points,
                                   time_points_choices = time_points,
                                   time_points_unit = "months",
                                   arm_var = "ARM",
                                   arm_var_choices = arm_var,
                                   ref_arm = NULL,
                                   strata_var = NULL,
                                   strata_var_choices = strata_var,
                                   paramcd = "OS",
                                   paramcd_choices = paramcd,
                                   pre_output = NULL, post_output = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_time_to_event_table,
    ui = ui_time_to_event_table,
    ui_args = args,
    server_args = list(ref_arm = ref_arm, time_points_unit = time_points_unit),
    filters = "ATE"
  )
}

ui_time_to_event_table <- function(id, label,
                                   time_points,
                                   time_points_choices,
                                   time_points_unit = "months",
                                   arm_var = "ARM",
                                   arm_var_choices = arm_var,
                                   ref_arm = NULL,
                                   strata_var = NULL,
                                   strata_var_choices = strata_var,
                                   paramcd = "OS",
                                   paramcd_choices = paramcd,
                                   pre_output = NULL, post_output = NULL) {
  ns <- NS(id)
  
  standard_layout(
    output = whiteSmallWell(uiOutput(ns("tte_table"))),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ATE")),
      optionalSelectInput(ns("paramcd"), "PARAMCD", paramcd_choices, paramcd, multiple = FALSE),
      helpText("PARAMCD selects the endpoint"),
      optionalSelectInput(ns("arm_var"), "ARM", arm_var_choices, arm_var, multiple = FALSE),
      selectInput(ns("ref_arm"), "Reference Group", choices = NULL, selected = NULL, multiple = TRUE),
      helpText("Reference groups automatically combined into a single group if more than one value selected."),
      selectInput(ns("comp_arm"), "Comparison Group", choices = NULL, selected = NULL, multiple = TRUE),
      checkboxInput(ns("combine_comp_arms"), "Combine all comparison groups?", value = FALSE),
      optionalSelectInput(ns("strata_var"), "Stratify by",
                          strata_var_choices, strata_var, multiple = TRUE,
                          label_help = helpText("crrently taken from ", tags$code("ATE"))),
      optionalSelectInput(ns("time_points"), "Time Points", time_points_choices, time_points, multiple = TRUE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
} 

srv_time_to_event_table <- function(input, output, session, datasets, ref_arm = NULL, time_points_unit = "months") {
  
  
  observe({
    arm_var <- input$arm_var
    
    ATE <- datasets$get_data("ATE", filtered = FALSE, reactive = "FALSE")
    
    arm <- ATE[[arm_var]]
    arms <- if (is.factor(arm)) levels(arm) else unique(arm)
     
    if (!is.null(arms)) {
      ref_arm <- if (!is.null(ref_arm) && all(ref_arm %in% arms)) {
        ref_arm
      } else {
        arms[1]
      }
      comp_arm <- setdiff(arms, ref_arm)
    } else {
      ref_arm <- NULL
      comp_arm <- NULL
    }
    updateSelectInput(session, "ref_arm", selected = ref_arm, choices = arms)
    updateSelectInput(session, "comp_arm", selected = comp_arm, choices = arms)
  })
  
  
  
  output$tte_table <- renderUI({

    # resolve all reactive expressions    
    ATE_filtered <- datasets$get_data("ATE", reactive = TRUE, filtered = TRUE)

    paramcd <- input$paramcd
    strata_var <- input$strata_var
    arm_var <- input$arm_var
    ref_arm <- input$ref_arm
    comp_arm <- input$comp_arm
    combine_comp_arms <- input$combine_comp_arms
    time_points <- input$time_points
    
    # teal:::as.global(ATE_filtered)
    # teal:::as.global(paramcd)
    # teal:::as.global(strata_var)
    # teal:::as.global(arm_var)
    # teal:::as.global(ref_arm)
    # teal:::as.global(comp_arm)
    # teal:::as.global(combine_comp_arms)
    # teal:::as.global(time_points)
    
    # then validate your input values
    validate(need(!is.null(ATE_filtered) && is.data.frame(ATE_filtered), "no data left"))
    validate(need(nrow(ATE_filtered) > 10 , "need more than 10 observations to calculate the table"))
    
    validate(need(ATE_filtered[[arm_var]], "no valid arm selected"))
    
    validate(need(!is.null(strata_var), "need strata variables"))
             
    validate(need(!is.null(ref_arm) && !is.null(comp_arm),
                  "need at least one reference and one comparison arm"))
    validate(need(length(intersect(ref_arm, comp_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    
    validate(need(paramcd %in% ATE_filtered$PARAMCD, "selected PARAMCD not in ATE"))
    validate(need(is.logical(combine_comp_arms), "need combine arm information"))
    
    validate(need(all(strata_var %in% names(ATE_filtered)),
                  "some baseline risk variables are not valid"))
    
    
    ## Now comes the static analysis code
    
    ## you need to add the encodings
    ATE_f <- ATE_filtered %>%
      filter(PARAMCD == paramcd, ARM %in% c(ref_arm, comp_arm))
    
    validate(need(nrow(ATE_f) > 15, "need at least 15 data points"))
    
    arm <- ATE_f[[arm_var]]
    
    if (length(ref_arm)>1) {
      new_ref_arm <- paste(ref_arm, collapse = "/")
      arm <- do.call(fct_collapse, setNames(list(arm, ref_arm), c("f", new_ref_arm)))
      ref_arm <- new_ref_arm
    }
    
    if (combine_comp_arms) {
      arm <- do.call(fct_collapse, setNames(list(arm, comp_arm), c("f", paste(comp_arm, collapse = "/"))))
    }
    
    arm <- fct_relevel(arm, ref_arm)
    
    if (!is.null(time_points)) {
      time_points <- setNames(as.numeric(time_points), paste(time_points, time_points_unit))
    }
    
    tbl <- try(time_to_event_table(
      time_to_event = ATE_f$AVAL,
      event = ATE_f$CNSR == 0,
      arm = arm,
      is_earliest_contr_event_death = ATE_f$EVNTDESC == "Death",
      strata_data = ATE_f[strata_var],
      time_points = time_points
    ))
    
    if (is(tbl, "try-error")) validate(need(FALSE, paste0("could not calculate time to event table:\n\n", tbl)))
    
    as_html(tbl)
  })
}
