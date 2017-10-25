
#' Forest Response Plot teal module
#' 
#' @export
#' 
#' @examples  
#' 
#' \donotrun{
#' library(atezo.data)
#' library(dplyr)
#' library(survival)
#' 
#' ARS <- ars(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' arms <- unique(ASL$ARM)
#' x <- teal::init(
#'   data = list(ASL = ASL, ARS = ARS),
#'   modules = root_modules(
#'     tm_variable_browser(),
#'     tm_data_table(),
#'     tm_forest_response(
#'        label = "Forest Response",
#'        paramcd = "OVRSPI",
#'        paramcd_choices = unique(ARS$PARAMCD),
#'        response = "CR",
#'        response_choices = unique(ARS$AVALC),
#'        subgroup_var = c("BAGED", "SEX", "BECOG"),
#'        arm.ref = arms[1],
#'        arm.ref_choices = arms,
#'        arm.comp = arms[-1],
#'        arm.comp_choices = arms
#'    )
#'   )
#' )   
#' shinyApp(x$ui, x$server) 
#' 
#'   
#' } 
tm_forest_response <- function(label,
                               paramcd,
                               paramcd_choices = paramcd,                               
                               arm.ref,
                               arm.ref_choices = arm.ref,
                               arm.comp,
                               arm.comp_choices = arm.comp,
                               subgroup_var,
                               subgroup_var_choices = subgroup_var,
                               response,
                               response_choices = response,
                               plot_height = c(600, 200, 2000),
                               pre_output = NULL, post_output = NULL) {
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_forest_response,
    ui = ui_forest_response,
    ui_args = args,
    filters = "ARS"
  )
}

ui_forest_response <- function(id, label,
                               paramcd,
                               paramcd_choices = paramcd,
                               arm.ref,
                               arm.ref_choices = arm.ref,
                               arm.comp,
                               arm.comp_choices = arm.comp,
                               subgroup_var,
                               subgroup_var_choices = subgroup_var,
                               response = "OS",
                               response_choices = "OS",
                               plot_height,
                               pre_output,
                               post_output) {
  ns <- NS(id)
  
  standard_layout(
    output = uiOutput(ns("forest_plot")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ARS")),
      optionalSelectInput(ns("paramcd"), "PARAMCD", paramcd_choices, paramcd, multiple = FALSE),
      optionalSelectInput(ns("response"), "Response", response_choices, response, multiple = TRUE),
      optionalSelectInput(ns("ref_arm"), "Reference Group", arm.ref_choices, arm.ref, multiple = TRUE),
      optionalSelectInput(ns("treat_arm"), "Treatment Group", arm.comp_choices, arm.comp, multiple = TRUE),
      optionalSelectInput(ns("subgroup_var"), "Subgroup Variables", subgroup_var_choices, subgroup_var, multiple = TRUE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
} 

srv_forest_response <- function(input, output, session, datasets) {
  
  output$forest_plot <- renderUI({
    
    ARS_filtered <- datasets$get_data("ARS", reactive = TRUE, filtered = TRUE)
    ASL_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
    
    validate(need(!is.null(ARS_filtered) && is.data.frame(ARS_filtered), "no data left"))
    validate(need(nrow(ARS_filtered) > 0 , "no observations left"))
    
    
    paramcd <- input$paramcd
    response <- input$response
    subgroup_var <- input$subgroup_var
    ref_arm <- input$ref_arm
    treat_arm <- input$treat_arm
    
    teal:::as.global(subgroup_var)
    
    validate(need(!is.null(treat_arm) && !is.null(ref_arm),
                  "need at least one treatment and one reference arm"))
    validate(need(length(intersect(ref_arm, treat_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    
    validate(need(!is.null(response) && all(response %in% ARS_filtered$AVALC),
                  "response AVALC does not exist"))
    
    validate(need(!is.null(paramcd) && paramcd %in% ARS_filtered$PARAMCD,
                  "PARAMCD does not exist"))
    
    
    ## you need to add the encodings
    ARS_f <- ARS_filtered %>% filter(PARAMCD == paramcd) %>%
      select(c("STUDYID", "USUBJID", "PARAMCD", "AVAL", "AVALC"))
    
    validate(need(nrow(ARS_f) > 0, "no data left"))
    
    ASL_filtered$BAGED <- ifelse(ASL_filtered$BAGE <= median(ASL_filtered$BAGE), "<=median", ">median")
    
    validate(need(all(subgroup_var %in% names(ASL_filtered)),
                  "some baseline risk variables are not valid"))
    
    ASL_f <- ASL_filtered %>% 
      filter(ARM %in% c(ref_arm, treat_arm)) %>% 
      select("STUDYID", "USUBJID", "ARM", subgroup_var) 
    
    validate(need(all(subgroup_var %in% names(ASL_f)),
                  "some subgroup variables are not valid"))
    ARS_f <- inner_join(ASL_f, ARS_f, by = c("STUDYID","USUBJID"))
    
    validate(need(all(c(ref_arm, treat_arm) %in% ARS_f$ARM), "data needs to include at least one patient from the reference and comparison arm"))  
    
    group_by <- ARS_f %>% select(c("USUBJID", "STUDYID", subgroup_var))
    head(group_by)
    ## add
    ## the arm combine & filtering and converting to a factor here...paste0(ref_arm, collapse = "/")
    ## using forcats
    arm <- fct_collapse(ARS_f$ARM, ref_arm = ref_arm, treat_arm = treat_arm)
    arm <- ifelse (arm == "ref_arm", paste0(ref_arm, collapse = "/"), paste0(treat_arm, collapse = "/")) 
    arm <- fct_relevel(arm, paste0(ref_arm, collapse = "/"))
    
    tbl <- forest_rsp(
      response = ARS_f$AVAL,
      event = ARS_f$AVALC %in% response,
      arm = arm, 
      group_by = group_by[, -c(1,2), drop=FALSE]
    )
    
    as_html(tbl)
  })
}
