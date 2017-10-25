
#' Forest Survival Plot teal module
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
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' options(teal_logging = FALSE)
#' 
#' arms <- unique(ASL$ARM)
#' x <- teal::init(
#'   data = list(ASL = ASL, ATE = ATE),
#'   modules = root_modules(
#'     tm_forest_survival(
#'        label = "Forest Survival",
#'        endpoint = "OS",
#'        endpoint_choices = unique(ATE$PARAMCD),
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
tm_forest_survival <- function(label,
                               arm.ref,
                               arm.ref_choices = arm.ref,
                               arm.comp,
                               arm.comp_choices = arm.comp,
                               subgroup_var,
                               subgroup_var_choices = subgroup_var,
                               endpoint = "OS",
                               endpoint_choices = "OS",
                               plot_height = c(600, 200, 2000),
                               pre_output = NULL, post_output = NULL) {
  
  # arm_var
  # arm_var_choices
  #  (choose arm variable: ARM, ARMCD, ACTARM)
  
  # subgroup_var
  # subgroup_var_choices
  
  args <- as.list(environment())
  
  module(
    label = label,
    server = srv_forest_survival,
    ui = ui_forest_survival,
    ui_args = args,
    filters = "ATE"
  )
}

ui_forest_survival <- function(id, label,
                               arm.ref,
                               arm.ref_choices = arm.ref,
                               arm.comp,
                               arm.comp_choices = arm.comp,
                               subgroup_var,
                               subgroup_var_choices = subgroup_var,
                               endpoint = "OS",
                               endpoint_choices = "OS",
                               plot_height,
                               pre_output,
                               post_output) {
  ns <- NS(id)
  
  
  ## use helpText to explain your user interface
  
  standard_layout(
    output = plotOutput(ns("forest_plot")),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code("ATE")),
      optionalSelectInput(ns("endpoint"), "Time to Event", endpoint_choices, endpoint, multiple = FALSE),
      optionalSelectInput(ns("ref_arm"), "Reference Group", arm.ref_choices, arm.ref, multiple = TRUE),
      optionalSelectInput(ns("treat_arm"), "Treatment Group", arm.comp_choices, arm.comp, multiple = TRUE),
      optionalSelectInput(ns("subgroup_var"), "Subgroup Variables", subgroup_var_choices, subgroup_var, multiple = TRUE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
} 

srv_forest_survival <- function(input, output, session, datasets) {
  
  
  
  output$forest_plot <- renderPlot({
    
    ## 1: Assign your reactive values to variables
    ATE_filtered <- datasets$get_data("ATE", reactive = TRUE, filtered = TRUE)
    ASL_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)
  
    endpoint <- input$endpoint
    subgroup_var <- input$subgroup_var
    ref_arm <- input$ref_arm
    treat_arm <- input$treat_arm
    
    
    teal:::as.global(ATE_filtered)
    teal:::as.global(ASL_filtered)
    teal:::as.global(endpoint)
    teal:::as.global(subgroup_var)
    teal:::as.global(ref_arm)
    teal:::as.global(treat_arm)
    
    ## 2: Validate if your inputs can produce the requested output
    
    validate(need(!is.null(ATE_filtered) && is.data.frame(ATE_filtered), "no data left"))
    validate(need(nrow(ATE_filtered) > 10 , "need more than 10 patients to do the plot"))
    
    validate(need(!is.null(treat_arm) && !is.null(ref_arm),
                  "need at least one treatment and one reference arm"))
    validate(need(length(intersect(ref_arm, treat_arm)) == 0,
                  "reference and treatment group cannot overlap"))
    
    validate(need(endpoint %in% ATE_filtered$PARAMCD, "time to event PARAMCD does not exist"))
    
    
    print(" ---------- passing by -------------")
    
    ## 3: Do your static code (this should also run in rmarkdown)
    
    ## you need to add the encodings
    ATE_f <- ATE_filtered %>% 
      filter(PARAMCD == endpoint) %>%
      select(c("STUDYID", "USUBJID", "PARAMCD", "AVAL", "CNSR"))
    
    validate(need(nrow(ATE_f) > 0, "no data left"))
    
    
    ASL_filtered$BAGED <- ifelse(ASL_filtered$BAGE <= median(ASL_filtered$BAGE), "<=median", ">median")
    
    
    ASL_f <- ASL_filtered %>% 
      filter(ARM %in% c(ref_arm, treat_arm)) %>% 
      select("STUDYID", "USUBJID", "ARM", subgroup_var) 
    
    validate(need(all(subgroup_var %in% names(ASL_f)),
                  "some subgroup variables are not valid"))
    ATE_f <- inner_join(ASL_f, ATE_f, by = c("STUDYID","USUBJID"))
      
    validate(need(all(c(ref_arm, treat_arm) %in% ATE_f$ARM), "data needs to include at least one patient from the reference and comparison arm"))  
    
    group_by <- ATE_f %>% select(c("USUBJID", "STUDYID", subgroup_var))
    head(group_by)

    # group_by <- merge(
    #   ATE_f[c("USUBJID", "STUDYID")],
    #   ASL_filtered[c("USUBJID", "STUDYID", subgroup_var)],
    #   all.y = FALSE, all.x = TRUE
    # )
    
    #head(group_by)
  
    ## add
    ## the arm combine & filtering and converting to a factor here...paste0(ref_arm, collapse = "/")
    ## using forcats
    arm <- fct_collapse(ATE_f$ARM, ref_arm = ref_arm, treat_arm = treat_arm)
    arm <- ifelse (arm == "ref_arm", paste0(ref_arm, collapse = "/"), paste0(treat_arm, collapse = "/")) 
    arm <- fct_relevel(arm, paste0(ref_arm, collapse = "/"))
    
    tbl <- forest_tte(
      time_to_event = ATE_f$AVAL,
      event = ATE_f$CNSR == 0,
      arm = arm, 
      group_by = group_by[, -c(1,2), drop=FALSE]
    )
    
    #as_html(tbl)
    
    forest_tte_plot(tbl)
  })
}
