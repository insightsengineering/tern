
#' alternative tablist
#'
#' @param time_to_event Response data
#' @param event is boolean, \code{TRUE} if event, \code{FALSE} if time_to_event
#'   is censored
#' @param group_by data frame with one column per grouping
#' @param arm vector with arm information
#' @param arm.ref a character vector defining which arms in arm should be taken 
#'   as the reference
#' @param arm.comp a character vector defining which arms in arm should be taken 
#'   as the comparison
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr)
#' '%needs%' <- teal.oncology:::'%needs%'
#' ARS <- ars(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' ARS_f <- ARS %>% filter(PARAMCD == "BESRSPI")
#' 
#' ASL$BAGED <- ifelse(ASL$BAGE <= median(ASL$BAGE), "<=median", ">median")
#' 
#' group_by <- merge(
#'  ARS_f[c("USUBJID", "STUDYID")],
#'  ASL[c("USUBJID", "STUDYID", "BAGED", "SEX", "BECOG")],
#'  all.x = TRUE, all.y = FALSE
#' )
#' 
#' head(group_by)
#' 
#' glm_subgroup(
#'    response = ARS_f$AVAL,
#'    event = ARS_f$AVALC %in% c("CR","PR"),
#'    arm = ARS_f$ARM, 
#'    group_by = group_by[, -c(1,2), drop=FALSE],
#'    arm.ref = "DUMMY A",
#'    arm.comp = "DUMMY B"
#' )
#' }
#' 
#'   
glm_subgroup <- function(response, event,
                         arm, arm.ref, arm.comp = setdiff(arm, arm.ref),
                         group_by, covariARSs = NULL) {
  
  # argument checking
  n <- length(response)
  if (length(event) != n) stop("event has wrong length")
  if (length(arm) != n) stop("arm has wrong length")
  if (!is.data.frame(group_by)) stop("group_by is expected to be a data.frame")
  if (nrow(group_by) != n) stop("group_by has wrong number of rows")
  
  
  arm_for_model <- combine_arm(arm, arm.ref, arm.comp)
  
  glm_data <- subset(
    data.frame(
      response,
      event,
      arm = arm_for_model
    ), !is.na(arm_for_model)
  )
  
  # split data into a tree for data
  # where each leaf is a data.frame with 
  # the data to compute the survival analysis with
  data_list <- c(
    list(ALL = list(ALL = glm_data)),
    lapply(group_by, function(var) {
      lapply(setNames(unique(var[var != ""]), unique(var[var != ""])), function(value) {
        glm_data[var == value , , drop= FALSE] 
      })
    })
  )
  
  # apply the survival analysis
  results_glm <- lapply(data_list, function(varname) {
    lapply(varname, function(data_for_value) {
      glm_results(data_for_value)
    })
  })
  
  # reduce results into a table
  results_glm2 <- unlist(results_glm, recursive = FALSE)
  X <- Reduce(rbind, results_glm2)
  row.names(X) <-names(results_glm2)
  
  structure(X, class= c("forest_response", "forest_table"))
}


#' @export
plot.forest_response <- function(x, ...) {
  
  grid.newpage()
  
  grid.text("Plot of a forst response table")
  
  
}


#' survival_results(data_for_value)
glm_results <- function(data){
  
  #Response Rate
  resp_ref_n <- table(data$arm)[1]
  resp_comp_n <- table(data$arm)[2]
  resp_ref_event <- table(data$event,data$arm)[2,1]
  resp_comp_event <- table(data$event,data$arm)[2,2]
  
  #Logistic Model
  glm_sum  <- summary(glm(event ~ arm, family=binomial(link='logit'), data = data))
  glm_or   <- exp(glm_sum$coefficient[2,1])
  glm_lcl  <- exp(glm_sum$coefficient[2,1]-1.96*sqrt(glm_sum$coefficient[2,2]))
  glm_ucl  <- exp(glm_sum$coefficient[2,1]+1.96*sqrt(glm_sum$coefficient[2,2]))
  glm_pval <- glm_sum$coefficients[2,4]
  
  resp_table <- data.frame(resp_ref_n, resp_comp_n, 
                         resp_ref_event, resp_comp_event, 
                         glm_or, glm_lcl, glm_ucl, glm_pval)
}

#' Forest Plot Numbers for Survival data with ADAM data structure
#' 
#' @export
#' 
#' @inheritParams glm_subgroup
#' @param ASL asl data frame
#' @param ARS data frame
#' 
#' @importFrom dplyr %>% filter
#' 
#' @examples 
#' \dontrun{
#' 
#' rm(list = ls())
#' library(ARSzo.data)
#' library(dplyr)
#' 
#' ARS <- ARS(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' ASL$BAGED <- ifelse(ASL$BAGE <= median(ASL$BAGE), "<=median", ">median")
#' 
#' glm_subgroup_ADAM(
#'   ASL, ARS,
#'   groupvar = c("SEX", "BECOG", "COUNTRY"),
#'   arm.ref = "DUMMY A", arm.comp = "DUMMY B"
#' )
#'   
#' }
glm_subgroup_ADAM <- function(ASL, ARS,
                               outcome = "Overall Survival",
                               groupvar,
                               arm.ref,
                               arm.comp,
                               arm.var = "ARM",
                               time_to_event.var = "AVAL",
                               event.var = "CNSR", negARS.event.var = TRUE) {
  
  ARS %needs% c("USUBJID", "STUDYID", "PARAM", time_to_event.var, event.var)
  ASL %needs% c("USUBJID", "STUDYID", groupvar, arm.var)
  
  event <- ARS[[event.var]]
  if (!(is.numeric(event) || is.logical(event))) stop("event var needs to be numeric or boolean")  
  
  ARS_f <- ARS %>% filter(PARAM == outcome)
  
  if (nrow(ARS_f) <= 0) stop("ARS data left after filtering")
  
  group_by <- merge(
    ARS_f[c("USUBJID", "STUDYID")],
    ASL[c("USUBJID", "STUDYID", groupvar)],
    all.x = TRUE, all.y = FALSE
  )
  
  glm_subgroup(
    time_to_event = ARS_f[[time_to_event.var]],
    event = if(negARS.event.var) !ARS_f[[event.var]] else ARS_f[[event.var]],
    arm = ARS_f[[arm.var]], 
    group_by = group_by[, -c(1,2), drop=FALSE],
    arm.ref = arm.ref,
    arm.comp = arm.comp
  )
}
