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
#' ARS_f <- ARS %>% filter(PARAMCD == "BESRSPI") %>% filter(ITTWTFL == "Y")
#' ASL_f <- ASL %>% filter(ITTWTFL == "Y")
#' 
#' ASL_f$BAGED <- ifelse(ASL_f$BAGE <= median(ASL_f$BAGE), "<=median", ">median")
#' 
#' group_by <- merge(
#'  ARS_f[c("USUBJID", "STUDYID")],
#'  ASL_f[c("USUBJID", "STUDYID", "ICLEVEL", "ICLVL1", "TC3IC3")],
#'  all.y = TRUE, all.x = FALSE
#' )
#' 
#' head(group_by)
#' 
#' tbl <- glm_subgroup(
#'           response = ARS_f$AVAL,
#'           event = ARS_f$AVALC %in% c("CR","PR"),
#'           arm = ARS_f$ARM, 
#'           group_by = group_by[, -c(1,2), drop=FALSE],
#'           arm.ref = "DUMMY A",
#'           arm.comp = "DUMMY C"
#' )
#' Viewer(tbl)
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
  
  #head(glm_data)
  
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
  
  #varname=data_list$TC3IC3
  #data_for_value = varname[1]
  # apply the glm analysis
  results_glm <- lapply(data_list, function(varname) {
    lapply(varname, function(data_for_value) {
      glm_results(data_for_value)
    })
  })
  
  # reduce results into a table
  results_glm2 <- unlist(results_glm, recursive = FALSE)
  X <- Reduce(rbind, results_glm2)
  row.names(X) <-names(results_glm2)
  
  
  # X <- results_glm2
  
  additonal_args <- list(
    col.names = c("Total n",
                  "n", "n Responder", "Responder Rate (%)",
                  "n", "n Responder", "Responder Rate (%)",
                  "Odds Ratio", "95% CI", "p value"),
    format = "xx"
  )
  
  # rname <- rownames(X)[1]
  # x <- split(X, 1:nrow(X))[[1]]
  last_header <- "ALL"
  rrow_collection <- Filter(
    function(x)!is.null(x),
    unlist(
      Map(function(x, rname) {
        
        i <- regexpr(".", rname, fixed = TRUE)
        header_row_name <- c(substr(rname, 1, i-1), substring(rname, i+1))
        
        is_new_category <- header_row_name[1] != last_header
        last_header <<- header_row_name[1]
        
        list(
          if (is_new_category) rrow() else NULL,
          if (is_new_category) rrow(last_header) else NULL,
          rrow(
            row.name = header_row_name[2],
            x$resp_ref_n + x$resp_comp_n, # total n
            x$resp_ref_n,
            x$resp_ref_event,
            rcell(x$resp_ref_event / (x$resp_ref_n + x$resp_ref_event), format = "xx.x"),
            x$resp_comp_n,
            x$resp_comp_event,
            rcell(x$resp_comp_event / (x$resp_comp_n + x$resp_comp_event), format = "xx.x"),
            rcell(x$glm_or, format = "xx.xx"),
            rcell(c(x$glm_lcl, x$glm_ucl), format = "(xx.xx, xx.xx)"),
            rcell(x$glm_pval, format = "xx.xx"),
            indent = if (header_row_name[1] == "ALL") 0 else 1
          )
        )
      }, split(X, 1:nrow(X)), rownames(X)),
      recursive = FALSE)
  )
  
  
  tbl <- do.call(rtable, c(additonal_args, rrow_collection))
  
  tbl
 # Viewer(tbl)
}


#' @export
plot.forest_response <- function(x, ...) {
  
  grid.newpage()
  
  grid.text("Plot of a forst response table")
  
  
}


#' glm_results(data_for_value)
#' data = data_for_value 
glm_results <- function(data){
  
  #Response Rate
  
  tbl_arm <- table(data$arm)
  resp_ref_n <- tbl_arm[names(tbl_arm)==levels(data$arm)[1]]
  resp_comp_n <- tbl_arm[names(tbl_arm)==levels(data$arm)[2]]
  if (is.na(resp_ref_n)) resp_ref_n = 0
  if (is.na(resp_comp_n)) resp_comp_n = 0
  
  tbl_freq <- table(data$event,data$arm)
  resp_ref_event <- tbl_freq[rownames(tbl_freq)=="TRUE",colnames(tbl_freq)==levels(data$arm)[1]]
  resp_comp_event <- tbl_freq[rownames(tbl_freq)=="TRUE",colnames(tbl_freq)==levels(data$arm)[2]]
  if (length(resp_ref_event)==0) resp_ref_event = 0
  if (length(resp_comp_event)==0) resp_comp_event = 0
  
  #Logistic Model
  if (length(levels(as.factor(as.character(data$arm)))) == 2){
  glm_sum  <- summary(glm(event ~ arm, family=binomial(link='logit'), data = data))
  glm_or   <- exp(glm_sum$coefficient[2,1])
  glm_lcl  <- exp(glm_sum$coefficient[2,1]-1.96*sqrt(glm_sum$coefficient[2,2]))
  glm_ucl  <- exp(glm_sum$coefficient[2,1]+1.96*sqrt(glm_sum$coefficient[2,2]))
  glm_pval <- glm_sum$coefficients[2,4]
  
  resp_table <- data.frame(resp_ref_n, resp_comp_n, 
                           resp_ref_event, resp_comp_event, 
                           glm_or, glm_lcl, glm_ucl, glm_pval)
  }else {
  resp_table <- data.frame(resp_ref_n, resp_comp_n, 
                             resp_ref_event, resp_comp_event, 
                             glm_or = NA, glm_lcl = NA, glm_ucl = NA, glm_pval = NA)
  }
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