
#' Create the Response Table
#' 
#' @param response Tumor response data
#' @param value.rsp Character vector, defining list of response values to be used as responders
#' @param value.nrsp Character vector, defining list of response values to be used as non-responders
#' @param arm Arm information data
#' @param arm.ref Character vector, defining which arm(s) from the list of arms should be used as reference group
#' @param arm.comp Character vector, defining which arm(s) from the list of arms should be used as comparison group
#' @param arm.comp.combine Boolean value, \code{TRUE} if want all non-ref arms to be combined into one comparison group, 
#'                       \code{FALSE} if want each of the non-ref arms to be a separate comparison group
#' @param incl_missing Boolean value, \code{TRUE} if missing values should be considered non-responders, 
#'                     \code{FALSE} if missing response should be removed from analysis
#'                     
#' @export
#' 
#' @author Chendi Liao (liaoc10), \email{chendi.liao@roche.com}
#' 
#' @examples 
#' 
#' 
#' \dontrun{
#' library(dplyr)
#' library(shiny)
#' library(atezo.data)
#' library(teal.oncology)
#' library(PropCIs)
#' 
#' '%needs%' <- teal.oncology:::'%needs%'
#' 
#' ARS <- ars(com.roche.cdt30019.go29436.re) %>% select(c("USUBJID", "STUDYID", "ARM", "PARAMCD", "AVALC")) %>% 
#'        filter(PARAMCD == "BESRSPI")
#' 
#' #Test params
#' value.resp   = c("CR","PR")
#' value.nresp  = c("SD", "NON CR/PD", "PD", "NE")
#' arm.ref      = "DUMMY B"
#' arm.comp     = c("DUMMY A", "DUMMY C")
#' arm.comp.combine = F
#' incl_missing = T
#' 
#' if (incl_missing) {
#'   ARS$AVALC[ARS$AVALC==""] <- "NE"
#' }
#' 
#' ARS_f <- ARS %>% filter(ARM %in% c(arm.ref, arm.comp) & AVALC %in% c(value.resp, value.nresp))
#' 
#' response = ARS_f$AVALC
#' arm      = ARS_f$ARM
#' 
#' response_table(response, arm, arm.ref, arm.comp, arm.comp.combine=F, value.resp, value.nresp)
#' 
#'  
#' }
#' 
#' 
response_table <- function(response, 
                           arm, 
                           arm.ref, 
                           arm.comp         = setdiff(arm, arm.ref),
                           arm.comp.combine = T,
                           value.resp,
                           value.nresp,
                           style = 1
                           ) {
  
  #Full list of response values to test for
  value_test <- c(value.resp, value.nresp)
  
  #Recode arm variable to reference and comparison group(s)
  ARM <- combine_arm(arm, arm.ref, arm.comp, arm.comp.combine)
  
  # Split response data into tree by ARM
  # Rate calculation for Responder/Non-responders
  rate_result <- Map(calc_rate, split(response, ARM), list(c(value.resp)))
  
  #Rate calculation by each response value
  data_byvalue <- data.frame(rep(list(response),6))
  names(data_byvalue) <- value_test
  
  rate_result_byvalue <- lapply(split(data_byvalue, ARM), function(x){
    Map(calc_rate, x, value_test)
  })


  #Diff in rate and OR calculation for Responder/Non-responders
  diffor_result <- lapply(rate_result, function(x) {
    calc_diffor(x, rate_result[[1]])
  })


  ##########################################
  # Build output data structure with rtable#
  ##########################################
  #--- Header Section ---#
  out_header <-  list(
    col.names = paste(names(rate_result), 
                      paste0("(N=", unlist(lapply(rate_result, '[[', "N")),")"), 
                      sep="\n"),
    format = "xx (xx.x%)",
    rrow()
  )
  
  #--- Responder/Non-responder section - counts and 95% CI ---#
  out_resp_rate  <- list(
    do.call(rrow, c("Responders",     lapply(rate_result, print_np))),
    do.call(rrow, c("Non-Responders", lapply(rate_result, function(x) rcell(c(x$n_nresp, x$p_nresp))))),
    rrow(),
    do.call(rrow, c("95% CI for Response Rates (Clopper-Pearson)", lapply(rate_result, print_ci, "ci"))),
    rrow()
  )
  
  #--- Responder/Non-responder section - diff in rates and OR ---#
  #Depending on number of comparison arms, display centered (if only 1 comp arm) or aligned in column (if more than 1 comp arm)
  if (arm.comp.combine || length(arm.comp) < 2) {
    out_resp_diffor <- list(
      rrow("Difference in Response Rates", rcell(diffor_result[[2]]$diff,   format = "xx.xx", colspan = 2)),
      rrow("95% CI (Wald)", indent = 1, rcell(diffor_result[[2]]$diffci, format = "(xx.xx, xx.xx)", colspan = 2)),
      rrow("p-value (Cochran-Mantel-Haenszel)", rcell(diffor_result[[2]]$diffp, format = "xx.xxx", colspan = 2)),
      rrow(),
      rrow("Odds Ratio", rcell(diffor_result[[2]]$or, format = "xx.xx", colspan = 2)),
      rrow("95% CI", indent = 1, rcell(diffor_result[[2]]$orci, format = "(xx.xx, xx.xx)", colspan = 2)),
      rrow()
    )
  } else {
    diffor_result[[1]] <- rapply(diffor_result[[1]],function(x) if (length(x) > 1) {rep(NULL,2)} else {NULL}, how = "list")
    out_resp_diffor <- list(
      do.call(rrow, c("Difference in Response Rates", lapply(diffor_result, print_diffor, "diff"))),
      do.call(rrow, c("95% CI (Wald)", indent = 1,    lapply(diffor_result, print_ci, "diffci"))),
      do.call(rrow, c("p-value (Cochran-Mantel-Haenszel)", lapply(diffor_result, function (x) rcell(x$diffp, format = "xx.xxx")))),
      rrow(),
      do.call(rrow, c("Odds Ratio",                   lapply(diffor_result, print_diffor, "or"))),
      do.call(rrow, c("95% CI", indent = 1,           lapply(diffor_result, print_ci, "orci"))),
      rrow()
    )
  }
  

  #--- Section for each response value - counts and 95% CI ---#
  rsp_full_label <- list(CR          = "Complete Response (CR)",
                         PR          = "Partial Response (PR)",
                         SD          = "Stable Disease (SD)",
                         `NON CR/PD` = "Non-CR or Non-PD (NON CR/PD)",
                         PD          = "Progressive Disease (PD)",
                         NE          = "Missing or unevaluable"
                         )
  
  out_byvalue_rate <- unlist(
    lapply(as.list(1:length(value_test)), function(i) {
      
      #Extract response value short name
      label <- sapply(rate_result_byvalue, names)[i]
      
      list(
        do.call(rrow, c(unname(rsp_full_label[label]),
                        lapply(sapply(rate_result_byvalue, function(x) x[i]), print_np))),
        #If response value is NE, do not display 95% CI
        if (label != "NE") {
          do.call(rrow, c("95% CI",
                          indent = 1,
                          lapply(sapply(rate_result_byvalue, function(x) x[i]), print_ci, "ci")))
        } else {rrow()} ,
        rrow()
      )
    }), recursive = F)
  
  
  #Put all sections together to display table
  tbl <- do.call(rtable,c(out_header,
                          out_resp_rate,
                          out_resp_diffor,
                          out_byvalue_rate
                         )
    
  )
  
  return(tbl)
  Viewer(tbl)
  
}

#########################################################################
# Helper functions                                                      #
#########################################################################
#' Function to calculate counts and percentage for rates
#' 
#' @param x vector with response information
#' @param value name of the response value to compute statistics for
#' 
#' @export
#' 
#' @author Chendi Liao (liaoc10), \email{chendi.liao@roche.com}
#' 
#' @examples 
#' calc_rate(LETTERS[sample(1:3, 10, replace=T)], value = "A")
#' 
calc_rate <- function(x, value) {
  N       <- length(x)
  n_resp  <- sum(x %in% value)
  ratestats    <- binom.test(n_resp, N)
  
  list(
    N       = N,
    n_resp  = n_resp,
    n_nresp = N-n_resp,
    p_resp  = n_resp/N,
    p_nresp = (N-n_resp)/N,
    ci      = ratestats$conf.int*100
  )
}

#' Calculate difference in rates and odds ratio between two lists produced by calc_rate function
#' 
#' @param comp list produced by calc_rate function for comparison arm
#' @param ref list produced by calc_rate function for reference arm
#' 
#' @export
#' 
#' @author Chendi Liao (liaoc10), \email{chendi.liao@roche.com}
#' 
#' @examples 
#' a <- LETTERS[sample(1:3, 20, replace=T)]
#' b <- LETTERS[sample(1:3, 20, replace=T)]
#' a_rate <- calc_rate(a, value = "A")
#' b_rate <- calc_rate(b, value = "A")
#' calc_diffor(b_rate, a_rate)
#' 
calc_diffor <- function(comp,ref) {
  diffstats <- prop.test(matrix(c(comp$n_resp, ref$n_resp, comp$n_nresp, ref$n_nresp), ncol=2), correct=F)
  orstats   <- (comp$n_resp * ref$n_nresp)/(ref$n_resp * comp$n_nresp)
  orcistats <- orscoreci(comp$n_resp, comp$N, ref$n_resp,ref$N, conf.level=0.95)
  
  list(
    diff   = (diffstats$estimate[1] - diffstats$estimate[2])*100,
    diffci = diffstats$conf.int*100,
    diffp  = diffstats$p.value,
    or     = orstats,
    orci   = orcistats$conf.int
  )
}

#' Helper functions to format rcells for various display
#' 
#' @param x list produced by calc_rate/calc_diffor function
#' @param y name of element to display 
#' 
#' @export
#' 
#' @author Chendi Liao (liaoc10), \email{chendi.liao@roche.com}
#' 
#' @examples 
#' 
#' a_rate <- calc_rate(LETTERS[sample(1:3, 20, replace=T)], value = "A")
#' b_rate <- calc_rate(LETTERS[sample(1:3, 20, replace=T)], value = "A")
#' diffor <- calc_diffor(b_rate, a_rate)
#' 
#' print_np(a_rate)
#' print_ci(a_rate, "ci")
#' print_ci(diffor, "diffci")
#' print_diffor(diffor, "diff")
#' print_diffor(diffor, "or")
#' 
print_np <- function(x) rcell(c(x$n_resp, x$p_resp))
print_ci <- function(x,y) rcell(x[[y]], format = "(xx.xx, xx.xx)")
print_diffor <- function(x,y) rcell(x[[y]], format = "xx.xx")


##################################################################################
##################################################################################

#' Response Table with ADaM data structure 
#' 
#' @param ASL dataset with following variables: USUBJID, STUDYID, the specified grouping variable
#' @param ARS ARS dataset containing the following variables: USUBJID, STUDYID, PARAMCD, AVALC
#' @param paramcd Name of overall response parameter
#' @param arm.var Name of variable with arm information
#' @param arm.ref Character vector, defining which arm(s) from the list of arms should be used as reference group
#' @param arm.comp Character vector, defining which arm(s) from the list of arms should be used as comparison group
#' @param arm.comp.combine Boolean value, \code{TRUE} if want all non-ref arms to be combined into one comparison group, 
#'                       \code{FALSE} if want each of the non-ref arms to be a separate comparison group
#' @param value.rsp Character vector, defining list of response values to be used as responders
#' @param value.nrsp Character vector, defining list of response values to be used as non-responders
#' @param incl_missing Boolean value, \code{TRUE} if missing values should be considered non-responders, 
#'                     \code{FALSE} if missing response should be removed from analysis
#' @param style Table display style, \code{1} , \code{2}
#' 
#' \dontrun{
#' library(dplyr)
#' library(shiny)
#' library(atezo.data)
#' library(teal.oncology)
#' library(PropCIs)
#' 
#' '%needs%' <- teal.oncology:::'%needs%'
#' 
#' ARS <- ars(com.roche.cdt30019.go29436.re) %>% select(c("USUBJID", "STUDYID", "ARM", "PARAMCD", "AVALC")) %>% 
#'        filter(PARAMCD == "BESRSPI")
#' 
#' ARS$AVALC[ARS$AVALC==""] <- "NE"
#' 
#' #Test params
#' value.resp   = c("CR","PR")
#' value.nresp  = c("SD", "NON CR/PD", "PD", "NE")
#' arm.ref      = "DUMMY A"
#' arm.comp     = c("DUMMY B", "DUMMY C")
#' arm.comp.combine = T
#' 
#' ARS_f <- ARS %>% filter(ARM %in% c(arm.ref, arm.comp) & AVALC %in% c(value.resp, value.nresp))
#' 
#' #Test params
#' response = ARS_f$AVALC
#' arm      = ARS_f$ARM
#'  
#' }


response_table_ADAM <- function(ASL, ARS,
                                paramcd          = "BESRSPI",
                                arm.var          = "ARM",
                                arm.ref,
                                arm.comp,
                                arm.comp.combine = TRUE,
                                value.resp       = c("CR","PR"),
                                value.nresp      = c("SD", "NON CR/PD", "PD", "NE"),
                                incl.missing     = TRUE,
                                style            = 1
                                ) {
  
  #Check all required variables are present in ADaM
  ASL %needs% c("USUBJID", "STUDYID", arm.var)
  ARS %needs% c("USUBJID", "STUDYID", "PARAMCD", "AVALC")
  
  #Select obs needed to analysis, merge ASL/ARS to create analysis dataset
  #Filter on selected PARAMCD, and ARM in arm.ref/comp
  ASL_f <- ASL %>% select(c("USUBJID", "STUDYID", arm.var))            %>% filter(get(arm_var) %in% c(arm.ref,arm.comp))
  ARS_f <- ARS %>% select(c("USUBJID", "STUDYID", "PARAMCD", "AVALC")) %>% filter(PARAMCD == paramcd)
  ANL <- merge(ASL_f, ARS_f, by=c("USUBJID", "STUDYID"))
  
  #If want to include missing as non-responders, check NE is in value.nrsp and recode data
  if (isTRUE(incl.missing)) {
    if (!"NE" %in% value.nresp) {
      stop("In order to include missing as non-responder, value.nrsp must contain NE")
    } else {
      ANL$AVALC[ANL$AVALC==""] <- "NE"
    }
  }

  #Filter on AVALC in value.rsp/nrsp
  ANL_f <- ANL %>% filter(AVALC %in% c(value.resp, value.nresp))
  
  #Check dataset is valid
  if (nrow(ANL_f) <= 0) {
    stop("No data left after filtering. Please check ARM or PARAMCD is selected correctly.")
  }
  #Check patient ID is unique
  if (ANL_f$USUBJID %>% duplicated %>% any) {
    stop("Incorrect PARAMCD selected or data contain multiple records for each subject. 
         Please check that input data contain only one observation for each subject")
  }
  
  response_table(
    response         = ANL_f$AVALC,
    value.resp       = value.resp,
    value.nresp      = value.nresp,
    arm              = ANL_f[[arm.var]],
    arm.ref          = arm.ref,
    arm.comp         = arm.comp,
    arm.comp.combine = arm.com.combine,
    style            = style
  )
  
}

  
  

