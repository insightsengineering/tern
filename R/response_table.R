
#' Create the Response Table
#' 
#' @param response Tumor response data
#' @param value.rsp Character vector, defining list of response values to be used as responders
#' @param value.nrsp Character vector, defining list of response values to be used as non-responders
#' @param arm Arm information data
#' @param arm.ref Character vector, defining which arm(s) from the list of arms should be used as reference group
#' @param arm.comp Character vector, defining which arm(s) from the list of arms should be used as comparison group
#' @param arm.comp.combine Logical, \code{TRUE} if want all non-ref arms to be combined into one comparison group, 
#'                                  \code{FALSE} if want each of the non-ref arms to be a separate comparison group
#' @param style Must be 1 or 2, \code{1} if only want to display rates summary for each response value category,
#'                              \code{2} if want to display rates, difference in rate and odds ratio for each response value category.
#'
#'                     
#' @details 
#' We use the \code{PropCIs} to calculate the CIs because...                                        
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
#' library(forcats)
#' 
#' 
#' ARS <- ars(com.roche.cdt30019.go29436.re) %>% select(c("USUBJID", "STUDYID", "ARM", "PARAMCD", "AVALC")) %>% 
#'        filter(PARAMCD == "OVRSPI")
#' 
#' #If want to include missing as non-responders
#' ARS$AVALC[ARS$AVALC==""] <- "NE"
#' 
#' 
#' tbl_stream <- get_response_table(com.roche.cdt30019.go29436.re)
#' Viewer(tbl_stream)
#' 
#' tbl <- response_table(response = ARS$AVALC, arm = ARS$ARM, arm.ref = "DUMMY B", arm.comp.combine=F)
#' Viewer(tbl)
#' 
#' response_table(response = ARS$AVALC, arm = ARS$ARM, arm.ref = "DUMMY B", arm.comp.combine=T)
#' response_table(response = ARS$AVALC, arm = ARS$ARM, arm.ref = "DUMMY B", arm.comp.combine=F, style = 2)
#' response_table(response = ARS$AVALC, arm = ARS$ARM, arm.ref = "DUMMY B", arm.comp.combine=T, style = 2)
#' 
#' 
#' ars_f2 <-  ars(com.roche.cdt30019.go29436.re) %>%
#'  filter(ITTGEFL=='Y', ITTWTFL=='Y', PARAMCD=='OVRSPI')
#' 
#' tbl <- response_table(
#'   response = vapply(ars_f2$AVALC, function(x) {if (x == "") "NE" else x}, character(1)),
#'   arm = ars_f2$ARM,
#'   arm.ref = "DUMMY C",
#'   arm.comp.combine = FALSE
#' )
#' 
#' Viewer(tbl)
#' 
#' compare_rtables(tbl, tbl_stream, comp.attr = FALSE)
#' compare_rtables(tbl, tbl_stream)
#' 
#' tbl[2,3]
#' tbl_stream[2,3]
#' 
#' Arm variable is armcd1
#' }
#' 
#' 
#' '%needs%' <- teal.oncology:::'%needs%' # for debugging purpuses
response_table <- function(response, 
                           value.resp       = c("CR","PR"),
                           value.nresp      = c("SD", "NON CR/PD", "PD", "NE"),
                           arm, 
                           style = 1
                           ) {
  
  #####################
  # Argument checking #
  #####################
  
  #Check length of input variables matches #
  if (length(arm) != length(response) ) stop("Length for arm and response variables does not match, please check data")
  
  # Check there are 2+ unique levels in arm variable for comparison ---#
  if (length(levels(factor(arm))) < 2){
    stop("Invalid arm variable selected. Minimum 2 unique levels required for arm variable.")
  }
  if (!style %in% c(1,2)) {
    stop("Style must equal to 1 or 2")
  }
  
  ############################
  # Format data for analysis #
  ############################
  rsp_for_test   <- response %in% c(value.resp, value.nresp)
  
  #Reorder response/non-response values - controlled codelist first, NE last
  resp_order <- c("CR", "PR", "SD", "NON CR/PD", "PD")
  
  value_reorder <- lapply(list(value.resp, value.nresp), function(x) {
    getorder <- match(x, resp_order)
    getna <- which(is.na(getorder))
    getorder[getna] <- getna + 10
    getorder[which(x == "NE")] <- 99
    xreorder <- x[order(getorder)]
    xreorder
  })
  
  value.resp <- value_reorder[[1]]
  value.nresp <- value_reorder[[2]]
  value_for_test <- c(value.resp, value.nresp)
  
  data_for_test <- subset(
    data.frame(arm, response),
    (!is.na(arm) & rsp_for_test == TRUE)
  )
  
  #Final arm and response data vectors for analysis
  ARM <- data_for_test$arm
  RESP <- data_for_test$resp
  
  
  #####################
  # Math calculation  #
  #####################
  
  # Split response data into tree by ARM
  # Rate calculation for Responder/Non-responders
  rate_result <- Map(calc_rate, split(RESP, ARM), list(c(value.resp)))
  
  #Rate calculation by each response value
  data_byvalue <- data.frame(rep(list(RESP), length(value_for_test)))
  names(data_byvalue) <- value_for_test
  rate_result_byvalue <- lapply(split(data_byvalue, ARM), function(x){
    Map(calc_rate, x, value_for_test)
  })


  #Depending on number of comparison arms, parameters for output display settings
  #centered (if only 1 comp arm) or aligned in column (if more than 1 comp arm)
  coltotal = length(rate_result)
  if (length(levels(ARM)) < 3) {
    colstart = 2; colsize = 2
  } else {
    colstart = 1; colsize = 1
  }
  
  #Calculation for diff in rate and OR for Responder/Non-responders
  diffor_result <- lapply(as.list(colstart:coltotal), function(i) {
    calc_diffor(rate_result[[i]], rate_result[[1]])
  })
  names(diffor_result) <- names(rate_result)[c(colstart:coltotal)]
  #If more than one comparison arm (display aligned by column), remove numbers from first(reference) column
  if (length(levels(factor(ARM))) > 2) {
    diffor_result[[1]] <- rapply(diffor_result[[1]],function(x) NULL, how = "list")
  }
  
  #If style 2 selected, calculate diff in rate and OR calculation by each response value
  if (style == 2) {
    diffor_result_byvalue <- lapply(as.list(colstart:coltotal), function(i) {
      Map(calc_diffor, rate_result_byvalue[[i]], rate_result_byvalue[[1]])
    })
    names(diffor_result_byvalue) <- names(rate_result_byvalue)[c(colstart:coltotal)]
    if (length(levels(factor(ARM))) > 2) {
      diffor_result_byvalue[[1]] <- rapply(diffor_result_byvalue[[1]], function(x) NULL, how = "list")
    }
  }
  
  ##########################################
  # Build output data structure with rtable#
  ##########################################
  #--- Header Section ---#
  out_header <-  list(
    col.names = paste(names(rate_result), 
                      paste0("(N=", unlist(lapply(rate_result, '[[', "N")),")"), 
                      sep="\n"),
    format = "xx (xx.x%)"
  )
  
  #--- Responder/Non-responder section - counts and 95% CI ---#
  out_resp_rate  <- list(
    do.call(rrow, c("Responders",     lapply(rate_result, print_np))),
    do.call(rrow, c("Non-Responders", lapply(rate_result, function(x) rcell(c(x$n_nresp, x$p_nresp))))),
    rrow(),
    do.call(rrow, c("95% CI for Response Rates (Clopper-Pearson)", lapply(rate_result, print_ci, "ci", 1))),
    rrow()
  )
  
  #--- Responder/Non-responder section - diff in rates and OR ---#
  #Depending on number of comparison arms, display centered (if only 1 comp arm) or aligned in column (if more than 1 comp arm)
  out_resp_diffor <- list(
    do.call(rrow, c("Difference in Response Rates", lapply(diffor_result, print_diffor, "diff", colsize))),
    do.call(rrow, c("95% CI (Wald)", indent = 1,    lapply(diffor_result, print_ci, "diffci", colsize))),
    do.call(rrow, c("p-value", lapply(diffor_result, function (x) rcell(x$diffp, format = "xx.xxxx", colspan = colsize)))),
    rrow(),
    do.call(rrow, c("Odds Ratio",                   lapply(diffor_result, print_diffor, "or", colsize))),
    do.call(rrow, c("95% CI", indent = 1,           lapply(diffor_result, print_ci, "orci", colsize))),
    rrow()
  )
  
  #--- Section for each response value - counts and 95% CI ---#
  #Display full labels for responses in controlled codelist 
  rsp_full_label <- list(CR          = "Complete Response (CR)",
                         PR          = "Partial Response (PR)",
                         SD          = "Stable Disease (SD)",
                         `NON CR/PD` = "Non-CR or Non-PD (NON CR/PD)",
                         PD          = "Progressive Disease (PD)",
                         NE          = "Missing or unevaluable"
                         )
  
  
  out_byvalue_rate <- unlist(
    lapply(as.list(1:length(value_for_test)), function(i) {
      #Extract response value short name
      rspname <- sapply(rate_result_byvalue, names)[i]
      if (rspname %in% names(rsp_full_label)) {
        label <- unname(rsp_full_label[rspname])
      } else {label <- rspname}
      
      #Display counts and percentage row
      part1 <-list(do.call(rrow, c(label,lapply(sapply(rate_result_byvalue, function(x) x[i]), print_np))))
      
      #If response value is NE, do not display 95% CI and difference in rate rows
      if (rspname != "NE") {
        if (style == 2) {
          part2 <- list(
            do.call(rrow, c("95% CI", indent = 1,
                            lapply(sapply(rate_result_byvalue, function(x) x[i]), print_ci, "ci", 1))),
            rrow(),
            do.call(rrow, c(paste("Difference in",label, "rate"), indent = 1,
                            lapply(sapply(diffor_result_byvalue, function(x) x[i]), print_diffor, "diff", colsize))),
            do.call(rrow, c(paste("95% CI for difference in", label, "rate"), indent = 1,
                            lapply(sapply(diffor_result_byvalue, function(x) x[i]), print_ci, "diffci", colsize))),
            do.call(rrow, c("p-value", indent = 1,
                            lapply(sapply(diffor_result_byvalue, function(x) x[i]), function (x) rcell(x$diffp, format = "xx.xxxx", colspan=colsize))))
          )
        } else {
          part2 <- list(do.call(rrow, c("95% CI", indent = 1,
                                        lapply(sapply(rate_result_byvalue, function(x) x[i]), print_ci, "ci", 1))))
        }
      } else {
        part2 <- NULL
      }
      
      #Last empty row
      part3 <- list(rrow())
      
      #Combine all rows to be parsed by unlist function
      c(part1, part2, part3)
      
    }), recursive = F
  )

  
  #Put all sections together to display table
  tbl <- do.call(rtable,c(out_header,
                          out_resp_rate,
                          out_resp_diffor,
                          out_byvalue_rate
                         )
                 )
  
  tbl
}

#########################################################################
# Helper functions                                                      #
#########################################################################
#' Function to calculate counts and percentage for rates
#' 
#' @param x vector with response information
#' @param value name of the response value to compute statistics for
#' 
#' 
#' @importFrom PropCIs orscoreci
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
#' @param z colspan for the cell
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
#' print_ci(a_rate, "ci", 2)
#' print_ci(diffor, "diffci", 2)
#' print_diffor(diffor, "diff", 1)
#' print_diffor(diffor, "or", 1)
#' 
print_np <- function(x) rcell(c(x$n_resp, x$p_resp))
print_ci <- function(x,y,z) rcell(x[[y]], format = "(xx.xx, xx.xx)", colspan = z)
print_diffor <- function(x,y,z) rcell(x[[y]], format = "xx.xx", colspan = z)


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
#' @param arm.comp.combine Logical, \code{TRUE} if want all non-ref arms to be combined into one comparison group, 
#'                                  \code{FALSE} if want each of the non-ref arms to be a separate comparison group
#' @param value.rsp Character vector, defining list of response values to be used as responders
#' @param value.nrsp Character vector, defining list of response values to be used as non-responders
#' @param incl_missing Logical, \code{TRUE} if missing values should be considered non-responders, 
#'                              \code{FALSE} if missing response should be removed from analysis
#' @param style Must be 1 or 2, \code{1} if only want to display rates summary for each response value category,
#'                              \code{2} if want to display rates, difference in rate and odds ratio for each response value category.
#' 
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(shiny)
#' library(atezo.data)
#' library(teal.oncology)
#' library(PropCIs)
#' 
#' '%needs%' <- teal.oncology:::'%needs%'
#' source("R/rtable.R")
#' source("R/combine_arm.R")
#' 
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' ARS <- ars(com.roche.cdt30019.go29436.re)
#' 
#' response_table_ADAM(ASL, ARS,
#'                     paramcd = "BESRSPI",
#'                     arm.ref = "DUMMY C",
#'                     arm.comp = c("DUMMY A", "DUMMY B"),
#'                     arm.comp.combine = T,
#'                     value.resp = c("CR", "PR"),
#'                     value.nresp = c("SD", "NON CR/PD", "PD", "NE"),
#'                     incl.missing = TRUE,
#'                     style = 2
#'                     )
#' }
#' 
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
  
  if (!is.logical(arm.comp.combine)) {
    stop("arm.comp.combine parameter must be logical value T or F")
  }
  if (!is.logical(incl.missing)) {
    stop("incl.missing parameter must be logical value T or F")
  }
  if (!style %in% c(1,2)) {
    stop("Style must equal to 1 or 2")
  }
  
  
  #--- Select obs needed to analysis, merge ASL/ARS to create analysis dataset ---#
  #Filter on selected PARAMCD, and ARM in arm.ref/comp
  ASL_f <- ASL %>% select(c("USUBJID", "STUDYID", arm.var))            %>% filter(get(arm.var) %in% c(arm.ref,arm.comp))
  ARS_f <- ARS %>% select(c("USUBJID", "STUDYID", "PARAMCD", "AVALC")) %>% filter(PARAMCD == paramcd)
  ANL <- merge(ASL_f, ARS_f, by=c("USUBJID", "STUDYID"))
  
  #If want to include missing as non-responders, check NE is in value.nrsp and recode data
  if (incl.missing == TRUE) {
    if (!"NE" %in% value.nresp) {
      stop("In order to include missing as non-responder, value.nrsp must contain NE")
    } else {
      ANL$AVALC[ANL$AVALC==""] <- "NE"
    }
  }

  #Check responder/non-responder values are in controlled terminology #
  # Controlled codelist, check responder/non-responder values and response data only contain controlled codelist
  value.all = c("CR", "PR", "SD", "NON CR/PD", "PD", "NE")
  if (any(!c(value.resp, value.nresp) %in% value.all)) {
    stop("Invalid response or non-response values selected, they should only contain controlled terms such as: ", toString(val_all))
  }
  if (any(!ANL$AVALC %in% value.all)) {
    stop("Incorrect PARAMCD selected or data contain invalid response values. Check that input data only contain controlled terms such as: ", toString(value.all))
  }
  
  # Check values selected for responder/non-responder exist in input data
  if (!all(value.resp %in% ANL$AVALC)) stop("Invalid responder value selected. Not all responder values are present in input data")
  if (!all(value.nresp %in% ANL$AVALC)) stop("Invalid non-responder values selected. Not all non-responder values are present in input data")
  
  
  #--- Filter on AVALC in value.rsp/nrsp ---#
  ANL_f <- ANL %>% filter(AVALC %in% c(value.resp, value.nresp))
  
  #Check input dataset is valid
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
    arm.comp.combine = arm.comp.combine,
    style            = style
  )
  
}

  
  

