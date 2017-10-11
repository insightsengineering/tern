
#' Create the Response Table
#' 
#' @param response Tumor response data
#' @param val_rsp Character vector, defining list of response values to be used as responders
#' @param val_nrsp Character vector, defining list of response values to be used as non-responders
#' @param arm Arm information data
#' @param arm_ref Character vector, defining which arm(s) from the list of arms should be used as reference group
#' @param arm_comp Character vector, defining which arm(s) from the list of arms should be used as comparison group
#' @param incl_missing Boolean value, \code{TRUE} if missing values should be considered non-responders, 
#'                     \code{FALSE} if missing response should be removed from analysis
#' @export
#' 
#' 
#' @examples 
#' 
#' 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr)
#' '%needs%' <- teal.oncology:::'%needs%'
#' 
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' ARS <- ars(com.roche.cdt30019.go29436.re)
#' 
#' ARS_f <- ARS %>% filter(PARAMCD == "BESRSPI")
#' 
#' response_table(
#'    response     = ARS_f$AVALC,
#'    val_rsp      = c("CR","PR"),
#'    val_nrsp     = c("SD", "NON CR/PD", "PD", "NE"),
#'    arm          = ARS_f$ARM,
#'    arm_ref      = "DUMMY A",
#'    arm_comp     = "DUMMY B",
#'    incl_missing = T
#' )
#' 
#' } 
#' 

response_table <- function(response, val_rsp, val_nrsp, arm, arm_ref, arm_comp, incl_missing) {
  
  #####################
  # Argument checking #
  #####################

  #--- Check responder/non-responder values are in controlled terminology ---#
  # Controlled codelist
  val_all = c("CR", "PR", "SD", "NON CR/PD", "PD", "NE")
  val_missing = "Missing"
  
  response[response==""] <- val_missing

  #--- Check length of other varibles matches length of response data ---#
  n <- length(response)
  if (length(arm) != n ) stop("ARM variable has wrong length, please check data")
  
  # Check response data only contain controlled codelist
  if (any(!response %in% c(val_all, val_missing))) {
    stop("Incorrect PARAMCD selected or data contain invalid response values. Check that input data only contain controlled terms such as: ", toString(val_all))
  }
  
  # Check values selected for responder/non-responder are in controlled codelist
  if (!all(val_rsp %in% val_all)) stop("Invalid responder value selected. Check responder value only contain controlled terms such as: ", toString(val_all))
  if (!all(val_nrsp %in% val_all)) stop("Invalid non-responder values selected. Check non-responder value only contain controlled terms such as: ", toString(val_all))
  
  # Check values selected for responder/non-responder exist in input data
  if (!all(val_rsp %in% response)) stop("Invalid responder value selected. Not all responder values are present in input data")
  if (!all(val_nrsp %in% response)) stop("Invalid non-responder values selected. Not all non-responder values are present in input data")
  
  #--- Check various aspets of ARM variables ---#
  # Check there are 2+ unique levels in arm variable for comparison ---#
  arm_lvls <- levels(factor(arm))
  if (length(arm_lvls) < 2){
    stop("Invalid arm variable selected. Minimum 2 unique levels required for arm variable.")
  }

  # Check arm_ref and arm_comp in list of arm
  if (!all(arm_ref %in% arm)) stop("Invalid arm_ref value(s) selected. Not all value(s) in arm_ref are present in arm")
  if (!all(arm_comp %in% arm)) stop("Invalid arm_comp value(s) selected. Not all value(s) in arm_comp are present in arm")
  
  #####################
  # Math calculations #
  #####################
  
  #--- Wraggling data to be used in analysis ---#
  if (incl_missing & !(val_missing %in% val_nrsp)) {
    val_nrsp = c(val_nrsp, val_missing)
  }
  
  #Creating mask for each categorical values in arm and response separately
  mask_arm <- mask2di(arm, arm_ref, arm_comp, "Reference", "Comparison", "All Groups")
  mask_rsp <- mask2di(response, val_nrsp, val_rsp, "Non-Responders", "Responders","All Evaluable")

  #Combine arm and response (union of both masks), if TRUE then to be included in analysis
  mask_armrsp <- apply(cbind(mask_arm[,c(1)], mask_rsp[,c(1)]), 1, all)
  
  #Subset data on mask_armrsp, create final analysis dataset  
  mask <- list(finalarm=mask_arm, finalrsp=mask_rsp)
  final <- lapply(mask, function(x) subset(x,mask_armrsp))
  list2env(final, envir=environment())

  #--- Statistical calculations for response rate and 95% CI ---#
  rate_result <- lapply(c("Responders","Non-Responders",val_rsp,val_nrsp), 
                        function(x) getrate(x, finalarm, finalrsp))
  names(rate_result) <- c("Responders","Non-Responders",val_rsp,val_nrsp)

  #--- Statistical calculations for difference in rates (comp - ref) ---#
  difftest <- prop.test(rate_result$Responders$count, rate_result$Responders$totaln)
  diff_result <- list(diff   = unname(difftest$estimate[2] - difftest$estimate[1])*100,
                      diffci = unname(difftest$conf.int)*100,
                      diffp  = difftest$p.value)
  
  #--- Statistical calculations for odds ratio ---#
  ortest <- glm(finalrsp[,colnames(finalrsp) == "Responders"] ~ finalarm[,colnames(finalarm) == "Comparison"], 
                family=binomial(link='logit'))
  or_result <- list(or   = unname(coef(ortest)[2]),
                    orci = unname(confint.default(ortest)[2,]), 
                    orp  = coef(summary(ortest))[2,4])
  
  ##########################################
  # Build output data structure with rtable#
  ##########################################
  
  #--- Create column header ----#
  arm_type <- c("Reference Group", "Comparison Group")
  arm_name <- c(paste(arm_ref, collapse="/"), paste(arm_comp, collapse="/"))
  arm_n    <- paste0("(N=",rate_result$Responders$totaln,")")

  #--- Build output data structure ---#
  
  tbl <- rtable(
    col.names = paste(arm_type, arm_name, arm_n, sep="\n"),
    format = "xx (xx.xx%)",
    do.call(rrow, printnpct("Responders", rate_result)),
    do.call(rrow, printnpct("Non-Responders", rate_result)),
    rrow(),
    #do.call(rrow, c(modifyList(printci("Responders"),list(label="95% CI for Response Rates")),format="(xx.xx, xx.xx)")),
    rrow(),
    rrow("Difference in Response Rates", rcell(diff_result$diff, format="xx.xx", colspan=2)),
    rrow("95% CI", indent=1, rcell(diff_result$diffci, format="(xx.x, xx.x)", colspan=2)),
    rrow("p-value (Chi-square)", indent=1, rcell(diff_result$diffp, format="xx.xxx", colspan=2)),
    rrow(),
    rrow("Odds Ratio", rcell(or_result$or, format="xx.xx", colspan=2)),
    rrow("95% CI", indent=1, rcell(or_result$orci, format="(xx.x, xx.x)", colspan=2)),
    rrow()
  )
  
  tbl
  
}


#####################
# Helper Functions #
#####################
#--- Function to mask data to boolean ----#
#Example: mask2di(arm, arm_ref, arm_comp, "Reference","Comparison", "All Groups")
mask2di <- function(data, valto0, valto1, label0, label1, labelboth) {
  vallist <- c(valto0, valto1)
  datn <- length(data)
  
  #create mask for each value category
  mask_byval <- vapply(vallist, function(x) data %in% x, rep(NA,datn))
  
  #create mask for reference group or non-responders
  if (length(valto0) < 2) {
    mask_valto0 <- mask_byval[,colnames(mask_byval) %in% valto0]
  } else {
    mask_valto0 <- apply(mask_byval[,colnames(mask_byval) %in% valto0], 1, any)
  }
  
  #create mask for comparison group or responders
  if (length(valto1) < 2) {
    mask_valto1 <- mask_byval[,colnames(mask_byval) %in% valto1]
  } else {
    mask_valto1 <- apply(mask_byval[,colnames(mask_byval) %in% valto1], 1, any)
  }
  
  mask_both <- apply(cbind(mask_valto0, mask_valto1),1,any)
  
  #combine by-value and dichotomized mask
  mask_valall <- cbind(mask_both, mask_valto0, mask_valto1, mask_byval)
  colnames(mask_valall) <- c(labelboth, label0, label1, colnames(mask_byval))
  
  return(mask_valall)
}

#--- Function to calculate count, rate and 95% CI for rate, across treatment arms ----#
#Example: getrate("CR")
getrate <- function(rspcat, data_arm, data_rsp) {
  #Extract analysis data for the specified response category
  armdat <- data_arm[,colnames(data_arm) %in% c("Reference", "Comparison")]
  rspdat <- data_rsp[,colnames(data_rsp) == rspcat]
  
  #Use matrix multiplication to get number of responders in each arm
  ratetest <- mapply(binom.test, rspdat%*%armdat,colSums(armdat))
  
  #Get the statistics of interest, and output result as list
  rateout <- lapply(c("parameter","statistic","estimate","conf.int"), 
                    function(x) unname(unlist(ratetest[x,])))
  names(rateout) <- c("totaln","count","rate","rateci")
  
  return(rateout)
}

#--- Function to extract labels and stats ----#
#Example: getnpct("Responders")
#         getci("Responders")
rsp_full_name <- list(CR        = "Complete Response (CR)",
                      PR        = "Partial Response (PR)",
                      SD        = "Stable Disease (SD)",
                      `NON CR/PD` = "NON CR/PD",
                      PD        = "Progressive Disease (PD)",
                      NE        = "Missing or unevaluable (NE)")


printnpct <- function(rsp_name, resultds) {
  #If short name in rsp_full_name, then use the long name as label
  rsp_label <- ifelse(rsp_name %in% names(rsp_full_name),
                      unlist(rsp_full_name[rsp_name]),
                      rsp_name)
  list(rsp_label,
       c(resultds[[rsp_name]]$count[1], resultds[[rsp_name]]$rate[1]),
       c(resultds[[rsp_name]]$count[2], resultds[[rsp_name]]$rate[2])
  )
}

printnpct2 <- function(rsp_name,resultds) {
  #If short name in rsp_full_name, then use the long name as label
  rsp_label <- ifelse(rsp_name %in% names(rsp_full_name),
                      unlist(rsp_full_name[rsp_name]),
                      rsp_name)
  list(label=rsp_label,
       npct1=c(resultds[[rsp_name]]$count[1], resultds[[rsp_name]]$rate[1]),
       npct2=c(resultds[[rsp_name]]$count[2], resultds[[rsp_name]]$rate[2])
  )
}


printci <- function(rsp_name) {
  list(label = "95% CI", 
       ci1   = rate_result[[rsp_name]]$rateci[1:2]*100, 
       ci2   = rate_result[[rsp_name]]$rateci[3:4]*100
  )
}


#' Response Table with ADaM data structure 
#' 
#' @param ASL ASL dataset with following variables: USUBJID, STUDYID, the specified grouping variable
#' @param ARS ARS dataset containing the following variables: USUBJID, STUDYID, PARAMCD, AVALC
#' @param arm_var Name of the variable to use as testing arms
#' @param arm_ref Character vector, defining which arm(s) from the list of arms should be used as reference group
#' @param arm_comp Character vector, defining which arm(s) from the list of arms should be used as comparison group
#' @param val_rsp Character vector, defining list of response values to be used as responders
#' @param val_nrsp Character vector, defining list of response values to be used as non-responders
#' @param incl_missing Boolean value, \code{TRUE} if missing values should be considered non-responders, 
#'                     \code{FALSE} if missing response should be removed from analysis
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' library(atezo.data)
#' '%needs%' <- teal.oncology:::'%needs%'
#' source("C:/Users/liaoc10/Desktop/teal.onco_miscfiles/rtable.R")
#' 
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' ARS <- ars(com.roche.cdt30019.go29436.re)
#' 
#' response_table_ADAM(ASL, ARS, 
#'                     arm_ref = "DUMMY A", 
#'                     arm_comp = "DUMMY B")
#' 
#' }
#' 

response_table_ADAM <- function(ASL, ARS,
                                response_par = "BESRSPI",
                                arm_var      = "ARM",
                                arm_ref,
                                arm_comp,
                                val_rsp      = c("CR","PR"),
                                val_nrsp     = c("SD", "NON CR/PD", "PD", "NE"),
                                incl_missing = T) {

  #Check all required variables are present in ADaM
  ASL %needs% c("USUBJID", "STUDYID", arm_var)
  ARS %needs% c("USUBJID", "STUDYID", "PARAMCD", "AVALC")
  
  #Select records needed to analysis, merge ASL/ARS to create analysis dataset
  ASL_f <- ASL %>% select(c("USUBJID", "STUDYID", arm_var))            %>% filter(get(arm_var) %in% c(arm_ref,arm_comp))
  ARS_f <- ARS %>% select(c("USUBJID", "STUDYID", "PARAMCD", "AVALC")) %>% filter(PARAMCD == response_par)
  ANL <- merge(ASL_f, ARS_f, by=c("USUBJID", "STUDYID"))
  
  #Check dataset is valid
  if (nrow(ANL) <= 0) {
    stop("No data left after filtering. Please check ARM or PARAMCD is selected correctly.")
  }
  #Check patient ID is unique
  if (ANL$USUBJID %>% duplicated %>% any) {
    stop("Incorrect PARAMCD selected or data contain multiple records for each subject. 
         Please check that input data contain only one observation for each subject")
  }

  response_table(
    response     = ANL$AVALC,
    val_rsp      = val_rsp,
    val_nrsp     = val_nrsp,
    arm          = ANL$ARM,
    arm_ref      = arm_ref,
    arm_comp     = arm_comp,
    incl_missing = T
  )
  
}
  
  

