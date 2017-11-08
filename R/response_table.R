#' Response Table Caclulation Function 
#' 
#' @param response Tumor response data
#' @param value.rsp Character vector, defining list of response values to be
#'   used as responders
#' @param value.nrsp Character vector, defining list of response values to be
#'   used as non-responders
#' @param arm Arm information data as factor
#' @param strata_data data for stratification factors (categorical variables) 
#' @param style Must be 1 or 2, \code{1} if only want to display rates summary
#'   for each response value category, \code{2} if want to display rates,
#'   difference in rate and odds ratio for each response value category.
#'
#'                     
#' @details 
#' We use the \code{epiR} package to compute 2-by-2 contingency table statistics
#' and analysis with stratification factors.
#' 
#' If no stratification, chi-squared test is performed by default to test for
#' equality of proportions in response rate between reference and comparison arms.
#' 
#' If stratification factors are added, Cochran-Mantel-Haensel test statistics are 
#' reported for the test of equal proportions, odds ratio and its corresponding 
#' 95% confidence interval.                           
#' 
#' @importFrom epiR epi.2by2                                                                                   
#' @export
#' 
#' @author Chendi Liao (liaoc10), \email{chendi.liao@roche.com}
#' 
#' @examples 
#' 
#' 
#' \dontrun{
#' library(dplyr)
#' library(atezo.data)
#' library(teal.oncology)
#' library(forcats)
#' library(epiR)
#' 
#' ARS <- ars(com.roche.cdt30019.go29436.re)
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' 
#' 
#' # Stream example
#' 
#' tbl_stream <- get_response_table(com.roche.cdt30019.go29436.re)
#' Viewer(tbl_stream)
#' 
#' ANL <- ARS %>% filter(PARAMCD == "OVRSPI", ITTGEFL == 'Y', ITTWTFL == 'Y')
#' 
#' tbl <- response_table(
#'  response = ANL$AVALC,
#'  value.resp = c("CR", "PR"),
#'  value.nresp = setdiff(ANL$AVALC, c("CR", "PR")),
#'  arm = fct_relevel(ANL$ARMCD1, "C", "B", "A")
#' )
#' 
#' compare_rtables(tbl, tbl_stream, comp.attr = FALSE)
#' 
#' 
#' 
#' # Other examples
#' 
#' paramcd = "OVRSPI"
#' var_arm = "ARM"
#' ref_arm = "DUMMY C"
#' comp_arm = c("DUMMY B", "DUMMY A")
#' combine_arm = FALSE
#' responders = c("CR", "PR")
#' var_strata = NULL
#' 
#' ANL <- ARS %>% 
#'   filter(PARAMCD == paramcd, ARM %in% c(ref_arm, comp_arm))
#'   
#' # If want to include missing as non-responders
#' ANL$AVALC[ANL$AVALC==""] <- "NE"
#' 
#' arm1 <- factor(ANL$ARM)
#' ARM <- fct_relevel(arm1, ref_arm, comp_arm)
#' 
#' tbl <- response_table(
#'  response = ANL$AVALC,
#'  value.resp = c("CR", "PR"),
#'  value.nresp = setdiff(ANL$AVALC, c("CR", "PR")),
#'  arm = ARM,
#'  strata_data = if (!is.null(var_strata)) ANL[var_strata] else NULL,
#'  style = 1
#' )
#' 
#' Viewer(tbl)
#' #clean up
#' rm(paramcd, var_arm, ref_arm, comp_arm, combine_arm, responders, var_strata) 
#' 
#' 
#' # Example 2
#' paramcd = "OVRSPI"
#' var_arm = "ARM"
#' ref_arm = c("DUMMY C", "DUMMY B")
#' comp_arm = "DUMMY A"
#' combine_arm = TRUE
#' responders = c("CR", "PR")
#' var_strata = "SEX"
#' 
#' refname <- paste0(ref_arm, collapse = "/")
#' ARM <- do.call(fct_collapse, setNames(list(arm1, ref_arm), c("f", refname))) %>%
#'   fct_relevel(refname, comp_arm)
#' 
#' #style 2 
#' tbl2 <- response_table(response = ANL$AVALC,
#'                        value.resp = c("CR", "PR"),
#'                        value.nresp = setdiff(ANL$AVALC, c("CR", "PR")),
#'                        arm = ARM,
#'                        strata_data = if (!is.null(var_strata)) ANL[var_strata] else NULL,
#'                        style = 2)
#' Viewer(tbl2)
#' 
#' }
#' 
#' 
#' '%needs%' <- teal.oncology:::'%needs%' # for debugging purpuses
response_table <- function(response, 
                           value.resp       = c("CR","PR"),
                           value.nresp      = c("SD", "NON CR/PD", "PD", "NE"),
                           arm, 
                           strata_data      = NULL, 
                           style = 1
                           ) {
  
  #####################
  # Argument checking #
  #####################
  
  #Check length of input variables matches #
  n <- length(response)
  if (length(arm) != n) stop("Length for arm and response variables does not match, please check data")
  if (!is.null(strata_data)) {
    if (nrow(strata_data) != n) stop("Length of strata_data does not match length of response variable, please check data")
    if (any(rowSums(is.na(strata_data) | strata_data == "") != 0)) stop("Cannot have NA or missing values in stratification factor")
  }

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

  df <- data.frame(arm, response)
  if (!is.null(strata_data)) {
    strata_name <- names(strata_data)
    strata <- do.call(paste, c(as.data.frame(strata_data), sep="/"))
    df <- data.frame(df, strata)
  }
  df <- subset(df, !is.na(arm) & rsp_for_test == TRUE)

  #Final arm vectors for analysis
  ARM <- df$arm
  arm_lvl <- levels(ARM)
  
  #####################
  # Math calculation  #
  #####################
  
  # Split response data into tree by ARM
  # Rate and CI calculation   
  value_list <- c(list(c(value.resp)), as.list(value_for_test))
  names(value_list) <- c("Responders", value_for_test)
  
  rate_result <- lapply(split(df$response, ARM), function(x){
    lapply(value_list, function(y) {
      N       <- length(x)
      n_resp  <- sum(x %in% y)
      ratestats    <- binom.test(n_resp, N)
      
      list(
        N       = N,
        n_resp  = n_resp,
        n_nresp = N-n_resp,
        p_resp  = n_resp/N,
        p_nresp = (N-n_resp)/N,
        ci      = ratestats$conf.int*100
      )
    })
  })


  #Calculation for diff in rate and OR for Responder/Non-responders
  coltotal = length(arm_lvl)
  comparm_list <- as.list(arm_lvl[2:coltotal])
  names(comparm_list) <- arm_lvl[2:coltotal]
  if (style == 1) {
    value_list <- list(value.resp)
    names(value_list) <- "Responders"
  } else if (style == 2) {
    value_list <- c(list(value.resp), as.list(value.resp))
    names(value_list) <- c("Responders", value.resp)
  }
  
  diffor_result <- lapply(comparm_list, function(i) {
    lapply(value_list, function(j) {
      df.diffor <- df %>% 
        filter(arm %in% c(levels(df$arm)[1], i)) %>% 
        mutate(arm = droplevels(arm),
               final.arm = fct_relevel(arm, i, levels(df$arm)[1]),
               resp = fct_collapse(response, "Responder" = j, "Non-Responder" = setdiff(response, j)),
               final.resp = fct_relevel(resp, "Responder", "Non-Responder"))
      
      if (!exists("strata_name")) {
        #If no strata
        df.table <- table(df.diffor[c("final.arm", "final.resp")])
        if (any(df.table ==0)) stop("At least one cell has frequency of 0, please check data")
        df.test  <- epi.2by2(df.table, method = "cohort.count",
                             conf.level = 0.95, outcome = "as.columns")
        list(
          diff   = unlist(df.test$res$ARisk.crude.wald[1]),
          diffci = unlist(df.test$res$ARisk.crude.wald[2:3]),
          diffp  = unlist(df.test$res$chisq.crude[3]),
          or     = unlist(df.test$res$OR.crude.wald[1]),
          orci   = unlist(df.test$res$OR.crude.wald[2:3])
        )
      } else {
        #If strata is requested
        df.table <- table(df.diffor[c("final.arm", "final.resp", "strata")])
        if (any(df.table ==0)) stop("At least one cell in one of the stratum has frequency of 0, please check data")
        df.test  <- epi.2by2(df.table, method = "cohort.count",
                             conf.level = 0.95, outcome = "as.columns")
        list(
          diff   = unlist(unname(df.test$res$ARisk.crude.wald[1])),
          diffci = unlist(unname(df.test$res$ARisk.crude.wald[2:3])),
          diffp  = unlist(unname(df.test$res$chisq.mh[3])),
          or     = unlist(unname(df.test$res$OR.mh.wald[1])),
          orci   = unlist(unname(df.test$res$OR.mh.wald[2:3]))
        )
      }
    })
  })
  
  if (!exists("strata_name")) {
    method <- " (Chi-squared)"
    notation <- ""
  } else {
    method <- " (Cochran-Mantel-Haenszel)"
    notation <- ifelse((length(strata_name) > 1 | length(levels(factor(strata))) > 3), "*^", "*")
  }
  
  ##########################################
  # Build output data structure with rtable#
  ##########################################
  rate_resp <- lapply(rate_result, '[[', 1)
  rate_byvalue <- lapply(rate_result, '[', c(-1))
  diffor_resp <- lapply(diffor_result, '[[', 1)
  if (style == 2) diffor_byvalue <- lapply(diffor_result, '[', c(-1))
  
  #Depending on number of comparison arms, parameters for output display settings
  #centered (if only 1 comp arm) or aligned in column (if more than 1 comp arm)
  colsize = ifelse(coltotal > 2, 1, 2)
  if (coltotal > 2) {
    diffor_resp <- c(list(fill=NULL), diffor_resp)
    if (exists("diffor_byvalue")) {
      diffor_byvalue <- c(list(fill=NULL), diffor_byvalue)
    }
  }
  
  #Helper functions for display with rtable
  lrrow <- function(row.name, l, ...) {
    do.call(rrow, c(list(row.name = row.name, ...), l))
  }
  print_np <- function(x) rcell(c(x$n_resp, x$p_resp))
  print_ci <- function(x,y,z) rcell(x[[y]], format = "(xx.xx, xx.xx)", colspan = z)
  print_diffor <- function(x,y,z) rcell(x[[y]], format = "xx.xx", colspan = z)
  
  #--- Header Section ---#
  out_header <-  list(
    col.names = paste(arm_lvl, 
                      paste0("(N=", unlist(lapply(rate_resp, '[[', "N")),")"), 
                      sep="\n"),
    format = "xx (xx.x%)"
  )
  
  #--- Responder/Non-responder section - counts and 95% CI ---#
  out_rate_resp  <- list(
    lrrow("Responders", lapply(rate_resp, print_np)),
    lrrow("Non-Responders", lapply(rate_resp, function(x) rcell(c(x$n_nresp, x$p_nresp)))),
    rrow(),
    lrrow("95% CI for Response Rates (Clopper-Pearson)", lapply(rate_resp, print_ci, "ci", 1)),
    rrow()
  )
  
  #--- Responder/Non-responder section - diff in rates and OR ---#
  #Depending on number of comparison arms, display centered (if only 1 comp arm) or aligned in column (if more than 1 comp arm)
  out_diffor_resp <- list(
    lrrow("Difference in Response Rates", lapply(diffor_resp, print_diffor, "diff", colsize)),
    lrrow("95% CI for difference in response rates (Wald)", indent = 1, lapply(diffor_resp, print_ci, "diffci", colsize)),
    lrrow(paste0("p-value", notation, method), indent = 1,      
          lapply(diffor_resp, function (x) rcell(x$diffp, format = "xx.xxxx", colspan = colsize))),
    rrow(),
    lrrow(paste0("Odds Ratio", notation),         lapply(diffor_resp, print_diffor, "or", colsize)),
    lrrow(paste0("95% CI", notation), indent = 1, lapply(diffor_resp, print_ci, "orci", colsize)),
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
  
  
  out_byvalue <- unlist(
    lapply(as.list(1:length(value_for_test)), function(i) {
      #Extract response value short name
      rspname <- sapply(rate_byvalue, names)[i]
      if (rspname %in% names(rsp_full_label)) {
        label <- unname(rsp_full_label[rspname])
      } else {label <- rspname}
      
      #Display counts and percentage row
      part1 <-list(lrrow(label,lapply(lapply(rate_byvalue, '[[', i), print_np)))
      
      #If response value is NE, do not display 95% CI and difference in rate rows
      if (rspname != "NE") {
        if (style == 2 & rspname %in% value.resp) {
          part2 <- list(
            lrrow("95% CI (Wald)", lapply(lapply(rate_byvalue, '[[', i), print_ci, "ci", 1), indent = 1),
            rrow(),
            lrrow(paste("Difference in",label, "rate"), 
                  lapply(lapply(diffor_byvalue, '[[', i), print_diffor, "diff", colsize), indent = 1),
            lrrow(paste("95% CI for difference in", rspname, "rate"), 
                  lapply(lapply(diffor_byvalue, '[[', i), print_ci, "diffci", colsize), indent = 1),
            lrrow(paste0("p-value", notation, method), 
                  lapply(lapply(diffor_byvalue, '[[', i), 
                         function (x) rcell(x$diffp, format = "xx.xxxx", colspan=colsize)),indent = 1)
          )
        } else {
          part2 <- list(lrrow("95% CI (Wald)", 
                              lapply(lapply(rate_byvalue, '[[', i), print_ci, "ci", 1), indent = 1))
        }
      } else {
        part2 <- NULL
      }
      
      #Last row - either empty or notation for stratification
      part3 <- list(rrow())

      #Combine all rows to be parsed by unlist function
      c(part1, part2, part3)
      
    }), recursive = F
  )
  
  #--- Footer section, if any for notes on stratification ---#
  out_footer <- NULL
  if (grepl("\\*", notation)) {
    n_strata <- length(strata_name)
    out_footer <- c(out_footer,
                    list(rrow(paste("* Model stratified by", 
                                    ifelse(n_strata < 2, strata_name, 
                                           paste(paste(strata_name[-(n_strata)], collapse = ", "), "and", strata_name[(n_strata)]))
                         ))))
  }
  if (grepl("\\^", notation)) {
    out_footer <- c(out_footer,
                    list(rrow("^ CAUTION! Multiple factors or more than 3 strata present. 
                              Make sure sample size within each stratum is sufficient for analysis.")))
  }

  #Put all sections together to display table
  tbl <- do.call(rtable,c(out_header,
                          out_rate_resp,
                          out_diffor_resp,
                          out_byvalue,
                          out_footer
                ))
  
  tbl
}

#########################################################################
# Helper functions                                                      #
#########################################################################
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
#' @param ASL dataset with following variables: USUBJID, STUDYID, the specified
#'   grouping variable
#' @param ARS ARS dataset containing the following variables: USUBJID, STUDYID,
#'   PARAMCD, AVALC
#' @param paramcd Name of overall response parameter
#' @param arm.var Name of variable with arm information
#' @param arm.ref Character vector, defining which arm(s) from the list of arms
#'   should be used as reference group
#' @param arm.comp Character vector, defining which arm(s) from the list of arms
#'   should be used as comparison group
#' @param arm.comp.combine Logical, \code{TRUE} if want all non-ref arms to be
#'   combined into one comparison group, \code{FALSE} if want each of the
#'   non-ref arms to be a separate comparison group
#' @param value.rsp Character vector, defining list of response values to be
#'   used as responders
#' @param value.nrsp Character vector, defining list of response values to be
#'   used as non-responders
#' @param incl.missing Logical, \code{TRUE} if missing values should be
#'   considered non-responders, \code{FALSE} if missing response should be
#'   removed from analysis
#' @param strata Character vector, defining stratification factor for stratified 
#'   analysis, default is NULL 
#' @param style Must be 1 or 2, \code{1} if only want to display rates summary
#'   for each response value category, \code{2} if want to display rates,
#'   difference in rate and odds ratio for each response value category.
#' 
#' @details 
#' Package \code{forcats} was used to re-format arm data into leveled factors.
#' Package \code{epiR} was used to compute 2-by-2 contingency table statistics
#' and analysis with stratification factors.
#' 
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(atezo.data)
#' library(teal.oncology)
#' library(forcats)
#' library(epiR)
#' 
#' '%needs%' <- teal.oncology:::'%needs%'
#' 
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' ARS <- ars(com.roche.cdt30019.go29436.re)
#' 
#' teal.oncology:::response_table_ADAM(ASL, ARS,
#'                     paramcd = "OVRSPI",
#'                     arm.ref = "DUMMY C",
#'                     arm.comp = c("DUMMY A", "DUMMY B"),
#'                     arm.comp.combine = FALSE,
#'                     value.resp = c("CR", "PR"),
#'                     value.nresp = c("SD", "NON CR/PD", "PD", "NE"),
#'                     incl.missing = TRUE,
#'                     strata.var = "SEX",
#'                     style = 1
#'                     )
#' }
#' 
response_table_ADAM <- function(ASL, ARS,
                                paramcd          = "OVRSPI",
                                arm.var          = "ARM",
                                arm.ref,
                                arm.comp,
                                arm.comp.combine = TRUE,
                                value.resp       = c("CR","PR"),
                                value.nresp      = c("SD", "NON CR/PD", "PD", "NE"),
                                incl.missing     = TRUE,
                                strata.var       = NULL,
                                style            = 1
                                ) {
  
  #Check all required variables are present in ADaM
  ASL %needs% c("USUBJID", "STUDYID", arm.var, strata.var)
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
  ASL_f <- ASL %>% select_(c("USUBJID", "STUDYID", arm.var)) %>% 
    subset(., subset = .[[arm.var]] %in% c(arm.ref,arm.comp))
  
  ARS_f <- ARS %>% select_(c("USUBJID", "STUDYID", "PARAMCD", "AVALC")) %>% filter(PARAMCD == paramcd)
  ANL <- merge(ASL_f, ARS_f, by=c("USUBJID", "STUDYID"))
  
  # Recode/filter responses if want to include missing as non-responders
  if (incl.missing == TRUE) {
    ANL$AVALC[ANL$AVALC==""] <- "NE"
  } else {
    ANL <- ANL %>% filter(AVALC != "")
  }

  #Check responder/non-responder values are in controlled terminology #
  # Controlled codelist, check responder/non-responder values and response data only contain controlled codelist
  # value.all = c("CR", "PR", "SD", "NON CR/PD", "PD", "NE")
  # if (any(!c(value.resp, value.nresp) %in% value.all)) {
  #   stop("Invalid response or non-response values selected, they should only contain controlled terms such as: ", toString(val_all))
  # }
  # if (any(!ANL$AVALC %in% value.all)) {
  #   stop("Incorrect PARAMCD selected or data contain invalid response values. Check that input data only contain controlled terms such as: ", toString(value.all))
  # }
  
  # Check values selected for responder/non-responder exist in input data
  #if (!all(value.resp %in% ANL$AVALC)) stop("Invalid responder value selected. Not all responder values are present in input data")
  #if (!all(value.nresp %in% ANL$AVALC)) stop("Invalid non-responder values selected. Not all non-responder values are present in input data")
  
  
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

  # Recode grouping according to arm.ref, arm.comp and arm.comp.combine settings
  arm1 <- factor(ANL_f[[arm.var]])
  
  if (length(arm.ref) > 1) {
    refname <- paste0(arm.ref, collapse = "/")
    arm2 <- fct_collapse(arm1, refs = arm.ref)
    levels(arm2)[which(levels(arm2)=="refs")] <- refname
  } else {
    arm2 <- fct_relevel(arm1, arm.ref)
  }
  
  if (length(arm.comp) > 1 && arm.comp.combine == TRUE) {
    compname <- paste0(arm.comp, collapse = "/")
    ARM <- fct_collapse(arm2, comps = arm.comp)
    levels(ARM)[which(levels(ARM)=="comps")] <- compname
  } else {
    ARM <- arm2
  }
  
  response_table(
    response         = ANL_f$AVALC,
    value.resp       = value.resp,
    value.nresp      = value.nresp,
    arm              = ARM,
    strata_data      = if (is.null(strata)) ANL_f[strata] else NULL,
    style            = style
  )
  
}
