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
#' 
#' @importFrom stats binom.test
#' @export
#' 
#' @author Chendi Liao (liaoc10), \email{chendi.liao@roche.com}
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' library(forcats)
#' 
#' ASL <- radam("ASL")
#' ARS <- radam("ARS", ADSL = ASL)
#' 
#' ANL <- merge(ASL, ARS) %>% 
#'   filter(PARAMCD == "OVRSPI")
#'
#' tbl <- t_rsp(
#'  rsp = ANL$AVALC %in% c("CR", "PR"),
#'  col_by = fct_relevel(factor(ANL$ARMCD), "ARM B", "ARM A"),
#'  parition_rsp_by = factor(ANL$AVALC, levels =  c("CR", "PR", "SD", "NON CR/PD", "PD")),
#'  strata_data = NULL
#' )
#' 
#' tbl
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
#' ANL <- ARS %>% 
#'   filter(PARAMCD == "OVRSPI", ITTGEFL == 'Y', ITTWTFL == 'Y') %>%
#'   mutate(AVALC = ifelse(AVALC == "", "NE", AVALC))
#'
#' tbl <- response_table(
#'  response = ANL$AVALC,
#'  value.resp = c("CR", "PR"),
#'  value.nresp = setdiff(ANL$AVALC, c("CR", "PR")),
#'  arm = fct_relevel(ANL$ARMCD, "C", "B", "A")
#' )
#' 
#' Viewer(tbl, tbl_stream)
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
#' 
#'   #Reorder response/non-response values - controlled codelist first, NE last
#'
#  resp_order <-
t_rsp <- function(
  rsp,
  col_by,
  parition_rsp_by = NULL,
  strata_data = NULL) {
  
  # Argument checking
  # #################
  check_same_N(rsp = rsp, col_by = col_by, parition_rsp_by = parition_rsp_by, strata_data = strata_data)
  
  if (!is.logical(rsp)) stop("rsp is expected to be logical")
  if (any(is.na(rsp))) stop("rsp can not have any NAs")
  check_col_by(col_by, min_num_levels = 2)
  check_strata_data(strata_data)
  
  # Responder table
  tbl_response <- rbind(
    rtabulate(rsp, col_by = col_by, success_and_proportion, format = "xx.xx (xx.xx%)",
              row.name = "Responders"),
    rtabulate(!rsp, col_by = col_by, success_and_proportion, format = "xx.xx (xx.xx%)",
              row.name = "Non-Responders")
  )
  
  # Response Rate
  tbl_clopper_pearson <- rtabulate(
    x = rsp, col_by = col_by,
    function(x) { binom.test(sum(x), length(x))$conf.int * 100 },
    format = "(xx.xx, xx.xx)",
    row.name = "95% CI for Response Rates (Clopper-Pearson)"
  )
  
  # Difference in Response Rates
  tbl_difference <- rbind(
    
    tabulate_pairwise(rsp, col_by, function(x, by) {
      diff(tapply(x, by, mean))
    }, format = "xx.xx", row.name = "Difference in Response Rates"),
    
    # wald test without continuity correction
    tabulate_pairwise(rsp, col_by, function(x, by) {
      rcell("add logic")
    },
    indent = 1,
    row.name = "95% CI for difference (Wald without correction)"),
    
    # wald test with  continuety correction
    tabulate_pairwise(rsp, col_by, function(x, by) {
      rcell("add logic")
    }, 
    indent = 1,
    row.name = "95% CI for difference (Wald with continuity correction)"),
    
    # p-value dependent on strata_data
    if (is.null(strata_data)) {
      tabulate_pairwise(rsp, col_by, function(x, by) {
        rcell("add logic")
      }, 
      indent = 1,
      row.name = "p-value (Chi-squared)")
    } else {
      tabulate_pairwise(rsp, col_by, function(x, by) {
        rcell("add logic")
      }, 
      indent = 1,
      row.name = "p-value (Cochran-Mantel-Haenszel)")
    }
  )
  
  # Odds Ratio
  tbl_odds_ratio <- rbind(
    rtable(header = levels(col_by), rrow("odds ratio table"))
  )
  
  
  # Partition
  
  tbl_partition <- if (is.null(parition_rsp_by)) {
    NULL
  } else {
    df <- data.frame(rsp = rsp, col_by = col_by)
    df.split <- split(df, parition_rsp_by, drop = FALSE)
    
    tbls_part <- Map(function(dfi, name) {
      rbind(
        rtabulate(dfi$rsp, dfi$col_by, success_and_proportion, format = "xx.xx (xx.xx%)",
                  row.name = name),
        rtabulate(dfi$rsp, dfi$col_by, function(x) rcell("to be done"), format = "xx",
                  row.name = "95% CI (Wald)", indent = 1)
      )
    }, df.split, names(df.split))
    
    
    stack_rtables_l(tbls_part) 
  }
  
  
  stack_rtables(
    tbl_response,
    tbl_clopper_pearson,
    tbl_difference,
    tbl_odds_ratio,
    tbl_partition
  )
  

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




tmp <- function() {
  
  ## ???
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
 
  
   
}