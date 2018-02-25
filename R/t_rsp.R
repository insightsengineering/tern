#' Response Table Caclulation Function 
#' 
#' @template param_rsp
#' @template param_col_by
#' @param parition_rsp_by factor to partition \code{rsp} and run additional
#'   analysis
#' @param strata_data data for stratification factors (categorical variables),
#'   if \code{NULL} no stratified analysis is performed
#' 
#' @details 
#' We use the \code{epiR} package to compute 2-by-2 contingency table statistics
#' and analysis with stratification factors.
#'
#' If no stratification, chi-squared test is performed by default to test for
#' equality of proportions in response rate between reference and comparison
#' arms.
#'
#' If stratification factors are added, Cochran-Mantel-Haensel test statistics
#' are reported for the test of equal proportions, odds ratio and its
#' corresponding 95% confidence interval.
#' 
#' @template return_rtable
#' 
#' @importFrom stats binom.test
#' 
#' @export
#' 
#' @author Chendi Liao (liaoc10), \email{chendi.liao@roche.com}
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL")
#' ARS <- radam("ARS", ADSL = ASL)
#' 
#' ANL <- merge(ASL, subset(ARS, PARAMCD == "OVRSPI"))
#'
#' tbl <- t_rsp(
#'  rsp = ANL$AVALC %in% c("CR", "PR"),
#'  col_by = relevel(factor(ANL$ARMCD), "ARM B", "ARM A"),
#'  parition_rsp_by = factor(ANL$AVALC, levels =  c("CR", "PR", "SD", "NON CR/PD", "PD")),
#'  strata_data = NULL
#' )
#' 
#' tbl
#' 
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
  check_data_frame(strata_data)
  
  # Responder table
  tbl_response <- rbind(
    rtabulate(rsp, col_by = col_by, positives_and_proportion, format = "xx.xx (xx.xx%)",
              row.name = "Responders"),
    rtabulate(!rsp, col_by = col_by, positives_and_proportion, format = "xx.xx (xx.xx%)",
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
        rtabulate(dfi$rsp, dfi$col_by, positives_and_proportion, format = "xx.xx (xx.xx%)",
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
#' @author Chendi Liao (liaoc10) \email{chendi.liao@roche.com}
#' @noRd
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
