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
#' @template author_liaoc10
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL", N = 1000, start_with = list(RACE = c("white", "asian")))
#' ARS <- radam("ARS", ADSL = ASL)
#' 
#' ASL$ARM <- factor(sample(LETTERS[1:3], nrow(ASL), TRUE))
#' ASL$RACE <- factor(ASL$RACE)
#' 
#' ANL <- merge(ASL, subset(ARS, PARAMCD == "OVRSPI"))
#'
#' tbl <- t_rsp(
#'  rsp = ANL$AVALC %in% c("CR", "PR"),
#'  col_by = relevel(factor(ANL$ARMCD), "ARM B", "ARM A"),
#'  parition_rsp_by = droplevels(factor(ANL$AVALC, levels = c("CR", "PR", "SD", "NON CR/PD", "PD", "NE"))),
#'  strata_data = ANL[, c("SEX", "RACE")]
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
  
  if (!is.null(strata_data)) {
    check_data_frame(strata_data)
  }

  
  # Responder table
  tbl_response <- rbind(
    rtabulate(rsp, col_by = col_by, positives_and_proportion, format = "xx.xx (xx.xx%)",
              row.name = "Responders"),
    rtabulate(!rsp, col_by = col_by, positives_and_proportion, format = "xx.xx (xx.xx%)",
              row.name = "Non-Responders")
  )
  
  # Response Rate
  tbl_clopper_pearson <- rtabulate(
    x = rsp,
    col_by = col_by,
    function(x) {
      binom.test(sum(x), length(x))$conf.int * 100 
    },
    format = "(xx.xx, xx.xx)",
    row.name = "95% CI for Response Rates (Clopper-Pearson)"
  )
  
  # Difference in Response Rates
  tbl_difference <- rbind(
    # by is a factor with two levels: 1 ref arm, 2nd comp arm
    tabulate_pairwise(rsp, col_by, function(x, by) {
      
      diff(tapply(x, by, mean))
      
    }, format = "xx.xx", row.name = "Difference in Response Rates"),
    
    # wald test without continuity correction
    tabulate_pairwise(rsp, col_by, function(x, by) {
      
      t_wc <- prop.test(x = tapply(x, by, sum),
                        n = tapply(x, by, length),
                        correct = FALSE)
      
      rcell(t_wc$conf.int, format = "(xx.xx, xx.xx)")
    },
    indent = 1,
    row.name = "95% CI for difference (Wald without correction)"),
    
    # wald test with  continuety correction
    tabulate_pairwise(rsp, col_by, function(x, by) {
      
      t_wc <- prop.test(x = tapply(x, by, sum),
                        n = tapply(x, by, length),
                        correct = TRUE)
      
      rcell(t_wc$conf.int, format = "(xx.xx, xx.xx)")
    }, 
    indent = 1,
    row.name = "95% CI for difference (Wald with continuity correction)"),
    
    # p-value dependent on strata_data
    if (is.null(strata_data)) {
      tabulate_pairwise(rsp, col_by, function(x, by) {
        
        
        t_wc <- prop.test(x = tapply(x, by, sum),
                          n = tapply(x, by, length),
                          correct = FALSE)
        
        rcell(t_wc$p.value, format = "xx.xxxx")
      }, 
      indent = 1,
      row.name = "p-value (Chi-squared)")
    } else {
      tabulate_pairwise(rsp, col_by, function(x, by) {
        
        strat <- do.call(strata, strata_data)
        
        if (any(tapply(rsp, strat, length)<5)) {
          rcell("<5 data points")
        } else {
          t.tbl <- table(col_by, rsp, strat)
          t_m <- mantelhaen.test(t.tbl, correct = FALSE)
          
          rcell(t_m$p.value, format = "xx.xxxx")
        }
        
      }, 
      indent = 1,
      row.name = "p-value (Cochran-Mantel-Haenszel)")
    }
  )
  
  # Odds Ratio
  tbl_odds_ratio <- rbind(
    tabulate_pairwise(rsp, col_by, function(x, by) {
      if (!is.null(strata_data)) {
        
        fit <- odds.ratio(table(by, x))
        rcell(fit$estimator, "xx.xx")
        
      } else {
        
        strat <- do.call(strata, strata_data)
        
        if (any(tapply(rsp, strat, length)<5)) {
          rcell("<5 data points")
        } else {
          t.tbl <- table(col_by, rsp, strat)
          t_m <- mantelhaen.test(t.tbl, correct = FALSE)
          
          rcell(t_m$estimate, format = "xx.xx")
        }
        
      }
    }, row.name = "Odds Ratio"),
    tabulate_pairwise(rsp, col_by, function(x, by) {
      if (!is.null(strata_data)) {
        
        fit <- odds.ratio(table(by, x))
        rcell(fit$conf.interval, "(xx.xxxx, xx.xxxx)")
        
      } else {
        
        strat <- do.call(strata, strata_data)
        
        if (any(tapply(rsp, strat, length)<5)) {
          rcell("<5 data points")
        } else {
          t.tbl <- table(col_by, rsp, strat)
          t_m <- mantelhaen.test(t.tbl, correct = FALSE)
          
          rcell(t_m$conf.int, format = "(xx.xxx, xx.xxx)")
        }
        
      }
    }, row.name = "95% CI", indent = 1)
  )
  
  
  # Partition


  tbl_partition <- if (is.null(parition_rsp_by)) {
    NULL
  } else {
    
    df <- data.frame(rsp = rsp, col_by = col_by)
    df.split <- split(df, parition_rsp_by, drop = FALSE)
  
    
    values <- lapply(split(col_by, parition_rsp_by, drop = FALSE),
      function(x) {
        x.x <- split(x, x)
        vals <- lapply(x.x, function(y) {
          n_arm <- sum(col_by == as.character(y[1]))
          n_y <- length(y)
          
          list(
            n_p = rcell(n_y * c(1, 1/n_arm), "xx.xx (xx.xx%)"),
            ci = rcell(binom.test(n_y, n_arm)$conf.int * 100, "(xx.xx, xx.xx)")
          )
        })
      }
    )
    
    tbls_part <- Map(function(vals, name) {
      rtable(
        header = levels(col_by),
        rrowl(name, lapply(vals, `[[`, "n_p")),
        rrowl("95% CI (Wald)", lapply(vals, `[[`, "ci"), indent = 1)
      )  
    }, values, names(values))
    
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

#' calculate odds ratio
#' 
#' @param x ...
#' @param pad.zeros ...
#' @param conf.level
#' 
#' @noRd
#' 
#' @examples 
#' 
#' 
odds.ratio <- function(x, pad.zeros=FALSE, conf.level=0.95) {
  if (pad.zeros) {
    if (any(x==0)) x <- x + 0.5
  }
  
  theta <- x[1,1] * x[2,2] / ( x[2,1] * x[1,2] )
  ASE <- sqrt(sum(1/x))
  CI <- exp(log(theta) + c(-1,1) * qnorm(0.5*(1+conf.level)) * ASE )
  
  list(
    estimator = theta,
    ASE = ASE,
    conf.interval = CI,
    conf.level = conf.level
  )
}
