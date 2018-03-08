#' Response Table
#' 
#' The response table function summarizes response data by groups. The function 
#' produces frequency counts and rates for responders and each response 
#' categories, as well as conducts comparisons between groups' response rates 
#' and odds ratio.
#' 
#' @template param_rsp
#' @template param_col_by
#' @param partition_rsp_by factor with one or more response categories, generate
#'   additional statistics partitioned by each response categories. If 
#'   \code{NULL}, tabulation by each response categories will not be performed.
#' @param strata_data data for stratification factors (categorical variables). 
#'   If \code{NULL}, no stratified analysis is performed. See details for 
#'   further explanation.
#'   
#' @details For the test of difference in response rates, Wald confidence 
#'   interval with and without continuity correction are both reported. If no 
#'   stratification factors are specified, Chi-squared test is performed by 
#'   default to test for equality of proportions in response rate between 
#'   reference and comparison groups. If stratification factors are added, 
#'   Cochran-Mantel-Haensel test is performed instead. Statistics from the 
#'   stratificaiton-adjusted test will be reported for the p-value of test of 
#'   equal proportions, odds ratio and its corresponding 95\% confidence 
#'   interval.
#'   
#'   The display order of response categories in partitioned statistics section 
#'   inherits the factor level order of \code{partition_rsp_by}. Use 
#'   \code{\link[base]{factor()}} and its \code{levels} argument to include or 
#'   exclude response categories and arrange display order. If response values 
#'   contains missing or "Not Evaluable (NE)", 95\% confidence interval will not
#'   be calculated.
#'   
#' @template return_rtable
#'   
#' @importFrom stats binom.test prop.test mantelhaen.test
#'   
#' @export
#' 
#' @template author_liaoc10
#'   
#' @examples 
#' 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL", arm_choices = c("ARM A", "ARM B", "ARM C"), N = 1000, start_with = list(RACE = c("white", "asian")))
#' ARS <- radam("ARS", ADSL = ASL)
#' ANL <- merge(ASL, subset(ARS, PARAMCD == "OVRSPI"))
#' 
#' # Example 1 - ARM B as reference
#' #             "NON CR/PD" response category dropped from partition section since no observations
#' #             model with no stratifiaction factors, Chi-square test is performed
#' tbl <- t_rsp(
#'  rsp = ANL$AVALC %in% c("CR", "PR"),
#'  col_by = relevel(factor(ANL$ARMCD), "ARM B"),
#'  partition_rsp_by = droplevels(factor(ANL$AVALC, levels = c("CR", "PR", "SD", "NON CR/PD", "PD", "NE")))
#' )
#' tbl; Viewer(tbl)
#' 
#' # Example 2 - ARM B as reference, ARM C displayed before ARM A
#' #             "NON CR/PD" response category displayed in partition section, "NE" responses are not displayed 
#' #             model with two stratifiaction factors, CMH test performed
#' tbl2 <- t_rsp(
#'  rsp = ANL$AVALC %in% c("CR", "PR"),
#'  col_by = factor(ANL$ARMCD, c("ARM B", "ARM C", "ARM A")),
#'  partition_rsp_by = factor(ANL$AVALC, levels = c("CR", "PR", "SD", "NON CR/PD", "PD")),
#'  strata_data = ANL[c("RACE")]
#' )
#' tbl2; Viewer(tbl2)
#' 
t_rsp <- function(
  rsp,
  col_by,
  partition_rsp_by = NULL,
  strata_data = NULL) {
  
  # Argument checking
  # #################
  check_same_N(rsp = rsp, col_by = col_by, partition_rsp_by = partition_rsp_by, strata_data = strata_data)
  
  if (!is.logical(rsp)) stop("rsp is expected to be logical")
  if (any(is.na(rsp))) stop("rsp can not have any NAs")
  
  check_col_by(col_by, min_num_levels = 2)
  
  if (!is.null(strata_data)) {
    check_data_frame(strata_data)
  }

  # Calculations for table in sections
  #####################################
  # Responder section
  tbl_response <- rbind(
    rtabulate(rsp, col_by = col_by, positives_and_proportion, format = "xx.xx (xx.xx%)",
              row.name = "Responders"),
    rtabulate(!rsp, col_by = col_by, positives_and_proportion, format = "xx.xx (xx.xx%)",
              row.name = "Non-Responders")
  )
  
  # Response Rate section
  tbl_clopper_pearson <- rtabulate(
    x = rsp,
    col_by = col_by,
    function(x) {
      binom.test(sum(x), length(x))$conf.int * 100 
    },
    format = "(xx.xx, xx.xx)",
    row.name = "95% CI for Response Rates (Clopper-Pearson)"
  )
  
  # Difference in Response Rates section
  tbl_difference <- rbind(
    # by is a factor with two levels: 1st ref arm, 2nd comp arm
    tabulate_pairwise(rsp, col_by, function(x, by) {
      
      diff(tapply(x, by, mean)) * 100
      
    }, format = "xx.xx", row.name = "Difference in Response Rates (%)"),
    
    # wald test without continuity correction
    tabulate_pairwise(rsp, col_by, function(x, by) {

      t_wc <- prop.test(table(x, by), correct = FALSE)
      
      rcell(t_wc$conf.int * 100, format = "(xx.xx, xx.xx)")
    },
    indent = 1,
    row.name = "95% CI for difference (Wald without correction)"),
    
    # wald test with  continuety correction
    tabulate_pairwise(rsp, col_by, function(x, by) {
      
      t_wc <- prop.test(table(x, by), correct = TRUE)
      
      rcell(t_wc$conf.int * 100, format = "(xx.xx, xx.xx)")
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
          t.tbl.sub <- t.tbl[match(levels(by), dimnames(t.tbl)$col_by),,]
          
          t_m <- mantelhaen.test(t.tbl.sub, correct = FALSE)
          
          rcell(t_m$p.value, format = "xx.xxxx")
        }
        
      }, 
      indent = 1,
      row.name = "p-value (Cochran-Mantel-Haenszel)*")
    }
  )
  
  # Odds Ratio
  tbl_odds_ratio <- rbind(
    tabulate_pairwise(rsp, col_by, function(x, by) {
      if (is.null(strata_data)) {
        
        fit <- odds.ratio(table(by, x))
        rcell(fit$estimator, "xx.xx")
        
      } else {
        
        strat <- do.call(strata, strata_data)
        
        if (any(tapply(rsp, strat, length)<5)) {
          rcell("<5 data points")
        } else {
          t.tbl <- table(col_by, rsp, strat)
          t.tbl.sub <- t.tbl[match(levels(by), dimnames(t.tbl)$col_by),,]
          
          t_m <- mantelhaen.test(t.tbl.sub, correct = FALSE)
          
          rcell(t_m$estimate, format = "xx.xx")
        }
        
      }
    }, row.name = ifelse(is.null(strata_data), "Odds Ratio", "Odds Ratio*")),
    
    tabulate_pairwise(rsp, col_by, function(x, by) {
      if (is.null(strata_data)) {
        
        fit <- odds.ratio(table(by, x))
        rcell(fit$conf.int, "(xx.xx, xx.xx)")
        
      } else {
        
        strat <- do.call(strata, strata_data)
        
        if (any(tapply(rsp, strat, length)<5)) {
          rcell("<5 data points")
        } else {
          t.tbl <- table(col_by, rsp, strat)
          t.tbl.sub <- t.tbl[match(levels(by), dimnames(t.tbl)$col_by),,]
          
          t_m <- mantelhaen.test(t.tbl.sub, correct = FALSE)
      
          rcell(t_m$conf.int, format = "(xx.xx, xx.xx)")
        }
        
      }
    }, row.name = "95% CI", indent = 1)
  )
  
  
  # Partition by response categories
  tbl_partition <- if (is.null(partition_rsp_by)) {
    NULL
  } else {
    
    values <- lapply(split(col_by, partition_rsp_by, drop = FALSE),
      function(x) {
        
        x <- factor(x, levels = levels(col_by))
        x.x <- split(x, x, drop = FALSE)
        
        vals <- Map(function(y, arm) {
          
          n_arm <- sum(col_by == arm)
          n_y <- length(y)
          
          if (is.na(n_arm) || n_arm == 0) {
            list(n_p = rcell("-"), ci = rcell("-"))
          } else if (n_y == 0) {
            list(n_p = rcell(n_y, "xx.xx"), 
                 ci = rcell("-"))
          } else {
            list(
              n_p = rcell(n_y * c(1, 1/n_arm), "xx.xx (xx.xx%)"),
              ci = rcell(binom.test(n_y, n_arm)$conf.int * 100, "(xx.xx, xx.xx)")
            )
          }
        }, x.x, names(x.x))
        
      }
    )
    
    #Display full labels for responses in controlled codelist 
    rsp_full_label <- c(CR          = "Complete Response (CR)",
                        PR          = "Partial Response (PR)",
                        SD          = "Stable Disease (SD)",
                        `NON CR/PD` = "Non-CR or Non-PD (NON CR/PD)",
                        PD          = "Progressive Disease (PD)",
                        NE          = "Not Evaluable (NE)",
                        Missing     = "Missing",
                        `NE/Missing` = "Missing or unevaluable"
    )
    
    values_label <- ifelse(names(values) %in% names(rsp_full_label),
                           rsp_full_label[names(values)],
                           names(values))
    
    tbls_part <- Map(function(vals, name) {
      rtable(
        header = levels(col_by),
        rrowl(name, lapply(vals, `[[`, "n_p")),
        if (name %in% c("Not Evaluable (NE)", "Missing or unevaluable", "Missing")) {rrow(NULL)
          } else {rrowl("95% CI", lapply(vals, `[[`, "ci"), indent = 1)}
      ) 
      
    }, values, values_label)
    
    stack_rtables_l(tbls_part) 
  }
  
  
  #--- Footer section, if any for notes on stratification ---#
  if (is.null(strata_data)) {
    tbl_footer <- rtable(header = levels(col_by), rrow(NULL))
  } else {
    n_strata <- length(strata_data)
    tbl_footer <- rtable(header = levels(col_by), rrow(paste("* Model stratified by", 
                             ifelse(n_strata < 2, names(strata_data), 
                                    paste(paste(names(strata_data)[-(n_strata)], collapse = ", "), "and", names(strata_data)[(n_strata)]))
    )))
  }
  
  
  tbl <- stack_rtables(
    tbl_response,
    tbl_clopper_pearson,
    tbl_difference,
    tbl_odds_ratio,
    tbl_partition,
    tbl_footer
  )
  
  # add N to header 
  N <- tapply(col_by, col_by, length)
  header(tbl) <- rheader(
    rrowl("", levels(col_by)),
    rrowl("", paste0("(N=",N,")"))
  )
  
  tbl
}

#' Function to calculate odds ratio and confidence interval
#' 
#' @param x a matrix or table of 2-by-2 dimensions
#' @param conf.level confidence level for the returned confidence interval
#' 
#' @noRd
#' 
#' @examples 
#' 
#' odds.ratio(table(mtcars$vs, mtcars$am))
#' odds.ratio(matrix(c(12,6,7,7), nrow=2, byrow=T), conf.level = 0.90)
#' 
odds.ratio <- function(x, conf.level=0.95) {

  theta <- x[1,1] * x[2,2] / ( x[2,1] * x[1,2] )
  #Asymptotic standard error
  ASE <- sqrt(sum(1/x))
  CI <- exp(log(theta) + c(-1,1) * qnorm(0.5*(1+conf.level)) * ASE )
  
  list(
    estimator = theta,
    se = ASE,
    conf.int = CI,
    conf.level = conf.level
  )
}
