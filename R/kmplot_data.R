#' Prepare necessary data for Kaplan-Meier Plot 
#' 
#' Return a list of data for primitive element of grid drawing
#' 
#' @param formula_km formula specified for Kaplan-Meier curves.
#' @param data analysis data set.
#' @param xaxis_by break interval of x-axis.
#' 
#' @import survival
#' @importFrom scales col_factor
#' 
#' @export
#' 
#' 
#' @examples 
#' 
#' library(survival)
#' library(scales)
#' 
#' OS <- data.frame(AVAL = abs(rnorm(200)), 
#'                  CNSR = sample(c(0, 1), 200, TRUE), 
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#' kmCurveData(Surv(AVAL, 1-CNSR) ~ ARM, data = OS)


kmCurveData <- function(formula_km, data, xaxis_by = NULL){
  
  fit <- survfit(formula_km, data = data, conf.type = "plain")
  ngroup <- length(fit$strata)
  if (ngroup > 9) stop("unfortunately we currently do not have more than 9 colors to encode different groups")
  
  # extract kmplot relevant data
  df <- data.frame(
    time = fit$time,
    surv = fit$surv,
    n.risk = fit$n.risk,
    n.censor = fit$n.censor,
    n.event = fit$n.event,
    std.err = fit$std.err,
    upper = fit$upper,
    lower = fit$lower,
    group = factor(rep(names(fit$strata), fit$strata), levels = names(fit$strata))
  )
  
  # split by group
  df_s <- split(df, df$group)
  group <- fit$strata
  
  ### width of label in Risk table
  tmp.labels <- names(group)
  tmp.label <- tmp.labels[which.max(nchar(tmp.labels))[1]]
  nlines_labels <- convertWidth(stringWidth(tmp.label), "lines", TRUE) + 2
  
  
  ### interval in x-axis
  xpos <- seq(0, floor(max(df$time)), by = ifelse(is.null(xaxis_by), 
                                                  max(1, floor(max(df$time)/10)), 
                                                  xaxis_by))
  
  ypos <- 1 - 1:length(df_s)/(length(df_s) + 1)
  
  pt_risk <- lapply(df_s, function(x){
    n.r <- vapply(xpos, function(xi) {
      if (xi <= min(x$time)){
        i <- head(which(x$time >= xi), 1)
        x$n.risk[i]
      } else if (xi > max(x$time)){
        NA
      } else{
        i <- tail(which(x$time <= xi), 1)
        x$n.risk[i] - x$n.censor[i] - x$n.event[i]
      }
    }, numeric(1))
  })
  
  
  xData <- c(0, df$time)
  lines_x <- lapply(df_s, function(x){
    c(0, rep(x$time, each = 2))
  })
  lines_y <- lapply(df_s, function(x){
    c(rep(c(1, head(x$surv, -1)), each = 2), tail(x$surv, 1))
  })
  
  points_x <- lapply(df_s, function(x){
    x[x$n.censor !=0, "time"]
  })
  
  points_y <- lapply(df_s, function(x){
    x[x$n.censor !=0, "surv"]
  })
  
  col_pal <- col_factor("Set1", domain = names(df_s))
  col <- col_pal(names(df_s))
  return(list(nlines_labels = nlines_labels,
              xpos = xpos,
              xData = xData,
              lines_x = lines_x, 
              lines_y = lines_y,
              points_x = points_x,
              points_y = points_y,
              col_pal = col,
              group = group,
              ypos = ypos,
              pt_risk = pt_risk))
}



#' Prepare K-M model annotation data with rtable format
#' 
#' An rtable format of KM model data for further annotation on top of Kaplan-Meier grob
#' 
#' @param formula_km formula specified for Kaplan-Meier curves.
#' @param data analysis data set.
#' 
#' @import survival
#' @import rtables
#' 
#' @export
#' 
#' @examples 
#' library(survival)
#' library(rtables)
#' 
#' OS <- data.frame(AVAL = abs(rnorm(200)), 
#'                  CNSR = sample(c(0, 1), 200, TRUE), 
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#' kmAnnoData(Surv(AVAL, 1-CNSR) ~ ARM, data = OS)

kmAnnoData <- function(formula_km, data ){
  
  fit <- survfit(formula_km, data = data, conf.type = "plain")
  kminfo <- summary(fit)$table[ , c("records", "median", "0.95LCL", "0.95UCL")]
  skminfo <- split(as.data.frame(kminfo), 1:nrow(kminfo))
  tblkm <- do.call(
    rtable,
    c(
      list(col.names = c("N", "median", "95% CI for median")),
      lapply(skminfo, function(xi) {
        rrow(
          row.name = rownames(xi),
          rcell(xi$records, format = "xx"),
          rcell(xi$median, format = "xx.xx"),
          rcell(c(xi$`0.95LCL`, xi$`0.95UCL`), format = "(xx.xx, xx.xx)")
        )
      })
    )
  )
  tblstr <- toString(tblkm, gap = 1)
  return(tblstr)
}



#' Prepare Cox PH model annotation data with rtable format
#' 
#' An rtable format of Cox PH model data for further annotation on top of Kaplan-Meier grob
#' 
#' @param formula_coxph formula specified for Cox PH model.
#' @param data analysis data set.
#' @param cox_ties ties handling method for Cox PH model. options are "efron", "breslow" and "exact".
#' @param info_coxph label information for Cox PH model.
#' 
#' @import survival
#' @import rtables
#' 
#' @export
#' 
#' @examples 
#' library(survival)
#' library(rtables)
#' 
#' OS <- data.frame(AVAL = abs(rnorm(200)), 
#'                  CNSR = sample(c(0, 1), 200, TRUE), 
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#' coxphAnnoData(Surv(AVAL, 1-CNSR) ~ ARM + strata(RACE), data = OS)

coxphAnnoData <- function(formula_coxph, data, cox_ties = "exact", info_coxph = "Cox Porportional Model"){
  
  fitcox <- coxph(formula_coxph, data = data, ties = cox_ties)
  
  sfit <- summary(fitcox)
  
  hr <- sfit$coefficients[, "exp(coef)", drop = FALSE]  
  
  ci <- sfit$conf.int[, c("lower .95", "upper .95"), drop = FALSE]  
  
  pvalues <- sfit$coefficients[, "Pr(>|z|)", drop = FALSE]  
  
  
  info <- cbind(hr, ci, pvalues)
  sinfo <- split(as.data.frame(info), 1:nrow(info))
  
  tbl <- do.call(
    rtable,
    c(
      list(col.names = c("HR", "95% CI of HR", "Wald p-value")),
      lapply(sinfo, function(xi) {
        rrow(
          row.name = rownames(xi),
          rcell(xi$'exp(coef)', format = "xx.xxxx"),
          rcell(c(xi$`lower .95`, xi$`upper .95`), format = "(xx.xxxx, xx.xxxx)"),
          rcell(xi$'Pr(>|z|)', format = "xx.xxxx")
        )
      })
    )
  )
  tblstr <- toString(tbl, gap = 1)
  tblstr2 <- paste0(info_coxph, "\n", tblstr)
  #### add score test p-value for overall model
  scpval <- sfit$sctest["pvalue"] %>% round(digits = 4) %>% paste0("Overall Score p-value: ", . )
  tblstr3 <- paste0(scpval, "\n", tblstr2)
  
  ##### add log-rank test for modeling formula same as coxph
  mdl <- survdiff(formula_coxph, data = data)
  logr_p <- pchisq(mdl$chisq, length(mdl$n) - 1, lower.tail = FALSE)
  lab <- paste0(paste0("Log-rank test p-value: ", as.character(round(logr_p, 4))), "\n", tblstr3)
  return(lab)
}



#' Prepare Independent Cox PH model annotation data with rtable format
#' 
#' An rtable format of Indedendent Cox PH model (spcially for IMPower 131 study) data for further annotation on top of Kaplan-Meier grob
#' 
#' @param formula_coxph formula specified for Cox PH model.
#' @param data_list a list of analysis data set.
#' @param cox_ties ties handling method for Cox PH model. options are "efron", "breslow" and "exact".
#' @param info_coxph label information for Cox PH model.
#' 
#' @import survival
#' @import rtables
#' 
#' @export


coxphAnnoImpower131 <- function(formula_coxph, data_list, cox_ties = "exact", info_coxph = "Cox Porportional Model"){
  
  sinfo <- sapply(data_list, function(x){
    fitcox <- coxph(formula_coxph, data = x, ties = cox_ties)
    
    sfit <- summary(fitcox)
    
    hr <- sfit$coefficients[, "exp(coef)", drop = FALSE]  
    
    ci <- sfit$conf.int[, c("lower .95", "upper .95"), drop = FALSE]  
    
    pvalues <- sfit$coefficients[, "Pr(>|z|)", drop = FALSE]  
    
    scpval <- sfit$sctest["pvalue"] %>% rep(., nrow(pvalues)) %>% as.matrix
    colnames(scpval) <- "scorepval"
    
    info <- cbind(hr, ci, pvalues, scpval)
    sinfo <- split(as.data.frame(info), 1:nrow(info))
  })
  
  tbl <- do.call(
    rtable,
    c(
      list(col.names = c("HR*", "95% CI of HR*", "Wald p-value*",  "Score p-val*")),
      lapply(sinfo, function(xi) {
        rrow(
          row.name = rownames(xi),
          rcell(xi$'exp(coef)', format = "xx.xxxx"),
          rcell(c(xi$`lower .95`, xi$`upper .95`), format = "(xx.xxxx, xx.xxxx)"),
          rcell(xi$'Pr(>|z|)', format = "xx.xxxx"),
          rcell(xi$'scorepval', format = "xx.xxxx")
        )
      })
    )
  )
  tblstr <- toString(tbl, gap = 1)
  tblstr2 <- paste0(info_coxph, "\n", tblstr, "\n *From Seperate Model for each Comparision group")
  return(tblstr2)
}
