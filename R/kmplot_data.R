#' Prepare necessary data for Kaplan-Meier Plot 
#' 
#' Return a list of data for primitive element of grid drawing
#' 
#' @param fit_km a class \code{\link{survfit}} object.
#' @param xticks break interval of x-axis. It takes a numeric vector or \code{NULL}.
#' 
#' @import survival
#' @importFrom scales col_factor
#' 
#' @export 
#' 
#' @examples 
#' 
#' 
#' OS <- data.frame(AVAL = abs(rnorm(200)), 
#'                  CNSR = sample(c(0, 1), 200, TRUE), 
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = OS, conf.type = "plain")
#' kmCurveData(fit_km, xticks = c(0.5, 0.8, 1.5))
#' 
#' 
kmCurveData <- function(fit_km, xticks = NULL) {
  
  if (!is(fit_km, "survfit")) stop("fit_km needs to be of class survfit")
   
  ngroup <- length(fit_km$strata)
  if (ngroup > 9) stop("unfortunately we currently do not have more than 9 colors to encode different groups")
  
  # extract kmplot relevant data
  df <- data.frame(
    time = fit_km$time,
    surv = fit_km$surv,
    n.risk = fit_km$n.risk,
    n.censor = fit_km$n.censor,
    n.event = fit_km$n.event,
    std.err = fit_km$std.err,
    upper = fit_km$upper,
    lower = fit_km$lower,
    group = factor(rep(names(fit_km$strata), fit_km$strata), levels = names(fit_km$strata))
  )
  
  # split by group
  df_s <- split(df, df$group)
  group <- fit_km$strata
  
  ### width of label in Risk table
  tmp.labels <- names(group)
  tmp.label <- tmp.labels[which.max(nchar(tmp.labels))[1]]
  nlines_labels <- convertWidth(stringWidth(tmp.label), "lines", TRUE) + 2
  
  
  ### interval in x-axis
  if (length(xticks) <= 1){
    xpos <- seq(0, floor(max(df$time)), by = ifelse(is.null(xticks), 
                                                    max(1, floor(max(df$time)/10)), 
                                                    xticks))
  } else {
    xpos <-  c(0, xticks)
  }
  
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
  
  col_pal <- scales::col_factor("Set1", domain = names(df_s))
  colpal <- col_pal(names(df_s))
  return(list(nlines_labels = nlines_labels,
              xpos = xpos,
              xData = xData,
              lines_x = lines_x, 
              lines_y = lines_y,
              points_x = points_x,
              points_y = points_y,
              colpal = colpal,
              group = group,
              ypos = ypos,
              pt_risk = pt_risk))
}



#' Prepare K-M model annotation data with rtable format
#' 
#' An \code{\link[rtables]{rtable}} format of KM model data for further annotation on top of Kaplan-Meier grob
#' 
#' @param fit_km a class \code{\link{survfit}} object.
#' 
#' @import survival
#' @import rtables
#' 
#' @export
#' 
#' @examples 
#' 
#' library(dplyr)
#' library(rtables)
#' OS <- data.frame(AVAL = abs(rnorm(200)), 
#'                  CNSR = sample(c(0, 1), 200, TRUE), 
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#' fit_km <- survfit(Surv(AVAL, 1-CNSR) ~ ARM, data = OS, conf.type = "plain")
#' tbl <- t_km(fit_km)
#' Viewer(tbl)
#' 
t_km <- function(fit_km) {
  if (!is(fit_km, "survfit")) stop("fit_km needs to be of class survfit")
  kminfo <- summary(fit_km)$table[ , c("records", "median", "0.95LCL", "0.95UCL")]
  skminfo <- split(as.data.frame(kminfo), 1:nrow(kminfo))
  tbl  <- do.call(
    rtable,
    c(
      list(header = c("N", "median", "95% CI for median")),
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
 tbl 
}



#' Prepare Cox PH model annotation data with rtable format
#' 
#' An \code{\link[rtables]{rtable}} format of \code{\link{coxph}} object for further annotation on top of Kaplan-Meier grob
#' 
#' @param fit_coxph a class \code{\link{coxph}} object.
#' @param info_coxph label information for Cox PH model.
#' 
#' @import survival
#' @import rtables
#' 
#' @export
#' 
#' @examples 
#' library(dplyr)
#' library(rtables)
#' OS <- data.frame(AVAL = abs(rnorm(200)), 
#'                  CNSR = sample(c(0, 1), 200, TRUE), 
#'                  ARM = sample(LETTERS[1:3], 200, TRUE),
#'                  SEX = sample(c("M","F"), 200, TRUE),
#'                  RACE = sample(c("AA", "BB", "CC"), 200, TRUE),
#'                  ECOG = sample(c(0, 1), 200, TRUE))
#' fit_coxph <- coxph(Surv(AVAL, 1-CNSR) ~ ARM + strata(RACE), data = OS, ties = "exact")
#' tbl <- t_coxph(fit_coxph)
#' Viewer(tbl)
#' 

t_coxph <- function(fit_coxph, info_coxph = "Cox Porportional Hazard Model"){
  
  if (!is(fit_coxph, "coxph")) stop("fit_coxph needs to be of class coxph")
  sfit <- summary(fit_coxph)
  
  hr <- sfit$coefficients[, "exp(coef)", drop = FALSE]  
  
  ci <- sfit$conf.int[, c("lower .95", "upper .95"), drop = FALSE]  
  
  pvalues <- sfit$coefficients[, "Pr(>|z|)", drop = FALSE]  
  
  
  info <- cbind(hr, ci, pvalues)
  sinfo <- split(as.data.frame(info), 1:nrow(info))
  
  tbl <- do.call(
    rtable,
    c(
      list(header = c("HR", "95% CI of HR", "Wald p-value")),
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
 tbl
}


