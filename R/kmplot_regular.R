
#' kmPlot
#' 
#' descr
#' 
#' @param time_to_event time to event values
#' @param event boolean, \code{TRUE} if event and \code{FALSE} if censored
#' @param arm.ref character: reference arm 
#' 
#' 
#' @importFrom survival survfit Surv
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' library(atezo.data)
#' library(dplyr)
#' library(survival)
#' library(ggplot2)
#' library(survminer)
#' 
#' ATE <-  ate(com.roche.cdt30019.go29436.re)
#' 
#' ATE_filtered <- ATE %>% filter(PARAMCD == "OS")
#' kmPlot(
#'    time_to_event = ATE_filtered$AVAL,
#'    event = ATE_filtered$CNSR == 0,
#'    arm = ATE_filtered$ARM,
#'    arm.ref =  "DUMMY B" ,
#'    arm.rest =  c("DUMMY A", "DUMMY C" )
#'    )
#'    
#' kmPlot(
#'    time_to_event = ATE_filtered$AVAL,
#'    event = ATE_filtered$CNSR == 0,
#'    arm = ATE_filtered$ARM,
#'    facet_by = ATE_filtered$SEX,
#'    arm.ref =  "DUMMY B" ,
#'    arm.rest =  c("DUMMY A", "DUMMY C" )
#'    )
#'  }


kmPlot <- function( time_to_event, event, arm, arm.ref, arm.rest,
                    stratum = NULL, facet_by = NULL, n_col = 2, ... ){
  
  n <- length(time_to_event)
  if (length(event) != n) stop("event has wrong length")
  if (length(arm) != n) stop("arm has wrong length")
  arm_for_model <- arm_for_model2(arm, arm.ref, arm.rest)
  
  if (is.null(facet_by)){
    cox_data <- subset(
      data.frame(
        time_to_event,
        event,
        arm = arm_for_model
      ), arm %in% c(paste(arm.ref, collapse = "/"), arm.rest)
    )
    surv.plot <- kmPlot_anno(cox_data, ...)
    
  } else{
    cox_data <- subset(
      data.frame(
        time_to_event,
        event,
        arm = arm_for_model,
        facet_by
      ), arm %in% c(paste(arm.ref, collapse = "/"), arm.rest)
    )
    
    facet_lev <- unique(cox_data$facet_by)
    plot_list <- lapply(facet_lev, function(lev){
     
      kmPlot_anno(cox_data %>% filter(facet_by == lev), ...) 
    })
    n_row <- ceiling(length(facet_lev)/n_col)
    
    surv.plot <-  arrange_ggsurvplots(plot_list, ncol = n_col, nrow = n_row )
   
  }
  
  return(surv.plot)
}


#' update Function from tm_forestplot.R. Suggest to put function arm_for_model2 in R script for uitility functions
#' re-factor arm variable by input arm.ref and arm.rest
arm_for_model2 <- function(arm, arm.ref, arm.rest){
  
  if (!all(arm.ref %in% arm)) stop("not all arms in arm.ref are in arm")
  if (!all(arm.rest %in% arm)) stop("not all arms in arm.rest are in arm")
  
  name_arm_ref <- paste(arm.ref, collapse = "/")
  
  arm2 <- vapply(arm, function(x) {
    if (x %in% arm.ref) {
      name_arm_ref
    } else if (x %in% arm.rest) {
      x
    } else {
      NA_character_
    }
  }, character(1))
  
  factor(arm2, levels = c(name_arm_ref, arm.rest))
}

#' Statistics from KM estimators and Cox PH modeling
#'  
#'  

kmPlot_anno <- function(data, 
                        cox.tie = "efron", conf.int = FALSE, plot.median = FALSE, 
                        plot.nrisk = TRUE, time.interval = 4, nrisk.height = 0.2, size.nrisk = 4,
                        plot.cens = TRUE, size.cens = 4.5, shape.cens = "+",
                        plot.stats = TRUE, size.stats = 3,
                        xystats.up = c(0.7, 0.8), xyinterval.up = c(0.08, 0.06),
                        xystats.lo = c(0.05, 0.3), xyinterval.lo = c(0.07, 0.06),
                        line.color = NULL, line.type = 1, line.width = 1,
                        xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, plot.title = "Kaplan-Meier Plot",
                        legend.pos = "none" ){
  surv.fit <- survfit(Surv(time_to_event, event) ~ arm , data = data)
  arm.lev <- levels(data$arm)
  if (is.null(line.color))    line.color <- seq(1, length(arm.lev))
  med.line <- ifelse(plot.median, "hv", "none")
  if(is.null(xlim)){
    xlim2 <- max(data$time_to_event, na.rm = TRUE)
    xlim <- c(0, xlim2)
  }

  if(is.null(ylim)) ylim <- c(0,1)
  if(is.null(xlab)) xlab <- "Time to Event or Censoring"
  if(is.null(ylab)) ylab <- "Survival Probability"


  
  surv.plot <- ggsurvplot(surv.fit, data = data,
                          break.time.by = time.interval, conf.int = conf.int,
                          surv.median.line = med.line,
                          risk.table =  plot.nrisk, risk.table.title = "No. of Patients at Risk",
                          risk.table.col = "strata", risk.table.height = nrisk.height,
                          risk.table.fontsize = size.nrisk,
                          censor = plot.cens, censor.size = size.cens, censor.shape = shape.cens,
                          legend = legend.pos, legend.labs = arm.lev,
                          legend.title = "",
                          palette = line.color, linetype = line.type, size = line.width,
                          title = plot.title, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab )
  
  km_sum <- summary(surv.fit, data = data, conf.type = "plain")$table
  cox_sum <- summary(coxph(Surv(time_to_event, event) ~ arm , data = data, ties = cox.tie))

  upxstart  <- diff(xlim)*xystats.up[1]
  upxend    <- diff(xlim)*(xystats.up[1] + xyinterval.up[1]*length(arm.lev))
  upx.by    <- diff(xlim)*xyinterval.up[1]
  upystart  <- diff(ylim)*xystats.up[2]
  upy.by    <- diff(ylim)*xyinterval.up[2]
     #### should add check for text position: if the xend is beyond upper xlim, give warning message
  lev.label <- c("",  arm.lev)
  N.label <- c("N", km_sum[ , "records"])
  med <- as.character(round(km_sum[ , "median"], 2)); med[is.na(med)] <- "NA"
  med.label <- c('Median(KM)', med)
  medci <- paste0("(", round(km_sum[ , "0.95LCL"], 2),"; ", round(km_sum[ , "0.95UCL"], 2), ")")
  medci.label <- c('95% CI', medci)
  surv.plot$plot <- surv.plot$plot + annotate("text", x = seq(upxstart, upxend, by = upx.by),
                                              y = upystart, label = lev.label, hjust = 0, size = size.stats) +
                                     annotate("text", x = seq(upxstart, upxend, by = upx.by),
                                              y = upystart - upy.by, label = N.label, hjust = 0, size = size.stats) +

                                     annotate("text", x = seq(upxstart, upxend, by = upx.by),
                                              y = upystart - 2*upy.by, label = med.label, hjust = 0, size = size.stats) +
                                     annotate("text",x = seq(upxstart, upxend, by = upx.by),
                                              y = upystart - 3*upy.by, label = medci.label, hjust = 0, size = size.stats)

  model.label <- c("Cox PH:", "Unstratified")  ### need to update if stratum is not NULL
  compare.label <- c("", arm.lev[-1])
  hr.label <-  c("HR", as.numeric(round(cox_sum$coefficients[,"exp(coef)"], 2)))
  hrci.label <- c("95% CI(HR)",
           paste0("(", as.numeric(round(cox_sum$conf.int[,"lower .95"], 2)), "; ",
                  as.numeric(round(cox_sum$conf.int[,"upper .95"], 2)), ")"))
  pval <- as.numeric(cox_sum$coefficients[, "Pr(>|z|)"])
  pval.label <- c("p-value", ifelse(pval <= 0.0001, '<0.001', round(pval, 3)))


  loxstart  <- diff(xlim)*xystats.lo[1]
  lox.by    <- diff(xlim)*xyinterval.lo[1]
  loystart  <- diff(ylim)*xystats.lo[2]
  loyend    <- diff(ylim)*(xystats.lo[2] - xyinterval.lo[2]*(length(arm.lev) -1))
  loy.by    <- diff(ylim)*xyinterval.lo[2]
  surv.plot$plot <- surv.plot$plot + annotate("text", x = loxstart,
                                              y = seq(loystart, loyend, by = -loy.by),
                                              label = compare.label, vjust = 0, size = size.stats) +
                                     annotate("text", x = loxstart + lox.by,
                                              y = seq(loystart, loyend, by = -loy.by), label = hr.label,
                                              vjust=0, size = size.stats) +
                                     annotate("text", x = loxstart + 2*lox.by,
                                              y = seq(loystart, loyend, by = -loy.by), label = hrci.label,
                                              vjust=0, size = size.stats) +
                                     annotate("text", x = loxstart + 3*lox.by,
                                              y = seq(loystart, loyend, by = -loy.by), label = pval.label,
                                              vjust=0, size = size.stats) +
                                     annotate("text", x = c(loxstart, loxstart + lox.by),
                                              y = loystart + loy.by, label = model.label,
                                              vjust = 0, size = size.stats)
  return(surv.plot)
}

