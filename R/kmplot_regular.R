
#' kmPlot
#' 
#' descr
#' 
#' @param time_to_event time to event values
#' @param event boolean, \code{TRUE} if event and \code{FALSE} if censored
#' @param ref.arm character: reference arm 
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
#'    arm.ref = c("DUMMY C","DUMMY B"),
#'    arm.rest =  "DUMMY A"
#'    )


kmPlot <- function( time_to_event, event, arm, arm.ref, arm.rest,
                    stratum = NULL,  cox.tie = "efron", conf.int = FALSE, plot.median = FALSE, 
                    plot.nrisk = TRUE, time.interval = 5, nrisk.height = 0.25, size.nrisk = 4, 
                    plot.cens = FALSE, size.cens = 4.5, shape.cens = "+",
                    plot.stats = TRUE, size.stats = 4,
                    xystats.up = c(0.7, 0.8), xyinterval.up = c(0.1, 0.1),
                    xystats.lo = c(0.05, 0.3), xyinterval.lo = c(0.08, 0.1),
                    widget = c('N' = TRUE, 'Median(KM)' = TRUE, '95% CI Median' = TRUE,
                                'p-value' = TRUE, 'Hazard Ratio' = TRUE, '95% CI HR' = TRUE),
                    line.color = NULL, line.type = 1, line.width = 1,
                    xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, title = "Kaplan-Meier Plot",
                    legend.pos = c(0.9, 0.9) ){
  
  n <- length(time_to_event)
  if (length(event) != n) stop("event has wrong length")
  if (length(arm) != n) stop("arm has wrong length")
  arm_for_model <- arm_for_model2(arm, arm.ref, arm.rest)
  
  if (is.null(stratum)){
    stratum.df <- data.frame(stratum1 = rep(1, n))
    
  } else{
    stratum.df <- Reduce(data.frame, stratum)
    colnames(stratum.df) <- paste0("stratum", seq(1:length(stratum)))
  }
  

  cox_data <- subset(
    data.frame(
      time_to_event,
      event,
      arm = arm_for_model, 
      stratum.df
    ), arm %in% c(paste(arm.ref, collapse = "/"), arm.rest)
  )
  
   surv.fit <- survfit(Surv(time_to_event, event) ~ arm , data = cox_data)  ### need further update for stratified analysis
   
   ##### start of setting up plotting parameters
   trt.lev <- c(paste(arm.ref, collapse = "/"), arm.rest)
   if (is.null(line.color))    line.color <- seq(1, length(trt.lev))
   med.line <- ifelse(plot.median, "hv", "none")
   if(is.null(xlim)){
     xlim2 <- max(cox_data$time_to_event, na.rm = TRUE)
     xlim <- c(0, xlim2)
   }
   
   if(is.null(ylim)) ylim <- c(0,1)
   if(is.null(xlab)) xlab <- "Time to Event or Censoring"
   if(is.null(ylab)) ylab <- "Survival Probability"
   
   
   surv.plot <- ggsurvplot(surv.fit, data = cox_data, 
                           break.time.by = time.interval, conf.int = conf.int, 
                           surv.median.line = med.line, 
                           risk.table =  plot.nrisk, risk.table.title = "No. of Patients at Risk",
                           risk.table.col = "strata", risk.table.height = nrisk.height, 
                           risk.table.fontsize = size.nrisk, 
                           censor = plot.cens, censor.size = size.cens, censor.shape = shape.cens,
                           legend = legend.pos, legend.labs = trt.lev,
                           legend.title = "",
                           palette = line.color, linetype = line.type, size = line.width) 
   
   surv.plot$plot <- surv.plot$plot  + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))+
     coord_cartesian(ylim=ylim, xlim = xlim) + xlab(xlab) + ylab(ylab)
   
   
   km_sum <- summary(surv.fit, data = cox_data, conf.type = "plain")$table
   cox_sum <- summary(coxph(Surv(time_to_event, event) ~ arm , data = cox_data))
   
   upxstart  <- diff(xlim)*xystats.up[1]
   upxend    <- diff(xlim)*(xystats.up[1] + xyinterval.up[1]*length(trt.lev))
   upx.by    <- diff(xlim)*xyinterval.up[1]
   upystart  <- diff(ylim)*xystats.up[2]
   upy.by    <- diff(ylim)*xyinterval.up[2]
   
   lev.label <- c("",  trt.lev)
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
   
  return(surv.plot)
}


#' update Function from tm_forestplot.R. Suggest to put function arm_for_model2 in R script for uitility functions
#' re-factor arm variable by input arm.ref and arm.rest
arm_for_model2 <- function(arm, arm.ref, arm.rest) {
  
  if (!all(arm.ref %in% arm)) stop("not all arms in arm.ref are in arm")
  if (!all(arm.rest %in% arm)) stop("not all arms in arm.rest are in arm")
  
  name_arm_ref <- paste(arm.ref, collapse = "/")
  
  arm2 <- vapply(arm, function(x) {
    if (x %in% arm.ref) {
      name_arm_ref
    } else if (x %in% arm.rest) {
      x
    } else {
      "not possible"
    }
  }, character(1))
  
  factor(arm2, levels = c(name_arm_ref, arm.rest))
}

