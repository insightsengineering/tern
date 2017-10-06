
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
#'    arm.ref = "DUMMY A",
#'    arm.rest = "DUMMY B"
#'    )


kmPlot <- function( time_to_event, event, arm, arm.ref, arm.rest,
                    strata.var = NULL, cox.tie = "efron", conf.int = FALSE, plot.median = FALSE, 
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
  cox_data <- subset(
    data.frame(
      time_to_event,
      event,
      arm = arm_for_model
    ), arm %in% c(paste(arm.ref, collapse = "/"), arm.rest)
  )
  
  surv.fit1 <- survfit(Surv(time_to_event, event) ~ arm, data = cox_data)
  surv.plot <- ggsurvplot(surv.fit1, data = cox_data)
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