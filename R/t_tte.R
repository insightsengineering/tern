#' Time to Event Table
#' 
#' This is the description of the function
#' 
#' @param time_to_event time to event data
#' @param is_event boolean with \code{TRUE} if event and \code{FALSE} if censored
#' 
#' 
#' @return a named list with one element per row where each element contains the
#'   data for the row
#'
#' @importFrom survival Surv survfit survdiff
#' @export
#' 
#' 
#' @examples 
#' 
#' library(random.cdisc.data)
#' library(forcats)
#' 
#' ASL <- radam("ASL")
#' ATE <- merge(ASL, radam("ATE", ADSL = ASL))
#' 
#' ATE_f <- subset(ATE, PARAMCD == "OS")
#' 
#' 
#' tbl <- t_tte(
#'   tte = ATE_f$AVAL,
#'   is_event = ATE_f$CNSR == 0,
#'   event_descr = factor(ATE_f$EVNTDESC),
#'   col_by = factor(ATE_f$ARM),
#'   strata_data = ATE_f[, c('SEX', 'RACE')],
#'   time_points = 6,
#'   time_unit = "month"
#' )
#' 
#' tbl
#' 
t_tte <- function(tte,
                  is_event,
                  event_descr,
                  col_by,
                  strata_data,
                  time_points,
                  time_unit = "month",
                  ties = "exact") {
  
  # Argument Checking
  check_same_N(tte = tte, is_event = is_event,
               event_descr = event_descr, col_by = col_by,
               strata_data = strata_data)

  check_col_by(col_by, 2)
  check_data_frame(strata_data)
  
  if (!is.null(event_descr) && !is.factor(event_descr))
    stop("event_descr is required to be a factor") 

  if (!is.null(time_points) && !is.numeric(time_points))
    stop("time_points is required to be numeric")


  N <- tapply(col_by, col_by, length)
  
  # Event Table
  # ###########
  tbl_event <- rbind(
    rtabulate(is_event, col_by, positives_and_proportion, format = "xx.xx (xx.xx%)",
              row.name = "Patients without event (%)"),
    if (!is.null(event_descr)) {
      rbind(
        rtable(levels(col_by), rrow("Earliest Contributing Event", indent = 1)),
        rtabulate(as.factor(event_descr), col_by, length, indent = 2)      
      )
    } else {
      NULL
    },
    rtabulate(!is_event, col_by, positives_and_proportion, format = "xx.xx (xx.xx%)",
              row.name = "Patients without event (%)")
  )
   
  # Time to Event
  # #############
  surv_km_fit <- survfit(
    formula = Surv(tte, is_event) ~ col_by, 
    conf.type = "plain"
  )
  
  srv_tbl <- summary(surv_km_fit)$table
  med <- as.list(srv_tbl[, "median"])
  ci <- Map(function(x,y) c(x,y), srv_tbl[, "0.95LCL"], srv_tbl[, "0.95UCL"])
  
  srv_qt_tbl <- quantile(surv_km_fit)$quantile
  qnt <- Map(function(x,y) c(x,y), srv_qt_tbl[, "25"], srv_qt_tbl[, "75"])
  rng <- lapply(split(data.frame(tte, is_event), col_by), range)
  
  tbl_tte <- rtable(
    header = levels(col_by),
    rrow(paste0("Time to Event (", time_unit, "s)")),
    rrowl("Median", med, format = "xx.xx", indent = 1),
    rrowl("95% CI", ci, indent = 2, format = "(xx.x, xx.x)"),
    rrowl("25% and 75%−ile", qnt, indent = 1, format = "xx.x, xx.x"),
    rrowl("Range", rng, indent = 1, format = "xx.x to xx.x")  
  )
  
  
  # Unstratified Analysis
  # #####################

  # this function is reused for stratified analysis
  survival_anl <- function(formula, data, arm_var, label) {
    
    arm <- data[[arm_var]]
    reference_level <- levels(arm)[1]
    comparison_levels <- levels(arm)[-1]
    
    # create survival fit of comparison arm vs. reference arm
    values <- lapply(comparison_levels, function(lvl) {
      
      dfi <- subset(data, arm %in% c(reference_level, lvl))
      dfi$arm <- factor(dfi$arm, levels = c(reference_level, lvl))
      
      ## for log-rank test: use coxph score for log-rank
      fit_survdiff <- survdiff(formula, data = dfi)
      fit_coxph <- coxph(formula, data = dfi, ties = ties)
      
      list(
        pval = pchisq(fit_survdiff$chisq, length(fit_survdiff$n) - 1, lower.tail = FALSE),
        hr = (summary(fit_coxph)$conf.int)[1, 1],
        hr_ci = (summary(fit_coxph)$conf.int)[1, 3:4]
      )
    })
    
    # first column is empty
    pval <- start_with_NULL(lapply(values, `[[`, "pval"))
    hr <- start_with_NULL(lapply(values, `[[`, "hr"))
    hr_ci <- start_with_NULL(lapply(values, `[[`, "hr_ci"))
    
    rtable(
      header = levels(arm),
      rrow(label),
      rrowl("p-value (log-rank)", pval, indent = 1, format = "xx.xxxx"),
      rrow(),
      rrowl("Hazard Ratio", hr, indent = 1, format = "xx.xxxx"),
      rrowl("95% CI", hr_ci, indent = 2, format = "(xx.xxxx, xx.xxxx)")
    )
  }
  
  ANL <- data.frame(tte = tte, event = is_event, arm = col_by)
  
  tbl_unstratified <- survival_anl(
    formula = Surv(tte, event) ~ arm,
    data = ANL,
    arm_var = "arm",
    label = "Unstratified Analysis"
  )
  
  # Stratified Analysis
  # ###################
  tbl_stratified <- if (is.null(strata_data)) {
    NULL
  } else {
    
    if (length(intersect(names(ANL), names(strata_data))) != 0) {
      stop("illegal strata variable names 'tte', 'event', 'arm'")      
    }

    ANL_stratified <- cbind(ANL, strata_data)
    
    tbl_stratified <- survival_anl(
      formula = as.formula(
        paste("Surv(tte, event) ~ arm + strata(", paste(names(strata_data), collapse = ","), ")")
      ),
      data = ANL_stratified,
      arm_var = "arm",
      label = "Stratified Analysis"
    )
  }
  
  # Time Point Analysis
  # ###################
  
  tbl_timepoints <- if (is.null(time_points)) {
    NULL
  } else {
    
    tp <- summary(surv_km_fit, times = time_points)
  
    df_tp <- as.data.frame(tp[c("time", "n.risk", "surv", "lower", "upper", "strata", "std.err")])
    s_df_tp <- split(df_tp, df_tp$time)
    
    ## dfi <- s_df_tp[[1]]; time_point = time_points[1]
    tp_rtables <- Map(function(dfi, time_point) {
      
      name <- paste(time_point, if(time_point == 1) time_unit else paste0(time_unit, "s"))
      
      if (!all(dfi$time == time_point)) stop("time points do not match")
      
      d <- dfi$surv[-1] - dfi$surv[1]
      sd <- sqrt(dfi$std.err[-1]^2 + dfi$std.err[1]^2)
      
      # z-test
      l.ci <- Map(function(di, si) di + qnorm(c(0.025, 0.975)) * si, d, sd)
      pval <- 2*(1 - pnorm(abs(d)/sd))
      
      rtable(
        header = levels(col_by),
        rrow(name, indent = 1),
        rrowl("Patients remaining at risk", dfi$n.risk, format = "xx", indent = 2),
        rrowl("Event Free Rate (%)", dfi$surv, format = "xx.xx%", indent = 2),
        rrowl("95% CI",  as.data.frame(t(dfi[c("lower", "upper")]*100)), format = "(xx.xx, xx.xx)", indent = 3),
        rrowl("Difference in Event Free Rate", c(list(NULL), as.list(d*100)), format = "xx.xx", indent = 2),
        rrowl("95% CI", c(list(NULL), lapply(l.ci, function(x) 100*x)), format = "(xx.xx, xx.xx)", indent = 3),
        rrowl("p-value (Z-test)", c(list(NULL), as.list(pval)), format = "xx.xxxx", indent = 2)
      )
    }, s_df_tp, time_points)  

    rbind(
      rtable(header = levels(col_by), rrow("Time Point Analysis")),    
      stack_rtables_l(tp_rtables)      
    )
  }
  
  ## Now Stack Tables together
  tbl <- stack_rtables(
    tbl_event,
    tbl_tte,
    tbl_stratified,
    tbl_unstratified,
    tbl_timepoints
  )
  
  # add N to header 
  names(tbl) <- paste(names(tbl), paste("N =", unlist(N)), sep = "\n")

  tbl
}