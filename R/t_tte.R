#' Time to Event Table
#' 
#' The time to event table summarizes time to event data with different models
#' as described in the details section.
#' 
#' @param formula a survival formula, the arm variable needs to be wrapped in
#'   \code{\link{arm}}. The \code{\link[survival]{strata}} special will only be
#'   used for the stratified analysis. If there is not
#'   \code{\link[survival]{strata}} specification then the stratified analysis
#'   is omitted.
#' @param data a \code{data.frame} with all the variable that are used in
#'   \code{formula}
#' @param event_descr a factor that partitions the the events into earliest
#'   contributing event. The variable name can be provided unquoted in which
#'   case it is looked up in \code{data} and then the calling environment.
#' @param time_points numeric vector with times displayed in the time point
#'   analysis, if \code{NULL} this section of the table will be omitted
#' @param time_unit a string with the unit of the \code{tte} argument
#' @param ties passed forward to \code{\link[survival]{coxph}}
#' 
#' @details 
#' The time to event section is derived from a kaplan meier estimator for the
#' \code{formula} argument with the strata special dropped.
#'
#' The stratified and unstratified analysis is evaluated pair-wise (reference to
#' comparison) and \code{\link[survival]{survdiff}} is used to get the p-value
#' whereas \code{\link[survival]{coxph}} is used to calculate the hazard ratio
#' and confidence interval.
#'
#' The time point analysis is based on the kaplan meier estimator.
#' 
#' @template return_rtable
#' 
#' @importFrom stats terms quantile pchisq qnorm pnorm 
#' @export
#' 
#' @author Mark Rothe (rothem1)
#' @template author_waddella
#' 
#' @examples 
#' library(random.cdisc.data)
#' 
#' ASL <- radam("ASL", start_with = list(ARM = paste("ARM", LETTERS[1:3])))
#' ATE <- radam("ATE", ADSL = ASL)
#' 
#' ASL$ARM <- as.factor(ASL$ARM)
#' ATE_f <- subset(ATE, PARAMCD == "OS")
#' 
#' ANL <- merge(ASL, ATE_f, all.x =TRUE, all.y = FALSE, by = c("USUBJID", "STUDYID"))
#' 
#' 
#' tbl <- t_tte(
#'   formula = Surv(AVAL, !CNSR) ~ arm(ARM) + strata(SEX),
#'   data = ANL,
#'   event_descr = factor(EVNTDESC),
#'   time_points = c(6, 2000),
#'   time_unit = "month"
#' )
#' 
#' tbl
#' 
t_tte <- function(formula,
                  data,
                  event_descr,
                  time_points,
                  time_unit = "month",
                  ties = "exact") {
  
  cl <- match.call()
  
  if (!is.data.frame(data)) stop("data needs to be a data.frame")
  
  # extracted data
  tm <- t_tte_items(formula, cl, data, parent.frame())
  
  tte <- tm$tte
  is_event <- as.logical(tm$event)
  arm <- tm$arm
  event_descr <- if (missing(event_descr)) {
    NULL
  } else {
    eval(substitute(event_descr), data, parent.frame())
  }
  
  
  # Argument Checking
  if (length(tte) != nrow(data)) {
    stop("some of the following variable contain missing values:\n   ",
         sub("^list", "", deparse(attr(terms(formula), "variables"))),
         "\nmissing data in for the survival analysis is currently disabled")    
  }

  check_same_N(is_event = is_event, event_descr = event_descr, arm = arm)
  check_col_by(arm, 2)
  if (!is.null(event_descr) && !is.factor(event_descr))
    stop("event_descr is required to be a factor") 
  if (!is.null(time_points) && !is.numeric(time_points))
    stop("time_points is required to be numeric")
  
  # Calculate elements of the table

  
  # Event Table
  # ###########
  
  tbl_event <- rbind(
    rtabulate(is_event, arm, positives_and_proportion, format = "xx.xx (xx.xx%)",
              row.name = "Patients with event (%)"),
    if (!is.null(event_descr)) {
      rbind(
        rtable(levels(arm), rrow("Earliest Contributing Event", indent = 1)),
        rtabulate(droplevels(factor(event_descr)[is_event]), arm[is_event], 
                  length, indent = 2)
      )
    } else {
      NULL
    },
    rtabulate(!is_event, arm, positives_and_proportion, format = "xx.xx (xx.xx%)",
              row.name = "Patients without event (%)")
  )
  
  
  # Time to Event
  # #############
  f <- tm$formula_nostrata
  environment(f) <- environment()
  
  surv_km_fit <- survfit(
    formula = f,
    data = data,
    conf.type = "plain"
  )

  srv_tbl <- summary(surv_km_fit)$table
  med <- as.list(srv_tbl[, "median"])
  ci <- Map(function(x,y) c(x,y), srv_tbl[, "0.95LCL"], srv_tbl[, "0.95UCL"])
  
  srv_qt_tbl <- quantile(surv_km_fit)$quantile
  qnt <- Map(function(x,y) c(x,y), srv_qt_tbl[, "25"], srv_qt_tbl[, "75"])
  rng <- lapply(split(data.frame(tte, is_event), arm), range)
  
  tbl_tte <- rtable(
    header = levels(arm),
    rrow(paste0("Time to Event (", time_unit, "s)")),
    rrowl("Median", med, format = "xx.xx", indent = 1),
    rrowl("95% CI", ci, indent = 2, format = "(xx.x, xx.x)"),
    rrowl("25% and 75%-ile", qnt, indent = 1, format = "xx.x, xx.x"),
    rrowl("Range", rng, indent = 1, format = "xx.x to xx.x")  
  )
  
  
  
  # Unstratified Analysis
  # #####################
  
  # this function is reused for stratified analysis
  survival_anl <- function(formula, label) {
    
    reference_level <- levels(arm)[1]
    comparison_levels <- levels(arm)[-1]
    
    # create survival fit of comparison arm vs. reference arm
    values <- lapply(comparison_levels, function(lvl) {
      
      df_i <- data[arm %in% c(reference_level, lvl), , drop = FALSE]

      varname <- attr(arm, "varname")
      df_i[[varname]] <- droplevels(df_i[[varname]])
      
      environment(formula) <- environment()
      
      ## for log-rank test: use coxph score for log-rank
      fit_survdiff <- survdiff(formula, data = df_i)
      fit_coxph <- tryCatch(
        coxph(formula, data = df_i, ties = ties), # weights are not supported if ties = 'exact'
        error = function(e) NULL
      )
      
      list(
        pval = pchisq(fit_survdiff$chisq, length(fit_survdiff$n) - 1, lower.tail = FALSE),
        hr = if (is.null(fit_coxph)) NA else (summary(fit_coxph)$conf.int)[1, 1],
        hr_ci = if (is.null(fit_coxph)) c(NA, NA) else (summary(fit_coxph)$conf.int)[1, 3:4]
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
  
  tbl_unstratified <- survival_anl(
    formula = tm$formula_nostrata,
    label = "Unstratified Analysis"
  )
  
  # Stratified Analysis
  # ###################
  tbl_stratified <- if (is.null(tm$formula_strata)) {
    NULL
  } else {
    survival_anl(
      formula = tm$formula_strata,
      label = "Stratified Analysis"
    )
  }
  
  # Time Point Analysis
  # ###################
  
  tbl_timepoints <- if (is.null(time_points)) {
    NULL
  } else {
    
    tp <- try(summary(surv_km_fit, times = time_points), silent = TRUE)
    
    
    tp_rtables <- if (is(tp, "try-error")) {
      
      list(
        rtable(
          header = levels(arm),
          rrow(paste("time points: ", paste(time_points, collapse = ", ")), indent = 1),
          rrow("-- no data", indent = 2)
        )
      )
      
    } else {
      
      df_tp <- as.data.frame(tp[c("time", "n.risk", "surv", "lower", "upper", "strata", "std.err")])
      s_df_tp <- split(df_tp, factor(df_tp$time, levels = time_points), drop = FALSE)
      
      ## dfi <- s_df_tp[[1]]; time_point = time_points[1]
      Map(function(dfi, time_point) {
        
        name <- paste(time_point, if(time_point == 1) time_unit else paste0(time_unit, "s"))
        
        
        if (nrow(dfi) <= 1) {
          rtable(
            header = levels(arm),
            rrow(name, indent = 1),
            rrow(if (nrow(dfi) == 0) "-- no data" else "-- not enough data", indent = 2)
          )
        } else {
          
          if (!all(dfi$time == time_point)) stop("time points do not match")
          
          d <- dfi$surv[-1] - dfi$surv[1]
          sd <- sqrt(dfi$std.err[-1]^2 + dfi$std.err[1]^2)
          
          # z-test
          l.ci <- Map(function(di, si) di + qnorm(c(0.025, 0.975)) * si, d, sd)
          pval <- 2*(1 - pnorm(abs(d)/sd))
          
          rtable(
            header = levels(arm),
            rrow(name, indent = 1),
            rrowl("Patients remaining at risk", dfi$n.risk, format = "xx", indent = 2),
            rrowl("Event Free Rate (%)", dfi$surv, format = "xx.xx%", indent = 2),
            rrowl("95% CI",  as.data.frame(t(dfi[c("lower", "upper")]*100)), format = "(xx.xx, xx.xx)", indent = 3),
            rrowl("Difference in Event Free Rate", c(list(NULL), as.list(d*100)), format = "xx.xx", indent = 2),
            rrowl("95% CI", c(list(NULL), lapply(l.ci, function(x) 100*x)), format = "(xx.xx, xx.xx)", indent = 3),
            rrowl("p-value (Z-test)", c(list(NULL), as.list(pval)), format = "xx.xxxx", indent = 2)
          )
        }
      }, s_df_tp, time_points)  
    }
    
    
    
    rbind(
      rtable(header = levels(arm), rrow("Time Point Analysis")),    
      stack_rtables_l(tp_rtables)      
    )
    
  }
  ## Now Stack Tables together
  tbl <- stack_rtables(
    tbl_event,
    tbl_tte,
    tbl_unstratified,
    tbl_stratified,
    tbl_timepoints
  )
  
  # add N to header 
  N <- tapply(arm, arm, length)
  header(tbl) <- rheader(
    rrowl("", levels(arm)),
    rrowl("", paste0("(N=",N,")"))
  )
  
  tbl
}


t_tte_items <- function(formula, cl, data, env) {
  # extract information
  mf <- cl
  mt <- terms(formula, specials = c("arm", "strata", "cluster", "tt"),
              data = data)
  if (!all(all.vars(attr(mt, "variables")) %in% names(data)))
    stop("All formula variables must appear in 'data'")
  irsp <- attr(mt, "response")
  istr <- attr(mt, "specials")$strata
  iarm <- attr(mt, "specials")$arm
  if (is.null(irsp) | is.null(iarm))
    stop("formula must include a response and arm")
  if (is.null(istr)) {
    uf <- formula
    f <- NULL
  } else {
    uf <- drop_special(mt, "strata")
    f <- formula
  }
  m <- match(c("formula", "data", "weights"), names(mf), 0L)
  
  mf <- mf[c(1L, m)]
  mf[[1L]] <- quote(stats::model.frame)
  mf$na.action <- quote(stats::na.omit)
  
  mf <- eval(mf, env)
  if (!inherits(mf[, irsp], "Surv"))
    stop("Response is not a 'Surv' object")
  if (attr(mf[, irsp], "type") != "right")
    stop("Response is not a right-censored 'Surv' object")
  
  list(
    tte = mf[, irsp][, "time"],
    event = mf[, irsp][, "status"],
    arm = mf[, iarm],
    formula_strata = f,
    formula_nostrata = uf,
    model_frame  = mf
  )

}
