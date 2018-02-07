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
#' tbl <- time_to_event_table(
#'   time_to_event = ATE_f$AVAL,
#'   event = ATE_f$CNSR == 0,
#'   earliest_contributing_event = ATE_f$EVNTDESC,
#'   arm = factor(ATE_f$ARM),
#'   strata_data = ATE_f %>% select(SEX, RACE),
#'   time_points = 6,
#'   time_unit = "month"
#' )
#' 
#' tbl
#' 
#' \dontrun{
#' 
#' library(atezo.data)
#' library(dplyr)
#' library(forcats)
#'  
#' ASL <- asl(com.roche.cdt30019.go29436.re)
#' ATE <- ate(com.roche.cdt30019.go29436.re)
#' 
#' tte_tbl_stream <- get_time_to_event_table(com.roche.cdt30019.go29436.re)
#' Viewer(tte_tbl_stream)
#' 
#' ATE_f <- ATE %>%
#'  filter(ITTFL == "Y", PARAMCD == "OS") %>%
#'  mutate(ARM_ANL = fct_relevel(ARM1, "DUMMY C", "DUMMY B", "DUMMY A"))
#' 
#' 
#' tbl <- time_to_event_table(
#'   time_to_event = ATE_f$AVAL,
#'   event = ATE_f$CNSR == 0,
#'   earliest_contributing_event = ATE_f$EVNTDESC,
#'   arm = ATE_f$ARM_ANL,
#'   strata_data = ATE_f %>% select(SEX, MLIVER, TCICLVL2),
#'   time_points = 6,
#'   time_unit = "month"
#' )
#' 
#' Viewer(tbl)
#' 
#' Viewer(tbl, tte_tbl_stream)
#' 
#' compare_rtables(tbl, tte_tbl_stream, comp.attr = FALSE)
#' 
#' tbl[13,1]
#' tte_tbl_stream[13,1]
#' }
#'
time_to_event_table <- function(time_to_event, event, arm,
                                earliest_contributing_event,
                                strata_data,
                                time_points,
                                time_unit = "month",
                                ties = "exact") {
  
  # Argument Checking #
  n <- length(time_to_event)
  if (length(event) != n) stop("event has incorrect length!")
  if (length(arm) != n) stop("arm has incorrect length!")
  
  if (is.null(strata_data)) stop("need strata_data")
  if (nrow(strata_data) != n) stop("strata_data wrong")
  
  if (length(earliest_contributing_event) != n) stop("earliest_contributing_event has wrong length") 
  
  if (!is.factor(arm)) stop("Argument ARM needs to be a factor")
  
  ARM <- arm
 
  N <- tapply(ARM, ARM, length)
  
  patients_with_event <- tapply(event, ARM, function(x) {
    c(sum(x), sum(x)/length(x))
  }, simplify = FALSE)
  
  
  # earliest contributing event

  df_event <- data.frame(
    event = earliest_contributing_event,
    arm = ARM,
    stringsAsFactors = FALSE
  ) 
  
  l_event <- lapply(split(df_event, df_event$event), function(x) {
    tapply(x$event, x$arm, length, simplify = FALSE)
  })

  r_event <- Map(function(x, name) {
    do.call(rrow, c(
      list(
        row.name = name,
        indent = 2, 
        format = "xx"
      ),
      x
    )) 
  }, l_event, names(l_event))
  
  patients_wo_event <- tapply(event, ARM, function(x) {
    c(sum(!x), sum(!x)/length(x))
  }, simplify = FALSE)
   
  ## Time to Event (Months)
  ## maybe use log
  surv_km_fit <- survival::survfit(
    formula = Surv(time_to_event, event) ~ ARM, 
    conf.type = "plain"
  )
  
  
  srv_tbl <- summary(surv_km_fit)$table
  med <- as.list(srv_tbl[, "median"])
  ci <- Map(function(x,y)c(x,y), srv_tbl[, "0.95LCL"], srv_tbl[, "0.95UCL"])
  
  srv_qt_tbl <- quantile(surv_km_fit)$quantile
  qnt <- Map(function(x,y)c(x,y), srv_qt_tbl[, "25"], srv_qt_tbl[, "75"])
  rng <- lapply(split(data.frame(time_to_event, event), ARM), function(df) {
    
    # is min and max from cnsr
    rng_arm <- range(df$time_to_event)
    # max_cnsr <- max(df$time_to_event[!df$event])
    #attr(rng_arm, is_max_from_cnsr = rng_arm == max_cnsr)
   
    rng_arm 
  })
  
  
  # Unstratified Analysis
  ref_lvl <- levels(ARM)[1]
  
  if (any( c('tte', 'evnt', "arm") %in% names(strata_data))) stop("illegal stata variable names 'tte', 'evnt', 'arm'")
    
  df <- cbind(data.frame(tte = time_to_event, evnt = event, arm = ARM), strata_data)
  
  strata_formula <- as.formula(
    paste("Surv(tte, evnt) ~ arm + strata(", paste(names(strata_data), collapse = ","), ")")
  )
  
  # create survival fit of comparison arm vs. reference arm
  fits <- lapply(levels(ARM)[-1], function(lvl) {
    dfi <- df[df$arm %in% c(ref_lvl, lvl), ]
    dfi$arm <- factor(dfi$arm, levels = c(ref_lvl, lvl))
    
    ## for log-rank test 
    ## use coxph score for log-rank
    ## 
    list(
      us_diff = survdiff(Surv(tte, evnt) ~ arm, data = dfi),
      us_ph = coxph(Surv(tte, evnt) ~ arm, data = dfi, ties = ties),
      str_diff = survdiff(strata_formula, data = dfi),
      str_ph = coxph(strata_formula, data = dfi, ties = ties)
    )
  })
  
  us_p <- lapply(fits, function(fit) {
    ft <- fit$us_diff
    pchisq(ft$chisq, length(ft$n) - 1, lower.tail = FALSE)
  })
  
  us_hr <- lapply(fits, function(fit) {
    ft <- fit$us_ph
    (summary(ft)$conf.int)[1,1]
  })
  
  us_ci <- lapply(fits, function(fit) {
    ft <- fit$us_ph
    (summary(ft)$conf.int)[1,3:4]
  })
  

  # Stratified Analysis
  str_p <- lapply(fits, function(fit) {
    ft <- fit$str_diff
    
    #summary(ft)$sctest["pvalue"]    
    pchisq(ft$chisq, length(ft$n) - 1, lower.tail = FALSE)
  })
  
  str_hr <- lapply(fits, function(fit) {
    ft <- fit$str_ph
    (summary(ft)$conf.int)[1,1]
  })
  
  str_ci <- lapply(fits, function(fit) {
    ft <- fit$str_ph
    (summary(ft)$conf.int)[1,3:4]
  })
  
  
  # Time Point Analysis
  tp <- summary(surv_km_fit, times = time_points)

  df_tp <- as.data.frame(tp[c("time", "n.risk", "surv", "lower", "upper", "strata", "std.err")])

  ## helper function
  lrrow <- function(row.name, l, ...) {
    do.call(rrow, c(list(row.name = row.name, ...), l))
  }

  s_df_tp <- split(df_tp, df_tp$time)
  
  ## dfi <- s_df_tp[[1]]; time_point = time_points[1]
  l_tp_rows <- Map(function(dfi, time_point) {
    
    name <- paste(time_point, if(time_point == 1) time_unit else paste0(time_unit, "s"))
    
    if (!all(dfi$time == time_point)) stop("time points do not match")
    
    d <- dfi$surv[-1] - dfi$surv[1]
    sd <- sqrt(dfi$std.err[-1]^2 + dfi$std.err[1]^2)
    
    # z-test
    l.ci <- Map(function(di, si) di + qnorm(c(0.025, 0.975)) * si, d, sd)
    pval <- 2*(1 - pnorm(abs(d)/sd))
  
    list(
      rrow(name, indent = 1),
      lrrow("Patients remaining at risk", dfi$n.risk, format = "xx", indent = 2),
      lrrow("Event Free Rate (%)", dfi$surv, format = "xx.xx%", indent = 2),
      lrrow("95% CI",  as.data.frame(t(dfi[c("lower", "upper")]*100)), format = "(xx.xx, xx.xx)", indent = 3),
      lrrow("Difference in Event Free Rate", c(list(NULL), as.list(d*100)), format = "xx.xx", indent = 2),
      lrrow("95% CI", c(list(NULL), lapply(l.ci, function(x) 100*x)), format = "(xx.xx, xx.xx)", indent = 3),
      lrrow("p-value (Z-test)", c(list(NULL), as.list(pval)), format = "xx.xxxx", indent = 2),
      rrow()
    )
  }, s_df_tp, time_points)  

  rrows_tp_part <- unlist(l_tp_rows, recursive = FALSE)
  
  rrows_tp <- if (is.null(rrows_tp_part)) {
    NULL
  } else {
    c(
      list(rrow(), rrow("Time Point Analysis")),
      rrows_tp_part[-length(rrows_tp_part)]
    )
  }
  
  ## Now create table
  tbl_args <- c(
    list(
      col.names = paste0(levels(ARM), "\n", paste0("(N=", N,")")),
      format = "xx.xxx",
      lrrow("Patients with event (%)", patients_with_event, format = "xx (xx.xx%)"),
      rrow("Earliest contributing event", indent = 1)
    ),
    r_event,
    list(
      lrrow("Patients without event (%)", patients_wo_event, format = "xx (xx.xx%)"),
      rrow(),
      rrow(paste0("Time to Event (", time_unit, "s)")),
      lrrow("Median", med, indent = 1),
      lrrow("95% CI", ci, indent = 2, format = "(xx.x, xx.x)"),
      lrrow("25% and 75%−ile", qnt, indent = 1, format = "xx.x, xx.x"),
      lrrow("Range", rng, indent = 1, format = "xx.x to xx.x"),
      rrow(),
      rrow("Unstratified Analysis"),
      lrrow("p-value (log-rank)", c(list(NULL), us_p), indent = 1, format = "xx.xxxx"),
      rrow(),
      lrrow("Hazard Ratio", c(list(NULL), us_hr), indent = 1, format = "xx.xxxx"),
      lrrow("95% CI", c(list(NULL), us_ci), indent = 2, format = "(xx.xxxx, xx.xxxx)"),
      rrow(),
      rrow("Stratified Analysis"),
      lrrow("p-value (log-rank)", c(list(NULL), str_p), indent = 1, format = "xx.xxxx"),
      rrow(),
      lrrow("Hazard Ratio", c(list(NULL), str_hr), indent = 1, format = "xx.xxxx"),
      lrrow("95% CI", c(list(NULL), str_ci), indent = 2, format = "(xx.xxxx, xx.xxxx)") 
    ),
    rrows_tp
  )
  
  do.call(rtable, tbl_args)
  
}