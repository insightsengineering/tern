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
#'   arm = ATE_f$ARM_ANL,
#'   is_earliest_contr_event_death = ATE_f$EVNTDESC == "Death",
#'   strata_data = ATE_f %>% select(SEX, MLIVER, TCICLVL2),
#'   time_points = setNames(6, paste(6, "Months"))
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
                                is_earliest_contr_event_death,
                                strata_data,
                                time_points) {
  
  # Argument Checking #
  n <- length(time_to_event)
  if (length(event) != n) stop("event has incorrect length!")
  if (length(arm) != n) stop("arm has incorrect length!")
  
  if (is.null(strata_data)) stop("need strata_data")
  if (nrow(strata_data) != n) stop("strata_data wrong")
  
  ARM <- arm
 
  N <- tapply(ARM, ARM, length)
  
  patients_with_event <- tapply(event, ARM, function(x) {
    c(sum(x), sum(x)/length(x))
  }, simplify = FALSE)
  
  early_death <- tapply(is_earliest_contr_event_death, ARM, sum)
  
  patients_wo_event <- tapply(event, ARM, function(x) {
    c(sum(!x), sum(!x)/length(x))
  }, simplify = FALSE)
   
  ## Time to Event (Months)
  surv_km_fit <- survival::survfit(
    formula = Surv(time_to_event, event) ~ ARM, 
    conf.type="plain"
  )
  
  srv_tbl <- summary(surv_km_fit)$table
  med <- as.list(srv_tbl[, "median"])
  ci <- Map(function(x,y)c(x,y), srv_tbl[, "0.95LCL"], srv_tbl[, "0.95UCL"])
  
  srv_qt_tbl <- quantile(surv_km_fit)$quantile
  qnt <- Map(function(x,y)c(x,y), srv_qt_tbl[, "25"], srv_qt_tbl[, "75"])
  rng <- lapply(split(data.frame(time_to_event, event), ARM), function(df) {
    range(df$time_to_event[!df$event])
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
    
    list(
      us_diff = survdiff(Surv(tte, evnt) ~ arm, rho=0, data = dfi),
      us_ph = coxph(Surv(tte, evnt) ~ arm, data = dfi),
      str_diff = survdiff(strata_formula, data = dfi),
      str_ph = coxph(strata_formula, data = dfi)
    )
  })
  
  us_p <- lapply(fits, function(fit) {
    ft <- fit$us_diff
    pchisq(ft$chisq, length(ft$n)-1, lower.tail = FALSE)
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
    pchisq(ft$chisq, length(ft$n)-1, lower.tail = FALSE)
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
  
  l_tp_rows <- Map(function(dfi, time_point, name) {
    
    if (!all(dfi$time == time_point)) stop("time points do not match")
    
    diff_surv <- dfi$surv[-1] - dfi$surv[1]
    sd <- sqrt(dfi$std.err[-1]^2 + dfi$std.err[1]^2)
    
    diff_surv_ci <- Map(function(x, sdi) 100 * (x + c(-1, 1) * 1.96 * sdi), diff_surv, sd)
    diff_surv_p <- Map(function(x, sdi) 2*(1 - pnorm(abs(x/sqrt(sdi)))), diff_surv, sd)
    
    list(
      rrow(name, indent = 1),
      lrrow("Patients remaining at risk", dfi$n.risk, format = "xx", indent = 2),
      lrrow("Event Free Rate (%)", dfi$surv, format = "xx.xx%", indent = 2),
      lrrow("95% CI",  as.data.frame(t(dfi[c("lower", "upper")]*100)), format = "(xx.xx, xx.xx)", indent = 3),
      lrrow("Difference in Event Free Rate", c(list(NULL), as.list(diff_surv*100)), format = "xx.xx", indent = 2),
      lrrow("95% CI", c(list(NULL), diff_surv_ci), format = "(xx.xx, xx.xx)", indent = 3),
      lrrow("p-value (Z-test)", c(list(NULL), diff_surv_p), format = "xx.xxxx", indent = 2),
      rrow()
    )
  }, s_df_tp, time_points, if (is.null(names(time_points))) time_points else names(time_points))  

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
      rrow("Earliest contributing event", indent = 1),
      lrrow("Death", early_death, indent = 2),
      lrrow("Patients without event (%)", patients_wo_event, format = "xx (xx.xx%)"),
      rrow(),
      rrow("Time to Event (Months)"),
      lrrow("Median", med, indent = 1),
      lrrow("95% CI", ci, indent = 2, format = "(xx.x, xx.x)"),
      lrrow("25% and 75%−ile", qnt, indent = 1, format = "xx.x, xx.x"),
      lrrow("Range", rng, indent = 1, format = "xx.x to xx.x"),
      rrow(),
      rrow("Unstratified Analysis"),
      lrrow("p-value (log-rank)", c(list(NULL), us_p), indent = 1, format = "xx.xxx"),
      rrow(),
      lrrow("Hazard Ratio", c(list(NULL), us_hr), indent = 1, format = "xx.xx"),
      lrrow("95% CI", c(list(NULL), us_ci), indent = 2, format = "(xx.xx, xx.xx)"),
      rrow(),
      rrow("Stratified Analysis"),
      lrrow("p-value (log-rank)", c(list(NULL), str_p), indent = 1, format = "xx.xxx"),
      rrow(),
      lrrow("Hazard Ratio", c(list(NULL), str_hr), indent = 1, format = "xx.xx"),
      lrrow("95% CI", c(list(NULL), str_ci), indent = 2, format = "(xx.xx, xx.xx)") 
    ),
    rrows_tp
  )
  
  do.call(rtable, tbl_args)
  
}









time_to_event_table2 <- function(time_to_event,event,arm,big_n_arm,arm.ref,comp1.arm,comp2.arm,
                                strata1,strata2,strata3,time_point,desc_event) {

 

  ARM <- factor(arm, levels = c(arm.ref, setdiff(arm, arm.ref)))
  
  levels(big_n_arm) <- as.vector(levels(ARM))
  
  surv_km_fit <- survival::survfit(
    formula = Surv(time_to_event, event) ~ ARM, 
    conf.type="plain"
  )
  
  survdiff_data <- data.frame(time_to_event = as.numeric(time_to_event),
                              event = as.numeric(event),
                              ARM = ARM,
                              strata1 = strata1,
                              strata2 = strata2,
                              strata3 = strata3,
                              stringsAsFactors = FALSE)
  
  comp1_vs_ref <- subset(survdiff_data, ARM %in% c(arm.ref,comp1.arm))
  
  surv_km_test1 <- survival::survdiff(
    formula = Surv(time_to_event, event) ~ ARM, rho=0, data=comp1_vs_ref
  )
  
  strat_surv_km_test1 <- survival::survdiff(
    formula = Surv(time_to_event, event) ~ ARM + strata(strata1,strata2,strata3), rho=0, data=comp1_vs_ref
  )
  
  comp2_vs_ref <- subset(survdiff_data, ARM %in% c(arm.ref,comp2.arm))
  
  surv_km_test2 <- survival::survdiff(
    formula = Surv(time_to_event, event) ~ ARM, rho=0, data=comp2_vs_ref
  )
  
  strat_surv_km_test2 <- survival::survdiff(
    formula = Surv(time_to_event, event) ~ ARM + strata(strata1,strata2,strata3), rho=0, data=comp2_vs_ref
  )
  
  surv_cox_ph <- survival::coxph(
    formula = Surv(time_to_event, event) ~ ARM
  )
  
  strat_surv_cox_ph <- survival::coxph(
    formula = Surv(time_to_event, event) ~ ARM + strata(strata1,strata2,strata3)
  )
  
  surv_km_summary <- summary(surv_km_fit)
  
  surv_km_table <- surv_km_summary$table
  
  surv_km_broom <- broom::tidy(surv_km_fit)
  
  ref_surv_km_ranges <- surv_km_broom[which(stringr::str_sub(surv_km_broom$strata,5,
                                      nchar(surv_km_broom$strata)) == as.vector(levels(ARM))[1]),]$time

  comp1_surv_km_ranges <- surv_km_broom[which(stringr::str_sub(surv_km_broom$strata,5,
                                        nchar(surv_km_broom$strata)) == as.vector(levels(ARM))[2]),]$time

  comp2_surv_km_ranges <- surv_km_broom[which(stringr::str_sub(surv_km_broom$strata,5,
                                        nchar(surv_km_broom$strata)) == as.vector(levels(ARM))[3]),]$time
  
  surv_km_quantile <- quantile(surv_km_fit)$quantile

  # Time Point Analysis #  
  surv_km_time_point <- summary(surv_km_fit,times=c(time_point))

  ref_patients_remaining_at_risk <- surv_km_time_point$n.risk[[1]]
  ref_patients_event_free_rate <- surv_km_time_point$surv[[1]]*100
  ref_patients_event_free_rate_lcl <- surv_km_time_point$lower[[1]]*100
  ref_patients_event_free_rate_ucl <- surv_km_time_point$upper[[1]]*100
  
  comp1_patients_remaining_at_risk <- surv_km_time_point$n.risk[[2]]
  comp1_patients_event_free_rate <- surv_km_time_point$surv[[2]]*100
  comp1_patients_event_free_rate_lcl <- surv_km_time_point$lower[[2]]*100
  comp1_patients_event_free_rate_ucl <-surv_km_time_point$upper[[2]]*100
  
  comp2_patients_remaining_at_risk <- surv_km_time_point$n.risk[[3]]
  comp2_patients_event_free_rate <- surv_km_time_point$surv[[3]]*100
  comp2_patients_event_free_rate_lcl <- surv_km_time_point$lower[[3]]*100
  comp2_patients_event_free_rate_ucl <- surv_km_time_point$upper[[3]]*100
  
  comp1_ref_diff_event_free_rate <- comp1_patients_event_free_rate - ref_patients_event_free_rate 
  comp2_ref_diff_event_free_rate <- comp2_patients_event_free_rate - ref_patients_event_free_rate
  
  comp1_ref_diff_event_free_rate_lcl <- comp1_ref_diff_event_free_rate - 1.96*((surv_km_time_point$std.err[[2]]*100)^2 + (surv_km_time_point$std.err[[1]]*100)^2)^0.5
  comp1_ref_diff_event_free_rate_ucl <- comp1_ref_diff_event_free_rate + 1.96*((surv_km_time_point$std.err[[2]]*100)^2 + (surv_km_time_point$std.err[[1]]*100)^2)^0.5
 
  comp2_ref_diff_event_free_rate_lcl <- comp2_ref_diff_event_free_rate - 1.96*((surv_km_time_point$std.err[[3]]*100)^2 + (surv_km_time_point$std.err[[1]]*100)^2)^0.5
  comp2_ref_diff_event_free_rate_ucl <- comp2_ref_diff_event_free_rate + 1.96*((surv_km_time_point$std.err[[3]]*100)^2 + (surv_km_time_point$std.err[[1]]*100)^2)^0.5
  
  p_comp1_ref_diff_event_free_rate <- 2*(1 - pnorm(abs(comp1_ref_diff_event_free_rate/100)/((surv_km_time_point$std.err[[2]])^2 + (surv_km_time_point$std.err[[1]])^2)^0.5))
  p_comp2_ref_diff_event_free_rate <- 2*(1 - pnorm(abs(comp2_ref_diff_event_free_rate/100)/((surv_km_time_point$std.err[[3]])^2 + (surv_km_time_point$std.err[[1]])^2)^0.5))
  
  time_point_analysis <- data.frame(time_point,ref_patients_remaining_at_risk,ref_patients_event_free_rate,
                                    ref_patients_event_free_rate_lcl,ref_patients_event_free_rate_ucl,
                                    comp1_patients_remaining_at_risk,comp1_patients_event_free_rate,
                                    comp1_patients_event_free_rate_lcl,comp1_patients_event_free_rate_ucl,
                                    comp2_patients_remaining_at_risk,comp2_patients_event_free_rate,
                                    comp2_patients_event_free_rate_lcl,comp2_patients_event_free_rate_ucl,
                                    comp1_ref_diff_event_free_rate, comp2_ref_diff_event_free_rate,
                                    comp1_ref_diff_event_free_rate_lcl, comp1_ref_diff_event_free_rate_ucl,
                                    comp2_ref_diff_event_free_rate_lcl, comp2_ref_diff_event_free_rate_ucl,
                                    p_comp1_ref_diff_event_free_rate, p_comp2_ref_diff_event_free_rate)
  
  # BIG N #
  ref_BIG_N <- length(big_n_arm[big_n_arm == as.vector(levels(big_n_arm))[1]])
  comp1_BIG_N <- length(big_n_arm[big_n_arm == as.vector(levels(big_n_arm))[2]])
  comp2_BIG_N <- length(big_n_arm[big_n_arm == as.vector(levels(big_n_arm))[3]])

  ref_big_n <- surv_km_table[1,1]
  comp1_big_n <- surv_km_table[2,1]
  comp2_big_n <- surv_km_table[3,1]
  
  # ASL vs. ATE Checking #
  if (as.numeric(ref_BIG_N) != ref_big_n) stop("Reference Population Differences Between ASL and ATE!")
  if (as.numeric(comp1_BIG_N) != comp1_big_n) stop("Comparator 1 Population Differences Between ASL and ATE!")
  if (as.numeric(comp2_BIG_N) != comp2_big_n) stop("Comparator 2 Population Differences Between ASL and ATE!")
  
  tte_big_n <- data.frame(ref_big_n,comp1_big_n,comp2_big_n)
  
  # Patients With Events (N & %) #
  ref_n_events <- surv_km_table[1,4]
  comp1_n_events <- surv_km_table[2,4]
  comp2_n_events <- surv_km_table[3,4]
  
  ref_p_events <- ref_n_events / ref_big_n
  comp1_p_events <- comp1_n_events / comp1_big_n
  comp2_p_events <- comp2_n_events / comp2_big_n

  tte_np_events <- data.frame(ref_n_events,ref_p_events,comp1_n_events,comp1_p_events,
                              comp2_n_events,comp2_p_events)
  
  # Earliest Contributing Event #
  event_desc_df <- as.data.frame(table(ARM,desc_event))
  event_desc_dis_prog <- subset(event_desc_df, desc_event == "Disease Progression")
  event_desc_death <- subset(event_desc_df, desc_event == "Death")
  ref_dis_prog <- ifelse(is.na(event_desc_dis_prog[1,3]),0,event_desc_dis_prog[1,3])
  comp1_dis_prog <- ifelse(is.na(event_desc_dis_prog[2,3]),0,event_desc_dis_prog[2,3])
  comp2_dis_prog <- ifelse(is.na(event_desc_dis_prog[3,3]),0,event_desc_dis_prog[3,3])
  ref_death <- ifelse(is.na(event_desc_death[1,3]),0,event_desc_death[1,3])
  comp1_death <- ifelse(is.na(event_desc_death[2,3]),0,event_desc_death[2,3])
  comp2_death <- ifelse(is.na(event_desc_death[3,3]),0,event_desc_death[3,3])
  
  ece_dp <- data.frame(ref_dis_prog, comp1_dis_prog, comp2_dis_prog)
  ece_death <- data.frame(ref_death, comp1_death, comp2_death)
  
  # Patients Without Events (N & %) #  
  ref_n_wo_events <- ref_big_n - ref_n_events
  comp1_n_wo_events <- comp1_big_n - comp1_n_events
  comp2_n_wo_events <- comp2_big_n - comp2_n_events
  
  ref_p_wo_events <- ref_n_wo_events / ref_big_n
  comp1_p_wo_events <- comp1_n_wo_events / comp1_big_n
  comp2_p_wo_events <- comp2_n_wo_events / comp2_big_n

  tte_np_wo_events <- data.frame(ref_n_wo_events,ref_p_wo_events,comp1_n_wo_events,comp1_p_wo_events,
                                 comp2_n_wo_events,comp2_p_wo_events)
  
  # Median Time to Event + LCL & UCL #
  ref_med_tte <- surv_km_table[1,7]
  comp1_med_tte <- surv_km_table[2,7]
  comp2_med_tte <- surv_km_table[3,7]
  
  ref_med_tte_lcl <- surv_km_table[1,8]
  ref_med_tte_ucl <- surv_km_table[1,9]
  
  comp1_med_tte_lcl <- surv_km_table[2,8]
  comp1_med_tte_ucl <- surv_km_table[2,9]

  comp2_med_tte_lcl <- surv_km_table[3,8]
  comp2_med_tte_ucl <- surv_km_table[3,9]
 
  tte_median <- data.frame(ref_med_tte,ref_med_tte_lcl,ref_med_tte_ucl,
                           comp1_med_tte,comp1_med_tte_lcl,comp1_med_tte_ucl,
                           comp2_med_tte,comp2_med_tte_lcl,comp2_med_tte_ucl)

  # 25% and 75% Time to Event Percentiles #
  ref_25th <- surv_km_quantile[1,1]
  ref_75th <- surv_km_quantile[1,3]
  
  comp1_25th <- surv_km_quantile[2,1]
  comp1_75th <- surv_km_quantile[2,3]
  
  comp2_25th <- surv_km_quantile[3,1]
  comp2_75th <- surv_km_quantile[3,3]

  tte_quantiles <- data.frame(ref_25th,ref_75th,comp1_25th,comp1_75th,comp2_25th,comp2_75th)
  
  # Minimum and Maximum Time to Events #
  ref_km_min <- min(ref_surv_km_ranges)
  ref_km_max <- max(ref_surv_km_ranges)
  
  comp1_km_min <- min(comp1_surv_km_ranges)
  comp1_km_max <- max(comp1_surv_km_ranges)
  
  comp2_km_min <- min(comp2_surv_km_ranges)
  comp2_km_max <- max(comp2_surv_km_ranges
                      )  
  tte_range <- data.frame(ref_km_min,ref_km_max,comp1_km_min,comp1_km_max,comp2_km_min,comp2_km_max)
  
  # Unstratified COX Proportional Hazard Ratio:  log-rank p-value, HR, LCL, and LCL # 
  pdiff1 <- pchisq(surv_km_test1$chisq, length(surv_km_test1$n)-1, lower.tail = FALSE)
  cox_ph_hr_lcl_ucl <- summary(surv_cox_ph)$conf.int
  comp1_cox_ph_hr <- cox_ph_hr_lcl_ucl[1,1]
  comp1_cox_ph_hr_lcl <- cox_ph_hr_lcl_ucl[1,3]
  comp1_cox_ph_hr_ucl <- cox_ph_hr_lcl_ucl[1,4]
  
  pdiff2 <- pchisq(surv_km_test2$chisq, length(surv_km_test2$n)-1, lower.tail = FALSE)
  comp2_cox_ph_hr <- cox_ph_hr_lcl_ucl[2,1]
  comp2_cox_ph_hr_lcl <- cox_ph_hr_lcl_ucl[2,3]
  comp2_cox_ph_hr_ucl <- cox_ph_hr_lcl_ucl[2,4]
  
  unstrat_cox <- data.frame(pdiff1,comp1_cox_ph_hr,comp1_cox_ph_hr_lcl,comp1_cox_ph_hr_ucl,
                            pdiff2,comp2_cox_ph_hr,comp2_cox_ph_hr_lcl,comp2_cox_ph_hr_ucl)
  
  # Stratified COX Proportional Hazard Ratio:  log-rank p-value, HR, LCL, and LCL # 
  p_strat_diff1 <- pchisq(strat_surv_km_test1$chisq, length(strat_surv_km_test1$n)-1, lower.tail = FALSE)
  strat_cox_ph_hr_lcl_ucl <- summary(strat_surv_cox_ph)$conf.int
  comp1_strat_cox_ph_hr <- strat_cox_ph_hr_lcl_ucl[1,1]
  comp1_strat_cox_ph_hr_lcl <- strat_cox_ph_hr_lcl_ucl[1,3]
  comp1_strat_cox_ph_hr_ucl <- strat_cox_ph_hr_lcl_ucl[1,4]

  p_strat_diff2 <- pchisq(strat_surv_km_test2$chisq, length(strat_surv_km_test2$n)-1, lower.tail = FALSE)
  comp2_strat_cox_ph_hr <- strat_cox_ph_hr_lcl_ucl[2,1]
  comp2_strat_cox_ph_hr_lcl <- strat_cox_ph_hr_lcl_ucl[2,3]
  comp2_strat_cox_ph_hr_ucl <- strat_cox_ph_hr_lcl_ucl[2,4]

  strat_cox <- data.frame(p_strat_diff1,comp1_strat_cox_ph_hr,comp1_strat_cox_ph_hr_lcl,comp1_strat_cox_ph_hr_ucl,
                          p_strat_diff2,comp2_strat_cox_ph_hr,comp2_strat_cox_ph_hr_lcl,comp2_strat_cox_ph_hr_ucl)
  
  tte_tbl_R_data <- list(tte_big_n,tte_np_events,ece_death,ece_dp,tte_np_wo_events,
                         tte_median,tte_quantiles,tte_range,
                         unstrat_cox,strat_cox,time_point_analysis)
  
  # Time-to-Event rtable Generation #
  tte_tbl_R <- rtable(
    col.names = c(paste("DUMMY C\n(N=",tte_tbl_R_data[[1]]$ref_big_n,")",sep=""), 
                  paste("DUMMY B\n(N=",tte_tbl_R_data[[1]]$comp1_big_n,")",sep=""),
                  paste("DUMMY A\n(N=",tte_tbl_R_data[[1]]$comp2_big_n,")",sep="")),
    format = "xx",
    rrow("Patients with event (%)", 
         c(tte_tbl_R_data[[2]]$ref_n_events,   tte_tbl_R_data[[2]]$ref_p_events), 
         c(tte_tbl_R_data[[2]]$comp1_n_events, tte_tbl_R_data[[2]]$comp1_p_events), 
         c(tte_tbl_R_data[[2]]$comp2_n_events, tte_tbl_R_data[[2]]$comp2_p_events), format = "xx (xx.x%)"),
    rrow("Earliest contributing event"),
    rrow("Death",indent=4,
         c(tte_tbl_R_data[[3]]$ref_death), c(tte_tbl_R_data[[3]]$comp1_death), c(tte_tbl_R_data[[3]]$comp2_death), format = "xx"),
    rrow("Disease Progression", indent=4,
         c(tte_tbl_R_data[[4]]$ref_dis_prog), c(tte_tbl_R_data[[4]]$comp1_dis_prog), c(tte_tbl_R_data[[4]]$comp2_dis_prog), format = "xx"),
    rrow("Patients without event (%)", 
         c(tte_tbl_R_data[[5]]$ref_n_wo_events,   tte_tbl_R_data[[5]]$ref_p_wo_events), 
         c(tte_tbl_R_data[[5]]$comp1_n_wo_events, tte_tbl_R_data[[5]]$comp1_p_wo_events), 
         c(tte_tbl_R_data[[5]]$comp2_n_wo_events, tte_tbl_R_data[[5]]$comp2_p_wo_events), format = "xx (xx.x%)"),
    rrow(),
    rrow("Time to Event (Months)"),
    rrow("Median", indent=2,
         c(tte_tbl_R_data[[6]]$ref_med_tte),
         c(tte_tbl_R_data[[6]]$comp1_med_tte),
         c(tte_tbl_R_data[[6]]$comp2_med_tte), format = "xx.x"),
    rrow("95% CI", indent=4,
         c(tte_tbl_R_data[[6]]$ref_med_tte_lcl,   tte_tbl_R_data[[6]]$ref_med_tte_ucl),
         c(tte_tbl_R_data[[6]]$comp1_med_tte_lcl, tte_tbl_R_data[[6]]$comp1_med_tte_ucl),
         c(tte_tbl_R_data[[6]]$comp2_med_tte_lcl, tte_tbl_R_data[[6]]$comp2_med_tte_ucl), format = "(xx.x, xx.x)"),
    rrow("25% and 75%−ile", indent=2,
         c(tte_tbl_R_data[[7]]$ref_25th,   9999),
         c(tte_tbl_R_data[[7]]$comp1_25th, tte_tbl_R_data[[7]]$comp1_75th),
         c(tte_tbl_R_data[[7]]$comp2_25th, 9999), format = "xx.x, xx.x"),
    rrow("Range", indent=2,
         c(tte_tbl_R_data[[8]]$ref_km_min,   tte_tbl_R_data[[8]]$ref_km_max),
         c(tte_tbl_R_data[[8]]$comp1_km_min, tte_tbl_R_data[[8]]$comp1_km_max),
         c(tte_tbl_R_data[[8]]$comp2_km_min, tte_tbl_R_data[[8]]$comp2_km_max), format = "xx.x to xx.x"),
    rrow(),
    rrow("Unstratified Analysis"),
    rrow("p-value (log-rank)", indent=2,
         NULL, c(tte_tbl_R_data[[9]]$pdiff1), c(tte_tbl_R_data[[9]]$pdiff2), format = "xx.xxxx"),
    rrow(),
    rrow("Hazard Ratio", indent=2,
         NULL, c(tte_tbl_R_data[[9]]$comp1_cox_ph_hr), c(tte_tbl_R_data[[9]]$comp2_cox_ph_hr), format = "xx.xx"),
    rrow("95% CI", indent=4,
         NULL, c(tte_tbl_R_data[[9]]$comp1_cox_ph_hr_lcl, tte_tbl_R_data[[9]]$comp1_cox_ph_hr_ucl),
         c(tte_tbl_R_data[[9]]$comp2_cox_ph_hr_lcl, tte_tbl_R_data[[9]]$comp2_cox_ph_hr_ucl),
         format = "(xx.xx, xx.xx)"),
    rrow("Stratified Analysis"),
    rrow("p-value (log-rank)", indent=2,
         NULL, c(tte_tbl_R_data[[10]]$p_strat_diff1), c(tte_tbl_R_data[[10]]$p_strat_diff2), format = "xx.xxxx"),
    rrow(),
    rrow("Hazard Ratio", indent=2,
         NULL, c(tte_tbl_R_data[[10]]$comp1_strat_cox_ph_hr), c(tte_tbl_R_data[[10]]$comp2_strat_cox_ph_hr), format = "xx.xx"),
    rrow("95% CI", indent=4,
         NULL, c(tte_tbl_R_data[[10]]$comp1_strat_cox_ph_hr_lcl, tte_tbl_R_data[[10]]$comp1_strat_cox_ph_hr_ucl),
         c(tte_tbl_R_data[[10]]$comp2_strat_cox_ph_hr_lcl, tte_tbl_R_data[[10]]$comp2_strat_cox_ph_hr_ucl),
         format = "(xx.xx, xx.xx)"),
    rrow(),
    rrow("Time Point Analysis"),
    rrow("6 Months <-- Need to make dynamic & indent!!!"),
    rrow("Patients remaining at risk", indent=4,
         c(tte_tbl_R_data[[11]]$ref_patients_remaining_at_risk), c(tte_tbl_R_data[[11]]$comp1_patients_remaining_at_risk),
         c(tte_tbl_R_data[[11]]$comp2_patients_remaining_at_risk), format = "xx"),
    rrow("Event Free Rate (%)", indent=4,
         c(tte_tbl_R_data[[11]]$ref_patients_event_free_rate),
         c(tte_tbl_R_data[[11]]$comp1_patients_event_free_rate),
         c(tte_tbl_R_data[[11]]$comp2_patients_event_free_rate), format = "xx.xx"),
    rrow("95% CI", indent=6,
         c(tte_tbl_R_data[[11]]$ref_patients_event_free_rate_lcl,tte_tbl_R_data[[11]]$ref_patients_event_free_rate_ucl),
         c(tte_tbl_R_data[[11]]$comp1_patients_event_free_rate_lcl,tte_tbl_R_data[[11]]$comp1_patients_event_free_rate_ucl),
         c(tte_tbl_R_data[[11]]$comp2_patients_event_free_rate_lcl,tte_tbl_R_data[[11]]$comp2_patients_event_free_rate_ucl),
         format = "(xx.xx, xx.xx)"),
    rrow("Difference in Event Free Rate", indent=4,
         NULL, c(tte_tbl_R_data[[11]]$comp1_ref_diff_event_free_rate), 
         c(tte_tbl_R_data[[11]]$comp2_ref_diff_event_free_rate), format = "xx.xx"),
    rrow("95% CI", indent=6,
         NULL, c(tte_tbl_R_data[[11]]$comp1_ref_diff_event_free_rate_lcl,tte_tbl_R_data[[11]]$comp1_ref_diff_event_free_rate_ucl),
         c(tte_tbl_R_data[[11]]$comp2_ref_diff_event_free_rate_lcl,tte_tbl_R_data[[11]]$comp2_ref_diff_event_free_rate_ucl), format = "(xx.xx, xx.xx)"),
    rrow("p-value (Z-test)", indent=4,
         NULL, c(tte_tbl_R_data[[11]]$p_comp1_ref_diff_event_free_rate), c(tte_tbl_R_data[[11]]$p_comp2_ref_diff_event_free_rate), format = "xx.xxxx")
  )
  
  return(tte_tbl_R)

}  
