
library(haven)
library(survival)
library(broom)

#=============================================================================#
# Key Statistics to Obtain for the TTE Summary Table:                         #
# ---------------------------------------------------                         #
# Reference & Treatment Groups <=== User-Specified                            #
# Treatment Variable (e.g. ARM) <=== User-Specified                           #
# Type of Analysis (OS vs. PFS) <=== User-Specified                           #
# ASL and ATE Filters (e.g. ITTFL) <=== User-Specified                        #
# Time Point of Interest <=== User-Specified                                  #
#                                                                             #
# * "Big N"                                                                   #
#                                                                             #
# * n and percentage ~> patients with event [km]                              #
# * n and percentage ~> patients without event [km]                           #
# * median (50th-percentile) of time-to-event (months) [km]                   #
# * 95% CIs for the median time-to-event [km]                                 #
# * 25th-percentile and 75th-percentile of time-to-event (months) [km]        #
# * range (minimum, maximum) of time-to-event (months) [km]                   #
#                                                                             #
# * p-value from unstratified analysis (log-rank) [cox-ph]                    #
# * un-stratified hazard ratio [cox-ph]                                       #
# * 95% CI for hazard ratio [cox-ph]                                          #
# * p-value from stratified analysis (log-rank) [cox-ph] <=== User-Specified  #
# * stratified hazard ratio [cox-ph] <=== User-Specified                      #
#                                                                             #
# * time point of interest <=== User-Specified                                #
# * n and percentage ~> patients remaining at risk @ time point               #
# * event-free-rate (percentage) @ time point                                 #
# * 95% CI for the event-free-rate @ time point                               #
# * difference in the event-free-rate @ time point                            #
#                                                                             #
#=============================================================================#

# Obtaining the OS & PFS for FIR
# ATE.PARAMCD = "OS": Overall survival
# ATE.PARAMCD = "PFSINV": Investigator progression free survival as per RECISTv1.1

drop_label <- function(x){
  for (j in (1:ncol(x))){
    attr(x[[j]],"label") <- ""
  }
  return(x)
}

asl_ <- haven::read_sas("./REPRO_DATA/FIR/asl.sas7bdat")
ate_ <- haven::read_sas("./REPRO_DATA/FIR/ate.sas7bdat")

asl <- drop_label(asl_)
ate <- drop_label(ate_)

tte_vars <- c("USUBJID","PARAMCD","PARAM","EVNTDESC","CNSR","AVAL","AVALU")
tte1 <- ate[tte_vars]
tte2 <- tte1[which(tte1$PARAMCD == "OS"),]
tte2$time <- tte2$AVAL * (1 / 30.4368499)
tte2$status <- ifelse(is.na(tte2$CNSR),NA,
               ifelse(tte2$CNSR==1,0,1))

demo_vars <- c("USUBJID","ARM","SAFFL")
demo1 <- asl[demo_vars]
demo1$ARM <- as.factor(demo1$ARM)
demo2 <- demo1[which(demo1$SAFFL == "Y"),]

BIG_N <- aggregate(x=demo2,by=list(demo2$ARM),FUN=length)[,c(1,2)]
colnames(BIG_N) <- c("ARM","BIG_N")

tte3 <- merge(demo2, tte2, by=c("USUBJID"))

arm_parse <- function(x,y){
  
  n_row <- nrow(surv_km_results_tidy)
  
  for (i in 1:n_row) {
    x[i,y] <- strsplit(x[i,strata],"=")
  }
  
  var_list = unlist(strsplit(x,"-"))
  return(var_list)
}

surv_km_results <- survival::survfit(formula = Surv(time, status) ~ ARM, data=tte3, conf.type="plain")
surv_km_results_summary <- summary(surv_km_results)
surv_km_results_tidy <- broom::tidy(surv_km_results)

n_patients_event <- aggregate(x=surv_km_results_tidy,by=list(d))
n_patients_censored

print(surv_km_results)
