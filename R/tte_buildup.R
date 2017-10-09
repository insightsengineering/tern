
library(dplyr)
library(tidyr)
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

fir_asl <- haven::read_sas("./REPRO_DATA/FIR/asl.sas7bdat")
fir_ate <- haven::read_sas("./REPRO_DATA/FIR/ate.sas7bdat")

fir_os <- dplyr::select(fir_ate,USUBJID,PARAMCD,PARAM,EVNTDESC,CNSR,AVAL,AVALU) %>%
  dplyr::filter(PARAMCD == "OS")

fir_pfs <- dplyr::select(fir_ate,USUBJID,PARAMCD,PARAM,EVNTDESC,CNSR,AVAL,AVALU) %>%
  dplyr::filter(PARAMCD == "PFSINV")


# Reproducing Median OS Survival Times + 95% Confidence Intervals for FIR

fir_os_check <- dplyr::filter(fir_asl, SAFFL == "Y") %>%
                dplyr::select(USUBJID,ARM) %>%
                dplyr::inner_join(., fir_os, by=c("USUBJID")) %>%
                dplyr::mutate(time   = AVAL * (1 / 30.4368499),
                              status = ifelse(is.na(CNSR),NA,
                                       ifelse(CNSR==1,0,1))) 

fir.os <- survival::survfit(formula = Surv(time, status) ~ ARM, data=fir_os_check, conf.type="plain")
print(fir.os)
fir.os.check <- broom::tidy(fir.os)
fir.os.check


# Reproducing Median PFS Survival Times + 95% Confidence Intervals for FIR

fir_pfs_check <- dplyr::filter(fir_asl, SAFFL == "Y") %>%
                 dplyr::select(USUBJID,ARM) %>%
                 dplyr::inner_join(., fir_pfs, by=c("USUBJID")) %>%
                 dplyr::mutate(time   = AVAL * (1 / 30.4368499),
                               status = ifelse(is.na(CNSR),NA,
                               ifelse(CNSR==1,0,1)))

fir.pfs <- survival::survfit(formula = Surv(time, status) ~ ARM, data=fir_pfs_check, conf.type="plain")
print(fir.pfs)
fir.pfs.check <- broom::tidy(fir.pfs)
fir.pfs.check
