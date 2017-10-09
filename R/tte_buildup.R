
library(dplyr)
library(tidyr)
library(survival)
library(broom)

# Obtaining the OS & PFS for FIR

##### ATE.PARAMCD = "OS": Overall survival
##### ATE.PARAMCD = "PFSINV": Investigator progression free survival as per RECISTv1.1

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
