# Data loading for tests
adsl_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adsl")
adab_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adab")
adae_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adae")
adaette_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adaette")
adcm_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adcm")
addv_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "addv")
adeg_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adeg")
adex_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adex")
adlb_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adlb")
admh_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "admh")
adpc_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adpc")
adpp_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adpp")
adpc_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adpc")
adqs_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adqs")
adrs_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adrs")
adsub_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adsub")
adtte_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "adtte")
advs_raw <- scda::synthetic_cdisc_dataset("rcd_2022_06_27", "advs")

# Data as DM from `formatters`
adsl_dm <- adsl_raw %>%
  dplyr::select(c("AGE", "SEX", "RACE", "COUNTRY", "ARM", "BMRKR1", "STRATA1")) %>%
  dplyr::mutate(ID = paste0("S", seq_len(nrow(adsl_raw))))
adsl_dm <- var_relabel(adsl_dm, ID = "subject id")

# Bladder data from survival (previously function `get_bladder`)
dta_bladder_raw <- local({
  # Setting general random for data generation
  set.seed(1, kind = "Mersenne-Twister")
  dta_bladder <- with(
    data = survival::bladder[survival::bladder$enum < 5, ],
    data.frame(
      time = stop,
      status = event,
      arm = paste("ARM:", as.factor(rx)),
      armcd = as.factor(rx),
      covar1 = as.factor(enum),
      covar2 = factor(
        sample(as.factor(enum)),
        levels = 1:4, labels = c("F", "F", "M", "M")
      )
    )
  )
  attr(dta_bladder$armcd, "label") <- "ARM"
  attr(dta_bladder$covar1, "label") <- "A Covariate Label"
  attr(dta_bladder$covar2, "label") <- "Sex (F/M)"
  dta_bladder$age <- sample(
    20:60,
    size = nrow(dta_bladder), replace = TRUE
  )
  dta_bladder
})
