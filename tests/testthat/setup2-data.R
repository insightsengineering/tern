# Data loading for tests
test_data <- scda::synthetic_cdisc_data("rcd_2022_02_28")
test_data_recent <- scda::synthetic_cdisc_data("rcd_2022_06_27")

adlb_raw <- test_data$adlb
adsl_raw <- test_data$adsl
adae_raw <- test_data$adae
adaette_raw <- test_data$adaette
adpp_raw <- test_data$adpp
adsub_raw <- test_data$adsub
adqs_raw <- test_data$adqs
adcm_raw <- test_data$adcm
advs_raw <- test_data$advs
adeg_raw <- test_data$adeg
adex_raw <- test_data$adex
adrs_raw <- test_data_recent$adrs
adtte_raw <- test_data_recent$adtte

# Data as DM from `formatters` ## to discuss
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
