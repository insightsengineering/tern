library(scda)
library(dplyr)

preprocess_adtte <- function(adtte) {

  # Save variable labels before data processing steps.
  adtte_labels <- formatters::var_labels(adtte)

  adtte_mod <- adtte %>%
    dplyr::filter(PARAMCD == "OS") %>%
    dplyr::mutate(
      AVALU = as.character(AVALU),
      is_event = CNSR == 0
    )

  reapply_varlabels(
    adtte_mod,
    adtte_labels,
    is_event = "Event Flag"
  )
}

adtte <- synthetic_cdisc_data("rcd_2022_02_28")$adtte

# h_surv_to_coxreg_variables ----

testthat::test_that("h_surv_to_coxreg_variables works as expected", {
  result <- testthat::expect_silent(h_surv_to_coxreg_variables(
    variables = list(
      tte = "AVAL",
      is_event = "EVNT",
      covariates = c("A", "B"),
      strata = "D"
    ),
    biomarker = "AGE"
  ))
  expected <- list(
    time = "AVAL",
    event = "EVNT",
    arm = "AGE",
    covariates = c("A", "B"),
    strata = "D"
  )
  testthat::expect_identical(result, expected)
})

# h_coxreg_mult_cont_df ----

testthat::test_that("h_coxreg_mult_cont_df works as expected", {
  adtte_f <- adtte %>%
    preprocess_adtte()

  result <- testthat::expect_silent(h_coxreg_mult_cont_df(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      biomarkers = c("BMRKR1", "AGE"),
      covariates = "SEX",
      strata = "STRATA2"
    ),
    data = adtte_f
  ))
  expected <- data.frame(
    biomarker = c("BMRKR1", "AGE"),
    biomarker_label = c("Continuous Level Biomarker 1", "Age"),
    n_tot = c(400L, 400L),
    n_tot_events = c(282, 282),
    median = c(680.959764183532, 680.959764183532),
    hr = c(0.987763289663857, 1.00846344622999),
    lcl = c(0.954322484256844, 0.993087951019815),
    ucl = c(1.0223759080426, 1.02407699271519),
    conf_level = c(0.95, 0.95),
    pval = c(0.483520242485134, 0.282314224545607),
    pval_label = c("p-value (Wald)", "p-value (Wald)")
  )
  testthat::expect_equal(result, expected, tol = 1e-5)
})

testthat::test_that("h_coxreg_mult_cont_df returns missing values if data is empty (0 rows)", {
  adtte_f <- adtte %>%
    preprocess_adtte()

  result <- testthat::expect_silent(h_coxreg_mult_cont_df(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      biomarkers = c("BMRKR1", "AGE"),
      covariates = "REGION1",
      strata = c("STRATA1", "STRATA2")
    ),
    data = adtte_f[NULL, ]
  ))
  expected <- data.frame(
    biomarker = c("BMRKR1", "AGE"),
    biomarker_label = c("Continuous Level Biomarker 1", "Age"),
    n_tot = c(0L, 0L),
    n_tot_events = c(0L, 0L),
    median = c(NA, NA),
    hr = c(NA, NA),
    lcl = c(NA, NA),
    ucl = c(NA, NA),
    conf_level = c(0.95, 0.95),
    pval = c(NA, NA),
    pval_label = c("p-value (Wald)", "p-value (Wald)")
  )
  testthat::expect_identical(result, expected)
})

# h_tab_surv_one_biomarker ----

testthat::test_that("h_tab_surv_one_biomarker works as expected", {
  df <- data.frame(
    n_tot = c(48L, 48L),
    n_tot_events = c(25L, 25L),
    median = c(1269.40388857211, 1269.40388857211),
    hr = c(0.992727618706316, 1.00485769099575),
    lcl = c(0.859391304891713, 0.950491104268725),
    ucl = c(1.14675133356916, 1.06233396043214),
    conf_level = c(0.95, 0.95),
    pval = c(0.920991170690111, 0.864415775291559),
    pval_label = c("p-value (Wald)", "p-value (Wald)"),
    subgroup = c("All patients", "All patients"),
    row_type = c("content", "content"),
    var = c("ALL", "ALL"),
    var_label = c("All patients", "All patients")
  )
  result <- testthat::expect_silent(h_tab_surv_one_biomarker(
    df = df,
    vars = c("n_tot", "hr", "ci"),
    time_unit = "months"
  ))
  result_matrix <- to_string_matrix(result)
  expected_matrix <- matrix(
    nrow = 3, ncol = 4,
    data = c(
      "", "All patients", "All patients", "Total n", "48",
      "48", "Hazard Ratio", "0.99", "1.00", "95% Wald CI", "(0.86, 1.15)",
      "(0.95, 1.06)"
    )
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
