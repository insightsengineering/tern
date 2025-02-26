# Local data pre-processing
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

adtte_local <- tern_ex_adtte %>%
  preprocess_adtte()

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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

# h_coxreg_mult_cont_df ----

testthat::test_that("h_coxreg_mult_cont_df works as expected", {
  adtte_f <- adtte_local

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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_coxreg_mult_cont_df returns missing values if data is empty (0 rows)", {
  adtte_f <- adtte_local

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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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
  lifecycle::expect_deprecated(lifecycle::expect_deprecated(
    result <- h_tab_surv_one_biomarker(
      df = df,
      vars = c("n_tot", "hr", "ci"),
      time_unit = "months"
    )
  ))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
