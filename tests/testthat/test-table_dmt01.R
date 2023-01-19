# Tests DMT01

adsl <- adsl_raw
advs <- advs_raw

testthat::test_that("DMT01 default and alternative variants are produced correctly", {
  # Change description in variable SEX.
  adsl <- adsl %>%
    dplyr::mutate(SEX = factor(dplyr::case_when(
      SEX == "M" ~ "Male",
      SEX == "F" ~ "Female",
      SEX == "U" ~ "Unknown",
      SEX == "UNDIFFERENTIATED" ~ "Undifferentiated"
    )))

  adsl <- adsl %>%
    dplyr::mutate(AGEGRP = factor(
      dplyr::case_when(
        dplyr::between(AGE, 18, 40) ~ "18-40",
        dplyr::between(AGE, 41, 64) ~ "41-64",
        AGE > 64 ~ ">=65"
      ),
      levels = c("18-40", "41-64", ">=65")
    ))

  # Obtain SBP, DBP and weight.
  get_param_advs <- function(pname, plabel) {
    ds <- advs %>%
      dplyr::filter(PARAM == plabel & AVISIT == "BASELINE") %>%
      dplyr::select(USUBJID, AVAL)
    colnames(ds) <- c("USUBJID", pname)
    ds
  }

  adsl <- adsl %>%
    dplyr::inner_join(get_param_advs("SBP", "Systolic Blood Pressure"), by = "USUBJID") %>%
    dplyr::inner_join(get_param_advs("DBP", "Diastolic Blood Pressure"), by = "USUBJID") %>%
    dplyr::inner_join(get_param_advs("WGT", "Weight"), by = "USUBJID")

  vars <- c("AGE", "AGEGRP", "SEX", "RACE", "WGT", "SBP", "DBP")
  var_labels <- c(
    "Age (yr)",
    "Age group (yr)",
    "Sex",
    "Race",
    "Weight (kg) at Baseline",
    "Systolic Blood Pressure at Baseline",
    "Diastolic Blood Pressure at Baseline"
  )

  # Default variant for DMT01.
  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_vars(
      vars = vars,
      var_labels = var_labels
    ) %>%
    build_table(adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Alternative variant for DMT01 (Optional Subgrouping Analysis).
  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("SEX") %>%
    summarize_vars(vars = vars[4:7], var_labels = var_labels[4:7]) %>%
    summarize_vars(vars = vars[1:3], var_labels = var_labels[1:3], nested = FALSE) %>%
    build_table(adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
