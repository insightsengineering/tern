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

# extract_survival_biomarkers ----

testthat::test_that("extract_survival_biomarkers functions as expected with valid input and default arguments", {
  adtte_f <- adtte_local

  result <- extract_survival_biomarkers(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      biomarkers = c("AGE", "BMRKR1"),
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adtte_f
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("extract_survival_biomarkers works as expected with groups_lists", {
  adtte_f <- adtte_local

  result <- extract_survival_biomarkers(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      biomarkers = c("AGE", "BMRKR1"),
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adtte_f,
    groups_lists = list(
      BMRKR2 = list(
        "low" = "LOW",
        "low/medium" = c("LOW", "MEDIUM"),
        "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
      )
    )
  )
  sub_result <- subset(
    result,
    var == "BMRKR2",
    select = c(biomarker, subgroup)
  )

  res <- testthat::expect_silent(sub_result)
  testthat::expect_snapshot(res)
})

# tabulate_survival_biomarkers ----

testthat::test_that("tabulate_survival_biomarkers works as expected with valid input", {
  adtte_f <- adtte_local

  df <- extract_survival_biomarkers(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      biomarkers = c("AGE", "BMRKR1"),
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adtte_f
  )
  result <- tabulate_survival_biomarkers(df, time_unit = as.character(adtte_f$AVALU[1]))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(attributes(result)[c("col_x", "col_ci", "col_symbol_size", "forest_header")])
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_survival_biomarkers functions as expected with NULL subgroups", {
  adtte_f <- adtte_local

  df <- testthat::expect_silent(extract_survival_biomarkers(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      biomarkers = c("AGE", "BMRKR1")
    ),
    data = adtte_f
  ))
  result <- tabulate_survival_biomarkers(df, time_unit = as.character(adtte_f$AVALU[1]))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_survival_biomarkers works with only a single biomarker in the data frame", {
  df1 <- data.frame(
    biomarker = "BMRKR1",
    biomarker_label = "Continuous Level Biomarker 1",
    n_tot = 400L,
    n_tot_events = 282,
    median = 680,
    hr = 0.98,
    lcl = 0.95,
    ucl = 1.01,
    conf_level = 0.95,
    pval = 0.3,
    pval_label = "p-value (Wald)",
    subgroup = "All Patients",
    var = "ALL",
    var_label = "All Patients",
    row_type = "content"
  )
  result <- testthat::expect_silent(tabulate_survival_biomarkers(df1))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_survival_biomarkers na_str argument works as expected", {
  adtte_f <- adtte_local

  df <- extract_survival_biomarkers(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      biomarkers = c("AGE", "BMRKR1"),
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adtte_f
  )
  df$hr[2:5] <- NA

  result <- tabulate_survival_biomarkers(
    df,
    time_unit = as.character(adtte_f$AVALU[1]),
    na_str = "<No data>"
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
