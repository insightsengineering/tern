# Local data pre-processing
preprocess_adrs <- function(adrs) {
  # Save variable labels before data processing steps.
  adrs_labels <- formatters::var_labels(adrs)

  adrs_mod <- adrs %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::mutate(rsp = AVALC == "CR")

  reapply_varlabels(
    adrs_mod,
    adrs_labels,
    rsp = "Response"
  )
}

adrs_local <- tern_ex_adrs %>%
  preprocess_adrs()

# extract_rsp_biomarkers ----

testthat::test_that("extract_rsp_biomarkers functions as expected with valid input and default arguments", {
  adrs_f <- adrs_local

  result <- testthat::expect_silent(extract_rsp_biomarkers(
    variables = list(
      rsp = "rsp",
      biomarkers = c("AGE", "BMRKR1"),
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adrs_f
  ))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("extract_rsp_biomarkers works as expected with other custom options", {
  adrs_f <- adrs_local

  result <- testthat::expect_silent(extract_rsp_biomarkers(
    variables = list(
      rsp = "BMRKR1",
      biomarkers = "AGE",
      subgroups = c("SEX", "BMRKR2"),
      strat = c("STRATA1", "STRATA2")
    ),
    data = adrs_f,
    groups_lists = list(
      BMRKR2 = list(
        "low" = "LOW",
        "low/medium" = c("LOW", "MEDIUM"),
        "low/medium/high" = c("LOW", "MEDIUM", "HIGH")
      )
    ),
    control = control_logistic(
      response_definition = "I(response > 3)",
      conf_level = 0.9
    )
  ))

  # Check that `groups_list` was respected.
  sub_result <- subset(
    result,
    var == "BMRKR2",
    select = c(biomarker, subgroup)
  )

  res <- testthat::expect_silent(sub_result)
  testthat::expect_snapshot(res)

  # Check that custom control options were respected.
  testthat::expect_equal(unique(result$conf_level), 0.9)
  result_n_rsp <- subset(
    result,
    biomarker == "AGE" & var == "ALL"
  )$n_rsp
  expected_n_rsp <- sum(adrs_f$BMRKR1 > 3)
  testthat::expect_identical(result_n_rsp, expected_n_rsp)
})

# tabulate_rsp_biomarkers ----

testthat::test_that("tabulate_rsp_biomarkers works as expected with valid input", {
  adrs_f <- adrs_local

  df <- extract_rsp_biomarkers(
    variables = list(
      rsp = "rsp",
      biomarkers = c("AGE", "BMRKR1"),
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adrs_f
  )

  result <- tabulate_rsp_biomarkers(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(attributes(result)[c("col_x", "col_ci", "col_symbol_size", "forest_header")])
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_rsp_biomarkers functions as expected with NULL subgroups", {
  adrs_f <- adrs_local

  df <- testthat::expect_silent(extract_rsp_biomarkers(
    variables = list(
      rsp = "rsp",
      biomarkers = c("AGE", "BMRKR1")
    ),
    data = adrs_f
  ))
  result <- tabulate_rsp_biomarkers(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_rsp_biomarkers works with only a single biomarker in the data frame", {
  df1 <- data.frame(
    biomarker = "BMRKR1",
    biomarker_label = "Continuous Level Biomarker 1",
    n_tot = 400L,
    n_rsp = 282L,
    prop = 0.705,
    or = 0.98,
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
  result <- testthat::expect_silent(tabulate_rsp_biomarkers(df1))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("tabulate_rsp_biomarkers na_str argument works as expected", {
  adrs_f <- adrs_local

  df <- extract_rsp_biomarkers(
    variables = list(
      rsp = "rsp",
      biomarkers = c("AGE", "BMRKR1"),
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adrs_f
  )
  df$or[2:5] <- NA

  result <- tabulate_rsp_biomarkers(df, na_str = "<No data>")

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
