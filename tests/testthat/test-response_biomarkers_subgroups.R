library(scda)
library(dplyr)

preprocess_adrs <- function(adrs) {

  # Save variable labels before data processing steps.
  adrs_labels <- var_labels(adrs)

  adrs_mod <- adrs %>%
    dplyr::filter(PARAMCD == "BESRSPI") %>%
    dplyr::mutate(rsp = AVALC == "CR")

  reapply_varlabels(
    adrs_mod,
    adrs_labels,
    rsp = "Response"
  )
}

adrs <- synthetic_cdisc_data("rcd_2021_05_05")$adrs

# extract_rsp_biomarkers ----

testthat::test_that("extract_rsp_biomarkers functions as expected with valid input and default arguments", {

  adrs_f <- adrs %>%
    preprocess_adrs()

  result <- testthat::expect_silent(extract_rsp_biomarkers(
    variables = list(
      rsp = "rsp",
      biomarkers = c("AGE", "BMRKR1"),
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adrs_f
  ))
  expected <- data.frame(
    biomarker = c(
      "AGE", "BMRKR1", "AGE", "BMRKR1",
      "AGE", "BMRKR1", "AGE", "BMRKR1", "AGE", "BMRKR1", "AGE", "BMRKR1"
    ),
    biomarker_label = c(
      "Age", "Continous Level Biomarker 1",
      "Age", "Continous Level Biomarker 1", "Age", "Continous Level Biomarker 1",
      "Age", "Continous Level Biomarker 1", "Age", "Continous Level Biomarker 1",
      "Age", "Continous Level Biomarker 1"
    ),
    n_tot = c(
      400L, 400L, 231L, 231L, 169L, 169L, 135L, 135L, 135L, 135L,
      130L, 130L
    ),
    n_rsp = c(
      336L, 336L, 195L, 195L, 141L, 141L, 120L, 120L,
      110L, 110L, 106L, 106L
    ),
    prop = c(
      0.84, 0.84, 0.844155844155844,
      0.844155844155844, 0.834319526627219, 0.834319526627219,
      0.888888888888889, 0.888888888888889, 0.814814814814815,
      0.814814814814815, 0.815384615384615, 0.815384615384615
    ),
    or = c(
      0.998321299027899, 1.05690711265267, 0.997859264518495,
      1.03583638026983, 0.999996052481521, 1.09180032551023, 1.04192856028532,
      1.0638473841249, 1.01918928452218, 1.02242545380001, 0.968020609012163,
      1.10849199401806
    ),
    lcl = c(
      0.963153348599099, 0.971293742055313,
      0.94756817254846, 0.931352568424444, 0.950633258323825, 0.950361210619495,
      0.961591844951288, 0.887863374908248, 0.957123054333348,
      0.901691827779645, 0.918858442289107, 0.953005429324874
    ),
    ucl = c(
      1.03477334896086, 1.15006675777819, 1.05081949840867,
      1.15204171123468, 1.05192207007551, 1.25428935594627, 1.1289770503339,
      1.27471330465269, 1.08528030223693, 1.15932492274247, 1.01981312500957,
      1.28934680012538
    ),
    conf_level = c(
      0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95
    ),
    pval = c(
      0.926839705052765, 0.19908298761646, 0.935265419560033, 0.516321274078353, 0.99987805458886,
      0.214707046950953, 0.31572224952438, 0.502328023711066, 0.553230863708264,
      0.729408056352762, 0.221632714057668, 0.181634303465231
    ),
    pval_label = c(
      "p-value (Wald)", "p-value (Wald)",
      "p-value (Wald)", "p-value (Wald)", "p-value (Wald)", "p-value (Wald)",
      "p-value (Wald)", "p-value (Wald)", "p-value (Wald)", "p-value (Wald)",
      "p-value (Wald)", "p-value (Wald)"
    ),
    subgroup = c(
      "All Patients",
      "All Patients", "F", "F", "M", "M", "LOW", "LOW", "MEDIUM", "MEDIUM",
      "HIGH", "HIGH"
    ),
    var = c(
      "ALL", "ALL", "SEX", "SEX", "SEX", "SEX",
      "BMRKR2", "BMRKR2", "BMRKR2", "BMRKR2", "BMRKR2", "BMRKR2"
    ),
    var_label = c(
      "All Patients", "All Patients", "Sex", "Sex",
      "Sex", "Sex", "Categorical Level Biomarker 2", "Categorical Level Biomarker 2",
      "Categorical Level Biomarker 2", "Categorical Level Biomarker 2",
      "Categorical Level Biomarker 2", "Categorical Level Biomarker 2"
    ),
    row_type = c(
      "content", "content", "analysis", "analysis",
      "analysis", "analysis", "analysis", "analysis", "analysis",
      "analysis", "analysis", "analysis"
    )
  )
  testthat::expect_equal(result, expected, tolerance = 1e-5)
})

testthat::test_that("extract_rsp_biomarkers works as expected with other custom options", {

  adrs_f <- adrs %>%
    preprocess_adrs()

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
  expected <- data.frame(
    biomarker = c("AGE", "AGE", "AGE"),
    subgroup = c("low", "low/medium", "low/medium/high"),
    row.names = 4:6
  )
  testthat::expect_identical(sub_result, expected)

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

  adrs_f <- adrs %>%
    preprocess_adrs()

  df <- extract_rsp_biomarkers(
    variables = list(
      rsp = "rsp",
      biomarkers = c("AGE", "BMRKR1"),
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adrs_f
  )

  result <- tabulate_rsp_biomarkers(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    matrix(
      nrow = 19,
      ncol = 7,
      data = c(
        "", "Age", "All Patients", "Sex", "F", "M", "Categorical Level Biomarker 2",
        "LOW", "MEDIUM", "HIGH", "Continous Level Biomarker 1", "All Patients",
        "Sex", "F", "M", "Categorical Level Biomarker 2", "LOW", "MEDIUM",
        "HIGH", "Total n", "", "400", "", "231", "169", "", "135", "135",
        "130", "", "400", "", "231", "169", "", "135", "135", "130",
        "Responders", "", "336", "", "195", "141", "", "120", "110",
        "106", "", "336", "", "195", "141", "", "120", "110", "106",
        "Response (%)", "", "84%", "", "84.4%", "83.4%", "", "88.9%",
        "81.5%", "81.5%", "", "84%", "", "84.4%", "83.4%", "", "88.9%",
        "81.5%", "81.5%", "Odds Ratio", "", "1.00", "", "1.00", "1.00",
        "", "1.04", "1.02", "0.97", "", "1.06", "", "1.04", "1.09", "",
        "1.06", "1.02", "1.11", "95% CI", "", "(0.96, 1.03)", "", "(0.95, 1.05)",
        "(0.95, 1.05)", "", "(0.96, 1.13)", "(0.96, 1.09)", "(0.92, 1.02)",
        "", "(0.97, 1.15)", "", "(0.93, 1.15)", "(0.95, 1.25)", "", "(0.89, 1.27)",
        "(0.90, 1.16)", "(0.95, 1.29)", "p-value (Wald)", "", "0.9268",
        "", "0.9353", "0.9999", "", "0.3157", "0.5532", "0.2216", "",
        "0.1991", "", "0.5163", "0.2147", "", "0.5023", "0.7294", "0.1816"
      )
    )
  )
  testthat::expect_identical(result_matrix, expected_matrix)
  expected_attrs <- list(
    col_x = 4L,
    col_ci = 5L,
    col_symbol_size = 1L,
    forest_header = c("Lower\nBetter", "Higher\nBetter")
  )
  result_attrs <- attributes(result)[names(expected_attrs)]
  testthat::expect_identical(result_attrs, expected_attrs)
})

testthat::test_that("tabulate_rsp_biomarkers functions as expected with NULL subgroups", {

  adrs_f <- adrs %>%
    preprocess_adrs()

  df <- testthat::expect_silent(extract_rsp_biomarkers(
    variables = list(
      rsp = "rsp",
      biomarkers = c("AGE", "BMRKR1")
    ),
    data = adrs_f
  ))

  result <- tabulate_rsp_biomarkers(df)
  result_matrix <- to_string_matrix(result)
  expected_first_col <- c("", "Age", "All Patients", "Continous Level Biomarker 1", "All Patients")
  testthat::expect_identical(result_matrix[, 1L], expected_first_col)
})

testthat::test_that("tabulate_rsp_biomarkers works with only a single biomarker in the data frame", {
  df1 <- data.frame(
    biomarker = "BMRKR1",
    biomarker_label = "Continous Level Biomarker 1",
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
  result_matrix <- to_string_matrix(result)
  expected_matrix <- matrix(
    data = c(
      "", "Continous Level Biomarker 1", "All Patients",
      "Total n", "", "400", "Responders", "", "282", "Response (%)",
      "", "70.5%", "Odds Ratio", "", "0.98", "95% CI", "", "(0.95, 1.01)",
      "p-value (Wald)", "", "0.3000"
    ),
    nrow = 3,
    ncol = 7
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
