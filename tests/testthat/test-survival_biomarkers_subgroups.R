library(scda)
library(dplyr)

preprocess_adtte <- function(adtte) {

  # Save variable labels before data processing steps.
  adtte_labels <- var_labels(adtte)

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

adtte <- synthetic_cdisc_data("rcd_2021_05_05")$adtte

# extract_survival_biomarkers ----

testthat::test_that("extract_survival_biomarkers functions as expected with valid input and default arguments", {
  adtte_f <- adtte %>%
    preprocess_adtte()

  result <- extract_survival_biomarkers(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      biomarkers = c("AGE", "BMRKR1"),
      subgroups = c("SEX", "BMRKR2")
    ),
    data = adtte_f
  )
  expected <- data.frame(
    biomarker = c(
      "AGE", "BMRKR1", "AGE", "BMRKR1", "AGE", "BMRKR1", "AGE", "BMRKR1", "AGE", "BMRKR1", "AGE", "BMRKR1"
    ),
    biomarker_label = c(
      "Age", "Continous Level Biomarker 1",
      "Age", "Continous Level Biomarker 1", "Age", "Continous Level Biomarker 1",
      "Age", "Continous Level Biomarker 1", "Age", "Continous Level Biomarker 1",
      "Age", "Continous Level Biomarker 1"
    ),
    n_tot = c(
      400L, 400L, 231L, 231L, 169L, 169L, 135L, 135L, 135L, 135L, 130L, 130L
    ),
    n_tot_events = c(
      282, 282, 156, 156, 126, 126, 95, 95, 93,
      93, 94, 94
    ),
    median = c(
      680.959764183532, 680.959764183532, 788.234837746877, 788.234837746877,
      503.653495885665, 503.653495885665, 647.746702484198, 647.746702484198,
      646.406904568478, 646.406904568478, 761.22900999298, 761.22900999298
    ),
    hr = c(
      1.01074759635401, 0.987751007622262, 1.02115033217801, 0.997575735079561,
      0.993668902444139, 0.972576110467666, 1.00440570861299, 1.02441843858899,
      1.01879356004854, 0.978041588357075, 1.0108727138524, 0.957905456915515
    ),
    lcl = c(
      0.995863824494579,
      0.954646877539979, 1.00063613669172, 0.954686123489459, 0.971317032276077,
      0.920843842692934, 0.97680293142405, 0.967897523442556, 0.993176995907114,
      0.923596103946939, 0.986756280347781, 0.89893788149418
    ),
    ucl = c(
      1.02585381495698, 1.02200308408586, 1.04208509234412,
      1.04239217763231, 1.01653513206788, 1.02721465551227, 1.03278849299072,
      1.08423992406607, 1.04507084061928, 1.03569660424963, 1.0355785556806,
      1.02074112491883
    ),
    conf_level = c(
      0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95
    ),
    pval = c(
      0.157841195838841, 0.478567889821418, 0.0432402284499289, 0.913794755204246,
      0.584279175406864, 0.318704863382475, 0.757175472470498,
      0.404764421801377, 0.151849477984424, 0.447395743061151,
      0.380062384987337, 0.184615717075305
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

testthat::test_that("extract_survival_biomarkers works as expected with groups_lists", {
  adtte_f <- adtte %>%
    preprocess_adtte()

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
  expected <- data.frame(
    biomarker = c(
      "AGE", "BMRKR1", "AGE", "BMRKR1", "AGE", "BMRKR1"
    ),
    subgroup = c(
      "low", "low", "low/medium", "low/medium", "low/medium/high", "low/medium/high"
    ),
    row.names = 7:12
  )
  testthat::expect_identical(sub_result, expected)
})

# tabulate_survival_biomarkers ----

testthat::test_that("tabulate_survival_biomarkers works as expected with valid input", {
  skip_if_fail_rtables_refactor()

  adtte_f <- adtte %>%
    preprocess_adtte()

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
        "Total Events", "", "282", "", "156", "126", "", "95", "93",
        "94", "", "282", "", "156", "126", "", "95", "93", "94", "Median (DAYS)",
        "", "681", "", "788.2", "503.7", "", "647.7", "646.4", "761.2",
        "", "681", "", "788.2", "503.7", "", "647.7", "646.4", "761.2",
        "Hazard Ratio", "", "1.01", "", "1.02", "0.99", "", "1.00", "1.02",
        "1.01", "", "0.99", "", "1.00", "0.97", "", "1.02", "0.98", "0.96",
        "95% Wald CI", "", "(1.00, 1.03)", "", "(1.00, 1.04)", "(0.97, 1.02)",
        "", "(0.98, 1.03)", "(0.99, 1.05)", "(0.99, 1.04)", "", "(0.95, 1.02)",
        "", "(0.95, 1.04)", "(0.92, 1.03)", "", "(0.97, 1.08)", "(0.92, 1.04)",
        "(0.90, 1.02)", "p-value (Wald)", "", "0.1578", "", "0.0432",
        "0.5843", "", "0.7572", "0.1518", "0.3801", "", "0.4786", "",
        "0.9138", "0.3187", "", "0.4048", "0.4474", "0.1846"
      )
    )
  )
  testthat::expect_identical(result_matrix, expected_matrix)
  expected_attrs <- list(
    col_x = 4L,
    col_ci = 5L,
    col_symbol_size = 1L,
    forest_header = c("Higher\nBetter", "Lower\nBetter")
  )
  result_attrs <- attributes(result)[names(expected_attrs)]
  testthat::expect_identical(result_attrs, expected_attrs)
})

testthat::test_that("tabulate_survival_biomarkers functions as expected with NULL subgroups", {
  adtte_f <- adtte %>%
    preprocess_adtte()

  df <- testthat::expect_silent(extract_survival_biomarkers(
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      biomarkers = c("AGE", "BMRKR1")
    ),
    data = adtte_f
  ))

  result <- tabulate_survival_biomarkers(df, time_unit = as.character(adtte_f$AVALU[1]))
  result_matrix <- to_string_matrix(result)
  expected_first_col <- c("", "Age", "All Patients", "Continous Level Biomarker 1", "All Patients")
  testthat::expect_identical(result_matrix[, 1L], expected_first_col)
})

testthat::test_that("tabulate_survival_biomarkers works with only a single biomarker in the data frame", {
  skip_if_fail_rtables_refactor()

  df1 <- data.frame(
    biomarker = "BMRKR1",
    biomarker_label = "Continous Level Biomarker 1",
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
  result_matrix <- to_string_matrix(result)
  expected_matrix <- matrix(
    data = c(
      "", "Continous Level Biomarker 1", "All Patients",
      "Total n", "", "400", "Total Events", "", "282", "Median", "",
      "680", "Hazard Ratio", "", "0.98", "95% Wald CI", "", "(0.95, 1.01)",
      "p-value (Wald)", "", "0.3000"
    ),
    nrow = 3,
    ncol = 7
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
