library(scda)
library(rtables)
library(dplyr)

adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb
adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl

testthat::test_that("h_adsl_adlb_merge_using_worst_flag generates missing for patients without any lab", {
  adsl_f <- adsl %>% dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))
  adlb_f <- adlb %>% dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128"))

  result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  result_matrix <- matrix(
    c(as.matrix(result[c("USUBJID", "ARMCD", "AVISIT", "PARAMCD", "PARAM", "ATOXGR", "BTOXGR")])),
    6L,
    7L
  )

  expected_matrix <- structure(
    c(
      rep(c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"), each = 3L),
      rep(c("ARM A", "ARM C"), each = 3L),
      c("WEEK 3 DAY 22", "WEEK 5 DAY 36", "WEEK 3 DAY 22", "<Missing>", "<Missing>", "<Missing>"),
      rep(c("ALT", "CRP", "IGA"), 2L),
      rep(
        c("Alanine Aminotransferase Measurement", "C-Reactive Protein Measurement", "Immunoglobulin A Measurement"),
        2L
      ),
      c("2", "0", "0", "<Missing>", "<Missing>", "<Missing>"),
      c("0", "0", "3", "<Missing>", "<Missing>", "<Missing>")
    ),
    .Dim = c(6L, 7L)
  )

  expected_matrix <- matrix(c(expected_matrix), 6L, 7L)

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("h_adsl_adlb_merge_using_worst_flag generates missing for patients missing baseline lab", {
  adsl_f <- adsl %>% dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))
  adlb_f <- adlb %>% dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))

  adlb_f[adlb_f$USUBJID == "AB12345-CHN-15-id-262", ]$BTOXGR <- NA # nolint

  result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  result_matrix <- matrix(
    c(as.matrix(result[c("USUBJID", "ARMCD", "AVISIT", "PARAMCD", "PARAM", "ATOXGR", "BTOXGR")])),
    6L,
    7L
  )

  expected_matrix <- structure(
    c(
      rep(c("AB12345-CHN-15-id-262", "AB12345-CHN-3-id-128"), each = 3L),
      rep(c("ARM C", "ARM A"), each = 3L),
      c("WEEK 5 DAY 36", "WEEK 1 DAY 8", "WEEK 4 DAY 29", "WEEK 3 DAY 22", "WEEK 5 DAY 36", "WEEK 3 DAY 22"),
      rep(c("ALT", "CRP", "IGA"), 2L),
      rep(
        c("Alanine Aminotransferase Measurement", "C-Reactive Protein Measurement", "Immunoglobulin A Measurement"),
        2L
      ),
      c("0", "0", "0", "2", "0", "0"),
      c("<Missing>", "<Missing>", "<Missing>", "0", "0", "3")
    ),
    .Dim = c(6L, 7L)
  )

  expected_matrix <- matrix(c(expected_matrix), 6L, 7L)

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("h_adsl_adlb_merge_using_worst_flag generates missing for patients missing post-baseline lab", {
  adsl_f <- adsl %>% dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))
  adlb_f <- adlb %>% dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))

  adlb_f[adlb_f$USUBJID == "AB12345-CHN-15-id-262", ]$ATOXGR <- NA # nolint

  result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  result_matrix <- matrix(
    c(as.matrix(result[c("USUBJID", "ARMCD", "AVISIT", "PARAMCD", "PARAM", "ATOXGR", "BTOXGR")])),
    6L,
    7L
  )

  expected_matrix <- structure(
    c(
      rep(c("AB12345-CHN-15-id-262", "AB12345-CHN-3-id-128"), each = 3L),
      rep(c("ARM C", "ARM A"), each = 3L),
      c("WEEK 5 DAY 36", "WEEK 1 DAY 8", "WEEK 4 DAY 29", "WEEK 3 DAY 22", "WEEK 5 DAY 36", "WEEK 3 DAY 22"),
      rep(c("ALT", "CRP", "IGA"), 2L),
      rep(
        c("Alanine Aminotransferase Measurement", "C-Reactive Protein Measurement", "Immunoglobulin A Measurement"),
        2L
      ),
      c("<Missing>", "<Missing>", "<Missing>", "2", "0", "0"),
      c("0", "2", "0", "0", "0", "3")
    ),
    .Dim = c(6L, 7L)
  )

  expected_matrix <- matrix(c(expected_matrix), 6L, 7L)

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that(
  "h_adsl_adlb_merge_using_worst_flag generates missing for patients without
  any post-baseline values flagged as the worst",
  code = {
    adsl_f <- adsl %>% dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))
    adlb_f <- adlb %>% dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))

    adlb_f[adlb_f$USUBJID == "AB12345-CHN-15-id-262", ]$WGRHIFL <- "" # nolint

    result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

    result_matrix <- matrix(
      c(as.matrix(result[c("USUBJID", "ARMCD", "AVISIT", "PARAMCD", "PARAM", "ATOXGR", "BTOXGR")])),
      6L,
      7L
    )

    expected_matrix <- structure(
      c(
        rep(c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"), each = 3L),
        rep(c("ARM A", "ARM C"), each = 3L),
        c("WEEK 3 DAY 22", "WEEK 5 DAY 36", "WEEK 3 DAY 22", "<Missing>", "<Missing>", "<Missing>"),
        rep(c("ALT", "CRP", "IGA"), 2L),
        rep(
          c("Alanine Aminotransferase Measurement", "C-Reactive Protein Measurement", "Immunoglobulin A Measurement"),
          2L
        ),
        c("2", "0", "0", "<Missing>", "<Missing>", "<Missing>"),
        c("0", "0", "3", "<Missing>", "<Missing>", "<Missing>")
      ),
      .Dim = c(6L, 7L)
    )

    expected_matrix <- matrix(c(expected_matrix), 6L, 7L)

    testthat::expect_identical(result_matrix, expected_matrix)
  }
)

testthat::test_that("h_adsl_adlb_merge_using_worst_flag generates missing and by visit lab results", {
  adsl_f <- adsl %>% dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))
  adlb_f <- adlb %>% dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128"))

  result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIVFL" = "Y"), by_visit = TRUE)

  result_matrix <- matrix(
    c(as.matrix(result[c("USUBJID", "ARMCD", "AVISIT", "PARAMCD", "PARAM", "ATOXGR", "BTOXGR")])),
    30L,
    7L
  )

  expected_matrix <- structure(
    c(
      "AB12345-CHN-3-id-128", "AB12345-CHN-3-id-128", "AB12345-CHN-3-id-128",
      "AB12345-CHN-3-id-128", "AB12345-CHN-3-id-128", "AB12345-CHN-3-id-128",
      "AB12345-CHN-3-id-128", "AB12345-CHN-3-id-128", "AB12345-CHN-3-id-128",
      "AB12345-CHN-3-id-128", "AB12345-CHN-3-id-128", "AB12345-CHN-3-id-128",
      "AB12345-CHN-3-id-128", "AB12345-CHN-3-id-128", "AB12345-CHN-3-id-128",
      "AB12345-CHN-15-id-262", "AB12345-CHN-15-id-262", "AB12345-CHN-15-id-262",
      "AB12345-CHN-15-id-262", "AB12345-CHN-15-id-262", "AB12345-CHN-15-id-262",
      "AB12345-CHN-15-id-262", "AB12345-CHN-15-id-262", "AB12345-CHN-15-id-262",
      "AB12345-CHN-15-id-262", "AB12345-CHN-15-id-262", "AB12345-CHN-15-id-262",
      "AB12345-CHN-15-id-262", "AB12345-CHN-15-id-262", "AB12345-CHN-15-id-262",
      "ARM A", "ARM A", "ARM A", "ARM A", "ARM A", "ARM A", "ARM A",
      "ARM A", "ARM A", "ARM A", "ARM A", "ARM A", "ARM A", "ARM A",
      "ARM A", "ARM C", "ARM C", "ARM C", "ARM C", "ARM C", "ARM C",
      "ARM C", "ARM C", "ARM C", "ARM C", "ARM C", "ARM C", "ARM C",
      "ARM C", "ARM C", "WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22",
      "WEEK 4 DAY 29", "WEEK 5 DAY 36", "WEEK 1 DAY 8", "WEEK 2 DAY 15",
      "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36", "WEEK 1 DAY 8",
      "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36",
      "WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29",
      "WEEK 5 DAY 36", "WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22",
      "WEEK 4 DAY 29", "WEEK 5 DAY 36", "WEEK 1 DAY 8", "WEEK 2 DAY 15",
      "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36", "ALT", "ALT",
      "ALT", "ALT", "ALT", "CRP", "CRP", "CRP", "CRP", "CRP", "IGA",
      "IGA", "IGA", "IGA", "IGA", "ALT", "ALT", "ALT", "ALT", "ALT",
      "CRP", "CRP", "CRP", "CRP", "CRP", "IGA", "IGA", "IGA", "IGA",
      "IGA", "Alanine Aminotransferase Measurement", "Alanine Aminotransferase Measurement",
      "Alanine Aminotransferase Measurement", "Alanine Aminotransferase Measurement",
      "Alanine Aminotransferase Measurement", "C-Reactive Protein Measurement",
      "C-Reactive Protein Measurement", "C-Reactive Protein Measurement",
      "C-Reactive Protein Measurement", "C-Reactive Protein Measurement",
      "Immunoglobulin A Measurement", "Immunoglobulin A Measurement", "Immunoglobulin A Measurement",
      "Immunoglobulin A Measurement", "Immunoglobulin A Measurement",
      "Alanine Aminotransferase Measurement", "Alanine Aminotransferase Measurement",
      "Alanine Aminotransferase Measurement", "Alanine Aminotransferase Measurement",
      "Alanine Aminotransferase Measurement", "C-Reactive Protein Measurement",
      "C-Reactive Protein Measurement", "C-Reactive Protein Measurement",
      "C-Reactive Protein Measurement", "C-Reactive Protein Measurement",
      "Immunoglobulin A Measurement", "Immunoglobulin A Measurement", "Immunoglobulin A Measurement",
      "Immunoglobulin A Measurement", "Immunoglobulin A Measurement",
      "-2", "-3", "2", "0", "-3", "-1", "0", "0", "0", "0",
      "0", "0", "0", "0", "0", "<Missing>", "<Missing>", "<Missing>",
      "<Missing>", "<Missing>", "<Missing>", "<Missing>", "<Missing>",
      "<Missing>", "<Missing>", "<Missing>", "<Missing>", "<Missing>",
      "<Missing>", "<Missing>", "0", "0", "0", "0", "0", "0", "0",
      "0", "0", "0", "3", "3", "3", "3", "3", "<Missing>", "<Missing>",
      "<Missing>", "<Missing>", "<Missing>", "<Missing>", "<Missing>",
      "<Missing>", "<Missing>", "<Missing>", "<Missing>", "<Missing>",
      "<Missing>", "<Missing>", "<Missing>"
    ),
    .Dim = c(30L, 7L)
  )

  expected_matrix <- matrix(c(expected_matrix), 30L, 7L)

  testthat::expect_identical(result_matrix, expected_matrix)
})
