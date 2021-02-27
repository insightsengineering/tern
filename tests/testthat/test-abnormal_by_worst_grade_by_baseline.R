library(random.cdisc.data)
library(rtables)
library(dplyr)

adlb <- radlb(cached = TRUE)
adsl <- radsl(cached = TRUE)

test_that("h_adsl_adlb_merge_using_worst_flag generates missing for patients without any lab", {
  adsl_f <- adsl %>% filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))
  adlb_f <- adlb %>% filter(USUBJID %in% c("AB12345-CHN-3-id-128"))

  result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  result_matrix <- matrix(c(as.matrix(result)), 6L, 6L)

  expected_matrix <- structure(
    c(rep(c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"), each = 3L),
      rep(c("ARM A", "ARM C"), each = 3L),
      c("WEEK 3 DAY 22", "WEEK 5 DAY 36", "WEEK 3 DAY 22", "<Missing>", "<Missing>", "<Missing>"),
      rep(c("ALT", "CRP", "IGA"), 2L),
      c("2", "0", "0", "<Missing>", "<Missing>", "<Missing>"),
      c("0", "0", "3", "<Missing>", "<Missing>", "<Missing>")
    ),
    .Dim = c(6L, 6L)
  )

  expected_matrix <- matrix(c(expected_matrix), 6L, 6L)

  expect_identical(result_matrix, expected_matrix)
})

test_that("h_adsl_adlb_merge_using_worst_flag generates missing for patients missing baseline lab", {
  adsl_f <- adsl %>% filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))
  adlb_f <- adlb %>% filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))

  adlb_f[adlb_f$USUBJID == "AB12345-CHN-15-id-262", ]$BTOXGR <- NA #nolint

  result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  result_matrix <- matrix(c(as.matrix(result)), 6L, 6L)

  expected_matrix <- structure(
    c(
      rep(c("AB12345-CHN-15-id-262", "AB12345-CHN-3-id-128"), each = 3L),
      rep(c("ARM C", "ARM A"), each = 3L),
      c("WEEK 5 DAY 36", "WEEK 1 DAY 8", "WEEK 4 DAY 29", "WEEK 3 DAY 22", "WEEK 5 DAY 36", "WEEK 3 DAY 22"),
      rep(c("ALT", "CRP", "IGA"), 2),
      c("0", "0", "0", "2", "0", "0"),
      c("<Missing>", "<Missing>", "<Missing>", "0", "0", "3")
    ),
    .Dim = c(6L, 6L)
  )

  expected_matrix <- matrix(c(expected_matrix), 6L, 6L)

  expect_identical(result_matrix, expected_matrix)
})

test_that("h_adsl_adlb_merge_using_worst_flag generates missing for patients missing post-baseline lab", {
  adsl_f <- adsl %>% filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))
  adlb_f <- adlb %>% filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))

  adlb_f[adlb_f$USUBJID == "AB12345-CHN-15-id-262",]$ATOXGR <- NA #nolint

  result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  result_matrix <- matrix(c(as.matrix(result)), 6L, 6L)

  expected_matrix <- structure(
    c(
      rep(c("AB12345-CHN-15-id-262", "AB12345-CHN-3-id-128"), each = 3L),
      rep(c("ARM C", "ARM A"), each = 3L),
      c("WEEK 5 DAY 36", "WEEK 1 DAY 8", "WEEK 4 DAY 29", "WEEK 3 DAY 22", "WEEK 5 DAY 36", "WEEK 3 DAY 22"),
      rep(c("ALT", "CRP", "IGA"), 2L),
      c("<Missing>", "<Missing>", "<Missing>", "2", "0", "0"),
      c("0", "2", "0", "0", "0", "3")
    ),
    .Dim = c(6L, 6L)
  )

  expected_matrix <- matrix(c(expected_matrix), 6L, 6L)

  expect_identical(result_matrix, expected_matrix)
})

test_that("h_adsl_adlb_merge_using_worst_flag generates missing for patients without any post-baseline values flagged as the worst", { #nolint
  adsl_f <- adsl %>% filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))
  adlb_f <- adlb %>% filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))

  adlb_f[adlb_f$USUBJID == "AB12345-CHN-15-id-262", ]$WGRHIFL <- "" #nolint

  result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  result_matrix <- matrix(c(as.matrix(result)), 6L, 6L)

  expected_matrix <- structure(
    c(
      rep(c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"), each = 3L),
      rep(c("ARM A", "ARM C"), each = 3L),
      c("WEEK 3 DAY 22", "WEEK 5 DAY 36", "WEEK 3 DAY 22", "<Missing>", "<Missing>", "<Missing>"),
      rep(c("ALT", "CRP", "IGA"), 2L),
      c("2", "0", "0", "<Missing>", "<Missing>", "<Missing>"),
      c("0", "0", "3", "<Missing>", "<Missing>", "<Missing>")
    ),
    .Dim = c(6L, 6L)
  )

  expected_matrix <- matrix(c(expected_matrix), 6L, 6L)

  expect_identical(result_matrix, expected_matrix)
})

test_that("h_group_counter check ", {
  adlb_f <- adlb %>% filter(PARAMCD %in% "CRP")
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  result <- h_group_counter(
    df = adlb_out,
    id = "USUBJID",
    .var = "ATOXGR",
    grouping_list = list(
      `Missing` = "<Missing>",
      `Not High` = c(0, -1, -2, -3, -4),
      `1` = 1,
      `2` = 2,
      `3` = 3,
      `4` = 4
    )
  )

  expected <- list(
    count = list(Total = 400L),
    count_fraction = list(
      "Missing" = c(0, 0),
      "Not High" = c(239, 0.5975),
      "1" = c(41, 0.1025),
      "2" = c(44, 0.11),
      "3" = c(42, 0.105),
      "4" = c(34, 0.085)
      )
    )

  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("s_count_abnormal_by_worst_grade_by_baseline High works as expected", {

  adlb_f <- adlb %>% filter(PARAMCD %in% "CRP")
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIFL" = "Y"))

  result <- s_count_abnormal_by_worst_grade_by_baseline(
    df = adlb_out,
    .var = "ATOXGR",
    grouping_list = list(
      "Missing" = "<Missing>",
      "Not High" = c(0L, -1L, -2L, -3L, -4L),
      "1" = 1L,
      "2" = 2L,
      "3" = 3L,
      "4" = 4L
    ),
    variables = list(
      id = "USUBJID",
      baseline_var = "BTOXGR",
      baseline_grade = "1"
    )
  )

  expected <- list(
    count = list("Total" = 15L),
    count_fraction = list(
      "Missing" = c(0, 0),
      "Not High" = c(12, 0.8),
      "1" = c(0, 0),
      "2" = c(2, 0.133333333333333),
      "3" = c(0, 0),
      "4" = c(1, 0.0666666666666667)
      )
    )
  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("s_count_abnormal_by_worst_grade_by_baseline Low works as expected", {
  adlb_f <- adlb %>% filter(PARAMCD %in% "ALT")
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRLOFL" = "Y"))

  result <- s_count_abnormal_by_worst_grade_by_baseline(
    df = adlb_out,
    .var = "ATOXGR",
    grouping_list = list(
      "Missing" = "<Missing>",
      "Not Low" = c(0L, 1L, 2L, 3L, 4L),
      "1" = -1L,
      "2" = -2L,
      "3" = -3L,
      "4" = -4L
    ),
    variables = list(
      id = "USUBJID",
      baseline_var = "BTOXGR",
      baseline_grade = "2"
    )
  )

  expected <- list(
    count = list(Total = 7L),
    count_fraction = list(
      "Missing" = c(0, 0),
      "Not Low" = c(5, 0.714285714285714),
      "1" = c(0, 0),
      "2" = c(1, 0.142857142857143),
      "3" = c(1, 0.142857142857143),
      "4" = c(0, 0)
  ))

  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("count_abnormal_by_worst_grade_by_baseline works as expected", {

  adlb_f <- adlb %>%
    filter(PARAMCD %in% "ALT") %>%
    droplevels()
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRLOFL" = "Y"))


  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD") %>%
    count_abnormal_by_worst_grade_by_baseline(
      var = "ATOXGR",
      grouping_list = list(
        "Not Low" = c(0L, 1L, 2L, 3L, 4L),
        "1" = -1L,
        "2" = -2L,
        "3" = -3L,
        "4" = -4L,
        "Missing" = "<Missing>"
      ),
      variables = list(
        id = "USUBJID",
        baseline_var = "BTOXGR",
        baseline_grade_list = list("Not Low", "1")
      )
    ) %>%
    build_table(df = adlb_out, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "ALT", "Not Low", "Total", "Not Low", "1",
      "2", "3", "4", "Missing", "1", "Total", "Not Low", "1", "2",
      "3", "4", "Missing", "ARM A", "(N=134)", "", "", "124", "74 (59.7%)",
      "13 (10.5%)", "13 (10.5%)", "17 (13.7%)", "7 (5.6%)", "0", "",
      "3", "0", "1 (33.3%)", "0", "2 (66.7%)", "0", "0", "ARM B", "(N=134)",
      "", "", "122", "76 (62.3%)", "12 (9.8%)", "16 (13.1%)", "11 (9%)",
      "7 (5.7%)", "0", "", "6", "2 (33.3%)", "2 (33.3%)", "1 (16.7%)",
      "1 (16.7%)", "0", "0", "ARM C", "(N=132)", "", "", "117", "80 (68.4%)",
      "9 (7.7%)", "11 (9.4%)", "9 (7.7%)", "8 (6.8%)", "0", "", "7",
      "5 (71.4%)", "0", "0", "0", "2 (28.6%)", "0"),
    .Dim = c(19L, 4L)
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("count_abnormal_by_worst_grade_by_baseline sum check", {

  adlb_f <- adlb %>%
    filter(PARAMCD %in% "ALT") %>%
    droplevels()
  adsl_f <- adsl

  adlb_out <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRLOFL" = "Y"))


  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD") %>%
    count_abnormal_by_worst_grade_by_baseline(
      var = "ATOXGR",
      grouping_list = list(
        "Not Low" = c(-1L, -2L, -3L, -4L, -5L, 5L, 0L, 1L, 2L, 3L, 4L),
        "Missing" = "<Missing>"
      ),
      variables = list(
        id = "USUBJID",
        baseline_var = "BTOXGR",
        baseline_grade_list = list("Not Low")
      )
    ) %>%
    build_table(df = adlb_out, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "",          "",          "ALT",       "Not Low",   "Total",     "Not Low",
      "Missing",   "ARM A",     "(N=134)",   "",          "",          "134",       "134 (100%)",
      "0",         "ARM B",     "(N=134)",   "",          "",          "134",
      "134 (100%)", "0",         "ARM C",     "(N=132)",   "",          "",          "132",
      "132 (100%)", "0"
    ),
    .Dim = c(7L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("h_adsl_adlb_merge_using_worst_flag generates missing and by visit lab results", {
  adsl_f <- adsl %>% filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))
  adlb_f <- adlb %>% filter(USUBJID %in% c("AB12345-CHN-3-id-128"))

  result <- h_adsl_adlb_merge_using_worst_flag(adsl_f, adlb_f, worst_flag = c("WGRHIVFL" = "Y"), by_visit = TRUE)

  result_matrix <- matrix(c(as.matrix(result)), 30L, 6L)

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
      "IGA", "-2", "-3", "2", "0", "-3", "-1", "0", "0", "0", "0",
      "0", "0", "0", "0", "0", "<Missing>", "<Missing>", "<Missing>",
      "<Missing>", "<Missing>", "<Missing>", "<Missing>", "<Missing>",
      "<Missing>", "<Missing>", "<Missing>", "<Missing>", "<Missing>",
      "<Missing>", "<Missing>", "0", "0", "0", "0", "0", "0", "0",
      "0", "0", "0", "3", "3", "3", "3", "3", "<Missing>", "<Missing>",
      "<Missing>", "<Missing>", "<Missing>", "<Missing>", "<Missing>",
      "<Missing>", "<Missing>", "<Missing>", "<Missing>", "<Missing>",
      "<Missing>", "<Missing>", "<Missing>"
    ),
    .Dim = c(30L, 6L)
  )

  expected_matrix <- matrix(c(expected_matrix), 30L, 6L)

  expect_identical(result_matrix, expected_matrix)
})
