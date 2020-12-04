library(random.cdisc.data)
library(rtables)
library(dplyr)

get_adlb <- function() {
  adlb <- radlb(cached = TRUE) # nolintr
  adlb_f <- adlb %>%
    dplyr::filter(!AVISIT %in% c("SCREENING", "BASELINE")) %>%
    dplyr::mutate(
      ATOXGR = as.numeric(as.character(ATOXGR)),
      WGRLOFL = case_when(WGRLOFL == "Y" ~ TRUE, TRUE ~ FALSE),
      WGRHIFL = case_when(WGRHIFL == "Y" ~ TRUE, TRUE ~ FALSE)
    )
  adlb_f
}

test_that("s_count_abnormal_by_worst_grade works as expected", {
  adlb <- get_adlb()

  result <- s_count_abnormal_by_worst_grade(
    df = adlb %>% dplyr::filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
    .var = "ATOXGR",
    abnormal = "low",
    variables = list(id = "USUBJID", worst_grade_flag = "WGRLOFL")
  )
  expected <- list(count_fraction = list(
    "1" = c(13.00000000, 0.09701493),
    "2" = c(21.0000000, 0.1567164),
    "3" = c(12.00000000, 0.08955224),
    "4" = c(6.00000000, 0.04477612),
    "5" = c(8.00000000, 0.05970149),
    "Any" = c(60.0000000, 0.4477612)
  ))
  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("s_count_abnormal_by_worst_grade works as expected with single abnormality", {
  adlb <- get_adlb()
  result <- s_count_abnormal_by_worst_grade(
    df = adlb %>% dplyr::filter(ARMCD == "ARM A" & PARAMCD == "CRP"),
    .var = "ATOXGR",
    abnormal = c(High = "high"),
    variables = list(id = "USUBJID", worst_grade_flag = c(High = "WGRHIFL"))
  )
  expected <- list(count_fraction = list(
      "1" = c(16.000000, 0.119403),
      "2" = c(12.00000000, 0.08955224),
      "3" = c(13.00000000, 0.09701493),
      "4" = c(14.0000000, 0.1044776),
      "5" = c(5.00000000, 0.03731343),
      "Any" = c(60.0000000, 0.4477612)
    ))
  expect_equal(result, expected, tolerance = 0.000001)
})

test_that("count_abnormal_by_worst_grade works as expected", {
  adlb <- get_adlb()
  adlb_f <- adlb %>%
    dplyr::filter(PARAMCD == "IGA") %>%
    droplevels()
  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    split_rows_by("PARAMCD") %>%
    count_abnormal_by_worst_grade(
      var = "ATOXGR",
      abnormal = c(Low = "low", High = "high"),
      variables = list(id = "USUBJID", worst_grade_flag = c(Low = "WGRLOFL", High = "WGRHIFL"))
    ) %>%
    build_table(df = adlb_f)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "IGA", "Low", "1", "2", "3", "4", "5", "Any", "High", "1",
      "2", "3", "4", "5", "Any",
      "ARM A", "", "", "14 (10.4%)", "14 (10.4%)", "10 (7.5%)", "7 (5.2%)", "9 (6.7%)", "54 (40.3%)", "", "24 (17.9%)",
      "11 (8.2%)", "13 (9.7%)", "3 (2.2%)", "11 (8.2%)", "62 (46.3%)",
      "ARM B", "", "", "21 (15.7%)", "15 (11.2%)", "10 (7.5%)", "6 (4.5%)", "13 (9.7%)", "65 (48.5%)", "", "9 (6.7%)",
      "18 (13.4%)", "17 (12.7%)", "7 (5.2%)", "9 (6.7%)", "60 (44.8%)",
      "ARM C", "", "", "15 (11.4%)", "12 (9.1%)", "9 (6.8%)", "13 (9.8%)", "6 (4.5%)", "55 (41.7%)", "", "13 (9.8%)",
      "14 (10.6%)", "13 (9.8%)", "7 (5.3%)", "3 (2.3%)", "50 (37.9%)"
    ),
    .Dim = c(16L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
