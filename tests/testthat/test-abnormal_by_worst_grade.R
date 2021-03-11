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
    `1` = c(16, 0.119402985074627),
    `2` = c(21, 0.156716417910448),
    `3` = c(12, 0.0895522388059701),
    `4` = c(11, 0.082089552238806),
    Any = c(60, 0.447761194029851)
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
    `1` = c(17, 0.126865671641791),
    `2` = c(15, 0.111940298507463),
    `3` = c(16, 0.119402985074627),
    `4` = c(12, 0.0895522388059701),
    Any = c(60, 0.447761194029851)
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
      "", "IGA", "Low", "1", "2", "3", "4", "Any",
      "High", "1", "2", "3", "4", "Any", "ARM A", "", "", "17 (12.7%)",
      "12 (9%)", "15 (11.2%)", "10 (7.5%)", "54 (40.3%)", "",
      "25 (18.7%)", "14 (10.4%)", "12 (9%)", "11 (8.2%)", "62 (46.3%)",
      "ARM B", "", "", "23 (17.2%)", "15 (11.2%)", "11 (8.2%)", "16 (11.9%)",
      "65 (48.5%)", "", "14 (10.4%)", "20 (14.9%)", "13 (9.7%)",
      "13 (9.7%)", "60 (44.8%)", "ARM C", "", "", "17 (12.9%)",
      "14 (10.6%)", "14 (10.6%)", "10 (7.6%)", "55 (41.7%)", "",
      "13 (9.8%)", "16 (12.1%)", "17 (12.9%)", "4 (3%)", "50 (37.9%)"
    ),
    .Dim = c(14L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
