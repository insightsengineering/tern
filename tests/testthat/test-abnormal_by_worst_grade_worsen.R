library(scda)
library(rtables)
library(dplyr)

adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb
adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adlb <- adlb %>%
  dplyr::mutate(
    GRADDR = dplyr::case_when(
      PARAMCD == "ALT" ~ "B",
      PARAMCD == "CRP" ~ "L",
      PARAMCD == "IGA" ~ "H"
    )
  ) %>%
  dplyr::filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")

testthat::test_that("h_adlb_worsen stacks data correctly (simple case)", {
  set.seed(42)
  input_data <- data.frame(
    USUBJID = as.character(rep(1:5, each = 30)),
    PARAMCD = rep(rep(c("ABC", "OPQ", "XYZ"), each = 10), 5),
    VALUES = stats::runif(150, 0, 200)
  )

  input_data <- input_data %>%
    dplyr::group_by(USUBJID, PARAMCD) %>%
    dplyr::mutate(
      MIN = min(VALUES),
      MAX = max(VALUES),
      WGRLOFL = dplyr::case_when(
        VALUES == MIN ~ "Y",
        TRUE ~ ""
      ),
      WGRHIFL = dplyr::case_when(
        VALUES == MAX ~ "Y",
        TRUE ~ ""
      )
    )

  input_data <- input_data %>%
    dplyr::mutate(
      GRADDR = dplyr::case_when(
        PARAMCD == "ABC" ~ "L",
        PARAMCD == "OPQ" ~ "B",
        PARAMCD == "XYZ" ~ "H"
      )
    )


  result <- h_adlb_worsen(
    input_data,
    worst_flag_low = c("WGRLOFL" = "Y"),
    worst_flag_high = c("WGRHIFL" = "Y"),
    direction_var = "GRADDR"
  )

  result <- result[order(result$VALUES), ]


  p1 <- input_data %>% dplyr::filter(WGRLOFL == "Y" & GRADDR == "L")
  p2 <- input_data %>% dplyr::filter(WGRHIFL == "Y" & GRADDR == "H")
  p3 <- input_data %>% dplyr::filter(WGRLOFL == "Y" & GRADDR == "B")
  p4 <- input_data %>% dplyr::filter(WGRHIFL == "Y" & GRADDR == "B")

  p1$GRADDR <- "Low" # nolint
  p2$GRADDR <- "High" # nolint
  p3$GRADDR <- "Low" # nolint
  p4$GRADDR <- "High" # nolint

  expected <- rbind(p1, p2, p3, p4)
  expected <- expected[order(expected$VALUES), ]

  testthat::expect_identical(result, expected)
})

testthat::test_that("h_adlb_worsen stacks data correctly", {
  adlb_f <- adlb %>% dplyr::filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))

  result <- h_adlb_worsen(
    adlb_f,
    worst_flag_low = c("WGRLOFL" = "Y"),
    worst_flag_high = c("WGRHIFL" = "Y"),
    direction_var = "GRADDR"
  )

  result <- result %>% dplyr::select(USUBJID, ARMCD, AVISIT, PARAMCD, ATOXGR, BTOXGR, WGRLOFL, WGRHIFL, GRADDR)
  result_matrix <- matrix(c(as.matrix(result)), 8L, 9L)

  expected_matrix <- structure(
    c(
      rep(c("AB12345-CHN-15-id-262", "AB12345-CHN-3-id-128"), 4L),
      rep(c("ARM C", "ARM A"), 4L),
      c(
        "WEEK 4 DAY 29", "WEEK 3 DAY 22", "WEEK 5 DAY 36", "WEEK 3 DAY 22", "WEEK 3 DAY 22",
        "WEEK 1 DAY 8", "WEEK 5 DAY 36", "WEEK 2 DAY 15"
      ),
      c("IGA", "IGA", "ALT", "ALT", "CRP", "CRP", "ALT", "ALT"),
      c("0", "0", "0", "2", "-2", "-1", "0", "-3"),
      c("0", "3", "0", "0", "2", "0", "0", "0"),
      c("Y", "Y", "Y", "", "Y", "Y", "Y", "Y"),
      c("Y", "Y", "Y", "Y", "", "", "Y", ""),
      c("High", "High", "High", "High", "Low", "Low", "Low", "Low")
    ),
    .Dim = c(8L, 9L)
  )

  expected_matrix <- matrix(c(expected_matrix), 8L, 9L)

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("h_group_counter counts data (low) correctly", {
  df_test <- expand.grid(
    ATOXGR = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, "<Missing>"),
    BTOXGR = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, "<Missing>")
  )

  df_test <- cbind(USUBJID = 1:100, df_test, GRADDR = "Low")

  result <- h_worsen_counter(
    df_test,
    id = "USUBJID",
    .var = "ATOXGR",
    baseline_var = "BTOXGR",
    direction_var = "GRADDR"
  )

  expected <- list(
    fraction = list(
      "1" = c(num = 6, denom = 54),
      "2" = c(num = 7, denom = 63),
      "3" = c(num = 8, denom = 72),
      "4" = c(num = 9, denom = 81),
      "Any" = c(num = 30, denom = 81)
    )
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("h_group_counter counts data (high) correctly", {
  df_test <- expand.grid(
    ATOXGR = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, "<Missing>"),
    BTOXGR = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, "<Missing>")
  )

  df_test <- cbind(USUBJID = 1:100, df_test, GRADDR = "High")

  result <- h_worsen_counter(
    df_test,
    id = "USUBJID",
    .var = "ATOXGR",
    baseline_var = "BTOXGR",
    direction_var = "GRADDR"
  )

  expected <- list(
    fraction = list(
      "1" = c(num = 6, denom = 54),
      "2" = c(num = 7, denom = 63),
      "3" = c(num = 8, denom = 72),
      "4" = c(num = 9, denom = 81),
      "Any" = c(num = 30, denom = 81)
    )
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("h_group_counter counts data (low), no high correctly", {
  df_test <- expand.grid(
    ATOXGR = c(-4, -3, -2, -1, 0, "<Missing>"),
    BTOXGR = c(-4, -3, -2, -1, 0, "<Missing>")
  )

  df_test <- cbind(USUBJID = 1:36, df_test, GRADDR = "Low")

  result <- h_worsen_counter(
    df_test,
    id = "USUBJID",
    .var = "ATOXGR",
    baseline_var = "BTOXGR",
    direction_var = "GRADDR"
  )

  expected <- list(
    fraction = list(
      "1" = c(num = 2, denom = 10),
      "2" = c(num = 3, denom = 15),
      "3" = c(num = 4, denom = 20),
      "4" = c(num = 5, denom = 25),
      "Any" = c(num = 14, denom = 25)
    )
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("h_group_counter counts data (low), no low correctly", {
  df_test <- expand.grid(
    ATOXGR = c(0, 1, 2, 3, 4, "<Missing>"),
    BTOXGR = c(0, 1, 2, 3, 4, "<Missing>")
  )

  df_test <- cbind(USUBJID = 1:36, df_test, GRADDR = "Low")

  result <- h_worsen_counter(
    df_test,
    id = "USUBJID",
    .var = "ATOXGR",
    baseline_var = "BTOXGR",
    direction_var = "GRADDR"
  )

  expected <- list(
    fraction = list(
      "1" = c(num = 0, denom = 30),
      "2" = c(num = 0, denom = 30),
      "3" = c(num = 0, denom = 30),
      "4" = c(num = 0, denom = 30),
      "Any" = c(num = 0, denom = 30)
    )
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("s_count_abnormal_lab_worsen_by_baseline", {
  df_test <- expand.grid(
    ATOXGR = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, "<Missing>"),
    BTOXGR = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, "<Missing>")
  )

  df_test <- cbind(USUBJID = 1:100, df_test, GRADDR = "Low")

  result <- s_count_abnormal_lab_worsen_by_baseline(
    df = df_test,
    .var = "ATOXGR",
    variables = list(
      id = "USUBJID",
      baseline_var = "BTOXGR",
      direction_var = "GRADDR"
    )
  )

  expected <- list(
    fraction = list(
      "1" = c(num = 6, denom = 54),
      "2" = c(num = 7, denom = 63),
      "3" = c(num = 8, denom = 72),
      "4" = c(num = 9, denom = 81),
      "Any" = c(num = 30, denom = 81)
    )
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("count_abnormal_lab_worsen_by_baseline", {
  df <- h_adlb_worsen(
    adlb,
    worst_flag_low = c("WGRLOFL" = "Y"),
    worst_flag_high = c("WGRHIFL" = "Y"),
    direction_var = "GRADDR"
  )

  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAMCD") %>%
    split_rows_by("GRADDR") %>%
    count_abnormal_lab_worsen_by_baseline(
      var = "ATOXGR",
      variables = list(
        id = "USUBJID",
        baseline_var = "BTOXGR",
        direction_var = "GRADDR"
      )
    ) %>%
    build_table(df = df, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "ALT", "High", "1", "2", "3", "4", "Any",
      "Low", "1", "2", "3", "4", "Any", "CRP", "Low", "1", "2", "3",
      "4", "Any", "IGA", "High", "1", "2", "3", "4", "Any", "ARM A",
      "(N=134)", "", "", "16/119 (13.4%)", "14/123 (11.4%)", "9/127 (7.1%)",
      "12/129 (9.3%)", "51/129 (39.5%)", "", "13/124 (10.5%)", "13/127 (10.2%)",
      "19/128 (14.8%)", "7/130 (5.4%)", "52/130 (40%)", "", "", "14/122 (11.5%)",
      "21/124 (16.9%)", "12/129 (9.3%)", "10/131 (7.6%)", "57/131 (43.5%)",
      "", "", "24/118 (20.3%)", "13/120 (10.8%)", "11/124 (8.9%)",
      "11/129 (8.5%)", "59/129 (45.7%)", "ARM B", "(N=134)", "", "",
      "13/117 (11.1%)", "12/121 (9.9%)", "15/125 (12%)", "11/130 (8.5%)",
      "51/130 (39.2%)", "", "12/121 (9.9%)", "17/127 (13.4%)", "12/128 (9.4%)",
      "7/131 (5.3%)", "48/131 (36.6%)", "", "", "17/125 (13.6%)", "12/130 (9.2%)",
      "9/131 (6.9%)", "7/133 (5.3%)", "45/133 (33.8%)", "", "", "12/120 (10%)",
      "19/124 (15.3%)", "10/128 (7.8%)", "13/130 (10%)", "54/130 (41.5%)",
      "ARM C", "(N=132)", "", "", "17/116 (14.7%)", "17/119 (14.3%)",
      "13/123 (10.6%)", "13/128 (10.2%)", "60/128 (46.9%)", "", "9/116 (7.8%)",
      "11/123 (8.9%)", "10/127 (7.9%)", "10/131 (7.6%)", "40/131 (30.5%)",
      "", "", "13/117 (11.1%)", "16/122 (13.1%)", "16/123 (13%)", "4/124 (3.2%)",
      "49/124 (39.5%)", "", "", "13/119 (10.9%)", "13/125 (10.4%)",
      "17/128 (13.3%)", "4/130 (3.1%)", "47/130 (36.2%)"
    ),
    .Dim = c(29L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("h_adlb_worsen all high", {
  adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb
  adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
  adlb <- adlb %>%
    dplyr::mutate(
      GRADDR = dplyr::case_when(
        PARAMCD == "ALT" ~ "H",
        PARAMCD == "CRP" ~ "H",
        PARAMCD == "IGA" ~ "H"
      )
    ) %>%
    dplyr::filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")

  result <- h_adlb_worsen(
    adlb,
    worst_flag_high = c("WGRHIFL" = "Y"),
    direction_var = "GRADDR"
  )
  result <- result[order(result$USUBJID, result$AVISIT, result$PARAMCD, result$PCHG), ]


  expected <- adlb[adlb$WGRHIFL == "Y", ]
  expected$GRADDR <- "High" # nolint
  expected <- expected[order(expected$USUBJID, expected$AVISIT, expected$PARAMCD, expected$PCHG), ]

  result_matrix <- matrix(c(as.matrix(result)), nrow(result), ncol(result))
  expected_matrix <- matrix(c(as.matrix(expected)), nrow(expected), ncol(expected))

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("h_adlb_worsen all low", {
  adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb
  adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
  adlb <- adlb %>%
    dplyr::mutate(
      GRADDR = dplyr::case_when(
        PARAMCD == "ALT" ~ "L",
        PARAMCD == "CRP" ~ "L",
        PARAMCD == "IGA" ~ "L"
      )
    ) %>%
    dplyr::filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")

  result <- h_adlb_worsen(
    adlb,
    worst_flag_low = c("WGRLOFL" = "Y"),
    direction_var = "GRADDR"
  )
  result <- result[order(result$USUBJID, result$AVISIT, result$PARAMCD, result$PCHG), ]


  expected <- adlb[adlb$WGRLOFL == "Y", ]
  expected$GRADDR <- "Low" # nolint
  expected <- expected[order(expected$USUBJID, expected$AVISIT, expected$PARAMCD, expected$PCHG), ]

  result_matrix <- matrix(c(as.matrix(result)), nrow(result), ncol(result))
  expected_matrix <- matrix(c(as.matrix(expected)), nrow(expected), ncol(expected))

  testthat::expect_identical(result_matrix, expected_matrix)
})
