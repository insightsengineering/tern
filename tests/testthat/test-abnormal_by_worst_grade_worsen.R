library(random.cdisc.data)
library(rtables)
library(dplyr)

adlb <- radlb(cached = TRUE)
adsl <- radsl(cached = TRUE)
adlb <- adlb %>% mutate(
  GRADDR = case_when(
    PARAMCD == "ALT" ~ "B",
    PARAMCD == "CRP" ~ "L",
    PARAMCD == "IGA" ~ "H"
  )
) %>% filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")

test_that("h_adlb_worsen stacks data correctly (simple case)", {
  set.seed(42)
  input_data <- data.frame(
    USUBJID = as.character(rep(1:5, each = 30)),
    PARAMCD = rep(rep(c("ABC", "OPQ", "XYZ"), each = 10), 5),
    VALUES = runif(150, 0, 200)
  )

  input_data <- input_data %>%
    group_by(USUBJID, PARAMCD) %>%
    mutate(
      MIN = min(VALUES),
      MAX = max(VALUES),
      WGRLOFL = case_when(
        VALUES == MIN ~ "Y",
        TRUE ~ ""
        ),
      WGRHIFL = case_when(
        VALUES == MAX ~ "Y",
        TRUE ~ ""
        )
      )

  input_data <- input_data %>%
    mutate(
      GRADDR = case_when(
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


  p1 <- input_data %>% filter(WGRLOFL == "Y" & GRADDR == "L")
  p2 <- input_data %>% filter(WGRHIFL == "Y" & GRADDR == "H")
  p3 <- input_data %>% filter(WGRLOFL == "Y" & GRADDR == "B")
  p4 <- input_data %>% filter(WGRHIFL == "Y" & GRADDR == "B")

  p1$GRADDR <- "Low"  #nolint
  p2$GRADDR <- "High" #nolint
  p3$GRADDR <- "Low" #nolint
  p4$GRADDR <- "High" #nolint

  expected <- rbind(p1, p2, p3, p4)

  expected <- expected[order(expected$VALUES), ]

  expect_identical(result, expected)
})

test_that("h_adlb_worsen stacks data correctly", {
  adlb_f <- adlb %>% filter(USUBJID %in% c("AB12345-CHN-3-id-128", "AB12345-CHN-15-id-262"))

  result <- h_adlb_worsen(
    adlb_f,
    worst_flag_low = c("WGRLOFL" = "Y"),
    worst_flag_high = c("WGRHIFL" = "Y"),
    direction_var = "GRADDR"
    )

  result <- result %>% select(USUBJID, ARMCD, AVISIT, PARAMCD, ATOXGR, BTOXGR, WGRLOFL, WGRHIFL, GRADDR)
  result_matrix <- matrix(c(as.matrix(result)), 8L, 9L)

  expected_matrix <- structure(
    c(rep(c("AB12345-CHN-15-id-262", "AB12345-CHN-3-id-128"), 4L),
      rep(c("ARM C", "ARM A"), 4L),
      c("WEEK 4 DAY 29", "WEEK 3 DAY 22", "WEEK 5 DAY 36", "WEEK 3 DAY 22", "WEEK 3 DAY 22",
        "WEEK 1 DAY 8", "WEEK 5 DAY 36", "WEEK 5 DAY 36"),
      c("IGA", "IGA", "ALT", "ALT", "CRP", "CRP", "ALT", "ALT"),
      c("0", "0", "0", "2", "-3", "-1", "0", "-4"),
      c("0", "4", "0", "0", "2", "0", "0", "0"),
      c("Y", "Y", "Y", "", "Y", "Y", "Y", "Y"),
      c("Y", "Y", "Y", "Y", "", "", "Y", ""),
      c("High", "High", "High", "High", "Low", "Low", "Low", "Low")
    ),
    .Dim = c(8L, 9L)
  )

  expected_matrix <- matrix(c(expected_matrix), 8L, 9L)

  expect_identical(result_matrix, expected_matrix)
})

test_that("h_group_counter counts data (low) correctly", {

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
      "Any" = c(num = 30,  denom = 81)
    )
  )
  expect_equal(result, expected)

})

test_that("h_group_counter counts data (high) correctly", {

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
      "Any" = c(num = 30,  denom = 81)
    )
  )
  expect_equal(result, expected)

})

test_that("h_group_counter counts data (low), no high correctly", {

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
      "Any" = c(num = 14,  denom = 25)
    )
  )
  expect_equal(result, expected)

})

test_that("h_group_counter counts data (low), no low correctly", {

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
      "Any" = c(num = 0,  denom = 30)
    )
  )
  expect_equal(result, expected)

})

test_that("s_count_abnormal_lab_worsen_by_baseline", {

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
      "Any" = c(num = 30,  denom = 81)
    )
  )
  expect_equal(result, expected)

})

test_that("count_abnormal_lab_worsen_by_baseline", {

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
    c("",              "",              "ALT",           "High",          "1",             "2",
      "3",             "4",             "Any",           "Low",           "1",             "2",
      "3",             "4",             "Any",           "CRP",           "Low",           "1",
      "2",             "3",             "4",             "Any",           "IGA",           "High",
      "1",             "2",             "3",             "4",             "Any",           "ARM A",
      "(N=134)",       "",              "",              "15/116 (12.9%)", "13/120 (10.8%)", "9/123 (7.3%)",
      "8/125 (6.4%)",  "45/130 (34.6%)", "",              "13/122 (10.7%)", "8/125 (6.4%)",  "13/126 (10.3%)",
      "13/127 (10.2%)", "48/132 (36.4%)", "",              "",              "11/120 (9.2%)", "20/122 (16.4%)",
      "12/124 (9.7%)", "6/129 (4.7%)",  "50/131 (38.2%)", "",              "",              "23/117 (19.7%)",
      "11/119 (9.2%)", "11/122 (9%)",   "3/125 (2.4%)",  "48/128 (37.5%)", "ARM B",         "(N=134)",
      "",              "",              "12/116 (10.3%)", "5/120 (4.2%)",  "14/123 (11.4%)", "15/127 (11.8%)",
      "47/131 (35.9%)", "",              "11/118 (9.3%)", "11/124 (8.9%)", "14/124 (11.3%)", "6/128 (4.7%)",
      "44/132 (33.3%)", "",              "",              "14/122 (11.5%)", "14/126 (11.1%)", "8/128 (6.2%)",
      "5/129 (3.9%)",  "41/133 (30.8%)", "",              "",              "8/119 (6.7%)",  "17/121 (14%)",
      "13/126 (10.3%)", "7/128 (5.5%)",  "45/129 (34.9%)", "ARM C",         "(N=132)",       "",
      "",              "17/116 (14.7%)", "15/118 (12.7%)", "9/123 (7.3%)",  "12/127 (9.4%)", "53/129 (41.1%)",
      "",              "9/114 (7.9%)",  "7/120 (5.8%)",  "10/125 (8%)",   "9/127 (7.1%)",  "36/129 (27.9%)",
      "",              "",              "13/115 (11.3%)", "11/119 (9.2%)", "14/120 (11.7%)", "9/121 (7.4%)",
      "48/126 (38.1%)", "",              "",              "13/119 (10.9%)", "11/125 (8.8%)", "13/127 (10.2%)",
      "7/129 (5.4%)",  "44/130 (33.8%)"),
    .Dim = c(29L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("h_adlb_worsen all high", {
  adlb <- radlb(cached = TRUE)
  adsl <- radsl(cached = TRUE)
  adlb <- adlb %>% mutate(
    GRADDR = case_when(
      PARAMCD == "ALT" ~ "H",
      PARAMCD == "CRP" ~ "H",
      PARAMCD == "IGA" ~ "H"
    )
  ) %>% filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")

  result <- h_adlb_worsen(
    adlb,
    worst_flag_high = c("WGRHIFL" = "Y"),
    direction_var = "GRADDR"
  )
  result <- result[order(result$USUBJID, result$AVISIT, result$PARAMCD, result$PCHG), ]


  expected <- adlb[adlb$WGRHIFL == "Y", ]
  expected$GRADDR <- "High" #nolint
  expected <- expected[order(expected$USUBJID, expected$AVISIT, expected$PARAMCD, expected$PCHG), ]

  result_matrix <- matrix(c(as.matrix(result)), nrow(result), ncol(result))
  expected_matrix <- matrix(c(as.matrix(expected)), nrow(expected), ncol(expected))

  expect_identical(result_matrix, expected_matrix)
})

test_that("h_adlb_worsen all low", {
  adlb <- radlb(cached = TRUE)
  adsl <- radsl(cached = TRUE)
  adlb <- adlb %>% mutate(
    GRADDR = case_when(
      PARAMCD == "ALT" ~ "L",
      PARAMCD == "CRP" ~ "L",
      PARAMCD == "IGA" ~ "L"
    )
  ) %>% filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")

  result <- h_adlb_worsen(
    adlb,
    worst_flag_low = c("WGRLOFL" = "Y"),
    direction_var = "GRADDR"
  )
  result <- result[order(result$USUBJID, result$AVISIT, result$PARAMCD, result$PCHG), ]


  expected <- adlb[adlb$WGRLOFL == "Y", ]
  expected$GRADDR <- "Low" #nolint
  expected <- expected[order(expected$USUBJID, expected$AVISIT, expected$PARAMCD, expected$PCHG), ]

  result_matrix <- matrix(c(as.matrix(result)), nrow(result), ncol(result))
  expected_matrix <- matrix(c(as.matrix(expected)), nrow(expected), ncol(expected))

  expect_identical(result_matrix, expected_matrix)
})
