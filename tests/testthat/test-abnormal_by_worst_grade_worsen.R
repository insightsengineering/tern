# Data pre-processing
adlb_local <- tern_ex_adlb %>%
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
  adlb_f <- adlb_local %>% dplyr::filter(USUBJID %in% c("AB12345-CHN-1-id-53", "AB12345-CHN-3-id-128"))

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
      rep(c("AB12345-CHN-1-id-53", "AB12345-CHN-3-id-128"), 4L),
      rep(c("ARM B", "ARM B"), 4L),
      c(
        "WEEK 5 DAY 36", "WEEK 5 DAY 36", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36",
        "WEEK 2 DAY 15", "WEEK 5 DAY 36", "WEEK 5 DAY 36"
      ),
      c("IGA", "IGA", "ALT", "ALT", "CRP", "CRP", "ALT", "ALT"),
      c("0", "4", "4", "1", "0", "0", "-4", "-4"),
      c("0", "0", "0", "0", "1", "0", "0", "0"),
      c("", "", "", "", "Y", "Y", "Y", "Y"),
      c("Y", "Y", "Y", "Y", "Y", "", "", ""),
      c("High", "High", "High", "High", "Low", "Low", "Low", "Low")
    ),
    .Dim = c(8L, 9L)
  )

  expected_matrix <- matrix(c(expected_matrix), 8L, 9L)

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("h_worsen_counter counts data (low) correctly", {
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

testthat::test_that("h_worsen_counter counts data (high) correctly", {
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

testthat::test_that("h_worsen_counter counts data (low), no high correctly", {
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

testthat::test_that("h_worsen_counter counts data (low), no low correctly", {
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
    adlb_local,
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
    build_table(df = df, alt_counts_df = tern_ex_adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- matrix(
    c(
      "", "ARM A", "ARM B", "ARM C",
      "", "(N=69)", "(N=73)", "(N=58)",
      "IGA", "", "", "",
      "High", "", "", "",
      "1", "6/63 (9.5%)", "6/64 (9.4%)", "4/50 (8%)",
      "2", "8/64 (12.5%)", "5/67 (7.5%)", "8/53 (15.1%)",
      "3", "7/66 (10.6%)", "5/68 (7.4%)", "9/57 (15.8%)",
      "4", "6/68 (8.8%)", "2/72 (2.8%)", "3/58 (5.2%)",
      "Any", "27/68 (39.7%)", "18/72 (25%)", "24/58 (41.4%)",
      "ALT", "", "", "",
      "High", "", "", "",
      "1", "7/62 (11.3%)", "6/62 (9.7%)", "2/48 (4.2%)",
      "2", "12/62 (19.4%)", "4/67 (6%)", "11/50 (22%)",
      "3", "4/64 (6.2%)", "11/71 (15.5%)", "7/56 (12.5%)",
      "4", "1/66 (1.5%)", "8/71 (11.3%)", "4/57 (7%)",
      "Any", "24/66 (36.4%)", "29/71 (40.8%)", "24/57 (42.1%)",
      "Low", "", "", "",
      "1", "12/67 (17.9%)", "4/66 (6.1%)", "7/52 (13.5%)",
      "2", "9/68 (13.2%)", "12/69 (17.4%)", "6/55 (10.9%)",
      "3", "6/69 (8.7%)", "4/71 (5.6%)", "5/56 (8.9%)",
      "4", "7/69 (10.1%)", "7/73 (9.6%)", "6/58 (10.3%)",
      "Any", "34/69 (49.3%)", "27/73 (37%)", "24/58 (41.4%)",
      "CRP", "", "", "",
      "Low", "", "", "",
      "1", "11/66 (16.7%)", "10/67 (14.9%)", "4/47 (8.5%)",
      "2", "8/66 (12.1%)", "1/70 (1.4%)", "6/50 (12%)",
      "3", "4/68 (5.9%)", "9/70 (12.9%)", "5/53 (9.4%)",
      "4", "7/69 (10.1%)", "6/72 (8.3%)", "4/55 (7.3%)",
      "Any", "30/69 (43.5%)", "26/72 (36.1%)", "19/55 (34.5%)"
    ),
    nrow = 29, ncol = 4, byrow = TRUE
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("h_adlb_worsen all high", {
  adlb_local <- tern_ex_adlb %>%
    dplyr::mutate(
      GRADDR = dplyr::case_when(
        PARAMCD == "ALT" ~ "H",
        PARAMCD == "CRP" ~ "H",
        PARAMCD == "IGA" ~ "H"
      )
    ) %>%
    dplyr::filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")

  result <- h_adlb_worsen(
    adlb_local,
    worst_flag_high = c("WGRHIFL" = "Y"),
    direction_var = "GRADDR"
  )
  result <- result[order(result$USUBJID, result$AVISIT, result$PARAMCD), ]

  expected <- adlb_local[adlb_local$WGRHIFL == "Y", ]
  expected$GRADDR <- "High" # nolint
  expected <- expected[order(expected$USUBJID, expected$AVISIT, expected$PARAMCD), ]

  result_matrix <- matrix(c(as.matrix(result)), nrow(result), ncol(result))
  expected_matrix <- matrix(c(as.matrix(expected)), nrow(expected), ncol(expected))

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("h_adlb_worsen all low", {
  adlb_local <- tern_ex_adlb %>%
    dplyr::mutate(
      GRADDR = dplyr::case_when(
        PARAMCD == "ALT" ~ "L",
        PARAMCD == "CRP" ~ "L",
        PARAMCD == "IGA" ~ "L"
      )
    ) %>%
    dplyr::filter(SAFFL == "Y" & ONTRTFL == "Y" & GRADDR != "")

  result <- h_adlb_worsen(
    adlb_local,
    worst_flag_low = c("WGRLOFL" = "Y"),
    direction_var = "GRADDR"
  )
  result <- result[order(result$USUBJID, result$AVISIT, result$PARAMCD), ]


  expected <- adlb_local[adlb_local$WGRLOFL == "Y", ]
  expected$GRADDR <- "Low" # nolint
  expected <- expected[order(expected$USUBJID, expected$AVISIT, expected$PARAMCD), ]

  result_matrix <- matrix(c(as.matrix(result)), nrow(result), ncol(result))
  expected_matrix <- matrix(c(as.matrix(expected)), nrow(expected), ncol(expected))

  testthat::expect_identical(result_matrix, expected_matrix)
})
