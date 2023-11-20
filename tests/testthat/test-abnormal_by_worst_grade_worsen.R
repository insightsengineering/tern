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

  result <- result[order(result$VALUES), ] %>% data.frame()

  p1 <- input_data %>% dplyr::filter(WGRLOFL == "Y" & GRADDR == "L")
  p2 <- input_data %>% dplyr::filter(WGRHIFL == "Y" & GRADDR == "H")
  p3 <- input_data %>% dplyr::filter(WGRLOFL == "Y" & GRADDR == "B")
  p4 <- input_data %>% dplyr::filter(WGRHIFL == "Y" & GRADDR == "B")

  p1$GRADDR <- "Low"
  p2$GRADDR <- "High"
  p3$GRADDR <- "Low"
  p4$GRADDR <- "High"

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_adlb_worsen stacks data correctly", {
  adlb_f <- adlb_local %>% dplyr::filter(USUBJID %in% c("AB12345-CHN-1-id-53", "AB12345-CHN-3-id-128"))

  result <- h_adlb_worsen(
    adlb_f,
    worst_flag_low = c("WGRLOFL" = "Y"),
    worst_flag_high = c("WGRHIFL" = "Y"),
    direction_var = "GRADDR"
  )

  result <- result %>%
    dplyr::select(USUBJID, ARMCD, AVISIT, PARAMCD, ATOXGR, BTOXGR, WGRLOFL, WGRHIFL, GRADDR) %>%
    data.frame()

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  testthat::expect_true(all(result$WGRHIFL == "Y" & result$GRADDR == "High"))
  testthat::expect_identical(nrow(result), 600L)
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

  testthat::expect_true(all(result$WGRLOFL == "Y" & result$GRADDR == "Low"))
  testthat::expect_identical(nrow(result), 600L)
})
