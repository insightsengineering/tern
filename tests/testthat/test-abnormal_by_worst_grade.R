# Local data pre-processing
adlb_tmp <- tern_ex_adlb

adlb_local <- local({
  # Data set is modified in order to have some parameters with grades only in one direction
  # and simulate the real data.
  adlb_tmp$ATOXGR[adlb_tmp$PARAMCD == "ALT" & adlb_tmp$ATOXGR %in% c("1", "2", "3", "4")] <- "-1"
  adlb_tmp$ANRIND[adlb_tmp$PARAMCD == "ALT" & adlb_tmp$ANRIND == "HIGH"] <- "LOW"
  adlb_tmp$WGRHIFL[adlb_tmp$PARAMCD == "ALT"] <- ""

  adlb_tmp$ATOXGR[adlb_tmp$PARAMCD == "IGA" & adlb_tmp$ATOXGR %in% c("-1", "-2", "-3", "-4")] <- "1"
  adlb_tmp$ANRIND[adlb_tmp$PARAMCD == "IGA" & adlb_tmp$ANRIND == "LOW"] <- "HIGH"
  adlb_tmp$WGRLOFL[adlb_tmp$PARAMCD == "IGA"] <- ""

  # Pre-processing
  adlb_tmp %>% h_adlb_abnormal_by_worst_grade()
})

testthat::test_that("s_count_abnormal_by_worst_grade works as expected", {
  adlb_alt <- adlb_local %>%
    dplyr::filter(PARAMCD == "ALT") %>%
    droplevels()
  full_parent_df <- list(adlb_alt, "not_needed")
  cur_col_subset <- list(adlb_alt$ARMCD == "ARM A", "not_needed")

  spl_context <- data.frame(
    split = c("PARAM", "GRADE_DIR"),
    full_parent_df = I(full_parent_df),
    cur_col_subset = I(cur_col_subset)
  )

  result <- s_count_abnormal_by_worst_grade(
    df = adlb_local %>% dplyr::filter(
      ARMCD == "ARM A" & PARAMCD == "ALT" & GRADE_DIR == "LOW"
    ) %>%
      droplevels(),
    .spl_context = spl_context,
    .var = "GRADE_ANL",
    variables = list(id = "USUBJID", param = "PARAM", grade_dir = "GRADE_DIR")
  )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("count_abnormal_by_worst_grade works as expected", {
  adlb_f <- adlb_local %>%
    dplyr::filter(
      PARAMCD == "IGA"
    ) %>%
    droplevels()

  map <- unique(
    adlb_local[adlb_local$GRADE_DIR != "ZERO", c("PARAM", "GRADE_DIR", "GRADE_ANL")]
  ) %>%
    lapply(as.character) %>%
    as.data.frame() %>%
    dplyr::arrange(PARAM, dplyr::desc(GRADE_DIR), GRADE_ANL)

  result <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    split_rows_by("PARAM") %>%
    split_rows_by("GRADE_DIR", split_fun = trim_levels_to_map(map)) %>%
    count_abnormal_by_worst_grade(
      var = "GRADE_ANL",
      variables = list(id = "USUBJID", param = "PARAM", grade_dir = "GRADE_DIR")
    ) %>%
    build_table(df = adlb_f)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that(
  "count_abnormal_by_worst_grade returns an error when variables$param
  and variables$grade_dir are taking variable names not used
  for splitting the layout in rows.",
  code = {
    adlb_f <- adlb_local %>%
      dplyr::filter(
        PARAMCD == "IGA"
      ) %>%
      droplevels()

    testthat::expect_error(result <- basic_table() %>%
      split_cols_by("ARMCD") %>%
      split_rows_by("PARAM") %>%
      split_rows_by("GRADE_DIR") %>%
      count_abnormal_by_worst_grade(
        var = "GRADE_ANL",
        variables = list(id = "USUBJID", param = "PARAMCD", grade_dir = "ANRIND")
      ) %>%
      build_table(df = adlb_f))
  }
)
