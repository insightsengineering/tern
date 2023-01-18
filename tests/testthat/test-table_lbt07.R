# Tests LBT07

adsl <- adsl_raw
adlb <- adlb_raw

adlb_local <- local({
  adlb <- adlb_raw # nolintr

  # Data set is modified in order to have some parameters with grades only in one direction
  # and simulate the real data.
  adlb$ATOXGR[adlb$PARAMCD == "ALT" & adlb$ATOXGR %in% c("1", "2", "3", "4")] <- "-1"
  adlb$ANRIND[adlb$PARAMCD == "ALT" & adlb$ANRIND == "HIGH"] <- "LOW"
  adlb$WGRHIFL[adlb$PARAMCD == "ALT"] <- ""

  adlb$ATOXGR[adlb$PARAMCD == "IGA" & adlb$ATOXGR %in% c("-1", "-2", "-3", "-4")] <- "1"
  adlb$ANRIND[adlb$PARAMCD == "IGA" & adlb$ANRIND == "LOW"] <- "HIGH"
  adlb$WGRLOFL[adlb$PARAMCD == "IGA"] <- ""

  # Here starts the real preprocessing.
  adlb_f <- adlb %>%
    dplyr::filter(!AVISIT %in% c("SCREENING", "BASELINE")) %>%
    dplyr::mutate(
      GRADE_DIR = factor(
        dplyr::case_when(
          ATOXGR %in% c("-1", "-2", "-3", "-4") ~ "LOW",
          ATOXGR == "0" ~ "ZERO",
          ATOXGR %in% c("1", "2", "3", "4") ~ "HIGH"
        ),
        levels = c("LOW", "ZERO", "HIGH")
      ),
      GRADE_ANL = forcats::fct_relevel(
        forcats::fct_recode(
          ATOXGR,
          `1` = "-1",
          `2` = "-2",
          `3` = "-3",
          `4` = "-4"
        ),
        c("0", "1", "2", "3", "4")
      )
    ) %>%
    dplyr::filter(WGRLOFL == "Y" | WGRHIFL == "Y") %>%
    droplevels()
  adlb_f
})

testthat::test_that("LBT07 is produced correctly", {
  adlb_f <- adlb_local
  map <- unique(adlb_f[adlb_f$GRADE_DIR != "ZERO", c("PARAM", "GRADE_DIR", "GRADE_ANL")]) %>%
    lapply(as.character) %>%
    as.data.frame() %>%
    dplyr::arrange(PARAM, dplyr::desc(GRADE_DIR), GRADE_ANL)

  lyt <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    add_colcounts() %>%
    split_rows_by("PARAM", label_pos = "topleft") %>%
    summarize_num_patients(
      var = "USUBJID",
      required = "GRADE_ANL",
      .stats = "unique_count"
    ) %>%
    split_rows_by(
      "GRADE_DIR",
      label_pos = "topleft",
      split_label = "Direction of abnormality",
      split_fun = trim_levels_to_map(map = map)
    ) %>%
    count_abnormal_by_worst_grade(
      var = "GRADE_ANL",
      variables = list(id = "USUBJID", param = "PARAM", grade_dir = "GRADE_DIR")
    )

  result <- build_table(lyt, adlb_f, alt_counts_df = adsl)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
