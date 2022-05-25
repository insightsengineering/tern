# Tests LBT07.

library(scda)
library(dplyr)
library(forcats)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb

adlb_raw <- local({
  adlb <- synthetic_cdisc_data("rcd_2022_02_28")$adlb # nolintr

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
  adlb_f <- adlb_raw
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
  result_matrix <- to_string_matrix(result)

  expected_matrix <-
    structure(
      c(
        "PARAM",
        "  Direction of abnormality",
        "Alanine Aminotransferase Measurement (n)",
        "LOW",
        "1",
        "2",
        "3",
        "4",
        "Any",
        "C-Reactive Protein Measurement (n)",
        "LOW",
        "1",
        "2",
        "3",
        "4",
        "Any",
        "HIGH",
        "1",
        "2",
        "3",
        "4",
        "Any",
        "Immunoglobulin A Measurement (n)",
        "HIGH",
        "1",
        "2",
        "3",
        "4",
        "Any",
        "ARM A",
        "(N=134)",
        "134",
        "",
        "14 (10.4%)",
        "13 (9.7%)",
        "20 (14.9%)",
        "7 (5.2%)",
        "54 (40.3%)",
        "134",
        "",
        "16 (11.9%)",
        "21 (15.7%)",
        "12 (9%)",
        "11 (8.2%)",
        "60 (44.8%)",
        "",
        "17 (12.7%)",
        "15 (11.2%)",
        "16 (11.9%)",
        "12 (9%)",
        "60 (44.8%)",
        "134",
        "",
        "25 (18.7%)",
        "14 (10.4%)",
        "12 (9%)",
        "11 (8.2%)",
        "62 (46.3%)",
        "ARM B",
        "(N=134)",
        "134",
        "",
        "15 (11.2%)",
        "18 (13.4%)",
        "12 (9%)",
        "8 (6%)",
        "53 (39.6%)",
        "134",
        "",
        "19 (14.2%)",
        "13 (9.7%)",
        "9 (6.7%)",
        "7 (5.2%)",
        "48 (35.8%)",
        "",
        "15 (11.2%)",
        "16 (11.9%)",
        "12 (9%)",
        "12 (9%)",
        "55 (41%)",
        "134",
        "",
        "14 (10.4%)",
        "20 (14.9%)",
        "13 (9.7%)",
        "13 (9.7%)",
        "60 (44.8%)",
        "ARM C",
        "(N=132)",
        "132",
        "",
        "10 (7.6%)",
        "11 (8.3%)",
        "10 (7.6%)",
        "10 (7.6%)",
        "41 (31.1%)",
        "132",
        "",
        "15 (11.4%)",
        "16 (12.1%)",
        "18 (13.6%)",
        "5 (3.8%)",
        "54 (40.9%)",
        "",
        "9 (6.8%)",
        "13 (9.8%)",
        "14 (10.6%)",
        "10 (7.6%)",
        "46 (34.8%)",
        "132",
        "",
        "13 (9.8%)",
        "16 (12.1%)",
        "17 (12.9%)",
        "4 (3%)",
        "50 (37.9%)"
      ),
      .Dim = c(29L, 4L)
    )
  testthat::expect_identical(result_matrix, expected_matrix)
})
