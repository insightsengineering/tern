library(scda)
library(rtables)
library(dplyr)
library(forcats)

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

testthat::test_that("s_count_abnormal_by_worst_grade works as expected", {
  adlb <- adlb_raw

  adlb_alt <- adlb %>%
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
    df = adlb %>% dplyr::filter(
      ARMCD == "ARM A" & PARAMCD == "ALT" & GRADE_DIR == "LOW"
    ) %>%
      droplevels(),
    .spl_context = spl_context,
    .var = "GRADE_ANL",
    variables = list(id = "USUBJID", param = "PARAM", grade_dir = "GRADE_DIR")
  )

  expected <- list(count_fraction = list(
    `1` = formatters::with_label(c(count = 14, fraction = 0.1044776), "1"),
    `2` = formatters::with_label(c(count = 13, fraction = 0.09701493), "2"),
    `3` = formatters::with_label(c(count = 20, fraction = 0.1492537), "3"),
    `4` = formatters::with_label(c(count = 7, fraction = 0.05223881), "4"),
    Any = formatters::with_label(c(count = 54, fraction = 0.4029851), "Any")
  ))
  testthat::expect_equal(result, expected, tolerance = 0.000001)
})

testthat::test_that("count_abnormal_by_worst_grade works as expected", {
  adlb <- adlb_raw
  adlb_f <- adlb %>%
    dplyr::filter(
      PARAMCD == "IGA"
    ) %>%
    droplevels()

  map <- unique(
    adlb[adlb$GRADE_DIR != "ZERO", c("PARAM", "GRADE_DIR", "GRADE_ANL")]
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

  result_matrix <- to_string_matrix(result)
  expected_matrix <-
    structure(
      c(
        "",
        "Immunoglobulin A Measurement",
        "HIGH",
        "1",
        "2",
        "3",
        "4",
        "Any",
        "ARM A",
        "",
        "",
        "25 (18.7%)",
        "14 (10.4%)",
        "12 (9%)",
        "11 (8.2%)",
        "62 (46.3%)",
        "ARM B",
        "",
        "",
        "14 (10.4%)",
        "20 (14.9%)",
        "13 (9.7%)",
        "13 (9.7%)",
        "60 (44.8%)",
        "ARM C",
        "",
        "",
        "13 (9.8%)",
        "16 (12.1%)",
        "17 (12.9%)",
        "4 (3%)",
        "50 (37.9%)"
      ),
      .Dim = c(8L, 4L)
    )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that(
  "count_abnormal_by_worst_grade returns an error when variables$param
  and variables$grade_dir are taking variable names not used
  for splitting the layout in rows.",
  code = {
    adlb <- adlb_raw
    adlb_f <- adlb %>%
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
