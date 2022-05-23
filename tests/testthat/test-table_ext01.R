# Tests all variants of EXT01

library(scda)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adex <- synthetic_cdisc_data("rcd_2022_02_28")$adex

testthat::test_that("EXT01 default variant with numeric parameters is produced correctly", {
  adex <- adex %>%
    dplyr::filter(PARCAT1 == "OVERALL" & PARCAT2 == "Drug A") %>%
    dplyr::select(STUDYID, USUBJID, ARM, PARAMCD, PARAM, AVAL) %>%
    dplyr::mutate(
      PARAMCD = as.character(PARAMCD),
      AVALC = ""
    ) %>%
    droplevels()
  # Add new param tdurd for treatment duration.
  set.seed(99)
  tdurd <- adsl %>%
    dplyr::select(STUDYID, USUBJID, ARM) %>%
    dplyr::mutate(
      PARAMCD = "TDURD",
      PARAM = "Treatment duration (days)",
      AVAL = sample(1:150, size = nrow(adsl), replace = TRUE),
      AVALC = dplyr::case_when(
        0 <= AVAL & AVAL <= 30 ~ "0 - 30",
        31 <= AVAL & AVAL <= 60 ~ "31 - 60",
        61 <= AVAL & AVAL <= 90 ~ "61 - 90",
        TRUE ~ ">= 91"
      )
    )
  tdurd <- adex %>%
    dplyr::filter(PARAMCD == "TNDOSE") %>%
    dplyr::select(STUDYID, USUBJID) %>%
    dplyr::left_join(tdurd, by = c("STUDYID", "USUBJID"))

  # Add new param tndosmis for missed doses.
  tndosmis <- adsl %>%
    dplyr::select(STUDYID, USUBJID, ARM) %>%
    dplyr::mutate(
      PARAMCD = "TNDOSMIS",
      PARAM = "Total number of missed doses during study",
      AVAL = sample(0:20, size = nrow(adsl), replace = TRUE),
      AVALC = ""
    )
  tndosmis <- adex %>%
    dplyr::filter(PARAMCD == "TNDOSE") %>%
    dplyr::select(STUDYID, USUBJID) %>%
    dplyr::left_join(tndosmis, by = c("STUDYID", "USUBJID"))
  adex <- rbind(adex, tdurd, tndosmis)

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("PARAM", split_fun = drop_split_levels) %>%
    summarize_vars(vars = "AVAL") %>%
    build_table(adex, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Total dose administered", "n", "Mean (SD)", "Median", "Min - Max",
      "Total number of doses administered", "n", "Mean (SD)", "Median", "Min - Max",
      "Treatment duration (days)", "n", "Mean (SD)", "Median", "Min - Max",
      "Total number of missed doses during study", "n", "Mean (SD)", "Median", "Min - Max",
      "A: Drug X", "(N=134)", "", "75", "6675.2 (1110.9)", "6720.0", "4800.0 - 9360.0",
      "", "75", "7.0 (0.0)", "7.0", "7.0 - 7.0", "", "75", "74.3 (41.6)", "77.0", "5.0 - 149.0",
      "", "75", "10.5 (5.9)", "10.0", "0.0 - 20.0",
      "B: Placebo", "(N=134)", "", "67", "6505.1 (1249.3)", "6480.0", "4080.0 - 9360.0",
      "", "67", "7.0 (0.0)", "7.0", "7.0 - 7.0", "", "67", "79.0 (43.1)", "80.0", "2.0 - 150.0",
      "", "67", "10.0 (6.1)", "11.0", "0.0 - 19.0",
      "C: Combination", "(N=132)", "", "75", "6982.4 (1272.5)", "7200.0", "4320.0 - 9360.0",
      "", "75", "7.0 (0.0)", "7.0", "7.0 - 7.0", "", "75", "74.2 (39.5)", "78.0", "1.0 - 147.0",
      "", "75", "9.5 (5.5)", "9.0", "0.0 - 20.0"
    ),
    .Dim = c(22L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("EXT01 variant: with both numeric and categorical parameters", {
  adex <- adex %>%
    dplyr::filter(PARCAT1 == "OVERALL" & PARCAT2 == "Drug A") %>%
    dplyr::select(STUDYID, USUBJID, ARM, PARAMCD, PARAM, AVAL) %>%
    dplyr::mutate(
      PARAMCD = as.character(PARAMCD),
      AVALC = ""
    ) %>%
    droplevels()
  # Add new param tdurd for treatment duration.
  set.seed(99)
  tdurd <- adsl %>%
    dplyr::select(STUDYID, USUBJID, ARM) %>%
    dplyr::mutate(
      PARAMCD = "TDURD",
      PARAM = "Treatment duration (days)",
      AVAL = sample(1:150, size = nrow(adsl), replace = TRUE),
      AVALC = dplyr::case_when(
        0 <= AVAL & AVAL <= 30 ~ "0 - 30",
        31 <= AVAL & AVAL <= 60 ~ "31 - 60",
        61 <= AVAL & AVAL <= 90 ~ "61 - 90",
        TRUE ~ ">= 91"
      )
    )
  tdurd <- adex %>%
    dplyr::filter(PARAMCD == "TNDOSE") %>%
    dplyr::select(STUDYID, USUBJID) %>%
    dplyr::left_join(tdurd, by = c("STUDYID", "USUBJID"))

  # Add new param tndosmis for missed doses.
  tndosmis <- adsl %>%
    dplyr::select(STUDYID, USUBJID, ARM) %>%
    dplyr::mutate(
      PARAMCD = "TNDOSMIS",
      PARAM = "Total number of missed doses during study",
      AVAL = sample(0:20, size = nrow(adsl), replace = TRUE),
      AVALC = ""
    )
  tndosmis <- adex %>%
    dplyr::filter(PARAMCD == "TNDOSE") %>%
    dplyr::select(STUDYID, USUBJID) %>%
    dplyr::left_join(tndosmis, by = c("STUDYID", "USUBJID"))
  adex <- rbind(adex, tdurd, tndosmis)

  # need to transpose data to wide when both numeric and categorical parameters
  # are to be summarized
  adex_avalc_wide <- adex %>%
    dplyr::filter(PARAMCD == "TDURD") %>%
    dplyr::select(STUDYID, USUBJID, PARAMCD, AVALC) %>%
    tidyr::pivot_wider(
      id_cols = c(STUDYID, USUBJID),
      names_from = PARAMCD,
      values_from = AVALC
    ) %>%
    dplyr::mutate(
      TDURDC = factor(TDURD, levels = c("0 - 30", "31 - 60", "61 - 90", ">= 91"))
    ) %>%
    dplyr::select(-TDURD)
  anl <- adex %>%
    dplyr::select(STUDYID, USUBJID, ARM, PARAMCD, AVAL) %>%
    tidyr::pivot_wider(
      id_cols = c(STUDYID, USUBJID, ARM),
      names_from = PARAMCD,
      values_from = AVAL
    ) %>%
    dplyr::left_join(adex_avalc_wide, by = c("STUDYID", "USUBJID"))
  columns <- c("TDOSE", "TNDOSE", "TDURD", "TNDOSMIS", "TDURDC")
  labels <- c(
    "Total dose administered",
    "Total number of doses administered",
    "Treatment duration (days)",
    "Total number of missed doses during study",
    "Treatment duration (days)"
  )
  formatters::var_labels(anl)[columns] <- labels

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_vars(
      vars = c("TDURD", "TDURDC", "TDOSE", "TNDOSE"),
      var_labels = formatters::var_labels(anl)[c("TDURD", "TDURDC", "TDOSE", "TNDOSE")]
    ) %>%
    build_table(anl, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Treatment duration (days)", "n", "Mean (SD)", "Median", "Min - Max",
      "Treatment duration (days)", "n", "0 - 30", "31 - 60", "61 - 90", ">= 91",
      "Total dose administered", "n", "Mean (SD)", "Median", "Min - Max",
      "Total number of doses administered", "n", "Mean (SD)", "Median", "Min - Max",
      "A: Drug X", "(N=134)", "", "75", "74.3 (41.6)", "77.0", "5.0 - 149.0",
      "", "75", "12 (16%)", "18 (24%)", "19 (25.3%)", "26 (34.7%)",
      "", "75", "6675.2 (1110.9)", "6720.0", "4800.0 - 9360.0",
      "", "75", "7.0 (0.0)", "7.0", "7.0 - 7.0",
      "B: Placebo", "(N=134)", "", "67", "79.0 (43.1)", "80.0", "2.0 - 150.0",
      "", "67", "12 (17.9%)", "12 (17.9%)", "15 (22.4%)", "28 (41.8%)",
      "", "67", "6505.1 (1249.3)", "6480.0", "4080.0 - 9360.0",
      "", "67", "7.0 (0.0)", "7.0", "7.0 - 7.0",
      "C: Combination", "(N=132)", "", "75", "74.2 (39.5)", "78.0", "1.0 - 147.0",
      "", "75", "15 (20%)", "14 (18.7%)", "18 (24%)", "28 (37.3%)",
      "", "75", "6982.4 (1272.5)", "7200.0", "4320.0 - 9360.0",
      "", "75", "7.0 (0.0)", "7.0", "7.0 - 7.0"
    ),
    .Dim = c(23L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("EXT01 variant: with user specified categories for missed doses", {
  adex <- adex %>%
    dplyr::filter(PARCAT1 == "OVERALL" & PARCAT2 == "Drug A") %>%
    dplyr::select(STUDYID, USUBJID, ARM, PARAMCD, PARAM, AVAL) %>%
    dplyr::mutate(
      PARAMCD = as.character(PARAMCD),
      AVALC = ""
    ) %>%
    droplevels()
  # Add new param tdurd for treatment duration.
  set.seed(99)
  tdurd <- adsl %>%
    dplyr::select(STUDYID, USUBJID, ARM) %>%
    dplyr::mutate(
      PARAMCD = "TDURD",
      PARAM = "Treatment duration (days)",
      AVAL = sample(1:150, size = nrow(adsl), replace = TRUE),
      AVALC = dplyr::case_when(
        0 <= AVAL & AVAL <= 30 ~ "0 - 30",
        31 <= AVAL & AVAL <= 60 ~ "31 - 60",
        61 <= AVAL & AVAL <= 90 ~ "61 - 90",
        TRUE ~ ">= 91"
      )
    )
  tdurd <- adex %>%
    dplyr::filter(PARAMCD == "TNDOSE") %>%
    dplyr::select(STUDYID, USUBJID) %>%
    dplyr::left_join(tdurd, by = c("STUDYID", "USUBJID"))

  # Add new param tndosmis for missed doses.
  tndosmis <- adsl %>%
    dplyr::select(STUDYID, USUBJID, ARM) %>%
    dplyr::mutate(
      PARAMCD = "TNDOSMIS",
      PARAM = "Total number of missed doses during study",
      AVAL = sample(0:20, size = nrow(adsl), replace = TRUE),
      AVALC = ""
    )
  tndosmis <- adex %>%
    dplyr::filter(PARAMCD == "TNDOSE") %>%
    dplyr::select(STUDYID, USUBJID) %>%
    dplyr::left_join(tndosmis, by = c("STUDYID", "USUBJID"))
  adex <- rbind(adex, tdurd, tndosmis)

  # need to transpose data to wide when both numeric and categorical parameters
  # are to be summarized
  adex_avalc_wide <- adex %>%
    dplyr::filter(PARAMCD == "TDURD") %>%
    dplyr::select(STUDYID, USUBJID, PARAMCD, AVALC) %>%
    tidyr::pivot_wider(
      id_cols = c(STUDYID, USUBJID),
      names_from = PARAMCD,
      values_from = AVALC
    ) %>%
    dplyr::mutate(
      TDURDC = factor(TDURD, levels = c("0 - 30", "31 - 60", "61 - 90", ">= 91"))
    ) %>%
    dplyr::select(-TDURD)
  anl <- adex %>%
    dplyr::select(STUDYID, USUBJID, ARM, PARAMCD, AVAL) %>%
    tidyr::pivot_wider(
      id_cols = c(STUDYID, USUBJID, ARM),
      names_from = PARAMCD,
      values_from = AVAL
    ) %>%
    dplyr::left_join(adex_avalc_wide, by = c("STUDYID", "USUBJID"))
  columns <- c("TDOSE", "TNDOSE", "TDURD", "TNDOSMIS", "TDURDC")
  labels <- c(
    "Total dose administered",
    "Total number of doses administered",
    "Treatment duration (days)",
    "Total number of missed doses during study",
    "Treatment duration (days)"
  )
  formatters::var_labels(anl)[columns] <- labels

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_vars(
      vars = c("TDURD", "TDURDC", "TDOSE", "TNDOSE"),
      var_labels = formatters::var_labels(anl)[c("TDURD", "TDURDC", "TDOSE", "TNDOSE")]
    ) %>%
    count_missed_doses(
      "TNDOSMIS",
      thresholds = c(1, 5, 10, 15),
      var_labels = "Missed Doses"
    ) %>%
    build_table(anl, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Treatment duration (days)", "n", "Mean (SD)",
      "Median", "Min - Max", "Treatment duration (days)", "n", "0 - 30",
      "31 - 60", "61 - 90", ">= 91", "Total dose administered", "n",
      "Mean (SD)", "Median", "Min - Max", "Total number of doses administered",
      "n", "Mean (SD)", "Median", "Min - Max", "Missed Doses", "n",
      "At least 1 missed dose", "At least 5 missed doses", "At least 10 missed doses",
      "At least 15 missed doses", "A: Drug X", "(N=134)", "", "75",
      "74.3 (41.6)", "77.0", "5.0 - 149.0", "", "75", "12 (16%)", "18 (24%)",
      "19 (25.3%)", "26 (34.7%)", "", "75", "6675.2 (1110.9)", "6720.0",
      "4800.0 - 9360.0", "", "75", "7.0 (0.0)", "7.0", "7.0 - 7.0", "", "75", "74 (55.2%)",
      "59 (44%)", "41 (30.6%)", "26 (19.4%)", "B: Placebo", "(N=134)",
      "", "67", "79.0 (43.1)", "80.0", "2.0 - 150.0", "", "67", "12 (17.9%)",
      "12 (17.9%)", "15 (22.4%)", "28 (41.8%)", "", "67", "6505.1 (1249.3)",
      "6480.0", "4080.0 - 9360.0", "", "67", "7.0 (0.0)", "7.0", "7.0 - 7.0", "", "67",
      "63 (47%)", "49 (36.6%)", "38 (28.4%)", "21 (15.7%)", "C: Combination",
      "(N=132)", "", "75", "74.2 (39.5)", "78.0", "1.0 - 147.0", "", "75",
      "15 (20%)", "14 (18.7%)", "18 (24%)", "28 (37.3%)", "", "75",
      "6982.4 (1272.5)", "7200.0", "4320.0 - 9360.0", "", "75", "7.0 (0.0)",
      "7.0", "7.0 - 7.0", "", "75", "73 (55.3%)", "59 (44.7%)", "37 (28%)",
      "16 (12.1%)"
    ),
    .Dim = c(29L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
