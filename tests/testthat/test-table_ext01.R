# Tests all variants of EXT01

adsl <- adsl_raw
adex <- adex_raw

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

  res <- expect_silent(result)
  expect_snapshot(res)
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

  res <- expect_silent(result)
  expect_snapshot(res)
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

  res <- expect_silent(result)
  expect_snapshot(res)
})
