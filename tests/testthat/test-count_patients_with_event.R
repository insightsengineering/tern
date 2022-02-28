library(dplyr)

testthat::test_that("s_count_patients_with_event handles NA", {
  test_data <- data.frame(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002"),
    TRTEMFL = c("Y", "", "", "NA", "", ""),
    stringsAsFactors = FALSE
  )

  result <- s_count_patients_with_event(
    test_data,
    .var = "SUBJID",
    filters = c("TRTEMFL" = "Y")
  )

  expected <- list(n = 2L, count = 1L, count_fraction = c(1.0, 0.5))
  testthat::expect_identical(result, expected)
})

testthat::test_that("s_count_patients_with_event handles multiple columns", {
  test_data <- data.frame(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL"),
    stringsAsFactors = FALSE
  )

  result <- s_count_patients_with_event(
    test_data,
    .var = "SUBJID",
    filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL")
  )

  expected <- list(n = 3L, count = 1L, count_fraction = c(1.0, 0.3333333))

  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("count_patients_with_event works as expected", {
  test_data <- data.frame(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    ARM = c("A", "A", "A", "A", "A", "A", "B", "B", "B"),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL"),
    stringsAsFactors = FALSE
  )

  test_adsl_like <- test_data[!duplicated(test_data["SUBJID"]), ]

  l <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    count_patients_with_event(
      vars = "SUBJID",
      filters = c("TRTEMFL" = "Y"),
      .labels = c(count_fraction = "Total number of patients with at least one adverse event"),
      table_names = "total_pts_ae"
    ) %>%
    count_patients_with_event(
      vars = "SUBJID",
      filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
      .labels = c(count_fraction = "Total number of patients with fatal AEs"),
      table_names = "total_pts_fatal_ae"
    )

  result <- build_table(l, test_data, alt_counts_df = test_adsl_like)

  result <- to_string_matrix(result)

  expected <- structure(
    c(
      "", "", "Total number of patients with at least one adverse event",
      "Total number of patients with fatal AEs", "A", "(N=2)",
      "1 (50%)", "0", "B", "(N=1)", "1 (100%)", "1 (100%)"
    ),
    .Dim = c(4L, 3L)
  )

  testthat::expect_equal(result, expected, tolerance = 1e-4)
})

testthat::test_that("count_patients_with_event works as expected for different column count", {
  test_data <- data.frame(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    ARM = factor(c("A", "A", "A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL"),
    stringsAsFactors = FALSE
  )

  test_adsl_like <- data.frame(
    SUBJID = as.character(1001:1010),
    ARM = factor(c("A", "A", "B", "B", "A", "A", "A", "B", "B", "A"), levels = c("A", "B")),
    stringsAsFactors = FALSE
  )

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    count_patients_with_event(
      vars = "SUBJID",
      filters = c("TRTEMFL" = "Y"),
      .labels = c(count_fraction = "Total number of patients with at least one adverse event"),
      denom = "N_col",
      table_names = "total_pts_ae"
    ) %>%
    count_patients_with_event(
      vars = "SUBJID",
      filters = c("TRTEMFL" = "Y", "AEOUT" = "FATAL"),
      .labels = c(count_fraction = "Total number of patients with fatal AEs"),
      denom = "N_col",
      table_names = "total_pts_fatal_ae"
    )

  result <- build_table(lyt, df = test_data, alt_counts_df = test_adsl_like)

  result <- to_string_matrix(result)
  expected <- structure(
    c(
      "", "", "Total number of patients with at least one adverse event",
      "Total number of patients with fatal AEs", "A", "(N=6)",
      "1 (16.7%)", "0", "B", "(N=4)", "1 (25%)", "1 (25%)"
    ),
    .Dim = c(4L, 3L)
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("count_patients_with_flags works as expected", {
  test_data <- tibble::tibble(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    ARM = factor(c("A", "A", "A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL")
  )
  test_data <- test_data %>%
    dplyr::mutate(
      flag1 = TRTEMFL == "Y",
      flag2 = TRTEMFL == "Y" & AEOUT == "FATAL",
    )
  labels <- c(
    "A",
    "B",
    "C",
    "D",
    "Total number of patients with at least one adverse event",
    "Total number of patients with fatal AEs"
  )
  var_labels(test_data) <- labels

  test_adsl_like <- tibble::tibble(
    SUBJID = as.character(1001:1010),
    ARM = factor(c("A", "A", "B", "B", "A", "A", "A", "B", "B", "A"), levels = c("A", "B")),
    stringsAsFactors = FALSE
  )

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_patients_with_flags(
      "SUBJID",
      flag_variables = var_labels(test_data[, c("flag1", "flag2")]),
      denom = "N_col"
    )

  result <- build_table(lyt, df = test_data, alt_counts_df = test_adsl_like)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one adverse event",
      "Total number of patients with fatal AEs", "A", "(N=6)",
      "1 (16.7%)", "0", "B", "(N=4)", "1 (25%)", "1 (25%)"
    ),
    .Dim = c(4L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("count_patients_with_flags works as expected when specifying table_names", {
  test_data <- tibble::tibble(
    USUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    ARM = factor(c("A", "A", "A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    TRTEMFL = c("Y", "", "", "NA", "", "", "Y", "", ""),
    AEOUT = c("", "", "", "", "", "", "FATAL", "", "FATAL")
  )
  test_data <- test_data %>%
    dplyr::mutate(
      flag1 = TRTEMFL == "Y",
      flag2 = TRTEMFL == "Y" & AEOUT == "FATAL",
    )
  columns <- c("flag1", "flag2")
  labels <- c(
    "Total number of patients with at least one adverse event",
    "Total number of patients with fatal AEs"
  )
  var_labels(test_data)[columns] <- labels

  test_adsl_like <- tibble::tibble(
    USUBJID = as.character(1001:1010),
    SUBJID = as.character(1001:1010),
    ARM = factor(c("A", "A", "B", "B", "A", "A", "A", "B", "B", "A"), levels = c("A", "B")),
    stringsAsFactors = FALSE
  )

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_patients_with_flags(
      "SUBJID",
      flag_variables = var_labels(test_data[, c("flag1", "flag2")]),
      table_names = "SUBJID",
      denom = "N_col"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(test_data[, c("flag1", "flag2")]),
      table_names = "USUBJID",
      denom = "N_col"
    )

  result <- build_table(lyt, df = test_data, alt_counts_df = test_adsl_like)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one adverse event",
      "Total number of patients with fatal AEs", "Total number of patients with at least one adverse event",
      "Total number of patients with fatal AEs", "A", "(N=6)", "1 (16.7%)",
      "0", "1 (16.7%)", "0", "B", "(N=4)", "1 (25%)", "1 (25%)",
      "1 (25%)", "1 (25%)"
    ),
    .Dim = c(6L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("count_patients_with_flags works with label row specified", {
  adsl <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adsl
  adae <- scda::synthetic_cdisc_data("rcd_2021_07_07")$adae

  # Create custom flags:
  adae <- adae %>%
    dplyr::mutate(
      SER = AESER == "Y",
      REL = AEREL == "Y",
      CTC35 = AETOXGR %in% c("3", "4", "5"),
      CTC45 = AETOXGR %in% c("4", "5")
    )
  columns <- c("SER", "REL", "CTC35", "CTC45")
  labels <- c("Serious AE", "Related AE", "Grade 3-5 AE", "Grade 4/5 AE")
  for (i in seq_along(columns)) {
    attr(adae[[columns[i]]], "label") <- labels[i]
  }
  aesi_vars <- c("SER", "REL", "CTC35", "CTC45")

  # Create layout
  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    count_patients_with_event(
      vars = "USUBJID",
      filters = c("STUDYID" = as.character(unique(adae$STUDYID))),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients with at least one adverse event")
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = var_labels(adae[, aesi_vars]),
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    )

  result <- build_table(lyt, df = adae, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- t(structure(
    c(
      "", "A: Drug X", "B: Placebo", "C: Combination",
      "", "(N=134)", "(N=134)", "(N=132)",
      "Total number of patients with at least one adverse event", "122 (91%)", "123 (91.8%)", "120 (90.9%)",
      "Total number of patients with at least one", "", "", "",
      "Serious AE", "104 (77.6%)", "101 (75.4%)", "99 (75%)",
      "Related AE", "105 (78.4%)", "108 (80.6%)", "109 (82.6%)",
      "Grade 3-5 AE", "109 (81.3%)", "104 (77.6%)", "109 (82.6%)",
      "Grade 4/5 AE", "91 (67.9%)", "90 (67.2%)", "93 (70.5%)"
    ),
    .Dim = c(4L, 8L)
  ))
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("s_count_patients_with_event works with factor filters", {
  test_data <- data.frame(
    SUBJID = c("1001", "1001", "1001", "1002", "1002", "1002", "1003", "1003", "1003"),
    AEOUT = c(
      "RECOVERING/RESOLVING", "RECOVERED/RESOLVED", "RECOVERING/RESOLVING",
      "NOT RECOVERED/NOT RESOLVED", "RECOVERED/RESOLVED WITH SEQUELAE", "UNKNOWN",
      "FATAL", "RECOVERED/RESOLVED WITH SEQUELAE", "FATAL"
    ),
    stringsAsFactors = TRUE
  )
  result <- s_count_patients_with_event(
    test_data,
    .var = "SUBJID",
    filters = c("AEOUT" = "FATAL")
  )
  expected <- list(n = 3, count = 1, count_fraction = c(1.0000000, 0.3333333))
  testthat::expect_equal(result, expected, tolerance = 1e-7)
})
