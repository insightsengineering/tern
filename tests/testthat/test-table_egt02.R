# Tests the variants for EGT02.

library(random.cdisc.data)
library(dplyr)

test_that("(EGT02) 1. Regardless of Abnormality at Baseline", {

  adsl <- radsl(cached = TRUE)

  # Only keep post baseline visits.
  adeg <- radeg(cached = TRUE) %>%
    filter(ONTRTFL == "Y" & !is.na(ANRIND) & AVISITN >= 1) %>%
    mutate(PARAM = droplevels(PARAM))

  n_col_counts <- table(adsl$ARM)
  n_col_counts <- c(n_col_counts, Total = sum(n_col_counts))

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by(
      "PARAM",
      split_label = c("Parameter / Analysis Reference Range Indicator"),
      visible_label = TRUE
    ) %>%
    count_abnormal("ANRIND", abnormal = c(Low = "LOW", High = "HIGH")) %>%
    build_table(df = adeg, col_counts = n_col_counts)

  result_matrix <- to_string_matrix(result)

  # Expected input was checked against current result in TLG Catalog before refactoring.
  expected_matrix <- structure(
    c("", "", "Parameter / Analysis Reference Range Indicator",
      "Heart Rate", "Low", "High", "QT Duration", "Low", "High", "RR Duration",
      "Low", "High", "A: Drug X", "(N=134)", "", "", "39/134 (29.1%)",
      "39/134 (29.1%)", "", "33/134 (24.6%)", "30/134 (22.4%)", "",
      "45/134 (33.6%)", "29/134 (21.6%)", "B: Placebo", "(N=134)",
      "", "", "43/134 (32.1%)", "44/134 (32.8%)", "", "44/134 (32.8%)",
      "42/134 (31.3%)", "", "26/134 (19.4%)", "48/134 (35.8%)", "C: Combination",
      "(N=132)", "", "", "37/132 (28%)", "36/132 (27.3%)", "", "47/132 (35.6%)",
      "34/132 (25.8%)", "", "38/132 (28.8%)", "27/132 (20.5%)", "All Patients",
      "(N=400)", "", "", "119/400 (29.8%)", "119/400 (29.8%)", "",
      "124/400 (31%)", "106/400 (26.5%)", "", "109/400 (27.3%)", "104/400 (26%)"
    ),
    .Dim = c(12L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})


test_that("(EGT02) 2. Among Subjects Without Abnormality at Baseline", {
  adsl <- radsl(cached = TRUE)

  adeg <- radeg(cached = TRUE) %>%
    filter(ONTRTFL == "Y" & !is.na(ANRIND) & AVISITN >= 0) %>%
    mutate(PARAM = droplevels(PARAM))

  n_col_counts <- table(adsl$ARM)
  n_col_counts <- c(n_col_counts, Total = sum(n_col_counts))

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by(
      "PARAM",
      split_label = c("Parameter / Analysis Reference Range Indicator"),
      visible_label = TRUE
    ) %>%
    count_abnormal("ANRIND", abnormal = c(Low = "LOW", High = "HIGH")) %>%
    build_table(df = adeg, col_counts = n_col_counts)

  result_matrix <- to_string_matrix(result)
  dput(result_matrix)

  expected_matrix <- structure(
    c(
      "", "", "Parameter / Analysis Reference Range Indicator", "Heart Rate", "Low",
      "High", "QT Duration", "Low", "High", "RR Duration", "Low", "High",
      "A: Drug X", "(N=134)", "", "", "38/128 (29.7%)", "37/127 (29.1%)",
      "", "29/113 (25.7%)", "30/125 (24%)", "", "45/129 (34.9%)", "27/127 (21.3%)",
      "B: Placebo", "(N=134)", "", "", "42/124 (33.9%)", "42/129 (32.6%)",
      "", "42/126 (33.3%)", "40/130 (30.8%)", "", "26/123 (21.1%)",
      "42/122 (34.4%)", "C: Combination", "(N=132)", "", "", "33/117 (28.2%)",
      "31/122 (25.4%)", "", "43/124 (34.7%)", "30/119 (25.2%)", "",
      "37/126 (29.4%)", "25/125 (20%)", "All Patients", "(N=400)",
      "", "", "113/369 (30.6%)", "110/378 (29.1%)", "", "114/363 (31.4%)",
      "100/374 (26.7%)", "", "108/378 (28.6%)", "94/374 (25.1%)"
    ),
    .Dim = c(12L, 5L)
  )
  expect_identical(result_matrix, expected_matrix)
})
