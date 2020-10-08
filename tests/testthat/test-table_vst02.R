# Test the single variant for VST02

library(random.cdisc.data)
library(rtables)
library(dplyr)


test_that("VST02 default variant is produced correctly", {
  adsl <- radsl(cached = TRUE)
  advs <- radvs(cached = TRUE)

  advs_f <- advs %>% filter(ABLFL != "Y" & ABLFL2 != "Y")
  n_col_counts <- table(adsl$ARM)
  n_col_counts <- c(n_col_counts, sum(n_col_counts))

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    split_rows_by("PARAM", split_label = c("Parameter / Abnormality Direction"), visible_label = TRUE) %>%
    analyze_abnormal(vars = "ANRIND", abnormal = c(Low = "LOW", High = "HIGH")) %>%
    build_table(df = advs_f, col_counts = n_col_counts)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(c(
    "", "", "Parameter / Abnormality Direction", "Diastolic Blood Pressure",
    "Low", "High", "Pulse Rate", "Low", "High", "Respiratory Rate",
    "Low", "High", "Systolic Blood Pressure", "Low", "High", "Temperature",
    "Low", "High", "Weight", "Low", "High", "A: Drug X", "(N=134)",
    "", "", "53/134 (39.6%)", "53/134 (39.6%)", "", "63/134 (47%)",
    "52/134 (38.8%)", "", "59/134 (44%)", "62/134 (46.3%)", "", "54/134 (40.3%)",
    "58/134 (43.3%)", "", "59/134 (44%)", "45/134 (33.6%)", "", "62/134 (46.3%)",
    "54/134 (40.3%)", "B: Placebo", "(N=134)", "", "", "57/134 (42.5%)",
    "51/134 (38.1%)", "", "53/134 (39.6%)", "57/134 (42.5%)", "",
    "49/134 (36.6%)", "54/134 (40.3%)", "", "55/134 (41%)", "49/134 (36.6%)",
    "", "50/134 (37.3%)", "54/134 (40.3%)", "", "57/134 (42.5%)",
    "58/134 (43.3%)", "C: Combination", "(N=132)", "", "", "54/132 (40.9%)",
    "52/132 (39.4%)", "", "50/132 (37.9%)", "39/132 (29.5%)", "",
    "45/132 (34.1%)", "62/132 (47%)", "", "57/132 (43.2%)", "52/132 (39.4%)",
    "", "61/132 (46.2%)", "52/132 (39.4%)", "", "63/132 (47.7%)",
    "55/132 (41.7%)", "All Patients", "(N=400)", "", "", "164/400 (41%)",
    "156/400 (39%)", "", "166/400 (41.5%)", "148/400 (37%)", "",
    "153/400 (38.2%)", "178/400 (44.5%)", "", "166/400 (41.5%)",
    "159/400 (39.8%)", "", "170/400 (42.5%)", "151/400 (37.8%)",
    "", "182/400 (45.5%)", "167/400 (41.8%)"),
    .Dim = c(21L, 5L))
  expect_identical(result_matrix, expected_matrix)
})
