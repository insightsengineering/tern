library(random.cdisc.data)
library(dplyr)
library(rtables)

adsl <- radsl(cached = TRUE)
adcm <- radcm(ADSL = adsl, seed = 1234, who_coding = FALSE)

test_that("CMT01 default variant (Concomitant medications) is produced correctly", {
  adcm_c <- adcm %>% filter(ATIREL == "CONCOMITANT")

  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    split_rows_by("CMCLAS",
      split_fun = drop_split_levels, child_labels = "visible",
      nested = FALSE
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    count_occurrences(var = "CMDECOD") %>%
    build_table(adcm_c, alt_counts_df = adsl) %>%
    sort_at_path(path = c("CMCLAS", "*", "CMDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one treatment",
      "Total number of treatments", "medcl A", "Total number of patients with at least one treatment",
      "Total number of treatments", "medname A_2/3", "medname A_3/3",
      "medcl B", "Total number of patients with at least one treatment",
      "Total number of treatments", "medname B_1/4", "medname B_4/4",
      "medcl C", "Total number of patients with at least one treatment",
      "Total number of treatments", "medname C_2/2", "medname C_1/2",
      "A: Drug X", "(N=134)", "120 (89.6%)", "471", "", "87 (64.9%)",
      "161", "63 (47%)", "56 (41.8%)", "", "88 (65.7%)", "138", "58 (43.3%)",
      "54 (40.3%)", "", "89 (66.4%)", "172", "62 (46.3%)", "58 (43.3%)",
      "B: Placebo", "(N=134)", "118 (88.1%)", "438", "", "89 (66.4%)",
      "160", "58 (43.3%)", "58 (43.3%)", "", "92 (68.7%)", "147", "59 (44%)",
      "52 (38.8%)", "", "78 (58.2%)", "131", "47 (35.1%)", "51 (38.1%)",
      "C: Combination", "(N=132)", "109 (82.6%)", "436", "", "79 (59.8%)",
      "140", "49 (37.1%)", "54 (40.9%)", "", "80 (60.6%)", "138", "51 (38.6%)",
      "50 (37.9%)", "", "78 (59.1%)", "158", "57 (43.2%)", "51 (38.6%)",
      "All Patients", "(N=400)", "347 (86.8%)", "1345", "", "255 (63.7%)",
      "461", "170 (42.5%)", "168 (42%)", "", "260 (65%)", "423", "168 (42%)",
      "156 (39%)", "", "245 (61.3%)", "461", "166 (41.5%)", "160 (40%)"
    ),
    .Dim = c(19L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})


test_that("CMT01 variant 1 (prior medications) is produced correctly", {

  adcm_p <- adcm %>% filter(ATIREL == "PRIOR")

  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    split_rows_by("CMCLAS",
      split_fun = drop_split_levels, child_labels = "visible",
      nested = FALSE
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    count_occurrences(var = "CMDECOD") %>%
    build_table(adcm_p, alt_counts_df = adsl) %>%
    sort_at_path(path = c("CMCLAS", "*", "CMDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one treatment",
      "Total number of treatments", "medcl A", "Total number of patients with at least one treatment",
      "Total number of treatments", "medname A_1/3", "medcl B", "Total number of patients with at least one treatment",
      "Total number of treatments", "medname B_3/4", "medname B_2/4",
      "A: Drug X", "(N=134)", "107 (79.9%)", "256", "", "58 (43.3%)",
      "75", "58 (43.3%)", "", "92 (68.7%)", "181", "64 (47.8%)", "62 (46.3%)",
      "B: Placebo", "(N=134)", "97 (72.4%)", "237", "", "51 (38.1%)",
      "72", "51 (38.1%)", "", "88 (65.7%)", "165", "59 (44%)", "58 (43.3%)",
      "C: Combination", "(N=132)", "95 (72%)", "235", "", "47 (35.6%)",
      "66", "47 (35.6%)", "", "85 (64.4%)", "169", "60 (45.5%)", "57 (43.2%)",
      "All Patients", "(N=400)", "299 (74.8%)", "728", "", "156 (39%)",
      "213", "156 (39%)", "", "265 (66.2%)", "515", "183 (45.8%)",
      "177 (44.2%)"
    ),
    .Dim = c(13L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})



test_that("CMT01 variant 3 (Concomitant medications) is produced correctly", {
  adcm_c <- adcm %>% filter(ATIREL == "CONCOMITANT")

  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    split_rows_by("CMCLAS",
      split_fun = drop_split_levels, child_labels = "visible",
      nested = FALSE
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .labels = c(unique = "Total number of patients with at least one treatment"),
      .stats = "unique"
    ) %>%
    count_occurrences(var = "CMDECOD") %>%
    build_table(adcm_c, alt_counts_df = adsl) %>%
    sort_at_path(path = c("CMCLAS", "*", "CMDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one treatment",
      "Total number of treatments", "medcl A", "Total number of patients with at least one treatment",
      "medname A_2/3", "medname A_3/3", "medcl B", "Total number of patients with at least one treatment",
      "medname B_1/4", "medname B_4/4", "medcl C", "Total number of patients with at least one treatment",
      "medname C_2/2", "medname C_1/2", "A: Drug X", "(N=134)", "120 (89.6%)",
      "471", "", "87 (64.9%)", "63 (47%)", "56 (41.8%)", "", "88 (65.7%)",
      "58 (43.3%)", "54 (40.3%)", "", "89 (66.4%)", "62 (46.3%)", "58 (43.3%)",
      "B: Placebo", "(N=134)", "118 (88.1%)", "438", "", "89 (66.4%)",
      "58 (43.3%)", "58 (43.3%)", "", "92 (68.7%)", "59 (44%)", "52 (38.8%)",
      "", "78 (58.2%)", "47 (35.1%)", "51 (38.1%)", "C: Combination",
      "(N=132)", "109 (82.6%)", "436", "", "79 (59.8%)", "49 (37.1%)",
      "54 (40.9%)", "", "80 (60.6%)", "51 (38.6%)", "50 (37.9%)", "",
      "78 (59.1%)", "57 (43.2%)", "51 (38.6%)", "All Patients", "(N=400)",
      "347 (86.8%)", "1345", "", "255 (63.7%)", "170 (42.5%)", "168 (42%)",
      "", "260 (65%)", "168 (42%)", "156 (39%)", "", "245 (61.3%)",
      "166 (41.5%)", "160 (40%)"
    ),
    .Dim = c(16L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("CMT01 variant 4 (Concomitant medications) is produced correctly", {
  adcm_c <- adcm %>% filter(ATIREL == "CONCOMITANT")

  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c("Total number of patients with at least one treatment (%)", "Total number of treatments")
    ) %>%
    split_rows_by("CMCLAS", split_fun = drop_split_levels, child_labels = "visible", nested = FALSE) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c("Total number of patients with at least one treatment (%)", "Total number of treatments")
    ) %>%
    count_occurrences(var = "CMDECOD") %>%
    build_table(adcm_c, alt_counts_df = adsl) %>%
    sort_at_path(path = c("CMCLAS", "*", "CMDECOD"), scorefun = score_occurrences, decreasing = TRUE) %>%
    sort_at_path(path = c("CMCLAS"), scorefun = cont_n_onecol(4), decreasing = TRUE)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one treatment (%)",
      "Total number of treatments", "medcl B", "Total number of patients with at least one treatment (%)",
      "Total number of treatments", "medname B_1/4", "medname B_4/4",
      "medcl A", "Total number of patients with at least one treatment (%)",
      "Total number of treatments", "medname A_2/3", "medname A_3/3",
      "medcl C", "Total number of patients with at least one treatment (%)",
      "Total number of treatments", "medname C_2/2", "medname C_1/2",
      "A: Drug X", "(N=134)", "120 (89.6%)", "471", "", "88 (65.7%)",
      "138", "58 (43.3%)", "54 (40.3%)", "", "87 (64.9%)", "161", "63 (47%)",
      "56 (41.8%)", "", "89 (66.4%)", "172", "62 (46.3%)", "58 (43.3%)",
      "B: Placebo", "(N=134)", "118 (88.1%)", "438", "", "92 (68.7%)",
      "147", "59 (44%)", "52 (38.8%)", "", "89 (66.4%)", "160", "58 (43.3%)",
      "58 (43.3%)", "", "78 (58.2%)", "131", "47 (35.1%)", "51 (38.1%)",
      "C: Combination", "(N=132)", "109 (82.6%)", "436", "", "80 (60.6%)",
      "138", "51 (38.6%)", "50 (37.9%)", "", "79 (59.8%)", "140", "49 (37.1%)",
      "54 (40.9%)", "", "78 (59.1%)", "158", "57 (43.2%)", "51 (38.6%)",
      "All Patients", "(N=400)", "347 (86.8%)", "1345", "", "260 (65%)",
      "423", "168 (42%)", "156 (39%)", "", "255 (63.7%)", "461", "170 (42.5%)",
      "168 (42%)", "", "245 (61.3%)", "461", "166 (41.5%)", "160 (40%)"
    ),
    .Dim = c(19L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})
