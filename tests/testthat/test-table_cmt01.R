library(scda)
library(dplyr)
library(rtables)

adsl <- synthetic_cdisc_data("rcd_2021_05_05")$adsl
adcm <- synthetic_cdisc_data("rcd_2021_05_05")$adcm

test_that("CMT01 default variant (Concomitant medications) is produced correctly", {
  adcm_c <- adcm %>% dplyr::filter(ATIREL == "CONCOMITANT")

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
    count_occurrences(vars = "CMDECOD") %>%
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
      "A: Drug X", "(N=134)", "117 (87.3%)", "894", "", "75 (56%)",
      "402", "53 (39.6%)", "45 (33.6%)", "", "83 (61.9%)", "141", "52 (38.8%)",
      "50 (37.3%)", "", "82 (61.2%)", "351", "52 (38.8%)", "51 (38.1%)",
      "B: Placebo", "(N=134)", "116 (86.6%)", "895", "", "79 (59%)",
      "411", "50 (37.3%)", "54 (40.3%)", "", "74 (55.2%)", "137", "57 (42.5%)",
      "45 (33.6%)", "", "84 (62.7%)", "347", "58 (43.3%)", "50 (37.3%)",
      "C: Combination", "(N=132)", "116 (87.9%)", "976", "", "81 (61.4%)",
      "429", "56 (42.4%)", "48 (36.4%)", "", "88 (66.7%)", "162", "59 (44.7%)",
      "55 (41.7%)", "", "89 (67.4%)", "385", "60 (45.5%)", "56 (42.4%)",
      "All Patients", "(N=400)", "349 (87.2%)", "2765", "", "235 (58.8%)",
      "1242", "159 (39.8%)", "147 (36.8%)", "", "245 (61.3%)", "440", "168 (42%)",
      "150 (37.5%)", "", "255 (63.7%)", "1083", "170 (42.5%)", "157 (39.2%)"
    ),
    .Dim = c(19L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})


test_that("CMT01 variant 1 (prior medications) is produced correctly", {

  adcm_p <- adcm %>% dplyr::filter(ATIREL == "PRIOR")

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
    count_occurrences(vars = "CMDECOD") %>%
    build_table(adcm_p, alt_counts_df = adsl) %>%
    sort_at_path(path = c("CMCLAS", "*", "CMDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one treatment", "Total number of treatments",
      "medcl A", "Total number of patients with at least one treatment", "Total number of treatments",
      "medname A_1/3", "medcl B", "Total number of patients with at least one treatment",
      "Total number of treatments", "medname B_2/4", "medname B_3/4",
      "A: Drug X", "(N=134)", "89 (66.4%)", "253", "", "54 (40.3%)",
      "71", "54 (40.3%)", "", "76 (56.7%)", "182", "52 (38.8%)", "47 (35.1%)",
      "B: Placebo", "(N=134)", "95 (70.9%)", "273", "", "49 (36.6%)",
      "70", "49 (36.6%)", "", "80 (59.7%)", "203", "55 (41%)", "47 (35.1%)",
      "C: Combination", "(N=132)", "106 (80.3%)", "311", "", "69 (52.3%)",
      "99", "69 (52.3%)", "", "81 (61.4%)", "212", "56 (42.4%)", "52 (39.4%)",
      "All Patients", "(N=400)", "290 (72.5%)", "837", "", "172 (43%)",
      "240", "172 (43%)", "", "237 (59.2%)", "597", "163 (40.8%)",
      "146 (36.5%)"
    ),
    .Dim = c(13L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})



test_that("CMT01 variant 3 (Concomitant medications) is produced correctly", {
  adcm_c <- adcm %>% dplyr::filter(ATIREL == "CONCOMITANT")

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
    count_occurrences(vars = "CMDECOD") %>%
    build_table(adcm_c, alt_counts_df = adsl) %>%
    sort_at_path(path = c("CMCLAS", "*", "CMDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one treatment",
      "Total number of treatments", "medcl A", "Total number of patients with at least one treatment",
      "medname A_2/3", "medname A_3/3", "medcl B", "Total number of patients with at least one treatment",
      "medname B_1/4", "medname B_4/4", "medcl C", "Total number of patients with at least one treatment",
      "medname C_2/2", "medname C_1/2", "A: Drug X", "(N=134)", "117 (87.3%)",
      "894", "", "75 (56%)", "53 (39.6%)", "45 (33.6%)", "", "83 (61.9%)",
      "52 (38.8%)", "50 (37.3%)", "", "82 (61.2%)", "52 (38.8%)", "51 (38.1%)",
      "B: Placebo", "(N=134)", "116 (86.6%)", "895", "", "79 (59%)",
      "50 (37.3%)", "54 (40.3%)", "", "74 (55.2%)", "57 (42.5%)", "45 (33.6%)",
      "", "84 (62.7%)", "58 (43.3%)", "50 (37.3%)", "C: Combination",
      "(N=132)", "116 (87.9%)", "976", "", "81 (61.4%)", "56 (42.4%)",
      "48 (36.4%)", "", "88 (66.7%)", "59 (44.7%)", "55 (41.7%)", "",
      "89 (67.4%)", "60 (45.5%)", "56 (42.4%)", "All Patients", "(N=400)",
      "349 (87.2%)", "2765", "", "235 (58.8%)", "159 (39.8%)", "147 (36.8%)",
      "", "245 (61.3%)", "168 (42%)", "150 (37.5%)", "", "255 (63.7%)",
      "170 (42.5%)", "157 (39.2%)"
    ),
    .Dim = c(16L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("CMT01 variant 4 (Concomitant medications) is produced correctly", {
  adcm_c <- adcm %>% dplyr::filter(ATIREL == "CONCOMITANT")

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
    count_occurrences(vars = "CMDECOD") %>%
    build_table(adcm_c, alt_counts_df = adsl) %>%
    sort_at_path(path = c("CMCLAS", "*", "CMDECOD"), scorefun = score_occurrences, decreasing = TRUE) %>%
    sort_at_path(path = c("CMCLAS"), scorefun = cont_n_onecol(4), decreasing = TRUE)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one treatment (%)",
      "Total number of treatments", "medcl C", "Total number of patients with at least one treatment (%)",
      "Total number of treatments", "medname C_2/2", "medname C_1/2",
      "medcl B", "Total number of patients with at least one treatment (%)",
      "Total number of treatments", "medname B_1/4", "medname B_4/4",
      "medcl A", "Total number of patients with at least one treatment (%)",
      "Total number of treatments", "medname A_2/3", "medname A_3/3",
      "A: Drug X", "(N=134)", "117 (87.3%)", "894", "", "82 (61.2%)",
      "351", "52 (38.8%)", "51 (38.1%)", "", "83 (61.9%)", "141", "52 (38.8%)",
      "50 (37.3%)", "", "75 (56%)", "402", "53 (39.6%)", "45 (33.6%)",
      "B: Placebo", "(N=134)", "116 (86.6%)", "895", "", "84 (62.7%)",
      "347", "58 (43.3%)", "50 (37.3%)", "", "74 (55.2%)", "137", "57 (42.5%)",
      "45 (33.6%)", "", "79 (59%)", "411", "50 (37.3%)", "54 (40.3%)",
      "C: Combination", "(N=132)", "116 (87.9%)", "976", "", "89 (67.4%)",
      "385", "60 (45.5%)", "56 (42.4%)", "", "88 (66.7%)", "162", "59 (44.7%)",
      "55 (41.7%)", "", "81 (61.4%)", "429", "56 (42.4%)", "48 (36.4%)",
      "All Patients", "(N=400)", "349 (87.2%)", "2765", "", "255 (63.7%)",
      "1083", "170 (42.5%)", "157 (39.2%)", "", "245 (61.3%)", "440", "168 (42%)",
      "150 (37.5%)", "", "235 (58.8%)", "1242", "159 (39.8%)", "147 (36.8%)"
    ),
    .Dim = c(19L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})
