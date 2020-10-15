# Tests DMT01

library(random.cdisc.data)
library(dplyr)
library(rtables)


test_that("CMT01 default variant (Concomitant medications) is produced correctly", {
  adsl <- radsl(cached = TRUE)

  n_per_arm <- table(adsl$ARM)
  n_per_arm <- c(n_per_arm, "All Patients" = sum(n_per_arm))

  adcm <- radcm(cached = TRUE)
  adcm_c <- adcm %>% filter(ATIREL == "CONCOMITANT")

  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
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
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    count_occurrences(var = "CMDECOD") %>%
    build_table(adcm_c, col_counts = n_per_arm) %>%
    prune_table() %>%
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
      "A: Drug X", "(N=134)", "117 (87.3%)", "415", "", "75 (56%)",
      "134", "53 (39.6%)", "45 (33.6%)", "", "83 (61.9%)", "141", "52 (38.8%)",
      "50 (37.3%)", "", "82 (61.2%)", "140", "52 (38.8%)", "51 (38.1%)",
      "B: Placebo", "(N=134)", "116 (86.6%)", "414", "", "79 (59%)",
      "137", "50 (37.3%)", "54 (40.3%)", "", "74 (55.2%)", "137", "57 (42.5%)",
      "45 (33.6%)", "", "84 (62.7%)", "140", "58 (43.3%)", "50 (37.3%)",
      "C: Combination", "(N=132)", "116 (87.9%)", "460", "", "81 (61.4%)",
      "143", "56 (42.4%)", "48 (36.4%)", "", "88 (66.7%)", "162", "59 (44.7%)",
      "55 (41.7%)", "", "89 (67.4%)", "155", "60 (45.5%)", "56 (42.4%)",
      "All Patients", "(N=400)", "349 (87.2%)", "1289", "", "235 (58.8%)",
      "414", "159 (39.8%)", "147 (36.8%)", "", "245 (61.3%)", "440",
      "168 (42%)", "150 (37.5%)", "", "255 (63.7%)", "435", "170 (42.5%)",
      "157 (39.2%)"
    ),
    .Dim = c(19L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})


test_that("CMT01 variant 1 (prior medications) is produced correctly", {
  adsl <- radsl(cached = TRUE)

  n_per_arm <- table(adsl$ARM)
  n_per_arm <- c(n_per_arm, "All Patients" = sum(n_per_arm))

  adcm <- radcm(cached = TRUE)
  adcm_p <- adcm %>% filter(ATIREL == "PRIOR")

  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
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
      .labels = c(
        unique = "Total number of patients with at least one treatment",
        nonunique = "Total number of treatments"
      )
    ) %>%
    count_occurrences(var = "CMDECOD") %>%
    build_table(adcm_p, col_counts = n_per_arm) %>%
    prune_table() %>%
    sort_at_path(path = c("CMCLAS", "*", "CMDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one treatment",
      "Total number of treatments", "medcl A", "Total number of patients with at least one treatment",
      "Total number of treatments", "medname A_1/3", "medcl B", "Total number of patients with at least one treatment",
      "Total number of treatments", "medname B_2/4", "medname B_3/4",
      "A: Drug X", "(N=134)", "89 (66.4%)", "194", "", "54 (40.3%)",
      "71", "54 (40.3%)", "", "76 (56.7%)", "123", "52 (38.8%)", "47 (35.1%)",
      "B: Placebo", "(N=134)", "95 (70.9%)", "208", "", "49 (36.6%)",
      "70", "49 (36.6%)", "", "80 (59.7%)", "138", "55 (41%)", "47 (35.1%)",
      "C: Combination", "(N=132)", "106 (80.3%)", "243", "", "69 (52.3%)",
      "99", "69 (52.3%)", "", "81 (61.4%)", "144", "56 (42.4%)", "52 (39.4%)",
      "All Patients", "(N=400)", "290 (72.5%)", "645", "", "172 (43%)",
      "240", "172 (43%)", "", "237 (59.2%)", "405", "163 (40.8%)",
      "146 (36.5%)"
    ),
    .Dim = c(13L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})



test_that("CMT01 variant 3 (Concomitant medications) is produced correctly", {
  adsl <- radsl(cached = TRUE)

  n_per_arm <- table(adsl$ARM)
  n_per_arm <- c(n_per_arm, "All Patients" = sum(n_per_arm))

  adcm <- radcm(cached = TRUE)
  adcm_c <- adcm %>% filter(ATIREL == "CONCOMITANT")

  result <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
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
    build_table(adcm_c, col_counts = n_per_arm) %>%
    prune_table() %>%
    sort_at_path(path = c("CMCLAS", "*", "CMDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "Total number of patients with at least one treatment",
      "Total number of treatments", "medcl A", "Total number of patients with at least one treatment",
      "medname A_2/3", "medname A_3/3", "medcl B", "Total number of patients with at least one treatment",
      "medname B_1/4", "medname B_4/4", "medcl C", "Total number of patients with at least one treatment",
      "medname C_2/2", "medname C_1/2", "A: Drug X", "(N=134)", "117 (87.3%)",
      "415", "", "75 (56%)", "53 (39.6%)", "45 (33.6%)", "", "83 (61.9%)",
      "52 (38.8%)", "50 (37.3%)", "", "82 (61.2%)", "52 (38.8%)", "51 (38.1%)",
      "B: Placebo", "(N=134)", "116 (86.6%)", "414", "", "79 (59%)",
      "50 (37.3%)", "54 (40.3%)", "", "74 (55.2%)", "57 (42.5%)", "45 (33.6%)",
      "", "84 (62.7%)", "58 (43.3%)", "50 (37.3%)", "C: Combination",
      "(N=132)", "116 (87.9%)", "460", "", "81 (61.4%)", "56 (42.4%)",
      "48 (36.4%)", "", "88 (66.7%)", "59 (44.7%)", "55 (41.7%)", "",
      "89 (67.4%)", "60 (45.5%)", "56 (42.4%)", "All Patients", "(N=400)",
      "349 (87.2%)", "1289", "", "235 (58.8%)", "159 (39.8%)", "147 (36.8%)",
      "", "245 (61.3%)", "168 (42%)", "150 (37.5%)", "", "255 (63.7%)",
      "170 (42.5%)", "157 (39.2%)"
    ),
    .Dim = c(16L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})

test_that("CMT01 variant 4 (Concomitant medications) is produced correctly", {
  adsl <- radsl(cached = TRUE)

  n_per_arm <- table(adsl$ARM)
  n_per_arm <- c(n_per_arm, "All Patients" = sum(n_per_arm))

  adcm <- radcm(cached = TRUE)
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
      .labels = c("Total number of patients with at least one treatment (%)", "Total number of treatments")
    ) %>%
    count_occurrences(var = "CMDECOD") %>%
    build_table(adcm_c, col_counts = n_per_arm) %>%
    prune_table() %>%
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
      "A: Drug X", "(N=134)", "117 (87.3%)", "415", "", "82 (61.2%)",
      "140", "52 (38.8%)", "51 (38.1%)", "", "83 (61.9%)", "141", "52 (38.8%)",
      "50 (37.3%)", "", "75 (56%)", "134", "53 (39.6%)", "45 (33.6%)",
      "B: Placebo", "(N=134)", "116 (86.6%)", "414", "", "84 (62.7%)",
      "140", "58 (43.3%)", "50 (37.3%)", "", "74 (55.2%)", "137", "57 (42.5%)",
      "45 (33.6%)", "", "79 (59%)", "137", "50 (37.3%)", "54 (40.3%)",
      "C: Combination", "(N=132)", "116 (87.9%)", "460", "", "89 (67.4%)",
      "155", "60 (45.5%)", "56 (42.4%)", "", "88 (66.7%)", "162", "59 (44.7%)",
      "55 (41.7%)", "", "81 (61.4%)", "143", "56 (42.4%)", "48 (36.4%)",
      "All Patients", "(N=400)", "349 (87.2%)", "1289", "", "255 (63.7%)",
      "435", "170 (42.5%)", "157 (39.2%)", "", "245 (61.3%)", "440",
      "168 (42%)", "150 (37.5%)", "", "235 (58.8%)", "414", "159 (39.8%)",
      "147 (36.8%)"
    ),
    .Dim = c(19L, 5L)
  )

  expect_identical(result_matrix, expected_matrix)
})
