library(scda)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae

testthat::test_that("AET03 variant 1 is produced correctly", {
  adae$AEDECOD <- as.character(adae$AEDECOD) # nolint
  adae$AEBODSYS <- as.character(adae$AEBODSYS) # nolint
  adae$ASEV <- as.character(adae$AESEV) # nolint
  adae$ASEV[1:15] <- "LIFE THREATENING"
  adae$ASEV <- factor(adae$ASEV, levels = c("MILD", "MODERATE", "SEVERE", "LIFE THREATENING")) # nolint

  n_per_arm <- table(adsl$ACTARM)

  gr_grp <- list(
    "- Any Intensity -" = c("MILD", "MODERATE", "SEVERE", "LIFE THREATENING")
  )

  lyt <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    add_colcounts() %>%
    summarize_occurrences_by_grade(
      var = "ASEV",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEBODSYS",
      split_fun = trim_levels_in_group("ASEV"),
      child_labels = "visible", nested = TRUE, indent_mod = -1L
    ) %>%
    summarize_occurrences_by_grade(
      var = "ASEV",
      grade_groups = gr_grp
    ) %>%
    split_rows_by("AEDECOD",
      split_fun = trim_levels_in_group("ASEV"),
      child_labels = "visible", nested = TRUE, indent_mod = -1L
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = c("- Any Intensity -")
    ) %>%
    count_occurrences_by_grade(var = "ASEV", .indent_mods = -1L)

  result <- lyt %>%
    build_table(adae, alt_counts_df = adsl) %>%
    sort_at_path(
      path = "AEBODSYS",
      scorefun = cont_n_allcols,
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = cont_n_allcols,
      decreasing = TRUE
    )

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "", "- Any Intensity -", "MILD", "MODERATE",
      "SEVERE", "LIFE THREATENING", "cl A.1", "- Any Intensity -",
      "MILD", "MODERATE", "LIFE THREATENING", "dcd A.1.1.1.1", "- Any Intensity -",
      "MILD", "LIFE THREATENING", "dcd A.1.1.1.2", "- Any Intensity -",
      "MODERATE", "LIFE THREATENING", "cl B.2", "- Any Intensity -",
      "MILD", "MODERATE", "LIFE THREATENING", "dcd B.2.2.3.1", "- Any Intensity -",
      "MILD", "dcd B.2.1.2.1", "- Any Intensity -", "MODERATE", "LIFE THREATENING",
      "cl D.1", "- Any Intensity -", "MODERATE", "SEVERE", "LIFE THREATENING",
      "dcd D.1.1.1.1", "- Any Intensity -", "SEVERE", "LIFE THREATENING",
      "dcd D.1.1.4.2", "- Any Intensity -", "MODERATE", "LIFE THREATENING",
      "cl D.2", "- Any Intensity -", "MILD", "LIFE THREATENING", "dcd D.2.1.5.3",
      "- Any Intensity -", "MILD", "LIFE THREATENING", "cl B.1", "- Any Intensity -",
      "SEVERE", "LIFE THREATENING", "dcd B.1.1.1.1", "- Any Intensity -",
      "SEVERE", "LIFE THREATENING", "cl C.2", "- Any Intensity -",
      "MODERATE", "LIFE THREATENING", "dcd C.2.1.2.1", "- Any Intensity -",
      "MODERATE", "LIFE THREATENING", "cl C.1", "- Any Intensity -",
      "SEVERE", "dcd C.1.1.1.3", "- Any Intensity -", "SEVERE", "A: Drug X",
      "(N=134)", "122 (91%)", "7 (5.2%)", "23 (17.2%)", "91 (67.9%)",
      "1 (0.7%)", "", "78 (58.2%)", "30 (22.4%)", "47 (35.1%)", "1 (0.7%)",
      "", "50 (37.3%)", "50 (37.3%)", "0", "", "48 (35.8%)", "47 (35.1%)",
      "1 (0.7%)", "", "79 (59%)", "30 (22.4%)", "48 (35.8%)", "1 (0.7%)",
      "", "48 (35.8%)", "48 (35.8%)", "", "49 (36.6%)", "48 (35.8%)",
      "1 (0.7%)", "", "79 (59%)", "28 (20.9%)", "50 (37.3%)", "1 (0.7%)",
      "", "50 (37.3%)", "50 (37.3%)", "0", "", "48 (35.8%)", "47 (35.1%)",
      "1 (0.7%)", "", "47 (35.1%)", "47 (35.1%)", "0", "", "47 (35.1%)",
      "47 (35.1%)", "0", "", "47 (35.1%)", "47 (35.1%)", "0",
      "", "47 (35.1%)", "47 (35.1%)", "0", "", "35 (26.1%)", "35 (26.1%)",
      "0", "", "35 (26.1%)", "35 (26.1%)", "0", "", "43 (32.1%)",
      "43 (32.1%)", "", "43 (32.1%)", "43 (32.1%)", "B: Placebo", "(N=134)",
      "123 (91.8%)", "9 (6.7%)", "24 (17.9%)", "89 (66.4%)", "1 (0.7%)",
      "", "75 (56%)", "27 (20.1%)", "48 (35.8%)", "0", "", "45 (33.6%)",
      "45 (33.6%)", "0", "", "48 (35.8%)", "48 (35.8%)", "0",
      "", "74 (55.2%)", "30 (22.4%)", "44 (32.8%)", "0", "", "54 (40.3%)",
      "54 (40.3%)", "", "44 (32.8%)", "44 (32.8%)", "0", "", "67 (50%)",
      "25 (18.7%)", "42 (31.3%)", "0", "", "42 (31.3%)", "42 (31.3%)",
      "0", "", "42 (31.3%)", "42 (31.3%)", "0", "", "58 (43.3%)",
      "58 (43.3%)", "0", "", "58 (43.3%)", "58 (43.3%)", "0",
      "", "49 (36.6%)", "48 (35.8%)", "1 (0.7%)", "", "49 (36.6%)",
      "48 (35.8%)", "1 (0.7%)", "", "48 (35.8%)", "48 (35.8%)", "0",
      "", "48 (35.8%)", "48 (35.8%)", "0", "", "46 (34.3%)", "46 (34.3%)",
      "", "46 (34.3%)", "46 (34.3%)", "C: Combination", "(N=132)",
      "120 (90.9%)", "4 (3%)", "23 (17.4%)", "91 (68.9%)", "2 (1.5%)",
      "", "89 (67.4%)", "39 (29.5%)", "49 (37.1%)", "1 (0.8%)", "",
      "63 (47.7%)", "62 (47%)", "1 (0.8%)", "", "50 (37.9%)", "49 (37.1%)",
      "1 (0.8%)", "", "85 (64.4%)", "33 (25%)", "51 (38.6%)", "1 (0.8%)",
      "", "51 (38.6%)", "51 (38.6%)", "", "52 (39.4%)", "51 (38.6%)",
      "1 (0.8%)", "", "80 (60.6%)", "29 (22%)", "49 (37.1%)", "2 (1.5%)",
      "", "51 (38.6%)", "50 (37.9%)", "1 (0.8%)", "", "50 (37.9%)",
      "49 (37.1%)", "1 (0.8%)", "", "57 (43.2%)", "56 (42.4%)", "1 (0.8%)",
      "", "57 (43.2%)", "56 (42.4%)", "1 (0.8%)", "", "43 (32.6%)",
      "43 (32.6%)", "0", "", "43 (32.6%)", "43 (32.6%)", "0",
      "", "55 (41.7%)", "54 (40.9%)", "1 (0.8%)", "", "55 (41.7%)",
      "54 (40.9%)", "1 (0.8%)", "", "43 (32.6%)", "43 (32.6%)", "",
      "43 (32.6%)", "43 (32.6%)"
    ),
    .Dim = c(75L, 4L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
