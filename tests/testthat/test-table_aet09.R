library(scda)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae


testthat::test_that("AET09 variant 1 is produced correctly, AE related to study drug", {
  adae_r <- adae[adae$AEREL == "Y", ]

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All Patients") %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Overall total number of events related to study drug"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Total number of events related to study drug"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L)

  result <- build_table(lyt, adae_r, alt_counts_df = adsl)

  result <- result %>%
    prune_table() %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_onecol(4)) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)


  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "",
      "",
      "Total number of patients with at least one adverse event related to study drug",
      "Overall total number of events related to study drug",
      "cl D.2",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd D.2.1.5.3",
      "cl D.1",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd D.1.1.1.1",
      "cl B.1",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd B.1.1.1.1",
      "cl C.2",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd C.2.1.2.1",
      "cl C.1",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd C.1.1.1.3",
      "A: Drug X",
      "(N=134)",
      "105 (78.4%)",
      "282",
      "",
      "47 (35.1%)",
      "62",
      "47 (35.1%)",
      "",
      "50 (37.3%)",
      "61",
      "50 (37.3%)",
      "",
      "47 (35.1%)",
      "56",
      "47 (35.1%)",
      "",
      "35 (26.1%)",
      "48",
      "35 (26.1%)",
      "",
      "43 (32.1%)",
      "55",
      "43 (32.1%)",
      "B: Placebo",
      "(N=134)",
      "108 (80.6%)",
      "299",
      "",
      "58 (43.3%)",
      "72",
      "58 (43.3%)",
      "",
      "42 (31.3%)",
      "51",
      "42 (31.3%)",
      "",
      "49 (36.6%)",
      "60",
      "49 (36.6%)",
      "",
      "48 (35.8%)",
      "53",
      "48 (35.8%)",
      "",
      "46 (34.3%)",
      "63",
      "46 (34.3%)",
      "C: Combination",
      "(N=132)",
      "109 (82.6%)",
      "336",
      "",
      "57 (43.2%)",
      "74",
      "57 (43.2%)",
      "",
      "51 (38.6%)",
      "71",
      "51 (38.6%)",
      "",
      "43 (32.6%)",
      "62",
      "43 (32.6%)",
      "",
      "55 (41.7%)",
      "65",
      "55 (41.7%)",
      "",
      "43 (32.6%)",
      "64",
      "43 (32.6%)",
      "All Patients",
      "(N=400)",
      "322 (80.5%)",
      "917",
      "",
      "162 (40.5%)",
      "208",
      "162 (40.5%)",
      "",
      "143 (35.8%)",
      "183",
      "143 (35.8%)",
      "",
      "139 (34.8%)",
      "178",
      "139 (34.8%)",
      "",
      "138 (34.5%)",
      "166",
      "138 (34.5%)",
      "",
      "132 (33%)",
      "182",
      "132 (33%)"
    ),
    .Dim = c(24L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("AET09 variant 2 is produced correctly, AE related to study drug (including high-level terms)", {
  adae_r <- adae[adae$AEREL == "Y", ]

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Overall total number of events related to study drug"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Total number of events related to study drug"
      )
    ) %>%
    split_rows_by(
      "AEHLT",
      child_labels = "visible",
      nested = TRUE,
      indent_mod = -1L,
      split_fun = drop_split_levels
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Total number of events related to study drug"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = -1L))


  result <- build_table(lyt, adae_r, alt_counts_df = adsl) %>%
    prune_table()

  result <- result %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEHLT"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEHLT", "*", "AEDECOD"), scorefun = score_occurrences)



  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "",
      "",
      "Total number of patients with at least one adverse event related to study drug",
      "Overall total number of events related to study drug",
      "cl D.2",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "hlt D.2.1.5",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd D.2.1.5.3",
      "cl D.1",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "hlt D.1.1.1",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd D.1.1.1.1",
      "cl B.1",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "hlt B.1.1.1",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd B.1.1.1.1",
      "cl C.2",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "hlt C.2.1.2",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd C.2.1.2.1",
      "cl C.1",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "hlt C.1.1.1",
      "Total number of patients with at least one adverse event related to study drug",
      "Total number of events related to study drug",
      "dcd C.1.1.1.3",
      "A: Drug X",
      "(N=134)",
      "105 (78.4%)",
      "282",
      "",
      "47 (35.1%)",
      "62",
      "",
      "47 (35.1%)",
      "62",
      "47 (35.1%)",
      "",
      "50 (37.3%)",
      "61",
      "",
      "50 (37.3%)",
      "61",
      "50 (37.3%)",
      "",
      "47 (35.1%)",
      "56",
      "",
      "47 (35.1%)",
      "56",
      "47 (35.1%)",
      "",
      "35 (26.1%)",
      "48",
      "",
      "35 (26.1%)",
      "48",
      "35 (26.1%)",
      "",
      "43 (32.1%)",
      "55",
      "",
      "43 (32.1%)",
      "55",
      "43 (32.1%)",
      "B: Placebo",
      "(N=134)",
      "108 (80.6%)",
      "299",
      "",
      "58 (43.3%)",
      "72",
      "",
      "58 (43.3%)",
      "72",
      "58 (43.3%)",
      "",
      "42 (31.3%)",
      "51",
      "",
      "42 (31.3%)",
      "51",
      "42 (31.3%)",
      "",
      "49 (36.6%)",
      "60",
      "",
      "49 (36.6%)",
      "60",
      "49 (36.6%)",
      "",
      "48 (35.8%)",
      "53",
      "",
      "48 (35.8%)",
      "53",
      "48 (35.8%)",
      "",
      "46 (34.3%)",
      "63",
      "",
      "46 (34.3%)",
      "63",
      "46 (34.3%)",
      "C: Combination",
      "(N=132)",
      "109 (82.6%)",
      "336",
      "",
      "57 (43.2%)",
      "74",
      "",
      "57 (43.2%)",
      "74",
      "57 (43.2%)",
      "",
      "51 (38.6%)",
      "71",
      "",
      "51 (38.6%)",
      "71",
      "51 (38.6%)",
      "",
      "43 (32.6%)",
      "62",
      "",
      "43 (32.6%)",
      "62",
      "43 (32.6%)",
      "",
      "55 (41.7%)",
      "65",
      "",
      "55 (41.7%)",
      "65",
      "55 (41.7%)",
      "",
      "43 (32.6%)",
      "64",
      "",
      "43 (32.6%)",
      "64",
      "43 (32.6%)"
    ),
    .Dim = c(39L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
