# Test all variants of AET02

library(random.cdisc.data)
library(magrittr)


test_that("AET02 variant 1 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All Patients") %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )) %>%

    split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE, indent_mod = -1L) %>%

    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L)

  result <- build_table(lyt, adae, col_count = c(table(adsl$ARM), sum(table(adsl$ARM)))) %>%
    prune_table()

  result <- result %>%
    prune_table() %>%
    sort_at_path(path =  c("AEBODSYS"), scorefun = cont_n_onecol(4)) %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)


  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("",                                                        "",
      "Total number of patients with at least one adverse event",
      "Overall total number of events",
      "cl A.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd A.1.1.1.1",
      "dcd A.1.1.1.2",
      "cl B.2",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd B.2.2.3.1",
      "dcd B.2.1.2.1",
      "cl D.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd D.1.1.1.1",
      "dcd D.1.1.4.2",
      "cl D.2",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd D.2.1.5.3",
      "cl B.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd B.1.1.1.1",
      "cl C.2",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd C.2.1.2.1",
      "cl C.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd C.1.1.1.3",                                           "A: Drug X",
      "(N=134)",                                                 "122 (91%)",
      "609",                                                     "",
      "78 (58.2%)",                                              "132",
      "50 (37.3%)",                                              "48 (35.8%)",
      "",                                                        "79 (59%)",
      "129",                                                     "48 (35.8%)",
      "49 (36.6%)",                                              "",
      "79 (59%)",                                                "127",
      "50 (37.3%)",                                              "48 (35.8%)",
      "",                                                        "47 (35.1%)",
      "62",                                                      "47 (35.1%)",
      "",                                                        "47 (35.1%)",
      "56",                                                      "47 (35.1%)",
      "",                                                        "35 (26.1%)",
      "48",                                                      "35 (26.1%)",
      "",                                                        "43 (32.1%)",
      "55",                                                      "43 (32.1%)",
      "B: Placebo",                                              "(N=134)",
      "123 (91.8%)",                                             "622",
      "",                                                        "75 (56%)",
      "130",                                                     "45 (33.6%)",
      "48 (35.8%)",                                              "",
      "74 (55.2%)",                                              "138",
      "54 (40.3%)",                                              "44 (32.8%)",
      "",                                                        "67 (50%)",
      "106",                                                     "42 (31.3%)",
      "42 (31.3%)",                                              "",
      "58 (43.3%)",                                              "72",
      "58 (43.3%)",                                              "",
      "49 (36.6%)",                                              "60",
      "49 (36.6%)",                                              "",
      "48 (35.8%)",                                              "53",
      "48 (35.8%)",                                              "",
      "46 (34.3%)",                                              "63",
      "46 (34.3%)",                                              "C: Combination",
      "(N=132)",                                                 "120 (90.9%)",
      "703",                                                     "",
      "89 (67.4%)",                                              "160",
      "63 (47.7%)",                                              "50 (37.9%)",
      "",                                                        "85 (64.4%)",
      "143",                                                     "51 (38.6%)",
      "52 (39.4%)",                                              "",
      "80 (60.6%)",                                              "135",
      "51 (38.6%)",                                              "50 (37.9%)",
      "",                                                        "57 (43.2%)",
      "74",                                                      "57 (43.2%)",
      "",                                                        "43 (32.6%)",
      "62",                                                      "43 (32.6%)",
      "",                                                        "55 (41.7%)",
      "65",                                                      "55 (41.7%)",
      "",                                                        "43 (32.6%)",
      "64",                                                      "43 (32.6%)",
      "All Patients",                                            "(N=400)",
      "365 (91.2%)",                                             "1934",
      "",                                                        "242 (60.5%)",
      "422",                                                     "158 (39.5%)",
      "146 (36.5%)",                                             "",
      "238 (59.5%)",                                             "410",
      "153 (38.2%)",                                             "145 (36.2%)",
      "",                                                        "226 (56.5%)",
      "368",                                                     "143 (35.8%)",
      "140 (35%)",                                               "",
      "162 (40.5%)",                                             "208",
      "162 (40.5%)",                                             "",
      "139 (34.8%)",                                             "178",
      "139 (34.8%)",                                             "",
      "138 (34.5%)",                                             "166",
      "138 (34.5%)",                                             "",
      "132 (33%)",                                               "182",
      "132 (33%)"
    ),
    .Dim = c(35L, 5L)
  )
  expect_identical(result_matrix, expected_matrix)

})

test_that("AET02 variant 2 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)


  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    add_overall_col(label = "All Patients") %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )) %>%

    split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE, indent_mod = -1L) %>%

    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event"
      )) %>%

    count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%

    count_values(
      "STUDYID",
      values = "AB12345",
      .stats = "count",
      .labels = c(count = "Total number of events"),
      .indent_mods = c(count = -1L)
    )


  result <- build_table(lyt, adae, col_count = c(table(adsl$ARM), sum(table(adsl$ARM)))) %>%
    prune_table()

  result <- result %>%
    prune_table() %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences) %>%
    sort_at_path(path =  c("AEBODSYS"), scorefun = cont_n_onecol(4))


  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("",
      "",
      "Total number of patients with at least one adverse event",
      "Overall total number of events",
      "cl A.1",
      "Total number of patients with at least one adverse event",
      "dcd A.1.1.1.1",
      "dcd A.1.1.1.2",
      "Total number of events",
      "cl B.2",
      "Total number of patients with at least one adverse event",
      "dcd B.2.2.3.1",
      "dcd B.2.1.2.1",
      "Total number of events",
      "cl D.1",
      "Total number of patients with at least one adverse event",
      "dcd D.1.1.1.1",
      "dcd D.1.1.4.2",
      "Total number of events",
      "cl D.2",
      "Total number of patients with at least one adverse event",
      "dcd D.2.1.5.3",
      "Total number of events",
      "cl B.1",
      "Total number of patients with at least one adverse event",
      "dcd B.1.1.1.1",
      "Total number of events",
      "cl C.2",
      "Total number of patients with at least one adverse event",
      "dcd C.2.1.2.1",
      "Total number of events",
      "cl C.1",
      "Total number of patients with at least one adverse event",
      "dcd C.1.1.1.3",
      "Total number of events",
      "A: Drug X",
      "(N=134)",
      "122 (91%)",
      "609",
      "",
      "78 (58.2%)",
      "50 (37.3%)",
      "48 (35.8%)",
      "132",
      "",
      "79 (59%)",
      "48 (35.8%)",
      "49 (36.6%)",
      "129",
      "",
      "79 (59%)",
      "50 (37.3%)",
      "48 (35.8%)",
      "127",
      "",
      "47 (35.1%)",
      "47 (35.1%)",
      "62",
      "",
      "47 (35.1%)",
      "47 (35.1%)",
      "56",
      "",
      "35 (26.1%)",
      "35 (26.1%)",
      "48",
      "",
      "43 (32.1%)",
      "43 (32.1%)",
      "55",
      "B: Placebo",
      "(N=134)",
      "123 (91.8%)",
      "622",
      "",
      "75 (56%)",
      "45 (33.6%)",
      "48 (35.8%)",
      "130",
      "",
      "74 (55.2%)",
      "54 (40.3%)",
      "44 (32.8%)",
      "138",
      "",
      "67 (50%)",
      "42 (31.3%)",
      "42 (31.3%)",
      "106",
      "",
      "58 (43.3%)",
      "58 (43.3%)",
      "72",
      "",
      "49 (36.6%)",
      "49 (36.6%)",
      "60",
      "",
      "48 (35.8%)",
      "48 (35.8%)",
      "53",
      "",
      "46 (34.3%)",
      "46 (34.3%)",
      "63",
      "C: Combination",
      "(N=132)",
      "120 (90.9%)",
      "703",
      "",
      "89 (67.4%)",
      "63 (47.7%)",
      "50 (37.9%)",
      "160",
      "",
      "85 (64.4%)",
      "51 (38.6%)",
      "52 (39.4%)",
      "143",
      "",
      "80 (60.6%)",
      "51 (38.6%)",
      "50 (37.9%)",
      "135",
      "",
      "57 (43.2%)",
      "57 (43.2%)",
      "74",
      "",
      "43 (32.6%)",
      "43 (32.6%)",
      "62",
      "",
      "55 (41.7%)",
      "55 (41.7%)",
      "65",
      "",
      "43 (32.6%)",
      "43 (32.6%)",
      "64",
      "All Patients",
      "(N=400)",
      "365 (91.2%)",
      "1934",
      "",
      "242 (60.5%)",
      "158 (39.5%)",
      "146 (36.5%)",
      "422",
      "",
      "238 (59.5%)",
      "153 (38.2%)",
      "145 (36.2%)",
      "410",
      "",
      "226 (56.5%)",
      "143 (35.8%)",
      "140 (35%)",
      "368",
      "",
      "162 (40.5%)",
      "162 (40.5%)",
      "208",
      "",
      "139 (34.8%)",
      "139 (34.8%)",
      "178",
      "",
      "138 (34.5%)",
      "138 (34.5%)",
      "166",
      "",
      "132 (33%)",
      "132 (33%)",
      "182"
    ),
    .Dim = c(35L, 5L)
  )
  expect_identical(result_matrix, expected_matrix)

})

test_that("AET02 variant 3 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%

    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )) %>%

    split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE)  %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )) %>%
    split_rows_by("AEHLT", child_labels = "visible", nested = TRUE, indent_mod = -1L)  %>%

    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = -1L))


  result <- build_table(lyt, adae, col_count = table(adsl$ARM)) %>%
    prune_table()

  result <- result %>%
    sort_at_path(path =  c("AEBODSYS"), scorefun = cont_n_allcols) %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEHLT"), scorefun = cont_n_allcols) %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEHLT", "*", "AEDECOD"), scorefun = score_occurrences)



  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("",
      "",
      "Total number of patients with at least one adverse event",
      "Overall total number of events",
      "cl A.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "hlt A.1.1.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd A.1.1.1.1",
      "dcd A.1.1.1.2",
      "cl B.2",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "hlt B.2.2.3",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd B.2.2.3.1",
      "hlt B.2.1.2",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd B.2.1.2.1",
      "cl D.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "hlt D.1.1.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd D.1.1.1.1",
      "hlt D.1.1.4",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd D.1.1.4.2",
      "cl D.2",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "hlt D.2.1.5",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd D.2.1.5.3",
      "cl B.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "hlt B.1.1.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd B.1.1.1.1",
      "cl C.2",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "hlt C.2.1.2",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd C.2.1.2.1",
      "cl C.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "hlt C.1.1.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd C.1.1.1.3",
      "A: Drug X",
      "(N=134)",
      "122 (91%)",
      "609",
      "",
      "78 (58.2%)",
      "132",
      "",
      "78 (58.2%)",
      "132",
      "50 (37.3%)",
      "48 (35.8%)",
      "",
      "79 (59%)",
      "129",
      "",
      "48 (35.8%)",
      "64",
      "48 (35.8%)",
      "",
      "49 (36.6%)",
      "65",
      "49 (36.6%)",
      "",
      "79 (59%)",
      "127",
      "",
      "50 (37.3%)",
      "61",
      "50 (37.3%)",
      "",
      "48 (35.8%)",
      "66",
      "48 (35.8%)",
      "",
      "47 (35.1%)",
      "62",
      "",
      "47 (35.1%)",
      "62",
      "47 (35.1%)",
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
      "123 (91.8%)",
      "622",
      "",
      "75 (56%)",
      "130",
      "",
      "75 (56%)",
      "130",
      "45 (33.6%)",
      "48 (35.8%)",
      "",
      "74 (55.2%)",
      "138",
      "",
      "54 (40.3%)",
      "76",
      "54 (40.3%)",
      "",
      "44 (32.8%)",
      "62",
      "44 (32.8%)",
      "",
      "67 (50%)",
      "106",
      "",
      "42 (31.3%)",
      "51",
      "42 (31.3%)",
      "",
      "42 (31.3%)",
      "55",
      "42 (31.3%)",
      "",
      "58 (43.3%)",
      "72",
      "",
      "58 (43.3%)",
      "72",
      "58 (43.3%)",
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
      "120 (90.9%)",
      "703",
      "",
      "89 (67.4%)",
      "160",
      "",
      "89 (67.4%)",
      "160",
      "63 (47.7%)",
      "50 (37.9%)",
      "",
      "85 (64.4%)",
      "143",
      "",
      "51 (38.6%)",
      "77",
      "51 (38.6%)",
      "",
      "52 (39.4%)",
      "66",
      "52 (39.4%)",
      "",
      "80 (60.6%)",
      "135",
      "",
      "51 (38.6%)",
      "71",
      "51 (38.6%)",
      "",
      "50 (37.9%)",
      "64",
      "50 (37.9%)",
      "",
      "57 (43.2%)",
      "74",
      "",
      "57 (43.2%)",
      "74",
      "57 (43.2%)",
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
    .Dim = c(62L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)

})

test_that("AET02 variant 4 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = -1L))

  result <- build_table(lyt, adae, col_count = table(adsl$ARM))

  result <- result %>%
    sort_at_path(path =  c("AEDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("",                                                        "",
      "Total number of patients with at least one adverse event", "Total number of events",
      "dcd D.2.1.5.3",                                           "dcd A.1.1.1.1",
      "dcd B.2.2.3.1",                                           "dcd A.1.1.1.2",
      "dcd B.2.1.2.1",                                           "dcd D.1.1.1.1",
      "dcd D.1.1.4.2",                                           "dcd B.1.1.1.1",
      "dcd C.2.1.2.1",                                           "dcd C.1.1.1.3",
      "A: Drug X",                                               "(N=134)",
      "122 (91%)",                                            "609",
      "47 (35.1%)",                                              "50 (37.3%)",
      "48 (35.8%)",                                              "48 (35.8%)",
      "49 (36.6%)",                                              "50 (37.3%)",
      "48 (35.8%)",                                              "47 (35.1%)",
      "35 (26.1%)",                                              "43 (32.1%)",
      "B: Placebo",                                              "(N=134)",
      "123 (91.8%)",                                            "622",
      "58 (43.3%)",                                              "45 (33.6%)",
      "54 (40.3%)",                                              "48 (35.8%)",
      "44 (32.8%)",                                              "42 (31.3%)",
      "42 (31.3%)",                                              "49 (36.6%)",
      "48 (35.8%)",                                              "46 (34.3%)",
      "C: Combination",                                          "(N=132)",
      "120 (90.9%)",                                            "703",
      "57 (43.2%)",                                              "63 (47.7%)",
      "51 (38.6%)",                                              "50 (37.9%)",
      "52 (39.4%)",                                              "51 (38.6%)",
      "50 (37.9%)",                                              "43 (32.6%)",
      "55 (41.7%)",                                              "43 (32.6%)"
    ),
    .Dim = c(14L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("AET02 variant 5 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)
  adae_5 <- adae %>% dplyr::filter(ARM != "C: Combination")


  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )) %>%

    split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE, indent_mod = -1L) %>%

    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = -1L))

  result <- build_table(lyt, adae_5, col_count = table(adsl$ARM)) %>%
    prune_table()

  result <- result %>%
    sort_at_path(path =  c("AEBODSYS"), scorefun = cont_n_allcols) %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("",
      "",
      "Total number of patients with at least one adverse event",
      "Overall total number of events",
      "cl A.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd A.1.1.1.2",
      "dcd A.1.1.1.1",
      "cl B.2",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd B.2.2.3.1",
      "dcd B.2.1.2.1",
      "cl D.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd D.1.1.1.1",
      "dcd D.1.1.4.2",
      "cl D.2",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd D.2.1.5.3",
      "cl B.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd B.1.1.1.1",
      "cl C.1",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd C.1.1.1.3",
      "cl C.2",
      "Total number of patients with at least one adverse event",
      "Total number of events",
      "dcd C.2.1.2.1",
      "A: Drug X",
      "(N=134)",
      "122 (91%)",
      "609",
      "",
      "78 (58.2%)",
      "132",
      "48 (35.8%)",
      "50 (37.3%)",
      "",
      "79 (59%)",
      "129",
      "48 (35.8%)",
      "49 (36.6%)",
      "",
      "79 (59%)",
      "127",
      "50 (37.3%)",
      "48 (35.8%)",
      "",
      "47 (35.1%)",
      "62",
      "47 (35.1%)",
      "",
      "47 (35.1%)",
      "56",
      "47 (35.1%)",
      "",
      "43 (32.1%)",
      "55",
      "43 (32.1%)",
      "",
      "35 (26.1%)",
      "48",
      "35 (26.1%)",
      "B: Placebo",
      "(N=134)",
      "123 (91.8%)",
      "622",
      "",
      "75 (56%)",
      "130",
      "48 (35.8%)",
      "45 (33.6%)",
      "",
      "74 (55.2%)",
      "138",
      "54 (40.3%)",
      "44 (32.8%)",
      "",
      "67 (50%)",
      "106",
      "42 (31.3%)",
      "42 (31.3%)",
      "",
      "58 (43.3%)",
      "72",
      "58 (43.3%)",
      "",
      "49 (36.6%)",
      "60",
      "49 (36.6%)",
      "",
      "46 (34.3%)",
      "63",
      "46 (34.3%)",
      "",
      "48 (35.8%)",
      "53",
      "48 (35.8%)",
      "C: Combination",
      "(N=132)",
      "0",
      "0",
      "",
      "0",
      "0",
      "0",
      "0",
      "",
      "0",
      "0",
      "0",
      "0",
      "",
      "0",
      "0",
      "0",
      "0",
      "",
      "0",
      "0",
      "0",
      "",
      "0",
      "0",
      "0",
      "",
      "0",
      "0",
      "0",
      "",
      "0",
      "0",
      "0"
    ),
    .Dim = c(35L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("AET02 variant 6 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = FALSE, indent_mod = -1L)  %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L))


  result <- build_table(lyt, adae, col_count = table(adsl$ARM)) %>% prune_table()

  score_subtable_all <- score_occurrences_subtable(col_names = names(result))

  result <- result %>%
    sort_at_path(path =  c("AEBODSYS"), scorefun = score_subtable_all) %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  row_condition <- has_fraction_in_any_col(atleast = 0.40, col_names = names(table(adsl$ARM)))

  result <- prune_table(result, keep_rows(row_condition))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("",              "",              "cl A.1",        "dcd A.1.1.1.1",
      "cl B.2",        "dcd B.2.2.3.1", "cl D.2",        "dcd D.2.1.5.3",
      "cl C.2",        "dcd C.2.1.2.1", "A: Drug X",     "(N=134)",
      "",              "50 (37.3%)",    "",              "48 (35.8%)",
      "",              "47 (35.1%)",    "",              "35 (26.1%)",
      "B: Placebo",    "(N=134)",       "",              "45 (33.6%)",
      "",              "54 (40.3%)",    "",              "58 (43.3%)",
      "",              "48 (35.8%)",    "C: Combination", "(N=132)",
      "",              "63 (47.7%)",    "",              "51 (38.6%)",
      "",              "57 (43.2%)",    "",              "55 (41.7%)"
    ),
    .Dim = c(10L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("AET02 variant 7 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = TRUE, indent_mod = -2L)  %>%
    split_rows_by("AEHLT", child_labels = "visible", nested = TRUE, indent_mod = 1L)  %>%
    count_occurrences(vars = "AEDECOD")

  result <- build_table(lyt, adae, col_count = table(adsl$ARM)) %>% prune_table()

  score_subtable_all <- score_occurrences_subtable(col_names = names(result))

  result <- result %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = score_subtable_all, decreasing = TRUE) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEHLT"), scorefun = score_subtable_all, decreasing = TRUE) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEHLT", "*", "AEDECOD"), scorefun = score_occurrences, decreasing = TRUE)

  row_condition <- has_fraction_in_any_col(atleast = 0.40, col_names = names(table(adsl$ARM)))

  result <- prune_table(result, keep_rows(row_condition))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("",              "",              "cl A.1",        "hlt A.1.1.1",
      "dcd A.1.1.1.1", "cl B.2",        "hlt B.2.2.3",   "dcd B.2.2.3.1",
      "cl D.2",        "hlt D.2.1.5",   "dcd D.2.1.5.3", "cl C.2",
      "hlt C.2.1.2",   "dcd C.2.1.2.1", "A: Drug X",     "(N=134)",
      "",              "",              "50 (37.3%)",    "",
      "",              "48 (35.8%)",    "",              "",
      "47 (35.1%)",    "",              "",              "35 (26.1%)",
      "B: Placebo",    "(N=134)",       "",              "",
      "45 (33.6%)",    "",              "",              "54 (40.3%)",
      "",              "",              "58 (43.3%)",    "",
      "",              "48 (35.8%)",    "C: Combination", "(N=132)",
      "",              "",              "63 (47.7%)",    "",
      "",              "51 (38.6%)",    "",              "",
      "57 (43.2%)",    "",              "",              "55 (41.7%)"
    ),
    .Dim = c(14L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("AET02 variant 8 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = TRUE, indent_mod = -1L)  %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L))


  result <- build_table(lyt, adae, col_count = table(adsl$ARM)) %>% prune_table()

  score_subtable_all <- score_occurrences_subtable(col_names = names(result))

  result <- result %>%
    sort_at_path(path =  c("AEBODSYS"), scorefun = score_subtable_all) %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  row_condition <- has_fraction_in_any_col(atleast = 0.40, col_names = names(table(adsl$ARM)))

  result <- prune_table(result, keep_rows(row_condition))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("",              "",              "cl A.1",        "dcd A.1.1.1.1",
      "cl B.2",        "dcd B.2.2.3.1", "cl D.2",        "dcd D.2.1.5.3",
      "cl C.2",        "dcd C.2.1.2.1", "A: Drug X",     "(N=134)",
      "",              "50 (37.3%)",    "",              "48 (35.8%)",
      "",              "47 (35.1%)",    "",              "35 (26.1%)",
      "B: Placebo",    "(N=134)",       "",              "45 (33.6%)",
      "",              "54 (40.3%)",    "",              "58 (43.3%)",
      "",              "48 (35.8%)",    "C: Combination", "(N=132)",
      "",              "63 (47.7%)",    "",              "51 (38.6%)",
      "",              "57 (43.2%)",    "",              "55 (41.7%)"
    ),
    .Dim = c(10L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("AET02 variant 9 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = TRUE, indent_mod = -1L)  %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L))

  result <- build_table(lyt, adae, col_count = table(adsl$ARM)) %>% prune_table()

  score_subtable_all <- score_occurrences_subtable(col_names = names(result))

  result <- result %>%
    sort_at_path(path =  c("AEBODSYS"), scorefun = score_subtable_all) %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  row_condition <- has_count_in_any_col(atleast = 52, col_names = names(table(adsl$ARM)))

  result <- prune_table(result, keep_rows(row_condition))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("",              "",              "cl A.1",
      "dcd A.1.1.1.1", "cl B.2",        "dcd B.2.2.3.1", "dcd B.2.1.2.1", "cl D.2",
      "dcd D.2.1.5.3", "cl C.2",        "dcd C.2.1.2.1",
      "A: Drug X",     "(N=134)",       "",              "50 (37.3%)",    "",
      "48 (35.8%)",    "49 (36.6%)",    "",
      "47 (35.1%)",    "",              "35 (26.1%)",    "B: Placebo",    "(N=134)",
      "",              "45 (33.6%)",    "",
      "54 (40.3%)",    "44 (32.8%)",    "",              "58 (43.3%)",    "",
      "48 (35.8%)",    "C: Combination", "(N=132)",
      "",              "63 (47.7%)",    "",              "51 (38.6%)",    "52 (39.4%)",
      "",              "57 (43.2%)",    "",
      "55 (41.7%)"
    ),
    .Dim = c(11, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("AET02 variant 10 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = TRUE, indent_mod = -1L)  %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L))


  result <- build_table(lyt, adae, col_count = table(adsl$ARM)) %>% prune_table()

  score_subtable_all <- score_occurrences_subtable(col_names = names(result))

  result <- result %>%
    sort_at_path(path =  c("AEBODSYS"), scorefun = score_subtable_all) %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)


  row_condition <- has_fractions_difference(atleast = 0.05, col_names = names(table(adsl$ARM)))

  result <- prune_table(result, keep_rows(row_condition))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("",              "",              "cl A.1",
      "dcd A.1.1.1.1", "cl B.2",        "dcd B.2.1.2.1", "cl D.1",        "dcd D.1.1.1.1",
      "dcd D.1.1.4.2", "cl D.2",        "dcd D.2.1.5.3",
      "cl C.2",        "dcd C.2.1.2.1", "A: Drug X",     "(N=134)",       "",
      "50 (37.3%)",    "",              "49 (36.6%)",
      "",              "50 (37.3%)",    "48 (35.8%)",    "",              "47 (35.1%)",
      "",              "35 (26.1%)",    "B: Placebo",
      "(N=134)",       "",              "45 (33.6%)",    "",              "44 (32.8%)",
      "",              "42 (31.3%)",    "42 (31.3%)",
      "",              "58 (43.3%)",    "",              "48 (35.8%)",    "C: Combination",
      "(N=132)",       "",              "63 (47.7%)",
      "",              "52 (39.4%)",    "",              "51 (38.6%)",    "50 (37.9%)",
      "",              "57 (43.2%)",    "",
      "55 (41.7%)"
    ),
    .Dim = c(13L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("AET02 variant 11 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = TRUE, indent_mod = -1L)  %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L))

  result <- build_table(lyt, adae, col_count = table(adsl$ARM)) %>% prune_table()

  score_subtable_all <- score_occurrences_subtable(col_names = names(result))

  result <- result %>%
    sort_at_path(path =  c("AEBODSYS"), scorefun = score_subtable_all) %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  row_condition <- has_fraction_in_cols(atleast = 0.40, col_names = c("B: Placebo"))

  result <- prune_table(result, keep_rows(row_condition))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("",              "",              "cl B.2",        "dcd B.2.2.3.1",
      "cl D.2",        "dcd D.2.1.5.3", "A: Drug X",     "(N=134)",
      "",              "48 (35.8%)",    "",              "47 (35.1%)",
      "B: Placebo",    "(N=134)",       "",              "54 (40.3%)",
      "",              "58 (43.3%)",    "C: Combination", "(N=132)",
      "",              "51 (38.6%)",    "",              "57 (43.2%)"
    ),
    .Dim = c(6L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("AET02 variant 12 is produced correctly", {
  adsl <- radsl(cached = TRUE)
  adae <- radae(cached = TRUE)

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    add_colcounts() %>%
    split_rows_by("AEBODSYS", child_labels = "visible", nested = TRUE, indent_mod = -1L)  %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = c(count_fraction = 1L))

  result <- build_table(lyt, adae, col_count = table(adsl$ARM)) %>% prune_table()

  score_subtable_all <- score_occurrences_subtable(col_names = names(result))

  result <- result %>%
    sort_at_path(path =  c("AEBODSYS"), scorefun = score_subtable_all) %>%
    sort_at_path(path =  c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)


  row_condition1 <- has_fractions_difference(atleast = 0.05, col_names = c("A: Drug X", "B: Placebo"))
  row_condition2 <- has_fractions_difference(atleast = 0.05, col_names = c("A: Drug X", "C: Combination"))
  row_condition <- row_condition1 | row_condition1

  result <- prune_table(result, keep_rows(row_condition))

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("",              "",              "cl D.1",        "dcd D.1.1.1.1",
      "cl D.2",        "dcd D.2.1.5.3", "cl C.2",        "dcd C.2.1.2.1",
      "A: Drug X",     "(N=134)",       "",              "50 (37.3%)",
      "",              "47 (35.1%)",    "",              "35 (26.1%)",
      "B: Placebo",    "(N=134)",       "",              "42 (31.3%)",
      "",              "58 (43.3%)",    "",              "48 (35.8%)",
      "C: Combination", "(N=132)",       "",              "51 (38.6%)",
      "",              "57 (43.2%)",    "",              "55 (41.7%)"
    ),
    .Dim = c(8L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
