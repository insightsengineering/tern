# Tests all variants of DST01.
library(rtables)
library(dplyr)

get_adsl0 <- function() {
  # This specific ADSL is currently not part of RCD.
  set.seed(1, kind = "Mersenne-Twister")

  # nolint start
  adsl0 <- ex_adsl %>%
    dplyr::mutate(
      COMPSTUD = sample(
        c("Y", "N"),
        size = nrow(ex_adsl),
        replace = TRUE
      ) %>% as.factor(),
      STUDONS = sample(
        c("Alive: On Treatment", "Alive: In Follow-up", NA),
        size = nrow(ex_adsl),
        replace = TRUE
      ) %>% as.factor(),
      STDDRS = sample(
        c(
          "Death", "Lost To Follow-Up",
          "Protocol Violation", "Withdrawal By Subject",
          "Other"
        ),
        size = nrow(ex_adsl),
        replace = TRUE
      ) %>% as.factor(),
      GOTTRT = ifelse(!is.na(ACTARMCD), "Y", "N") %>%
        as.factor(),
      DISTRTFL = sample(
        c("Y", "N"),
        size = nrow(ex_adsl),
        replace = TRUE
      ) %>% as.factor(),
      TRTDRS = sample(
        c(
          "ADVERSE EVENT", "PROGRESSIVE DISEASE",
          "PHYSICIAN DECISION", "LACK OF EFFICACY",
          "OTHER"
        ),
        size = nrow(ex_adsl),
        replace = TRUE
      ) %>% as.factor(),
      STUDONS = dplyr::case_when(COMPSTUD == "N" ~ STUDONS),
      STDDRS = dplyr::case_when(COMPSTUD == "N" & is.na(STUDONS) ~ STDDRS),
      DISSTDFL = dplyr::case_when(!is.na(STDDRS) ~ "Y"),
      DISTRTFL = dplyr::case_when(GOTTRT == "Y" ~ DISTRTFL),
      TRTDRS = dplyr::case_when(DISTRTFL == "Y" ~ TRTDRS),
      DRSCAT = dplyr::case_when(
        TRTDRS %in% c("ADVERSE EVENT", "PHYSICIAN DECISION") ~ "Safety",
        !is.na(TRTDRS) ~ "Other"
      )
    )
  columns <- c("COMPSTUD", "STUDONS", "DISSTDFL", "STDDRS", "GOTTRT", "DISTRTFL", "TRTDRS", "DRSCAT")
  labels <- c(
    "Complete Study",
    "On-study Status",
    "Discontinued Study",
    "Reason for Study \r\nDiscontinuation",
    "Received Treatment",
    "Discontinued Treatment",
    "Reason for Treatment \r\nDiscontinuation",
    "Subcategory for Treatment Discontinuation"
  )
  var_labels(adsl0)[columns] <- labels
  # nolint end
  adsl0
}

testthat::test_that("DST01 default variant is produced correctly", {

  adsl0 <- get_adsl0()

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values("COMPSTUD", values = "Y", .labels = c(count_fraction = "Completed Study")) %>%
    split_rows_by("DISSTDFL", split_fun = keep_split_levels("Y")) %>%
    summarize_row_groups(label_fstr = "Discontinued Study") %>%
    summarize_vars("STDDRS", .stats = "count_fraction") %>%
    build_table(adsl0)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Completed Study", "Discontinued Study",
      "Death", "Lost To Follow-Up", "Other", "Protocol Violation",
      "Withdrawal By Subject", "A: Drug X", "(N=134)", "65 (48.51%)",
      "15 (11.2%)", "6 (40%)", "1 (6.7%)", "2 (13.3%)", "4 (26.7%)",
      "2 (13.3%)", "B: Placebo", "(N=134)", "67 (50.00%)", "28 (20.9%)",
      "5 (17.9%)", "3 (10.7%)", "6 (21.4%)", "4 (14.3%)", "10 (35.7%)",
      "C: Combination", "(N=132)", "69 (52.27%)", "24 (18.2%)", "4 (16.7%)",
      "8 (33.3%)", "5 (20.8%)", "4 (16.7%)", "3 (12.5%)", "All Patients",
      "(N=400)", "201 (50.25%)", "67 (16.8%)", "15 (22.4%)", "12 (17.9%)",
      "13 (19.4%)", "12 (17.9%)", "15 (22.4%)"
    ),
    .Dim = c(9L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("DST01 variant with grouping of reasons is produced correctly", {

  adsl0 <- get_adsl0()

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values("GOTTRT", values = "Y", .labels = c(count_fraction = "Received treatment")) %>%
    split_rows_by("DISTRTFL", split_fun = keep_split_levels("Y")) %>%
    summarize_row_groups(label_fstr = "Discontinued treatment") %>%
    split_rows_by("DRSCAT", split_fun = reorder_split_levels(c("Safety", "Other"))) %>%
    summarize_row_groups() %>%
    summarize_vars("TRTDRS", .stats = "count_fraction") %>%
    build_table(adsl0) %>%
    prune_table()

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Received treatment", "Discontinued treatment",
      "Safety", "ADVERSE EVENT", "PHYSICIAN DECISION", "Other", "LACK OF EFFICACY",
      "OTHER", "PROGRESSIVE DISEASE", "A: Drug X", "(N=134)", "134 (100.00%)",
      "70 (52.2%)", "21 (15.7%)", "10 (47.6%)", "11 (52.4%)", "49 (36.6%)",
      "14 (28.6%)", "20 (40.8%)", "15 (30.6%)", "B: Placebo", "(N=134)",
      "134 (100.00%)", "65 (48.5%)", "28 (20.9%)", "10 (35.7%)", "18 (64.3%)",
      "37 (27.6%)", "14 (37.8%)", "10 (27%)", "13 (35.1%)", "C: Combination",
      "(N=132)", "132 (100.00%)", "68 (51.5%)", "26 (19.7%)", "11 (42.3%)",
      "15 (57.7%)", "42 (31.8%)", "10 (23.8%)", "15 (35.7%)", "17 (40.5%)",
      "All Patients", "(N=400)", "400 (100.00%)", "203 (50.7%)", "75 (18.8%)",
      "31 (41.3%)", "44 (58.7%)", "128 (32.0%)", "38 (29.7%)", "45 (35.2%)",
      "45 (35.2%)"
    ),
    .Dim = c(11L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("DST01 variant with adding other optional rows is produced correctly", {

  adsl0 <- get_adsl0()

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values("COMPSTUD", values = "Y", .labels = c(count_fraction = "Completed Study")) %>%
    count_values("STUDONS", values = "Alive: In Follow-up", .labels = c(count_fraction = "Alive: In Follow-up")) %>%
    split_rows_by("DISSTDFL", split_fun = keep_split_levels("Y")) %>%
    summarize_row_groups(label_fstr = "Discontinued Study") %>%
    summarize_vars("STDDRS", .stats = "count_fraction") %>%
    build_table(adsl0)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Completed Study", "Alive: In Follow-up",
      "Discontinued Study", "Death", "Lost To Follow-Up", "Other",
      "Protocol Violation", "Withdrawal By Subject", "A: Drug X", "(N=134)",
      "65 (48.51%)", "25 (46.30%)", "15 (11.2%)", "6 (40%)", "1 (6.7%)",
      "2 (13.3%)", "4 (26.7%)", "2 (13.3%)", "B: Placebo", "(N=134)",
      "67 (50.00%)", "20 (51.28%)", "28 (20.9%)", "5 (17.9%)", "3 (10.7%)",
      "6 (21.4%)", "4 (14.3%)", "10 (35.7%)", "C: Combination", "(N=132)",
      "69 (52.27%)", "18 (46.15%)", "24 (18.2%)", "4 (16.7%)", "8 (33.3%)",
      "5 (20.8%)", "4 (16.7%)", "3 (12.5%)", "All Patients", "(N=400)",
      "201 (50.25%)", "63 (47.73%)", "67 (16.8%)", "15 (22.4%)", "12 (17.9%)",
      "13 (19.4%)", "12 (17.9%)", "15 (22.4%)"
    ),
    .Dim = c(10L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
