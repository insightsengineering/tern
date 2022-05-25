library(scda)
library(dplyr)

adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae

get_adsl <- function() {
  adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl # nolintr
  set.seed(1)
  # nolint start
  adsl_f <- adsl %>%
    dplyr::filter(SAFFL == "Y") %>% # Safety Evaluable Population
    dplyr::mutate(
      STSTFL = dplyr::case_when(
        is.na(RANDDT) ~ "N", # derive flag for "Started Study",
        TRUE ~ "Y"
      ),
      COMPSTUD = dplyr::case_when(
        EOSSTT == "COMPLETED" ~ "Y", # derive flag for "Completed Study"
        TRUE ~ "N"
      ),
      DISCSTUD = dplyr::case_when(
        EOSSTT == "DISCONTINUED" ~ "Y", # derive flag for "Discontinued study"
        TRUE ~ "N"
      ),
      AGEGRP = dplyr::case_when(
        AGE < 65 ~ "< 65 yrs",
        AGE >= 65 ~ ">= 65 yrs"
      ),
      ETHNIC = sample(
        c("Ethnicity 1", "Ethnicity 2", "Unknown"),
        nrow(.),
        replace = TRUE
      )
    )
  columns <- c("STSTFL", "COMPSTUD", "DISCSTUD", "AGEGRP", "ETHNIC")
  labels <- c("Start Study", "Study Completion Flag", "Study Discontinuation Flag", "Age Group", "Ethnicity")
  formatters::var_labels(adsl_f)[columns] <- labels

  adsl_f$AGEGRP <- factor(adsl_f$AGEGRP, levels = c("< 65 yrs", ">= 65 yrs"))
  adsl_f$ETHNIC <- factor(adsl_f$ETHNIC, levels = c("Ethnicity 1", "Ethnicity 2", "Unknown"))
  # nolint end
  adsl_f <- df_explicit_na(adsl_f)
  adsl_f
}

get_adae_trimmed <- function(adsl, adae, cutoff_rate) {
  n_per_arm <- adsl %>%
    dplyr::count(ARM)

  anl_terms <- adae %>%
    dplyr::group_by(ARM, AEBODSYS, AEDECOD) %>%
    dplyr::summarise(
      unique_terms = dplyr::n_distinct(USUBJID)
    ) %>%
    dplyr::ungroup()

  anl_terms <- dplyr::left_join(
    anl_terms,
    n_per_arm,
    by = "ARM"
  ) %>%
    dplyr::mutate(
      ae_rate = unique_terms / n
    ) %>%
    dplyr::filter(ae_rate >= cutoff_rate) %>%
    dplyr::select(AEDECOD) %>%
    unique()

  anl <- dplyr::left_join(
    anl_terms,
    adae,
    by = "AEDECOD"
  )
  anl
}


testthat::test_that("Patient Disposition table is produced correctly", {
  adsl <- get_adsl()

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_values(
      "STSTFL",
      values = "Y",
      .labels = c(count_fraction = "Started Study"),
      .formats = c(count_fraction = "xx (xx.xx%)")
    ) %>%
    count_values(
      "COMPSTUD",
      values = "Y",
      .labels = c(count_fraction = "Completed Study"),
      .formats = c(count_fraction = "xx (xx.xx%)")
    ) %>%
    count_values(
      "DISCSTUD",
      values = "Y",
      .labels = c(count_fraction = "Discontinued Study"),
      .formats = "xx (xx.xx%)"
    ) %>%
    summarize_vars(
      "DCSREAS",
      .stats = "count_fraction",
      show_labels = "hidden",
      .indent_mods = c(count_fraction = 2L),
      .formats = c(count_fraction = "xx (xx.xx%)"),
      denom = "N_col"
    ) %>%
    build_table(adsl)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Started Study", "Completed Study", "Discontinued Study",
      "ADVERSE EVENT", "DEATH", "LACK OF EFFICACY", "PHYSICIAN DECISION",
      "PROTOCOL VIOLATION", "WITHDRAWAL BY PARENT/GUARDIAN", "WITHDRAWAL BY SUBJECT",
      "A: Drug X", "(N=134)", "134 (100.00%)", "69 (51.49%)", "38 (28.36%)",
      "6 (4.48%)", "22 (16.42%)", "2 (1.49%)", "0 (0.00%)", "4 (2.99%)",
      "2 (1.49%)", "2 (1.49%)", "B: Placebo", "(N=134)", "134 (100.00%)",
      "69 (51.49%)", "43 (32.09%)", "1 (0.75%)", "26 (19.40%)", "1 (0.75%)",
      "1 (0.75%)", "7 (5.22%)", "5 (3.73%)", "2 (1.49%)", "C: Combination",
      "(N=132)", "132 (100.00%)", "72 (54.55%)", "39 (29.55%)", "2 (1.52%)",
      "19 (14.39%)", "2 (1.52%)", "7 (5.30%)", "6 (4.55%)", "3 (2.27%)",
      "0 (0.00%)", "All Patients", "(N=400)", "400 (100.00%)", "210 (52.50%)",
      "120 (30.00%)", "9 (2.25%)", "67 (16.75%)", "5 (1.25%)", "8 (2.00%)",
      "17 (4.25%)", "10 (2.50%)", "4 (1.00%)"
    ),
    .Dim = c(12L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Demographic table is produced correctly", {
  adsl <- get_adsl()
  vars <- c("AGE", "AGEGRP", "SEX", "RACE", "ETHNIC")
  var_labels <- c(
    "Age (yr)",
    "Age group (yr)",
    "Sex",
    "Race",
    "Ethnicity"
  )
  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_vars(vars = vars, var_labels = var_labels) %>%
    build_table(adsl) %>%
    prune_table()

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Age (yr)", "n", "Mean (SD)", "Median", "Min - Max",
      "Age group (yr)", "n", "< 65 yrs", ">= 65 yrs", "Sex", "n", "F",
      "M", "Race", "n", "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE",
      "AMERICAN INDIAN OR ALASKA NATIVE", "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
      "Ethnicity", "n", "Ethnicity 1", "Ethnicity 2", "Unknown", "A: Drug X",
      "(N=134)", "", "134", "33.8 (6.6)", "33.0", "21.0 - 50.0", "", "134",
      "134 (100%)", "0", "", "134", "79 (59%)", "55 (41%)", "", "134",
      "68 (50.7%)", "31 (23.1%)", "27 (20.1%)", "8 (6%)", "0", "0",
      "", "134", "50 (37.3%)", "46 (34.3%)", "38 (28.4%)", "B: Placebo",
      "(N=134)", "", "134", "35.4 (7.9)", "35.0", "21.0 - 62.0", "", "134",
      "134 (100%)", "0", "", "134", "82 (61.2%)", "52 (38.8%)", "",
      "134", "67 (50%)", "28 (20.9%)", "26 (19.4%)", "11 (8.2%)", "1 (0.7%)",
      "1 (0.7%)", "", "134", "51 (38.1%)", "38 (28.4%)", "45 (33.6%)",
      "C: Combination", "(N=132)", "", "132", "35.4 (7.7)", "35.0", "20.0 - 69.0",
      "", "132", "131 (99.2%)", "1 (0.8%)", "", "132", "70 (53%)",
      "62 (47%)", "", "132", "73 (55.3%)", "32 (24.2%)", "21 (15.9%)",
      "6 (4.5%)", "0", "0", "", "132", "46 (34.8%)", "41 (31.1%)",
      "45 (34.1%)", "All Patients", "(N=400)", "", "400", "34.9 (7.4)",
      "34.0", "20.0 - 69.0", "", "400", "399 (99.8%)", "1 (0.2%)", "", "400",
      "231 (57.8%)", "169 (42.2%)", "", "400", "208 (52%)", "91 (22.8%)",
      "74 (18.5%)", "25 (6.2%)", "1 (0.2%)", "1 (0.2%)", "", "400",
      "147 (36.8%)", "125 (31.2%)", "128 (32%)"
    ),
    .Dim = c(28L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Enrollment by Country Table is produced correctly", {
  adsl <- get_adsl()
  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    summarize_vars("COUNTRY", .formats = c(count_fraction = "xx (xx.xx%)")) %>%
    build_table(adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "n", "CHN", "USA", "BRA", "PAK", "NGA", "RUS",
      "JPN", "GBR", "CAN", "CHE", "A: Drug X", "(N=134)", "134", "74 (55.22%)",
      "10 (7.46%)", "13 (9.70%)", "12 (8.96%)", "8 (5.97%)", "5 (3.73%)",
      "5 (3.73%)", "4 (2.99%)", "3 (2.24%)", "0 (0.00%)", "B: Placebo",
      "(N=134)", "134", "81 (60.45%)", "13 (9.70%)", "7 (5.22%)", "9 (6.72%)",
      "7 (5.22%)", "8 (5.97%)", "4 (2.99%)", "3 (2.24%)", "2 (1.49%)",
      "0 (0.00%)", "C: Combination", "(N=132)", "132", "64 (48.48%)",
      "17 (12.88%)", "10 (7.58%)", "10 (7.58%)", "11 (8.33%)", "6 (4.55%)",
      "9 (6.82%)", "2 (1.52%)", "3 (2.27%)", "0 (0.00%)", "All Patients",
      "(N=400)", "400", "219 (54.75%)", "40 (10.00%)", "30 (7.50%)", "31 (7.75%)",
      "26 (6.50%)", "19 (4.75%)", "18 (4.50%)", "9 (2.25%)", "8 (2.00%)",
      "0 (0.00%)"
    ),
    .Dim = c(13L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Death table is produced correctly", {
  adsl <- get_adsl()

  result <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("AESDTH" = "Y"),
      .labels = c(count_fraction = "Total Number of Deaths"),
      .formats = c(count_fraction = "xx (xx.xx%)")
    ) %>%
    build_table(adae, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Total Number of Deaths", "A: Drug X", "(N=134)",
      "76 (62.30%)", "B: Placebo", "(N=134)", "70 (56.91%)", "C: Combination",
      "(N=132)", "75 (62.50%)", "All Patients", "(N=400)", "221 (60.55%)"
    ),
    .Dim = c(3L, 5L)
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Table of Serious Adverse Events is produced correctly (for one specific treatment arm)", {
  adae_serious <- adae %>% dplyr::filter(AESER == "Y", SAFFL == "Y")
  adae_serious_arm <- adae_serious %>% dplyr::filter(ARM == "A: Drug X")

  filters_list <- list(
    related = formatters::with_label(c(AEREL = "Y"), "Events (Related)"),
    fatal = formatters::with_label(c(AESDTH = "Y"), "Events (Fatal)"),
    fatal_related = formatters::with_label(c(AEREL = "Y", AESDTH = "Y"), "Events (Fatal & Related)")
  )

  result <- basic_table() %>%
    summarize_patients_events_in_cols(
      filters_list = filters_list,
      empty_stats = c("all", "related", "fatal", "fatal_related"),
      custom_label = "Total number of patients with at least one serious adverse event"
    ) %>%
    split_rows_by("AEBODSYS", nested = FALSE, split_fun = drop_split_levels, indent_mod = -1L) %>%
    split_rows_by("AEDECOD", split_fun = drop_split_levels) %>%
    summarize_patients_events_in_cols(
      filters_list = filters_list,
      col_split = FALSE
    ) %>%
    build_table(adae_serious_arm)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "Total number of patients with at least one serious adverse event",
      "cl A.1", "dcd A.1.1.1.2", "cl B.1", "dcd B.1.1.1.1", "cl B.2",
      "dcd B.2.2.3.1", "cl D.1", "dcd D.1.1.1.1", "Patients (All)",
      "104", "", "48", "", "47", "", "48", "", "50", "Events (All)",
      "", "", "68", "", "56", "", "64", "", "61", "Events (Related)",
      "", "", "0", "", "56", "", "0", "", "61", "Events (Fatal)",
      "", "", "0", "", "56", "", "0", "", "61", "Events (Fatal & Related)",
      "", "", "0", "", "56", "", "0", "", "61"
    ),
    .Dim = c(10L, 6L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("Table of Non-Serious Adverse Events is produced correctly", {
  adsl <- get_adsl()
  adae_nonser <- adae %>% dplyr::filter(AESER != "Y", SAFFL == "Y")
  adae_trim <- get_adae_trimmed(adsl, adae_nonser, cutoff_rate = 0.05)

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    summarize_patients_events_in_cols(
      custom_label = "Total number of patients with at least one non-SAE and number of events"
    ) %>%
    split_rows_by("AEBODSYS", nested = FALSE, split_fun = drop_split_levels, indent_mod = -1L) %>%
    split_rows_by("AEDECOD", split_fun = drop_split_levels) %>%
    summarize_patients_events_in_cols(
      col_split = FALSE
    ) %>%
    build_table(adae_trim, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "", "Total number of patients with at least one non-SAE and number of events",
      "cl A.1", "dcd A.1.1.1.1", "cl B.2", "dcd B.2.1.2.1", "cl C.1",
      "dcd C.1.1.1.3", "cl C.2", "dcd C.2.1.2.1", "cl D.1", "dcd D.1.1.4.2",
      "cl D.2", "dcd D.2.1.5.3", "A: Drug X", "Patients (All)", "(N=134)",
      "106", "", "50", "", "49", "", "43", "", "35", "", "48", "",
      "47", "A: Drug X", "Events (All)", "(N=134)", "360", "", "64",
      "", "65", "", "55", "", "48", "", "66", "", "62", "B: Placebo",
      "Patients (All)", "(N=134)", "112", "", "45", "", "44", "", "46",
      "", "48", "", "42", "", "58", "B: Placebo", "Events (All)", "(N=134)",
      "367", "", "62", "", "62", "", "63", "", "53", "", "55", "",
      "72", "C: Combination", "Patients (All)", "(N=132)", "112", "",
      "63", "", "52", "", "43", "", "55", "", "50", "", "57", "C: Combination",
      "Events (All)", "(N=132)", "421", "", "88", "", "66", "", "64",
      "", "65", "", "64", "", "74"
    ),
    .Dim = c(16L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
