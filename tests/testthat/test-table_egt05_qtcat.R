# Test the single variant for EGT05_QTCAT

library(scda)
library(rtables)
library(dplyr)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl

get_adeg <- function() {
  adeg <- synthetic_cdisc_data("rcd_2022_02_28")$adeg # nolintr
  adeg_labels <- formatters::var_labels(adeg)

  adeg_f <- adeg %>%
    dplyr::filter(PARAMCD == "QT" & ANL01FL == "Y") %>%
    # Categorize AVAL and CHG
    dplyr::mutate(
      AVALCAT1 = dplyr::case_when(
        AVAL <= 450 ~ "<=450 msec",
        AVAL <= 480 ~ ">450 to <=480 msec",
        AVAL <= 500 ~ ">480 to <= 500 msec",
        AVAL > 500 ~ ">500 msec"
      ),
      CHGCAT1 = dplyr::case_when(
        CHG <= 30 ~ "<=30 msec",
        CHG <= 60 ~ ">30 to <=60 msec",
        CHG > 60 ~ ">60 msec"
      )
    )

  adeg_f$AVALCAT1 <- factor( # nolint snake_case
    adeg_f$AVALCAT1,
    levels = c("<=450 msec", ">450 to <=480 msec", ">480 to <= 500 msec", ">500 msec")
  )

  adeg_f$CHGCAT1 <- factor( # nolint snake_case
    adeg_f$CHGCAT1,
    levels = c("<=30 msec", ">30 to <=60 msec", ">60 msec")
  )

  formatters::var_labels(adeg_f) <- c(adeg_labels, "AVALCAT1" = "Value at Visit", "CHGCAT1" = "Change from Baseline")

  adeg_f <- df_explicit_na(adeg_f)
  adeg_f
}

testthat::test_that("EGT05_QTCAT default variant is produced correctly", {
  adeg <- get_adeg()

  result <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    split_rows_by("AVISIT", split_label = "Visit", child_labels = "default", label_pos = "visible") %>%
    summarize_vars(
      vars = c("AVALCAT1", "CHGCAT1"),
      var_labels = c("Value at Visit", "Change from Baseline")
    ) %>%
    build_table(df = adeg, alt_counts_df = adsl) %>%
    prune_table()
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Visit", "BASELINE", "Value at Visit", "n",
      "<=450 msec", ">450 to <=480 msec", ">480 to <= 500 msec", ">500 msec",
      "WEEK 1 DAY 8", "Value at Visit", "n", "<=450 msec", ">450 to <=480 msec",
      ">480 to <= 500 msec", ">500 msec", "Change from Baseline", "n",
      "<=30 msec", ">30 to <=60 msec", ">60 msec", "WEEK 2 DAY 15",
      "Value at Visit", "n", "<=450 msec", ">450 to <=480 msec", ">480 to <= 500 msec",
      ">500 msec", "Change from Baseline", "n", "<=30 msec", ">30 to <=60 msec",
      ">60 msec", "WEEK 3 DAY 22", "Value at Visit", "n", "<=450 msec",
      ">450 to <=480 msec", ">480 to <= 500 msec", ">500 msec", "Change from Baseline",
      "n", "<=30 msec", ">30 to <=60 msec", ">60 msec", "WEEK 4 DAY 29",
      "Value at Visit", "n", "<=450 msec", ">450 to <=480 msec", ">480 to <= 500 msec",
      ">500 msec", "Change from Baseline", "n", "<=30 msec", ">30 to <=60 msec",
      ">60 msec", "WEEK 5 DAY 36", "Value at Visit", "n", "<=450 msec",
      ">450 to <=480 msec", ">480 to <= 500 msec", ">500 msec", "Change from Baseline",
      "n", "<=30 msec", ">30 to <=60 msec", ">60 msec", "A: Drug X",
      "(N=134)", "", "", "", "134", "115 (85.8%)", "6 (4.5%)", "4 (3%)",
      "9 (6.7%)", "", "", "134", "113 (84.3%)", "10 (7.5%)", "4 (3%)",
      "7 (5.2%)", "", "134", "76 (56.7%)", "7 (5.2%)", "51 (38.1%)",
      "", "", "134", "111 (82.8%)", "10 (7.5%)", "7 (5.2%)", "6 (4.5%)",
      "", "134", "71 (53%)", "11 (8.2%)", "52 (38.8%)", "", "", "134",
      "106 (79.1%)", "13 (9.7%)", "4 (3%)", "11 (8.2%)", "", "134",
      "63 (47%)", "14 (10.4%)", "57 (42.5%)", "", "", "134", "117 (87.3%)",
      "7 (5.2%)", "4 (3%)", "6 (4.5%)", "", "134", "79 (59%)", "11 (8.2%)",
      "44 (32.8%)", "", "", "134", "107 (79.9%)", "16 (11.9%)", "5 (3.7%)",
      "6 (4.5%)", "", "134", "72 (53.7%)", "10 (7.5%)", "52 (38.8%)",
      "B: Placebo", "(N=134)", "", "", "", "134", "117 (87.3%)", "10 (7.5%)",
      "3 (2.2%)", "4 (3%)", "", "", "134", "106 (79.1%)", "10 (7.5%)",
      "4 (3%)", "14 (10.4%)", "", "134", "75 (56%)", "13 (9.7%)", "46 (34.3%)",
      "", "", "134", "114 (85.1%)", "9 (6.7%)", "2 (1.5%)", "9 (6.7%)",
      "", "134", "87 (64.9%)", "9 (6.7%)", "38 (28.4%)", "", "", "134",
      "112 (83.6%)", "7 (5.2%)", "5 (3.7%)", "10 (7.5%)", "", "134",
      "80 (59.7%)", "8 (6%)", "46 (34.3%)", "", "", "134", "103 (76.9%)",
      "14 (10.4%)", "7 (5.2%)", "10 (7.5%)", "", "134", "80 (59.7%)",
      "7 (5.2%)", "47 (35.1%)", "", "", "134", "117 (87.3%)", "5 (3.7%)",
      "9 (6.7%)", "3 (2.2%)", "", "134", "82 (61.2%)", "11 (8.2%)",
      "41 (30.6%)", "C: Combination", "(N=132)", "", "", "", "132",
      "104 (78.8%)", "9 (6.8%)", "6 (4.5%)", "13 (9.8%)", "", "", "132",
      "106 (80.3%)", "11 (8.3%)", "3 (2.3%)", "12 (9.1%)", "", "132",
      "75 (56.8%)", "11 (8.3%)", "46 (34.8%)", "", "", "132", "112 (84.8%)",
      "9 (6.8%)", "5 (3.8%)", "6 (4.5%)", "", "132", "89 (67.4%)",
      "9 (6.8%)", "34 (25.8%)", "", "", "132", "118 (89.4%)", "3 (2.3%)",
      "2 (1.5%)", "9 (6.8%)", "", "132", "81 (61.4%)", "11 (8.3%)",
      "40 (30.3%)", "", "", "132", "114 (86.4%)", "6 (4.5%)", "3 (2.3%)",
      "9 (6.8%)", "", "132", "79 (59.8%)", "10 (7.6%)", "43 (32.6%)",
      "", "", "132", "112 (84.8%)", "13 (9.8%)", "3 (2.3%)", "4 (3%)",
      "", "132", "73 (55.3%)", "11 (8.3%)", "48 (36.4%)"
    ),
    .Dim = c(70L, 4L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
