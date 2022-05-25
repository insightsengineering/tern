# Tests the single variant of AET10

library(scda)
library(dplyr)
library(rtables)

adsl <- synthetic_cdisc_data("rcd_2022_02_28")$adsl
adae <- synthetic_cdisc_data("rcd_2022_02_28")$adae


testthat::test_that("AET10 default variant is produced correctly", {
  result1 <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_occurrences(vars = "AEDECOD") %>%
    build_table(adae, alt_counts_df = adsl)

  result2 <- prune_table(
    result1,
    keep_rows(
      has_fraction_in_any_col(atleast = 0.40, col_names = levels(adsl$ARM))
    )
  )

  result3 <- sort_at_path(result2, path = c("AEDECOD"), scorefun = score_occurrences)

  result_matrix <- to_string_matrix(result3)

  expected_matrix <- structure(
    c(
      "", "", "dcd D.2.1.5.3", "dcd A.1.1.1.1", "dcd B.2.2.3.1",
      "dcd C.2.1.2.1", "A: Drug X", "(N=134)", "47 (35.1%)", "50 (37.3%)",
      "48 (35.8%)", "35 (26.1%)", "B: Placebo", "(N=134)", "58 (43.3%)",
      "45 (33.6%)", "54 (40.3%)", "48 (35.8%)", "C: Combination", "(N=132)",
      "57 (43.2%)", "63 (47.7%)", "51 (38.6%)", "55 (41.7%)", "All Patients",
      "(N=400)", "162 (40.5%)", "158 (39.5%)", "153 (38.2%)", "138 (34.5%)"
    ),
    .Dim = 6:5
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
