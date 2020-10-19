# Tests the single variant of AET10

library(random.cdisc.data)
library(dplyr)
library(rtables)


test_that("AET10 default variant is produced correctly", {
  adsl <- radsl(cached = TRUE)
  n_per_arm <- table(adsl$ARM)

  # "All Patients" column is not requested in the mock-up, but we include it to show correct sorting
  n_per_arm <- c(n_per_arm, "All Patients" = sum(n_per_arm))

  adae <- radae(cached = TRUE)

  result1 <- basic_table() %>%
    split_cols_by(var = "ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    add_colcounts() %>%
    count_occurrences(var = "AEDECOD") %>%
    build_table(adae, col_count = n_per_arm)

  result2 <- prune_table(
    result1,
    keep_rows(
      has_fraction_in_cols(above = 0.40, col_names = names(result1)[1]) |
        has_fraction_in_cols(above = 0.40, col_names = names(result1)[2]) |
        has_fraction_in_cols(above = 0.40, col_names = names(result1)[3])
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

  expect_identical(result_matrix, expected_matrix)
})
