# Preparation of the test case.
library(dplyr)
adpc <- random.cdisc.data::cadpc %>% filter(AVAL != 0)

test_that("PKPT03 is produced correctly", {

  l <- basic_table() %>%
    split_rows_by(var = "ARM") %>%
    split_rows_by(var = "PARAM") %>%
    summarize_pk_in_cols(var = "AVAL", col_split = TRUE)

  result <- build_table(l, df = adpc)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- t(structure(
    c("", "n", "mean", "sd", "CV % Mean", "Geometric Mean", "CV % Geometric Mean", "Median", "Minimum", "Maximum",
      "A: Drug X", "", "", "", "", "", "", "", "", "",
      "Plasma Drug X", "1072", "8.9", "6.3", "70", "3.1", "1687.8", "10.3", "0", "19.861",
      "Plasma Drug Y", "1072", "17.9", "12.5", "70", "6.1", "1685.1", "20.7", "0", "39.721",
      "B: Placebo", "", "", "", "", "", "", "", "", "",
      "Plasma Drug X", "1072", "8.9", "6.3", "71", "2.9", "1989.6", "10.6", "0", "20.025",
      "Plasma Drug Y", "1072", "17.8", "12.6", "71", "5.8", "1978.9", "21.2", "0", "40.051",
      "C: Combination", "", "", "", "", "", "", "", "", "",
      "Plasma Drug X", "1056", "9", "6.4", "70.9", "3", "1971.1", "10.4", "0", "19.734",
      "Plasma Drug Y", "1056", "18", "12.7", "70.9", "5.9", "1972.3", "20.9", "0", "39.469"),
    .Dim = c(10L, 10L)
  ))
  expect_identical(result_matrix, expected_matrix)
})
