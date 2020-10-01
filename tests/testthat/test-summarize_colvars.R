get_dta <- function() {
  library(dplyr)
  data.frame(
    ARM = rep(c("A", "B"), 9),
    USUBJID = rep(1:6, each = 3),
    AVISIT = rep(paste0("V", 1:3), 6),
    AVAL = c(9:1, rep(NA, 9))
  ) %>%
    dplyr::mutate(
      ABLFLL = AVISIT == "V1"
    ) %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::mutate(
      BLVAL = AVAL[ABLFLL],
      CHG = AVAL - BLVAL
    ) %>%
    dplyr::ungroup()
}

test_that("summarize_colvars works as expected without column split and default behavior", {
  dta <- get_dta()

  l <- basic_table() %>%
    split_rows_by("AVISIT") %>%
    split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
    summarize_colvars()

  result <- build_table(l, dta)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "V1", "n", "Mean (SD)", "Median", "Min - Max",
      "V2", "n", "Mean (SD)", "Median", "Min - Max", "V3", "n", "Mean (SD)",
      "Median", "Min - Max", "AVAL", "", "3", "6 (3)", "6", "3 - 9",
      "", "3", "5 (3)", "5", "2 - 8", "", "3", "4 (3)", "4", "1 - 7",
      "CHG", "", "3", "0 (0)", "0", "0 - 0", "", "3", "-1 (0)", "-1",
      "-1 - -1", "", "3", "-2 (0)", "-2", "-2 - -2"),
    .Dim = c(16L, 3L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("summarize_colvars works as expected with column split but warns currently", {
  dta <- get_dta()

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("AVISIT") %>%
    split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
    summarize_colvars()

  result <- expect_warning(build_table(l, dta))
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "", "V1", "n", "Mean (SD)", "Median", "Min - Max",
      "V2", "n", "Mean (SD)", "Median", "Min - Max", "V3", "n", "Mean (SD)",
      "Median", "Min - Max", "A", "AVAL", "", "2", "6 (4.2)", "6",
      "3 - 9", "", "1", "5 (NA)", "5", "5 - 5", "", "2", "4 (4.2)",
      "4", "1 - 7", "A", "CHG", "", "2", "0 (0)", "0", "0 - 0", "",
      "1", "-1 (NA)", "-1", "-1 - -1", "", "2", "-2 (0)", "-2", "-2 - -2",
      "B", "AVAL", "", "1", "6 (NA)", "6", "6 - 6", "", "2", "5 (4.2)",
      "5", "2 - 8", "", "1", "4 (NA)", "4", "4 - 4", "B", "CHG", "",
      "1", "0 (NA)", "0", "0 - 0", "", "2", "-1 (0)", "-1", "-1 - -1",
      "", "1", "-2 (NA)", "-2", "-2 - -2"),
    .Dim = c(17L, 5L)
  )
  expect_identical(result_matrix, expected_matrix)
})
