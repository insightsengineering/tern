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

testthat::test_that("summarize_colvars works as expected without column split and default behavior", {
  dta <- get_dta()

  l <- basic_table() %>%
    split_rows_by("AVISIT") %>%
    split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
    summarize_colvars()

  result <- build_table(l, dta)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "V1", "n", "Mean (SD)", "Median", "Min - Max",
      "V2", "n", "Mean (SD)", "Median", "Min - Max", "V3", "n", "Mean (SD)",
      "Median", "Min - Max", "AVAL", "", "3", "6.0 (3.0)", "6.0", "3.0 - 9.0",
      "", "3", "5.0 (3.0)", "5.0", "2.0 - 8.0", "", "3", "4.0 (3.0)", "4.0", "1.0 - 7.0",
      "CHG", "", "3", "0.0 (0.0)", "0.0", "0.0 - 0.0", "", "3", "-1.0 (0.0)", "-1.0",
      "-1.0 - -1.0", "", "3", "-2.0 (0.0)", "-2.0", "-2.0 - -2.0"
    ),
    .Dim = c(16L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("summarize_colvars works as expected with column split", {
  dta <- get_dta()

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("AVISIT") %>%
    split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
    summarize_colvars()

  result <- build_table(l, dta)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "V1", "n", "Mean (SD)", "Median", "Min - Max",
      "V2", "n", "Mean (SD)", "Median", "Min - Max", "V3", "n", "Mean (SD)",
      "Median", "Min - Max", "A", "AVAL", "", "2", "6.0 (4.2)", "6.0",
      "3.0 - 9.0", "", "1", "5.0 (NA)", "5.0", "5.0 - 5.0", "", "2", "4.0 (4.2)",
      "4.0", "1.0 - 7.0", "A", "CHG", "", "2", "0.0 (0.0)", "0.0", "0.0 - 0.0", "",
      "1", "-1.0 (NA)", "-1.0", "-1.0 - -1.0", "", "2", "-2.0 (0.0)", "-2.0", "-2.0 - -2.0",
      "B", "AVAL", "", "1", "6.0 (NA)", "6.0", "6.0 - 6.0", "", "2", "5.0 (4.2)",
      "5.0", "2.0 - 8.0", "", "1", "4.0 (NA)", "4.0", "4.0 - 4.0", "B", "CHG", "",
      "1", "0.0 (NA)", "0.0", "0.0 - 0.0", "", "2", "-1.0 (0.0)", "-1.0", "-1.0 - -1.0",
      "", "1", "-2.0 (NA)", "-2.0", "-2.0 - -2.0"
    ),
    .Dim = c(17L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})


testthat::test_that("summarize_colvars works when selecting statistics and custom formatting", {
  dta <- get_dta()

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("AVISIT") %>%
    split_cols_by_multivar(vars = c("AVAL", "CHG")) %>%
    summarize_colvars(
      .stats = c("n", "mean_sd"),
      .formats = c("mean_sd" = "xx.x, xx.x"),
      .labels = c(n = "n", mean_sd = "Mean, SD"),
      .indent_mods = c(n = 2L, mean_sd = 5L)
    )

  result <- build_table(l, dta)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "V1", "n", "Mean, SD", "V2", "n",
      "Mean, SD", "V3", "n", "Mean, SD", "A", "AVAL", "",
      "2", "6.0, 4.2", "", "1", "5.0, NA", "", "2", "4.0, 4.2", "A", "CHG",
      "", "2", "0.0, 0.0", "", "1", "-1.0, NA", "", "2", "-2.0, 0.0", "B", "AVAL",
      "", "1", "6.0, NA", "", "2", "5.0, 4.2", "", "1", "4.0, NA", "B", "CHG",
      "", "1", "0.0, NA", "", "2", "-1.0, 0.0", "", "1", "-2.0, NA"
    ),
    .Dim = c(11L, 5L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
