get_anl <- function() {
  set.seed(1)
  df <- data.frame(
    USUBJID = c(paste("id", seq(1, 12), sep = "")),
    ARMCD = c(rep("ARM A", 6), rep("ARM B", 6)),
    SEX = c(rep("Female", 6), rep("Male", 6)),
    AVAL = as.numeric(sample(seq(1, 5), 12, replace = TRUE)),
    stringsAsFactors = TRUE
  )
}

get_adsl <- function() {
  adsl <- data.frame(
    USUBJID = c(paste("id", seq(1, 12), sep = "")),
    ARMCD = c(rep("ARM A", 6), rep("ARM B", 6)),
    SEX = c(rep("Female", 6), rep("Male", 6)),
    stringsAsFactors = TRUE
  )
}

testthat::test_that("s_count_patients_sum_exposure works as expected", {
  df <- get_anl()
  adsl <- get_adsl()
  result <- s_count_patients_sum_exposure(df = df, .N_col = nrow(adsl)) # nolintr
  expected <- list(
    n_patients = formatters::with_label(c(12, 1), "Total patients numbers/person time"),
    sum_exposure = formatters::with_label(35, "Total patients numbers/person time")
  )
  testthat::expect_equal(result, expected)
})


testthat::test_that("summarize_patients_exposure_in_cols works well with default arguments", {
  df <- get_anl()
  adsl <- get_adsl()

  result <- basic_table() %>%
    split_cols_by("ARMCD", split_fun = add_overall_level("Total", first = FALSE)) %>%
    summarize_patients_exposure_in_cols(var = "AVAL", col_split = TRUE) %>%
    split_rows_by("SEX") %>%
    summarize_patients_exposure_in_cols(var = "AVAL", col_split = FALSE) %>%
    build_table(df = df, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "Total patients numbers/person time", "Female",
      "Male", "ARM A", "Patients", "6 (100.0%)", "6 (100.0%)", "0 (0.0%)",
      "ARM A", "Person time", "16", "16", "0", "ARM B", "Patients",
      "6 (100.0%)", "0 (0.0%)", "6 (100.0%)", "ARM B", "Person time", "19",
      "0", "19", "Total", "Patients", "12 (100.0%)", "6 (50.0%)", "6 (50.0%)",
      "Total", "Person time", "35", "16", "19"
    ),
    .Dim = c(5L, 7L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("summarize_patients_exposure_in_cols works well with custom arguments", {
  df <- get_anl()
  adsl <- get_adsl()

  result <- basic_table() %>%
    split_cols_by("ARMCD", split_fun = add_overall_level("Total", first = FALSE)) %>%
    summarize_patients_exposure_in_cols(
      var = "AVAL",
      col_split = TRUE,
      custom_label = "xyz",
      .stats = "sum_exposure"
    ) %>%
    split_rows_by("SEX") %>%
    summarize_patients_exposure_in_cols(
      var = "AVAL",
      col_split = FALSE,
      .stats = "sum_exposure"
    ) %>%
    build_table(df = df, alt_counts_df = adsl)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "", "xyz", "Female", "Male", "ARM A", "Person time",
      "16", "16", "0", "ARM B", "Person time", "19", "0", "19", "Total",
      "Person time", "35", "16", "19"
    ),
    .Dim = 5:4
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that(
  "summarize_patients_exposure_in_cols returns the correct column label when there is no variable split
          and when just one statistics is shown",
  code = {
    df <- get_anl()
    adsl <- get_adsl()

    table <- basic_table() %>%
      summarize_patients_exposure_in_cols(
        var = "AVAL",
        col_split = TRUE,
        custom_label = "xyz",
        .stats = "n_patients"
      ) %>%
      build_table(df = df, alt_counts_df = adsl)

    result <- col_paths_summary(table)$label
    expected_string <- "Patients"
    testthat::expect_identical(result, expected_string)
  }
)
