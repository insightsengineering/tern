raw_data <- data.frame(
  USUBJID = rep(c("id1", "id2", "id3", "id4"), c(2, 3, 1, 1)),
  ARM = c("A", "A", "B", "B", "B", "B", "A"),
  AESER = rep("Y", 7),
  AESDTH = c("Y", "Y", "N", "Y", "Y", "N", "N"),
  AEREL = c("Y", "Y", "N", "Y", "Y", "N", "Y"),
  AEDECOD = c("A", "A", "A", "B", "B", "C", "D"),
  AEBODSYS = rep(c("SOC1", "SOC2", "SOC3"), c(3, 3, 1))
)

testthat::test_that("s_count_patients_and_multiple_events works as expected", {
  df <- raw_data
  result <- s_count_patients_and_multiple_events(
    df = df,
    id = "USUBJID",
    filters_list = list(
      serious = c(AESER = "Y"),
      fatal = c(AESDTH = "Y")
    )
  )
  expected <- list(
    unique = with_label(4, "counts"),
    all = with_label(7, "counts"),
    serious = with_label(7, "counts"),
    fatal = with_label(4, "counts")
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("s_count_patients_and_multiple_events can have empty stats if requested", {
  df <- raw_data
  result <- s_count_patients_and_multiple_events(
    df = df,
    id = "USUBJID",
    filters_list = list(
      serious = c(AESER = "Y"),
      fatal = c(AESDTH = "Y")
    ),
    empty_stats = c("all", "serious")
  )
  expected <- list(
    unique = with_label(4, "counts"),
    all = with_label(character(), "counts"),
    serious = with_label(character(), "counts"),
    fatal = with_label(4, "counts")
  )
  testthat::expect_equal(result, expected)
})

testthat::test_that("summarize_patients_events_in_cols works well with default arguments", {
  df <- raw_data
  result <- basic_table() %>%
    summarize_patients_events_in_cols(
      filters_list = list(
        related = with_label(c(AEREL = "Y"), "Events (Related)"),
        fatal = c(AESDTH = "Y"),
        fatal_related = c(AEREL = "Y", AESDTH = "Y")
      )
    ) %>%
    build_table(df)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c(
      "", "counts", "Patients (All)", "4", "Events (All)",
      "7", "Events (Related)", "5", "fatal", "4", "fatal_related",
      "4"
    ),
    .Dim = c(2L, 6L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})

testthat::test_that("summarize_patients_events_in_cols works well with custom arguments", {
  df <- raw_data
  result <- basic_table() %>%
    summarize_patients_events_in_cols(
      filters_list = list(
        related = with_label(c(AEREL = "Y"), "Events (Related)"),
        fatal = c(AESDTH = "Y"),
        fatal_related = c(AEREL = "Y", AESDTH = "Y")
      ),
      .stats = c("related", "all"),
      .labels = c(related = "Related", all = "All"),
      empty_stats = "all",
      custom_label = "bla"
    ) %>%
    build_table(df)

  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "bla", "Related", "5", "All", ""),
    .Dim = c(2L, 3L)
  )
  testthat::expect_identical(result_matrix, expected_matrix)
})
