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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
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

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_patients_events_in_cols works well with default arguments", {
  df <- raw_data
  result <- basic_table() %>%
    summarize_patients_events_in_cols(
      filters_list = list(
        related = formatters::with_label(c(AEREL = "Y"), "Events (Related)"),
        fatal = c(AESDTH = "Y"),
        fatal_related = c(AEREL = "Y", AESDTH = "Y")
      )
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("summarize_patients_events_in_cols works well with custom arguments", {
  df <- raw_data
  result <- basic_table() %>%
    summarize_patients_events_in_cols(
      filters_list = list(
        related = formatters::with_label(c(AEREL = "Y"), "Events (Related)"),
        fatal = c(AESDTH = "Y"),
        fatal_related = c(AEREL = "Y", AESDTH = "Y")
      ),
      .stats = c("related", "all"),
      .labels = c(related = "Related", all = "All"),
      empty_stats = "all",
      custom_label = "bla"
    ) %>%
    build_table(df)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
