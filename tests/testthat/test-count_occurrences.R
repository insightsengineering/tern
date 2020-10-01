test_that("s_count_occurrences functions as expected with valid input and default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3")
  )

  result <- s_count_occurrences(df = df, .N_col = 4)

  expected <- list(count_percent = c(
    MH1 = list(c(3.00, 0.75)),
    MH2 = list(c(1.00, 0.25)),
    MH3 = list(c(1.00, 0.25))
  ))
  expect_equal(result, expected)
})

test_that("count_occurrences functions as expected with valid input and default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(
      1, 1, 2, 4, 4, 4,
      6, 6, 6, 7, 7, 8)),
    MHDECOD = c(
      "MH1", "MH2", "MH1", "MH1", "MH1", "MH3",
      "MH2", "MH2", "MH3", "MH1", "MH2", "MH4"),
    ARM = rep(c("A", "B"), each = 6)
  )

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_occurrences(vars = "MHDECOD")

  result <- rtable_object <- lyt %>%
    build_table(df, col_counts = c(5L, 4L)) %>%
    prune_table()
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("", "", "MH1", "MH2", "MH3", "MH4", "A", "(N=5)",
      "3 (60%)", "1 (20%)", "1 (20%)", "0 (0%)", "B", "(N=4)", "1 (25%)",
      "2 (50%)", "1 (25%)", "1 (25%)"),
    .Dim = c(6L, 3L))

  expect_identical(result_matrix, expected_matrix)
})
