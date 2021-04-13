test_that("s_count_occurrences functions as expected with valid input and default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3")
  )


  result <- s_count_occurrences(df = df, .N_col = 4L, .df_row = df)

  expected <- list(
    count = list(
      MH1 = 3L,
      MH2 = 1L,
      MH3 = 1L
    ),
    count_fraction = list(
      MH1 = c(3L, 0.75),
      MH2 = c(1L, 0.25),
      MH3 = c(1L, 0.25)
    ),
    fraction = list(
      MH1 = c("num" = 3L, "denom" = 4L),
      MH2 = c("num" = 1L, "denom" = 4L),
      MH3 = c("num" = 1L, "denom" = 4L)
    )

  )
  expect_equal(result, expected)
})

test_that("s_count_occurrences drops non appearing levels by default", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3"),
      levels = c("MH1", "MH2", "MH3", "MHX")
     )
  )
  result <- s_count_occurrences(df = df, .N_col = 4L, .df_row = df)
  expect_false("MHX" %in% c(names(result$count), names(result$count_fraction), names(result$fraction)))
})

test_that("s_count_occurrences keeps non appearing levels if requested", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3"),
      levels = c("MH1", "MH2", "MH3", "MHX")
    )
  )
  result <- s_count_occurrences(df = df, .N_col = 4L, .df_row = df, drop = FALSE)
  expect_true("MHX" %in% names(result$count))
  expect_true("MHX" %in% names(result$count_fraction))
  expect_true("MHX" %in% names(result$fraction))
})

test_that("s_count_occurrences fails when it receives empty .df_row and drop = TRUE", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3"),
      levels = c("MH1", "MH2", "MH3", "MHX")
    )
  )
  df_sub <- df[df$USUBJID == "5", ]
  expect_error(s_count_occurrences(
    df = df_sub,
    .N_col = 4L,
    .df_row = df_sub,
    drop = TRUE
  ))
})

test_that("s_count_occurrences functions as expected when requesting different denominator", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4)),
    MHDECOD = c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3")
  )

  result <- s_count_occurrences(df = df, denom = "n", .N_col = 4L, .df_row = df)

  expected <- list(
    count = list(
      MH1 = 3L,
      MH2 = 1L,
      MH3 = 1L
    ),
    count_fraction = list(
      MH1 = c(3L, 1),
      MH2 = c(1L, 1 / 3),
      MH3 = c(1L, 1 / 3)
    ),
    fraction = list(
      MH1 = c("num" = 3L, "denom" = 3),
      MH2 = c("num" = 1L, "denom" = 3),
      MH3 = c("num" = 1L, "denom" = 3)
    )
  )
  expect_equal(result, expected)
})

test_that("count_occurrences functions as expected with valid input and default arguments", {
  df <- data.frame(
    USUBJID = as.character(c(1, 1, 2, 4, 4, 4, 6, 6, 6, 7, 7, 8)),
    MHDECOD = factor(
      c("MH1", "MH2", "MH1", "MH1", "MH1", "MH3", "MH2", "MH2", "MH3", "MH1", "MH2", "MH4"),
      levels = c("MH1", "MH2", "MH3", "MH4", "MHX")
    ),
    ARM = rep(c("A", "B"), each = 6)
  )
  df_adsl <- data.frame(
    USUBJID = 1:9,
    ARM = rep(c("A", "B"), c(5, 4))
  )

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    add_colcounts() %>%
    count_occurrences(vars = "MHDECOD")

  result <- rtable_object <- lyt %>%
    build_table(df, alt_counts_df = df_adsl)

  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c("", "", "MH1", "MH2", "MH3", "MH4", "A", "(N=5)",
      "3 (60%)", "1 (20%)", "1 (20%)", "0", "B", "(N=4)", "1 (25%)",
      "2 (50%)", "1 (25%)", "1 (25%)"),
    .Dim = c(6L, 3L))

  expect_identical(result_matrix, expected_matrix)
})
