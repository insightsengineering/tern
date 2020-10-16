get_table <- function() {
  basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE") %>%
    split_rows_by("STRATA1") %>%
    summarize_vars("COUNTRY", .stats = "count_fraction") %>%
    build_table(DM)
}

test_that("h_row_counts works as expected", {
  tab <- get_table()
  sub_tab <- tab[5, ]
  expect_identical(
    to_string_matrix(sub_tab),
    structure(
      c("", "BRA", "A: Drug X", "1 (3.7%)", "B: Placebo", "4 (20%)", "C: Combination", "1 (3.2%)"),
      .Dim = c(2L, 4L)
    )
  )
  table_row <- collect_leaves(sub_tab)[[1]]
  result <- h_row_counts(table_row, c("B: Placebo", "C: Combination"))
  expected <- c("B: Placebo" = 4, "C: Combination" = 1)
  expect_identical(result, expected)
})

test_that("h_row_fractions works as expected", {
  tab <- get_table()
  sub_tab <- tab[5, ]
  expect_identical(
    to_string_matrix(sub_tab),
    structure(
      c("", "BRA", "A: Drug X", "1 (3.7%)", "B: Placebo", "4 (20%)", "C: Combination", "1 (3.2%)"),
      .Dim = c(2L, 4L)
    )
  )
  table_row <- collect_leaves(sub_tab)[[1]]
  result <- h_row_counts(table_row, c("B: Placebo", "C: Combination"))
  expected <- c("B: Placebo" = 4L, "C: Combination" = 1L)
  expect_equal(result, expected)
})

test_that("h_col_counts works as expected", {
  tab <- get_table()
  result <- h_col_counts(tab, c("B: Placebo", "C: Combination"))
  expected <- c("B: Placebo" = 106L, "C: Combination" = 129L)
  expect_identical(result, expected)
})
