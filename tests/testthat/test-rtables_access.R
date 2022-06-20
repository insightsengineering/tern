tbl_example <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("RACE") %>%
  split_rows_by("STRATA1") %>%
  summarize_vars("COUNTRY", .stats = "count_fraction") %>%
  build_table(DM)

tbl_with_empty <- rtable(
  header = c("A: Drug X", "B: Placebo", "C: Combination"),
  rrow("empty_row", NULL, NULL, NULL)
)

testthat::test_that("h_row_counts works as expected", {
  tab <- tbl_example
  sub_tab <- tab[5, ]
  testthat::expect_identical(
    to_string_matrix(sub_tab),
    structure(
      c("", "BRA", "A: Drug X", "1 (3.7%)", "B: Placebo", "4 (20%)", "C: Combination", "1 (3.2%)"),
      .Dim = c(2L, 4L)
    )
  )
  table_row <- collect_leaves(sub_tab)[[1]]
  result <- h_row_counts(table_row, c("B: Placebo", "C: Combination"))
  expected <- c("B: Placebo" = 4, "C: Combination" = 1)
  testthat::expect_identical(result, expected)
})

testthat::test_that("h_row_counts returns NA with empty analysis row", {
  tab <- tbl_with_empty
  sub_tab <- tab[3, ]
  testthat::expect_identical(
    to_string_matrix(sub_tab),
    structure(
      c("", "empty_row", "A: Drug X", "", "B: Placebo", "", "C: Combination", ""),
      .Dim = c(2L, 4L)
    )
  )
  table_row <- collect_leaves(sub_tab)[[1]]
  result <- h_row_counts(table_row, col_names = c("B: Placebo", "C: Combination"))
  expected <- c(NA_real_, NA_real_)
  testthat::expect_identical(result, expected)
})

testthat::test_that("tern:::h_row_fractions works as expected", {
  tab <- tbl_example
  sub_tab <- tab[5, ]
  testthat::expect_identical(
    to_string_matrix(sub_tab),
    structure(
      c("", "BRA", "A: Drug X", "1 (3.7%)", "B: Placebo", "4 (20%)", "C: Combination", "1 (3.2%)"),
      .Dim = c(2L, 4L)
    )
  )
  table_row <- collect_leaves(sub_tab)[[1]]
  result <- h_row_counts(table_row, c("B: Placebo", "C: Combination"))
  expected <- c("B: Placebo" = 4L, "C: Combination" = 1L)
  testthat::expect_equal(result, expected)
})

testthat::test_that("h_col_counts works as expected", {
  tab <- tbl_example
  result <- h_col_counts(tab, c("B: Placebo", "C: Combination"))
  expected <- c("B: Placebo" = 106L, "C: Combination" = 129L)
  testthat::expect_identical(result, expected)
})

testthat::test_that("tern:::is_leaf_table works as expected", {
  simple_tab <- basic_table() %>%
    split_rows_by("RACE") %>%
    summarize_vars("COUNTRY", .stats = "count_fraction") %>%
    build_table(DM)
  testthat::expect_false(tern:::is_leaf_table(simple_tab))
  sub_tab <- tree_children(simple_tab)[[1]]
  testthat::expect_true(tern:::is_leaf_table(sub_tab))
})

testthat::test_that("h_content_first_row works as expected", {
  simple_tab <- basic_table() %>%
    split_cols_by("ARM") %>%
    summarize_row_groups() %>%
    build_table(DM)
  result <- h_content_first_row(simple_tab)
  testthat::expect_is(result, "ContentRow")
})
