tbl_example <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("RACE") %>%
  split_rows_by("STRATA1") %>%
  analyze_vars("COUNTRY", .stats = "count_fraction") %>%
  build_table(DM)

tbl_with_empty <- rtable(
  header = c("A: Drug X", "B: Placebo", "C: Combination"),
  rrow("empty_row", NULL, NULL, NULL)
)
testthat::test_that("h_row_first_values works as expected", {
  sub_tab <- tbl_example[5, ]

  # Selected table is correct
  res <- testthat::expect_silent(sub_tab)
  testthat::expect_snapshot(res)

  # Extract data row
  table_row <- collect_leaves(sub_tab)[[1]]

  # Extract all first values
  result <- h_row_first_values(table_row)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Extract first values by specific column names
  result <- h_row_first_values(table_row, col_names = c("B: Placebo", "C: Combination"))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Extract first values by specific column indices
  result <- h_row_first_values(table_row, col_indices = c(1L, 3L))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)

  # Error if both are used
  testthat::expect_error(h_row_first_values(table_row, col_indices = c(1L, 3L), col_names = c("B: Placebo")))
})

testthat::test_that("h_row_counts works as expected", {
  tab <- tbl_example
  sub_tab <- tab[5, ]

  res <- testthat::expect_silent(sub_tab)
  testthat::expect_snapshot(res)

  table_row <- collect_leaves(sub_tab)[[1]]
  result <- h_row_counts(table_row, col_names = c("B: Placebo", "C: Combination"))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_row_counts returns NA with empty analysis row", {
  tab <- tbl_with_empty
  sub_tab <- tab[3, ]

  res <- testthat::expect_silent(sub_tab)
  testthat::expect_snapshot(res)

  table_row <- collect_leaves(sub_tab)[[1]]
  result <- h_row_counts(table_row, col_names = c())

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_row_fractions works as expected", {
  tab <- tbl_example
  sub_tab <- tab[5, ]

  res <- testthat::expect_silent(sub_tab)
  testthat::expect_snapshot(res)

  table_row <- collect_leaves(sub_tab)[[1]]
  result <- h_row_fractions(table_row, c("B: Placebo", "C: Combination"))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("h_col_counts works as expected", {
  tab <- tbl_example
  result <- h_col_counts(tab, c("B: Placebo", "C: Combination"))

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("is_leaf_table works as expected", {
  simple_tab <- basic_table() %>%
    split_rows_by("RACE") %>%
    analyze_vars("COUNTRY", .stats = "count_fraction") %>%
    build_table(DM)
  testthat::expect_false(is_leaf_table(simple_tab))
  sub_tab <- tree_children(simple_tab)[[1]]
  testthat::expect_true(is_leaf_table(sub_tab))
})

testthat::test_that("h_content_first_row works as expected", {
  simple_tab <- basic_table() %>%
    split_cols_by("ARM") %>%
    summarize_row_groups() %>%
    build_table(DM)
  result <- h_content_first_row(simple_tab)
  testthat::expect_s4_class(result, "ContentRow")
})
