# Pre-processing the table
tab <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("RACE") %>%
  split_rows_by("STRATA1") %>%
  summarize_row_groups() %>%
  analyze_vars("COUNTRY", .stats = "count_fraction") %>%
  build_table(DM)

testthat::test_that("keep_rows works in a special case identical to standard pruning", {
  row_condition <- !CombinationFunction(all_zero_or_na)
  pruning_fun <- keep_rows(row_condition)
  testthat::expect_type(pruning_fun, "closure")
  result <- prune_table(tab, pruning_fun)
  expected <- prune_table(tab)
  testthat::expect_identical(result, expected)
})

testthat::test_that("keep_rows prunes everything if condition is always `FALSE`", {
  row_condition <- function(table_row) FALSE
  pruning_fun <- keep_rows(row_condition)
  result <- prune_table(tab, pruning_fun)
  expected <- NULL
  testthat::expect_identical(result, expected)
})

testthat::test_that("keep_rows keeps everything if condition is always `TRUE`", {
  row_condition <- function(table_row) TRUE
  pruning_fun <- keep_rows(row_condition)
  result <- prune_table(tab, pruning_fun)
  expected <- tab
  testthat::expect_identical(result, expected)
})

testthat::test_that("keep_content_rows works as expected", {
  more_than_twenty <- has_count_in_cols(atleast = 21L, col_names = names(tab))
  result <- prune_table(tab, keep_content_rows(more_than_twenty))
  result_leaves <- collect_leaves(result)
  result_content_rows <- result_leaves[sapply(result_leaves, class) == "ContentRow"]
  result_counts <- result_content_rows %>%
    lapply(h_row_counts, col_names = names(tab)) %>%
    sapply(sum)
  testthat::expect_true(all(result_counts > 20))
})

testthat::test_that("has_count_in_cols result works in a special case identical to standard pruning", {
  row_condition <- has_count_in_cols(atleast = 1L, col_names = names(tab))
  result <- prune_table(tab, keep_rows(row_condition))
  expected <- prune_table(tab)
  testthat::expect_identical(result, expected)
})

testthat::test_that("has_count_in_cols result performs comparisons correctly", {
  sub_tab <- tab[5, ]

  res <- testthat::expect_silent(sub_tab)
  testthat::expect_snapshot(res)

  table_row <- collect_leaves(sub_tab)[[1]]
  row_condition <- has_count_in_cols(atleast = 4L, col_names = "A: Drug X")
  testthat::expect_false(row_condition(table_row))
  row_condition <- has_count_in_cols(atleast = 1L, col_names = "A: Drug X")
  testthat::expect_true(row_condition(table_row))
  row_condition <- has_count_in_cols(atleast = 5L, col_names = c("A: Drug X", "B: Placebo"))
  testthat::expect_true(row_condition(table_row))
})

testthat::test_that("has_count_in_any_col result performs comparisons correctly", {
  sub_tab <- tab[5, ]

  res <- testthat::expect_silent(sub_tab)
  testthat::expect_snapshot(res)

  table_row <- collect_leaves(sub_tab)[[1]]
  row_condition <- has_count_in_any_col(atleast = 2L, col_names = "A: Drug X")
  testthat::expect_false(row_condition(table_row))
  row_condition <- has_count_in_any_col(atleast = 1L, col_names = "A: Drug X")
  testthat::expect_true(row_condition(table_row))
  row_condition <- has_count_in_any_col(atleast = 4L, col_names = c("A: Drug X", "B: Placebo"))
  testthat::expect_true(row_condition(table_row))
})

testthat::test_that("has_fraction_in_cols result works in a special case identical to standard pruning", {
  row_condition <- has_fraction_in_cols(atleast = 0.000001, col_names = names(tab))
  result <- prune_table(tab, keep_rows(row_condition))
  expected <- prune_table(tab)
  testthat::expect_identical(result, expected)
})

testthat::test_that("has_fraction_in_cols result performs comparisons correctly", {
  sub_tab <- tab[5, ]

  res <- testthat::expect_silent(sub_tab)
  testthat::expect_snapshot(res)

  res <- testthat::expect_silent(col_counts(tab))
  testthat::expect_snapshot(res)

  table_row <- collect_leaves(sub_tab)[[1]]
  row_condition <- has_fraction_in_cols(atleast = 0.01, col_names = "A: Drug X")
  testthat::expect_false(row_condition(table_row))
  row_condition <- has_fraction_in_cols(atleast = 0.008, col_names = "A: Drug X")
  testthat::expect_true(row_condition(table_row))
  row_condition <- has_fraction_in_cols(atleast = 0.02, col_names = c("A: Drug X", "B: Placebo"))
  testthat::expect_true(row_condition(table_row))
})

testthat::test_that("has_fraction_in_any_col result performs comparisons correctly", {
  sub_tab <- tab[5, ]

  res <- testthat::expect_silent(sub_tab)
  testthat::expect_snapshot(res)

  table_row <- collect_leaves(sub_tab)[[1]]
  row_condition <- has_fraction_in_any_col(atleast = 0.038, col_names = c("A: Drug X", "C: Combination"))
  testthat::expect_false(row_condition(table_row))
  row_condition <- has_fraction_in_any_col(atleast = 0.036, col_names = c("A: Drug X", "C: Combination"))
  testthat::expect_true(row_condition(table_row))
  row_condition <- has_fraction_in_any_col(atleast = 0.2, col_names = c("A: Drug X", "B: Placebo"))
  testthat::expect_true(row_condition(table_row))
})

testthat::test_that("has_fractions_difference result performs comparisons correctly", {
  sub_tab <- tab[5, ]

  res <- testthat::expect_silent(sub_tab)
  testthat::expect_snapshot(res)

  table_row <- collect_leaves(sub_tab)[[1]]
  row_condition <- has_fractions_difference(atleast = 0.01, col_names = c("A: Drug X", "B: Placebo"))
  testthat::expect_true(row_condition(table_row))
  row_condition <- has_fractions_difference(atleast = 0.5, col_names = c("A: Drug X", "B: Placebo"))
  testthat::expect_false(row_condition(table_row))
  row_condition <- has_fractions_difference(atleast = 0.01, col_names = c("A: Drug X", "C: Combination"))
  testthat::expect_false(row_condition(table_row))
})

testthat::test_that("has_counts_difference result performs comparisons correctly", {
  sub_tab <- tab[5, ]

  res <- testthat::expect_silent(sub_tab)
  testthat::expect_snapshot(res)

  table_row <- collect_leaves(sub_tab)[[1]]
  row_condition <- has_counts_difference(atleast = 3L, col_names = c("A: Drug X", "B: Placebo"))
  testthat::expect_true(row_condition(table_row))
  row_condition <- has_counts_difference(atleast = 4L, col_names = c("A: Drug X", "B: Placebo"))
  testthat::expect_false(row_condition(table_row))
  row_condition <- has_counts_difference(atleast = 1L, col_names = c("A: Drug X", "C: Combination"))
  testthat::expect_false(row_condition(table_row))
})

testthat::test_that("combination of pruning functions works", {
  result <- tab %>%
    prune_table(
      keep_rows(
        has_fractions_difference(atleast = 0.1, c("A: Drug X", "C: Combination")) &
          has_counts_difference(atleast = 2L, c("B: Placebo", "A: Drug X")) &
          has_count_in_cols(atleast = 3L, "A: Drug X")
      )
    )

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
