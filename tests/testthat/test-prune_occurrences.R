get_table <- function() {
  basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE") %>%
    split_rows_by("STRATA1") %>%
    summarize_row_groups() %>%
    summarize_vars("COUNTRY", .stats = "count_fraction") %>%
    build_table(DM)
}

test_that("keep_rows works in a special case identical to standard pruning", {
  tab <- get_table()
  row_condition <- !CombinationFunction(all_zero_or_na)
  pruning_fun <- keep_rows(row_condition)
  expect_is(pruning_fun, "function")
  result <- prune_table(tab, pruning_fun)
  expected <- prune_table(tab)
  expect_identical(result, expected)
})

test_that("keep_rows prunes everything if condition is always `FALSE`", {
  tab <- get_table()
  row_condition <- function(table_row) {
    FALSE
  }
  pruning_fun <- keep_rows(row_condition)
  result <- prune_table(tab, pruning_fun)
  expected <- NULL
  expect_identical(result, expected)
})

test_that("keep_rows keeps everything if condition is always `TRUE`", {
  tab <- get_table()
  row_condition <- function(table_row) {
    TRUE
  }
  pruning_fun <- keep_rows(row_condition)
  result <- prune_table(tab, pruning_fun)
  expected <- tab
  expect_identical(result, expected)
})

test_that("keep_content_rows works as expected", {
  tab <- get_table()
  more_than_twenty <- has_count_in_cols(atleast = 21L, col_names = names(tab))
  result <- prune_table(tab, keep_content_rows(more_than_twenty))
  result_leaves <- collect_leaves(result)
  result_content_rows <- result_leaves[sapply(result_leaves, class) == "ContentRow"]
  result_counts <- result_content_rows %>%
    lapply(h_row_counts, col_names = names(tab)) %>%
    sapply(sum)
  expect_true(all(result_counts > 20))
})

test_that("has_count_in_cols result works in a special case identical to standard pruning", {
  tab <- get_table()
  row_condition <- has_count_in_cols(atleast = 1L, col_names = names(tab))
  result <- prune_table(tab, keep_rows(row_condition))
  expected <- prune_table(tab)
  expect_identical(result, expected)
})

test_that("has_count_in_cols result performs comparisons correctly", {
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
  row_condition <- has_count_in_cols(atleast = 4L, col_names = "A: Drug X")
  expect_false(row_condition(table_row))
  row_condition <- has_count_in_cols(atleast = 1L, col_names = "A: Drug X")
  expect_true(row_condition(table_row))
  row_condition <- has_count_in_cols(atleast = 5L, col_names = c("A: Drug X", "B: Placebo"))
  expect_true(row_condition(table_row))
})

test_that("has_count_in_any_col result performs comparisons correctly", {
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
  row_condition <- has_count_in_any_col(atleast = 2L, col_names = "A: Drug X")
  expect_false(row_condition(table_row))
  row_condition <- has_count_in_any_col(atleast = 1L, col_names = "A: Drug X")
  expect_true(row_condition(table_row))
  row_condition <- has_count_in_any_col(atleast = 4L, col_names = c("A: Drug X", "B: Placebo"))
  expect_true(row_condition(table_row))
})

test_that("has_fraction_in_cols result works in a special case identical to standard pruning", {
  tab <- get_table()
  row_condition <- has_fraction_in_cols(atleast = 0.000001, col_names = names(tab))
  result <- prune_table(tab, keep_rows(row_condition))
  expected <- prune_table(tab)
  expect_identical(result, expected)
})

test_that("has_fraction_in_cols result performs comparisons correctly", {
  tab <- get_table()
  sub_tab <- tab[5, ]
  expect_identical(
    to_string_matrix(sub_tab),
    structure(
      c("", "BRA", "A: Drug X", "1 (3.7%)", "B: Placebo", "4 (20%)", "C: Combination", "1 (3.2%)"),
      .Dim = c(2L, 4L)
    )
  )
  expect_identical(
    col_counts(tab),
    c(121L, 106L, 129L)
  )
  table_row <- collect_leaves(sub_tab)[[1]]
  row_condition <- has_fraction_in_cols(atleast = 0.01, col_names = "A: Drug X")
  expect_false(row_condition(table_row))
  row_condition <- has_fraction_in_cols(atleast = 0.008, col_names = "A: Drug X")
  expect_true(row_condition(table_row))
  row_condition <- has_fraction_in_cols(atleast = 0.02, col_names = c("A: Drug X", "B: Placebo"))
  expect_true(row_condition(table_row))
})

test_that("has_fraction_in_any_col result performs comparisons correctly", {
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
  row_condition <- has_fraction_in_any_col(atleast = 0.038, col_names = c("A: Drug X", "C: Combination"))
  expect_false(row_condition(table_row))
  row_condition <- has_fraction_in_any_col(atleast = 0.036, col_names = c("A: Drug X", "C: Combination"))
  expect_true(row_condition(table_row))
  row_condition <- has_fraction_in_any_col(atleast = 0.2, col_names = c("A: Drug X", "B: Placebo"))
  expect_true(row_condition(table_row))
})

test_that("has_fractions_difference result performs comparisons correctly", {
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
  row_condition <- has_fractions_difference(atleast = 0.01, col_names = c("A: Drug X", "B: Placebo"))
  expect_true(row_condition(table_row))
  row_condition <- has_fractions_difference(atleast = 0.5, col_names = c("A: Drug X", "B: Placebo"))
  expect_false(row_condition(table_row))
  row_condition <- has_fractions_difference(atleast = 0.01, col_names = c("A: Drug X", "C: Combination"))
  expect_false(row_condition(table_row))
})

test_that("has_counts_difference result performs comparisons correctly", {
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
  row_condition <- has_counts_difference(atleast = 3L, col_names = c("A: Drug X", "B: Placebo"))
  expect_true(row_condition(table_row))
  row_condition <- has_counts_difference(atleast = 4L, col_names = c("A: Drug X", "B: Placebo"))
  expect_false(row_condition(table_row))
  row_condition <- has_counts_difference(atleast = 1L, col_names = c("A: Drug X", "C: Combination"))
  expect_false(row_condition(table_row))
})

test_that("combination of pruning functions works", {
  tab <- get_table()
  result <- tab %>%
    prune_table(
      keep_rows(
        has_fractions_difference(atleast = 0.1, c("A: Drug X", "C: Combination")) &
          has_counts_difference(atleast = 2L, c("B: Placebo", "A: Drug X")) &
          has_count_in_cols(atleast = 3L, "A: Drug X")
      )
    )
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "ASIAN", "A", "CHN", "C", "RUS", "WHITE", "B",
      "CHN", "A: Drug X", "", "27 (22.3%)", "14 (51.9%)", "28 (23.1%)",
      "4 (14.3%)", "", "7 (5.8%)", "4 (57.1%)", "B: Placebo", "", "20 (18.9%)",
      "9 (45%)", "19 (17.9%)", "2 (10.5%)", "", "5 (4.7%)", "1 (20%)",
      "C: Combination", "", "31 (24%)", "12 (38.7%)", "31 (24%)", "1 (3.2%)",
      "", "4 (3.1%)", "3 (75%)"),
    .Dim = c(9L, 4L)
  )
  expect_identical(result_matrix, expected_matrix)
})
