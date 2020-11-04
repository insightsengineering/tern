test_that("is_character_or_factor is TRUE with healthy input", {
  expect_true(is_character_or_factor(c("a", "b")))
  expect_true(is_character_or_factor(factor(c("a", "b"))))
  expect_true(is_character_or_factor(NA_character_))
})

test_that("is_character_or_factor is FALSE with wrong input", {
  expect_false(is_character_or_factor(c(5L, 3L)))
  expect_false(is_character_or_factor(NULL))
})

test_that("is_nonnegative_count is TRUE with healthy input", {
  expect_true(is_nonnegative_count(5L))
  expect_true(is_nonnegative_count(0L))
  expect_true(is_nonnegative_count(5))
  expect_true(is_nonnegative_count(0))
})

test_that("is_nonnegative_count is FALSE with wrong input", {
  expect_false(is_nonnegative_count(c(5L, 3L)))
  expect_false(is_nonnegative_count(- 5L))
  expect_false(is_nonnegative_count(NULL))
  expect_false(is_nonnegative_count(NA_integer_))
})

test_that("is_variables is TRUE with healthy input", {
  expect_true(is_variables(list(a = "bla", b = "bli")))
  expect_true(is_variables(list(a = "123")))
})

test_that("is_variables is FALSE with wrong input", {
  expect_false(is_variables(list("bla", b = "bli")))
  expect_false(is_variables(list(a = 1, b = "bla")))
  expect_false(is_variables(c(a = "blo", b = "bla")))
})

test_that("is_df_with_variables is TRUE with healthy input", {
  expect_true(is_df_with_variables(
    df = data.frame(a = 5, b = 3),
    variables = list(val = "a")
  ))
})

test_that("is_df_with_variables fails or is FALSE with wrong input", {
  expect_false(is_df_with_variables(
    df = data.frame(a = 5, b = 3),
    variables = list(val = "c")
  ))
  expect_error(is_df_with_variables(
    df = data.frame(a = 5, b = 3),
    variables = list("c")
  ))
  expect_error(is_df_with_variables(
    df = list(a = 5, b = 3),
    variables = list(aval = "b")
  ))
})

test_that("is_valid_factor is TRUE with healthy input", {
  expect_true(is_valid_factor(factor(c("a", "b"))))
  expect_true(is_valid_factor(factor(NA, exclude = factor())))
})

test_that("is_valid_factor is FALSE with wrong input", {
  expect_false(is_valid_factor(c(5L, 3L)))
  expect_false(is_valid_factor(NULL))
  expect_false(is_valid_factor(factor(c("a", ""))))
  expect_false(is_valid_factor(factor()))
})

test_that("is_valid_character is TRUE with healthy input", {
  expect_true(is_valid_character(c("a", "b")))
})

test_that("is_valid_character is FALSE with wrong input", {
  expect_false(is_valid_character(c(1L, 2L)))
  expect_false(is_valid_character(NULL))
  expect_false(is_valid_character(c("a", NA)))
  expect_false(is_valid_character(c("a", "")))
})

test_that("is_equal_length is TRUE with same-length inputs", {
  expect_true(is_equal_length(1, 5, "car", NA, list(a = list(5, 3))))
})

test_that("is_equal_length is FALSE with variable-length inputs", {
  expect_false(is_equal_length(1, NULL))
  expect_false(is_equal_length(1:10, LETTERS[1:3]))
})

test_that("is_proportion is TRUE with healthy input", {
  expect_true(is_proportion(0.99))
  expect_true(is_proportion(0.01))
  expect_true(is_proportion(0, include_boundaries = TRUE))
  expect_true(is_proportion(1, include_boundaries = TRUE))
})

test_that("is_proportion is FALSE with wrong input", {
  expect_false(is_proportion(0))
  expect_false(is_proportion(1))
  expect_false(is_proportion(-1.01))
  expect_false(is_proportion("abc"))
  expect_false(is_proportion(c(0.4, 0.3)))
})

test_that("all_elements_in_ref is TRUE with healthy input", {
  expect_true(all_elements_in_ref(x = 1, ref = c(1:3)))
  expect_true(all_elements_in_ref(x = c(1:2), ref = c(1:3)))
  expect_true(all_elements_in_ref(x = "a", ref = c("a", "b", "c")))
  expect_true(all_elements_in_ref(x = c("a", "c"), ref = c("a", "b", "c")))
})

test_that("all_elements_in_ref is FALSE with wrong input", {
  expect_false(all_elements_in_ref(x = 4, ref = c(1:3)))
  expect_false(all_elements_in_ref(x = "z", ref = c("a", "b", "c")))
  expect_error(all_elements_in_ref(x = character(0), ref = c("a", "b", "c")))
})

test_that("is_proportion_vector works as expected", {
  expect_true(is_proportion_vector(0.99))
  expect_true(is_proportion_vector(c(0.01, 0.5)))
  expect_false(is_proportion_vector(c(0, 2), include_boundaries = TRUE))
  expect_false(is_proportion_vector(c(1, -1), include_boundaries = TRUE))
})

test_that("has_tabletree_colnames works correctly", {
  tab <- basic_table() %>%
    analyze("SEX") %>%
    build_table(DM)
  expect_true(has_tabletree_colnames(tab, "all obs"))
  expect_false(has_tabletree_colnames(tab, c("all obs", "Arm A")))
})
