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
})

test_that("is_nonnegative_count is FALSE with wrong input", {
  expect_false(is_nonnegative_count(c(5L, 3L)))
  expect_false(is_nonnegative_count(- 5L))
  expect_false(is_nonnegative_count(1))
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
