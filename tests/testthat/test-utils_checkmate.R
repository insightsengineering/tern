# assert_character_or_factor ----

testthat::test_that("assert_character_or_factor is TRUE with healthy input", {
  testthat::expect_silent(assert_character_or_factor(c("a", "b")))
  testthat::expect_silent(assert_character_or_factor(factor(c("a", "b"))))
  testthat::expect_silent(assert_character_or_factor(NA_character_))
})

testthat::test_that("assert_character_or_factor is FALSE with wrong input", {
  testthat::expect_error(assert_character_or_factor(c(5L, 3L)))
  testthat::expect_error(assert_character_or_factor(NULL))
})

# assert_nonnegative_count ----

testthat::test_that("assert_nonnegative_count is TRUE with healthy input", {
  testthat::expect_silent(assert_nonnegative_count(5L))
  testthat::expect_silent(assert_nonnegative_count(0L))
  testthat::expect_silent(assert_nonnegative_count(5))
  testthat::expect_silent(assert_nonnegative_count(0))
})

testthat::test_that("assert_nonnegative_count is FALSE with wrong input", {
  testthat::expect_error(assert_nonnegative_count(c(5L, 3L)))
  testthat::expect_error(assert_nonnegative_count(-5L))
  testthat::expect_error(assert_nonnegative_count(NULL))
  testthat::expect_error(assert_nonnegative_count(NA_integer_))
})

# assert_list_of_variables ----

testthat::test_that("assert_list_of_variables is TRUE with healthy input", {
  testthat::expect_silent(assert_list_of_variables(list(a = "bla", b = "bli")))
  testthat::expect_silent(assert_list_of_variables(list(a = "123")))
  testthat::expect_silent(assert_list_of_variables(list(a = c("bla", "bli"))))
})

testthat::test_that("assert_list_of_variables is FALSE with wrong input", {
  testthat::expect_error(assert_list_of_variables(list("bla", b = "bli")))
  testthat::expect_error(assert_list_of_variables(list(a = 1, b = "bla")))
  testthat::expect_error(assert_list_of_variables(c(a = "blo", b = "bla")))
  testthat::expect_error(assert_list_of_variables(c(a = 1, a = 2)))
})

# assert_df_with_variables ----

testthat::test_that("assert_df_with_variables is TRUE with healthy input", {
  testthat::expect_silent(assert_df_with_variables(
    df = data.frame(a = 5, b = 3),
    variables = list(val = "a")
  ))
  testthat::expect_silent(assert_df_with_variables(
    df = data.frame(a = 5, b = 3),
    variables = list(val = c("a", "b"))
  ))
})

testthat::test_that("assert_df_with_variables fails or is FALSE with wrong input", {
  testthat::expect_error(assert_df_with_variables(
    df = matrix(1:6, nrow = 3, ncol = 2),
    variables = list(val = "c")
  ))
  testthat::expect_error(assert_df_with_variables(
    df = data.frame(a = 5, b = 3),
    variables = list(val = "c")
  ))
  testthat::expect_error(assert_df_with_variables(
    df = data.frame(a = 5, b = 3),
    variables = list("c")
  ))
  testthat::expect_error(assert_df_with_variables(
    df = list(a = 5, b = 3),
    variables = list(aval = "b")
  ))
})

# is_valid_factor ----

testthat::test_that("is_valid_factor is TRUE with healthy input", {
  testthat::expect_true(is_valid_factor(factor(c("a", "b"))))
  testthat::expect_true(is_valid_factor(factor(NA, exclude = factor())))
})

testthat::test_that("is_valid_factor is FALSE with wrong input", {
  testthat::expect_false(is_valid_factor(c(5L, 3L)))
  testthat::expect_false(is_valid_factor(NULL))
  testthat::expect_false(is_valid_factor(factor(c("a", ""))))
  testthat::expect_false(is_valid_factor(factor()))
})

# is_valid_character ----

testthat::test_that("is_valid_character is TRUE with healthy input", {
  testthat::expect_true(is_valid_character(c("a", "b")))
})

testthat::test_that("is_valid_character is FALSE with wrong input", {
  testthat::expect_false(is_valid_character(c(1L, 2L)))
  testthat::expect_false(is_valid_character(NULL))
  testthat::expect_false(is_valid_character(c("a", NA)))
  testthat::expect_false(is_valid_character(c("a", "")))
})

# is_equal_length ----

testthat::test_that("is_equal_length is TRUE with same-length inputs", {
  testthat::expect_true(is_equal_length(1, 5, "car", NA, list(a = list(5, 3))))
})

testthat::test_that("is_equal_length is FALSE with variable-length inputs", {
  testthat::expect_false(is_equal_length(1, NULL))
  testthat::expect_false(is_equal_length(1:10, LETTERS[1:3]))
})

# is_proportion ----

testthat::test_that("is_proportion is TRUE with healthy input", {
  testthat::expect_true(is_proportion(0.99))
  testthat::expect_true(is_proportion(0.01))
  testthat::expect_true(is_proportion(0, include_boundaries = TRUE))
  testthat::expect_true(is_proportion(1, include_boundaries = TRUE))
})

testthat::test_that("is_proportion is FALSE with wrong input", {
  testthat::expect_false(is_proportion(0))
  testthat::expect_false(is_proportion(1))
  testthat::expect_false(is_proportion(-1.01))
  testthat::expect_false(is_proportion("abc"))
  testthat::expect_false(is_proportion(c(0.4, 0.3)))
})

# all_elements_in_ref ----

testthat::test_that("all_elements_in_ref is TRUE with healthy input", {
  testthat::expect_true(all_elements_in_ref(x = 1, ref = c(1:3)))
  testthat::expect_true(all_elements_in_ref(x = c(1:2), ref = c(1:3)))
  testthat::expect_true(all_elements_in_ref(x = "a", ref = c("a", "b", "c")))
  testthat::expect_true(all_elements_in_ref(x = c("a", "c"), ref = c("a", "b", "c")))
})

testthat::test_that("all_elements_in_ref is FALSE with wrong input", {
  testthat::expect_false(all_elements_in_ref(x = 4, ref = c(1:3)))
  testthat::expect_false(all_elements_in_ref(x = "z", ref = c("a", "b", "c")))
  testthat::expect_error(all_elements_in_ref(x = character(0), ref = c("a", "b", "c")))
})

# is_proportion_vector ----

testthat::test_that("is_proportion_vector works as expected", {
  testthat::expect_true(is_proportion_vector(0.99))
  testthat::expect_true(is_proportion_vector(c(0.01, 0.5)))
  testthat::expect_false(is_proportion_vector(c(0, 2), include_boundaries = TRUE))
  testthat::expect_false(is_proportion_vector(c(1, -1), include_boundaries = TRUE))
})

# is_quantiles_vector ----

testthat::test_that("is_quantiles_vector works as expected", {
  testthat::expect_true(is_quantiles_vector(c(0.1, 0.3)))
  testthat::expect_false(is_quantiles_vector(c(0.3, 0.1)))
  testthat::expect_false(is_quantiles_vector(c(0.3, 0.3)))
  testthat::expect_true(is_quantiles_vector(0, include_boundaries = TRUE))
})

# has_tabletree_colnames ----

testthat::test_that("has_tabletree_colnames works correctly", {
  tab <- basic_table() %>%
    analyze("SEX") %>%
    build_table(DM)
  testthat::expect_true(has_tabletree_colnames(tab, "all obs"))
  testthat::expect_false(has_tabletree_colnames(tab, c("all obs", "Arm A")))
})

# is_df_with_factors ----

testthat::test_that("is_df_with_factors is TRUE with healthy input", {
  testthat::expect_true(is_df_with_factors(
    df = data.frame(a = factor("A", levels = c("A", "B")), b = 3),
    variables = list(val = "a")
  ))
})

testthat::test_that("is_df_with_factors fails or is FALSE with wrong input", {
  testthat::expect_false(is_df_with_factors(
    df = data.frame(a = 1, b = 3),
    variables = list(val = "a")
  ))
})

# is_df_with_nlevels_factor ----

testthat::test_that("is_df_with_nlevels_factor is TRUE with healthy input", {
  testthat::expect_true(is_df_with_nlevels_factor(
    df = data.frame(a = factor("A", levels = c("A", "B")), b = 3),
    variable = "a",
    n_levels = 2
  ))
})

testthat::test_that("is_df_with_nlevels_factor is FALSE with wrong input", {
  testthat::expect_false(is_df_with_nlevels_factor(
    df = data.frame(a = 1, b = factor("x", levels = c("a", "b", "x"))),
    variable = "b",
    n_levels = 2
  ))
})

testthat::test_that("is_df_with_nlevels_factor gives expected error message for == relation", {
  df <- data.frame(a = 1, b = factor("x", levels = c("a", "b", "x")))
  testthat::expect_error(assertthat::assert_that(is_df_with_nlevels_factor(
    df = df,
    variable = "b",
    n_levels = 2
  )), "variable b in data frame df should have exactly 2 levels, but has 3 levels: a, b, x")
})

testthat::test_that("is_df_with_nlevels_factor is TRUE with healthy input for >= relation", {
  testthat::expect_true(is_df_with_nlevels_factor(
    df = data.frame(a = factor("A", levels = c("A", "B")), b = 3),
    variable = "a",
    n_levels = 1,
    relation = ">="
  ))
})

testthat::test_that("is_df_with_nlevels_factor is FALSE with wrong input for >= relation", {
  testthat::expect_false(is_df_with_nlevels_factor(
    df = data.frame(a = 1, b = factor("x", levels = c("a", "b", "x"))),
    variable = "b",
    n_levels = 5,
    relation = ">="
  ))
})

testthat::test_that("is_df_with_nlevels_factor gives expected error message for >= relation", {
  df <- data.frame(a = 1, b = factor("x", levels = c("a", "b", "x")))
  testthat::expect_error(assertthat::assert_that(is_df_with_nlevels_factor(
    df = df,
    variable = "b",
    n_levels = 5,
    relation = ">="
  )), "variable b in data frame df should have at least 5 levels, but has 3 levels: a, b, x")
})

# is_df_with_no_na_level ----

testthat::test_that("is_df_with_no_na_level is TRUE with healthy input", {
  testthat::expect_true(is_df_with_no_na_level(
    df = data.frame(a = "A", b = 3),
    variables = list(vars = c("a", "b")),
    na_level = "<Missing>"
  ))
})

testthat::test_that("is_df_with_no_na_level is FALSE with missing data", {
  testthat::expect_false(is_df_with_no_na_level(
    df = data.frame(a = "A", b = "<Missing>"),
    variables = list(vars = c("a", "b")),
    na_level = "<Missing>"
  ))
})
