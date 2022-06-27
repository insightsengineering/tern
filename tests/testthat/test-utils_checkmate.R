# assert_list_of_variables ----

testthat::test_that("assert_list_of_variables is silent with healthy input", {
  testthat::expect_silent(assert_list_of_variables(list(a = "bla", b = "bli")))
  testthat::expect_silent(assert_list_of_variables(list(a = "123")))
  testthat::expect_silent(assert_list_of_variables(list(a = c("bla", "bli"))))
})

testthat::test_that("assert_list_of_variables fails with wrong input", {
  testthat::expect_error(assert_list_of_variables(list("bla", b = "bli")))
  testthat::expect_error(assert_list_of_variables(list(a = 1, b = "bla")))
  testthat::expect_error(assert_list_of_variables(c(a = "blo", b = "bla")))
  testthat::expect_error(assert_list_of_variables(c(a = 1, a = 2)))
})

# assert_df_with_variables ----

testthat::test_that("assert_df_with_variables is silent with healthy input", {
  testthat::expect_silent(assert_df_with_variables(
    df = data.frame(a = 5, b = 3),
    variables = list(val = "a")
  ))
  testthat::expect_silent(assert_df_with_variables(
    df = data.frame(a = 5, b = 3),
    variables = list(val = c("a", "b"))
  ))
  testthat::expect_silent(assert_df_with_variables(
    df = data.frame(a = "A", b = 3),
    variables = list(vars = c("a", "b")),
    na_level = "<Missing>"
  ))
})

testthat::test_that("assert_df_with_variables fails with wrong input", {
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
  testthat::expect_error(assert_df_with_variables(
    df = data.frame(a = "A", b = "<Missing>"),
    variables = list(vars = c("a", "b")),
    na_level = "<Missing>"
  ))
})

# assert_valid_factor ----

testthat::test_that("assert_valid_factor is silent with healthy input", {
  testthat::expect_silent(assert_valid_factor(factor(c("a", "b"))))
  testthat::expect_silent(assert_valid_factor(factor(NA, exclude = factor())))
})

testthat::test_that("assert_valid_factor fails with wrong input", {
  testthat::expect_error(assert_valid_factor(c(5L, 3L)))
  testthat::expect_error(assert_valid_factor(NULL))
  testthat::expect_error(assert_valid_factor(factor(c("a", ""))))
  testthat::expect_error(assert_valid_factor(factor()))
})

# assert_df_with_factors ----

testthat::test_that("assert_df_with_factors is silent with healthy input", {
  testthat::expect_silent(assert_df_with_factors(
    df = data.frame(a = factor("A", levels = c("A", "B")), b = 3),
    variables = list(val = "a")
  ))
  testthat::expect_silent(assert_df_with_factors(
    df = data.frame(a = factor("A", levels = c("A", "B")), b = 3),
    variable = list(val = "a"),
    min.levels = 1
  ))
  testthat::expect_silent(assert_df_with_factors(
    df = data.frame(a = factor("A", levels = c("A", NA, "B")), b = 3),
    variable = list(val = "a"),
    min.levels = 2,
    max.levels = 2
  ))
  testthat::expect_silent(assert_df_with_factors(
    df = data.frame(a = factor(c("A", NA, "B")), b = 3),
    variable = list(val = "a"),
    min.levels = 2,
    max.levels = 2
  ))
})

testthat::test_that("assert_df_with_factors fails with wrong input", {
  testthat::expect_error(assert_df_with_factors(
    df = data.frame(a = 1, b = 3),
    variables = list(val = "a")
  ))
  testthat::expect_error(assert_df_with_factors(
    df = data.frame(a = 1, b = factor("x", levels = c("a", "b", "x"))),
    variable = list(val = "b"),
    min.levels = 5
  ))
  testthat::expect_error(assert_df_with_factors(
    df = data.frame(a = 1, b = factor("x", levels = c("a", "b", "x"))),
    variable = list(val = "b"),
    min.levels = 2,
    max.levels = 2
  ))
  testthat::expect_error(assert_df_with_factors(
    df = data.frame(a = 1, b = factor("x", levels = c("a", "b", "x"))),
    variable = list(val = "b"),
    min.levels = 5,
    max.levels = 3
  ))
  bdf <- data.frame(a = factor(letters[1:3]), b = factor(c(1, 2, 3)), d = 3)
  testthat::expect_error(
    assert_df_with_factors(df = bdf, variables = list(val = "a", val = "b", val = ""))
  )
  testthat::expect_error(
    assert_df_with_factors(df = bdf, variables = list(val = "a", val = "b", val = "d", val = "e"))
  )
  testthat::expect_error(
    assert_df_with_factors(df = bdf, variables = list(val = "a", val = "b", val = "e"))
  )
  testthat::expect_error(
    assert_df_with_factors(df = bdf, variables = list(val = "a", val = "b"), min.levels = 1, max.levels = 1)
  )
})

# assert_equal_length ----

testthat::test_that("assert_equal_length is silent with same-length inputs", {
  testthat::expect_silent(assert_equal_length(1, 5, "car", NA, list(a = list(5, 3))))
})

testthat::test_that("assert_equal_length fails with variable-length inputs", {
  testthat::expect_error(assert_equal_length(1, NULL))
  testthat::expect_error(assert_equal_length(1:10, LETTERS[1:3]))
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
