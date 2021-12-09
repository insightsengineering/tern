
testthat::test_that("d_onco_rsp_label provide right response labels", {
  rsp <- c("CR", "NE", "PR")
  result <- d_onco_rsp_label(rsp)
  expected <- factor(
    c(CR = "Complete Response (CR)", NE = "Not Evaluable (NE)", PR = "Partial Response (PR)"),
    levels = c("Complete Response (CR)", "Not Evaluable (NE)", "Partial Response (PR)")
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("d_onco_rsp_label describe label with x being a factor", {
  a <- factor(c("CR", "SD", "PR", "PD", "NE"))
  result <- d_onco_rsp_label(a)
  expected <- factor(
    c(CR = "Complete Response (CR)", SD = "Stable Disease (SD)", PR = "Partial Response (PR)",
      PD = "Progressive Disease (PD)", NE = "Not Evaluable (NE)")
  )
  testthat::expect_identical(result, expected)
})

testthat::test_that("s_length_proportion works as expected with healthy input", {
  x <- rep("A", 10)
  n_col <- 20
  result <- s_length_proportion(x = x, .N_col = n_col, method = "jeffreys", conf_level = 0.8)
  expected <- s_proportion(x = rep(c(TRUE, FALSE), c(10, 10)), method = "jeffreys", conf_level = 0.8)
  testthat::expect_identical(result, expected)
})

testthat::test_that("s_length_proportion fails with bad input", {
  testthat::expect_error(s_length_proportion(x = c(1, 1, 1), 10))
  testthat::expect_error(s_length_proportion(x = c("A", "B"), 10))
  testthat::expect_error(s_length_proportion(x = rep("A", 20), 10))
})

testthat::test_that("estimate_multinomial_response returns right result", {

  # Preparation of a common test case for unit tests.
  dta_test <- data.frame(
    USUBJID = paste0("S", 1:12),
    ARM = rep(LETTERS[1:3], each = 4),
    AVAL = c(A = c(1, 1, 1, 1), B = c(0, 0, 1, 1), C = c(0, 0, 0, 0))
  )

  dta_test$AVALC <- as.factor(c( # nolint
    "Complete Response (CR)", "Partial Response (PR)"
  )[dta_test$AVAL + 1])

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    estimate_multinomial_response(var = "AVALC")
  result <- build_table(lyt = lyt, df = dta_test)
  result_matrix <- to_string_matrix(result)

  expected_matrix <- structure(
    c(
      "", "Complete Response (CR)", "95% CI (Wald, with correction)",
      "Partial Response (PR)", "95% CI (Wald, with correction)", "A",
      "0 (0%)", "(0, 12.5)", "4 (100%)", "(87.5, 100)", "B", "2 (50%)",
      "(0, 100)", "2 (50%)", "(0, 100)", "C", "4 (100%)", "(87.5, 100)",
      "0 (0%)", "(0, 12.5)"
    ),
    .Dim = 5:4
  )

  testthat::expect_identical(result_matrix, expected_matrix)
})
