testthat::test_that("d_onco_rsp_label provide right response labels", {
  rsp <- c("CR", "NE", "PR")
  result <- d_onco_rsp_label(rsp)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("d_onco_rsp_label describe label with x being a factor", {
  a <- factor(c("CR", "SD", "PR", "PD", "NE"))
  result <- d_onco_rsp_label(a)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})

testthat::test_that("s_length_proportion works as expected with healthy input", {
  x <- rep("A", 10)
  n_col <- 20
  result <- s_length_proportion(x = x, .N_col = n_col, method = "jeffreys", conf_level = 0.8)
  expected <- s_proportion(df = rep(c(TRUE, FALSE), c(10, 10)), method = "jeffreys", conf_level = 0.8)
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
  dta_test$AVALC <- as.factor(c(
    "Complete Response (CR)", "Partial Response (PR)"
  )[dta_test$AVAL + 1])

  lyt <- basic_table() %>%
    split_cols_by(var = "ARM") %>%
    estimate_multinomial_response(var = "AVALC")
  result <- build_table(lyt = lyt, df = dta_test)

  res <- testthat::expect_silent(result)
  testthat::expect_snapshot(res)
})
